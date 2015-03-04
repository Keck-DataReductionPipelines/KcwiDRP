;+
; NAME:
;  bidr2
; PURPOSE: (one line)
;  Compute the bi-directional reflectance (newer Hapke formula).
; DESCRIPTION:
;    This function is coded from equation 12.55 on page 346 in Hapke's book,
;    "Theory of Reflectance and Emittance Spectroscopy".
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  ans = bidr2(w,emu,imu,g,holes,p,b0,theta)
; INPUTS:
;  w     - Single scattering albedo.
;  emu   - Cosine of the emission angle.
;  imu   - Cosine of the incidence angle.
;  g     - Phase angle, in radians.
;  holes - Compaction parameter value (1986 formalism).
;  p     - Parameters of the single particle phase function
;          (default "function" is a constant of value p).
;          There are four legal input forms for p:
;            1. a scalar
;            2. an array of dimensionality same as w, emu, imu, etc.
;            3. an array of dimensionality Pparms
;            4. an array of dimensionality (n_elements(w),Pparms)
;  b0    - Backscatter value.
;  theta - Surface roughness value.  (radians)
; OPTIONAL INPUT PARAMETERS:
;  None.
; KEYWORD PARAMETERS:
;  H93   - Flag, if set, uses the 1993 version of Hapke's approximation to the
;            Chandresekar H function.  The 1993 version is more accurate but
;            considerably slower to compute.
;  Pfn   - Specify function to use for P(g) instead of the default constant.
;            The function must be a procedure taking arguments g,a,F,/radians
;              g  phase angle in radians (with keyword /radians set)
;              a  an array of Pparms parameters
;              F  phase function evaluated at phase angles g
;            Use "fn_hg3.pro" as a model.
;  Pparms- Specify number of parameters to be passed to P(g).
;             Really only needed if the number of parameters isn't one.
;  pedantic - Flag, if set, returns NaN for w > 1.
; OUTPUTS:
;  Return value is the bi-directional reflectance.
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
;  None.
; RESTRICTIONS:
;  Any input may be a vector.  If more than one is a vector then the
;     lengths must match.  The return will have the same dimensions as
;     the input.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1997/08/21
;  1997/09/18, MWB, added surface roughness parameter
;  2001/10/29, WMG, added "Pfn", "Pparms", and "pedantic" parameters
;  2003/04/03, MWB, changes loop counters to long
;  2004/06/24, MWB, fixed bug in imue equation, error in calculated
;                       value happened to be small but was a real
;                       computational error.  Thanks to Frank Seelos,
;                       EPRSL, Washington University in St. Louis
;-
function bidr2,in_w,in_emu,in_imu,in_g,in_holes,in_p,in_b0,in_theta, $
               H93=h93,Pfn=Pfn,Pparms=Pparms,pedantic=pedantic

   h93 = keyword_set(h93)
   if not keyword_set(Pparms) then Pparms = 1
   check=[n_elements(in_w),n_elements(in_emu),n_elements(in_imu), $
          n_elements(in_g),n_elements(in_holes),n_elements(in_p)/Pparms, $
          n_elements(in_b0),n_elements(in_theta)]
   tlen = max(check)
   z=where(check ne 1 and check ne tlen,count)
   if count ne 0 then begin
      print,'BIDR2: Error, lengths of inputs must match or be 1.'
      return,0.0
   endif

   ; Promote all inputs to same length
   if n_elements(in_w)     eq 1 then w     = replicate(in_w,    tlen) else w     = in_w
   if n_elements(in_emu)   eq 1 then emu   = replicate(in_emu,  tlen) else emu   = in_emu
   if n_elements(in_imu)   eq 1 then imu   = replicate(in_imu,  tlen) else imu   = in_imu
   if n_elements(in_g)     eq 1 then g     = replicate(in_g,    tlen) else g     = in_g
   if n_elements(in_holes) eq 1 then holes = replicate(in_holes,tlen) else holes = in_holes
   if n_elements(in_b0)    eq 1 then b0    = replicate(in_b0,   tlen) else b0    = in_b0
   if n_elements(in_theta) eq 1 then theta = replicate(in_theta,tlen) else theta = in_theta

   ; Four P input options: p(scalar), p[Pparms], p[tlen], p[tlen,Pparms]
   unhappy = 1

   ; p[tlen,Pparms]
   if n_elements(in_p) eq tlen*Pparms and Pparms gt 1 then begin
      sz = size(in_p)
      if sz[1] eq tlen and sz[2] eq Pparms then begin
         p = in_p
         unhappy = 0
      endif
   endif

   ; p[tlen]
   if n_elements(in_p) eq tlen and Pparms eq 1 then begin
      p = fltarr(tlen,Pparms)
      ; this works for both p[tlen,1] and p[tlen]
      for i=0,tlen-1 do p[i,0] = in_p[i]
      unhappy = 0
   endif

   ; p[Pparms]
   if n_elements(in_p) eq Pparms and Pparms gt 1 then begin
      p = fltarr(tlen,Pparms)
      ; this works for both p[1,Pparms] and p[Pparms]
      for i=0,tlen-1 do p[i,*] = in_p
      unhappy = 0
   endif

   ; p(scalar)
   if n_elements(in_p) eq 1 and Pparms eq 1 then begin
      p = fltarr(tlen,Pparms)
      for i=0L,tlen-1 do p[i,0] = in_p
      unhappy = 0
   endif

   if unhappy then begin
      print,'BIDR2: Error, dimensionality of input P is bad'
      return,0.0
   endif

   gamma = sqrt(1.-w)
   if not keyword_set(pedantic) then begin
      ; Unless we're in pedantic mode, this makes bidr2 return a non-NaN
      ; value where w > 1 (a meaningless situation, but it often arises
      ; when using optical constants which have some associated noise,
      ; or when iteratively fitting observed reflectances).
      z = where(w gt 1.,count)
      if count gt 0 then gamma[z] = -sqrt(w[z]-1.)
   endif

   bscat=replicate(0.,tlen)
   m = where( g lt !pi/2 and b0 ne 0.0, count)
   if count ne 0 then begin
      bscat[m] = 1.0/(1.0 + 1.0/holes[m]*tan(g[m]/2.0))
   end
   bscat = b0*bscat

   bidr = fltarr(tlen)

   ; This is the case where the surface roughness term is turned off.
   z=where(theta eq 0.0,count)
   if count ne 0 then begin

      twoemu = emu[z] + emu[z]
      twoimu = imu[z] + imu[z]

      if h93 then begin
         r0 = 2.0/(1.0+gamma[z])-1.0
         lnt = replicate(0.0,count)
         zn0 = where(emu[z] ne 0.0)
         if zn0[0] ne -1 then lnt[zn0]=alog((1.0+emu[z[zn0]])/emu[z[zn0]])
         hobs = 1.0/(1.0 - (1.0-gamma[z])*emu[z]*(r0+(1.0-0.5*r0-r0*emu[z])*lnt))
         lnt = replicate(0.0,count)
         zn0 = where(imu[z] ne 0.0)
         if zn0[0] ne -1 then lnt[zn0]=alog((1.0+imu[z[zn0]])/imu[z[zn0]])
         hsun = 1.0/(1.0 - (1.0-gamma[z])*imu[z]*(r0+(1.0-0.5*r0-r0*imu[z])*lnt))
      endif else begin
         hobs = (1+twoemu) / (1 + gamma[z] * twoemu)
         hsun = (1+twoimu) / (1 + gamma[z] * twoimu)
      endelse

      if not keyword_set(Pfn) then Pp = reform(p[z,0]) else begin
         Pp = fltarr(n_elements(z))
         for i=0L,n_elements(z)-1 do begin
            call_procedure,Pfn,g[z[i]],reform(p[z[i],*]),F,/radians
            Pp[i] = F
         endfor
      endelse

      bidr[z] = w[z]*imu[z]/(imu[z]+emu[z])*((1+bscat[z])*Pp+hobs*hsun-1)/(4*!pi)

   endif

   ; With surface roughness
   z=where(theta ne 0.0,count)
   if count ne 0 then begin

      emue  = emu[z]
      imue  = imu[z]
      emue0 = emu[z]
      imue0 = imu[z]
      sfun  = emu[z]

      ; ancillary values
      tanthe  = tan(theta[z])
      costhe  = cos(theta[z])
      cotthe  = 1.0/tanthe
      cotthe2 = cotthe^2

      i       = acos(imu[z])
      sini    = sin(i)
      e       = acos(emu[z])
      sine    = sin(e)

      cosg    = cos(g[z])
      cosphi  = replicate(1.0,count)
      zz = where(i*e ne 0.0,countzz)
      if countzz ne 0 then $
         cosphi[zz]=(cosg - imu[z[zz]]*emu[z[zz]])/(sini[zz]*sine[zz])
      zz = where(cosphi ge 1.0,countzz)
      if countzz ne 0 then cosphi[zz] = 1.0
      zz = where(cosphi le -1.0,countzz)
      if countzz ne 0 then cosphi[zz] = -1.0
      ; If cosphi hits -1, then fg = exp(-2*tan(phi/2.0)) becomes infinite
      cosphi = (cosphi > (-0.999999999D))
      phi     = acos(cosphi)
      sinphi2_2=sin(phi/2.0)^2

      gold=1.0e-7
      z0=where(abs(sini) lt gold,count0)
      if count0 ne 0 then sini[z0]=gold
      z0=where(abs(sine) lt gold,count0)
      if count0 ne 0 then sine[z0]=gold

      coti    = imu[z]/sini
      coti2   = coti^2
      cote    = emu[z]/sine
      cote2   = cote^2

      e1i = exp( -2.0/!pi*cotthe*coti   )    ; eqn. 12.45b, p. 344
      e2i = exp( -1.0/!pi*cotthe2*coti2 )    ; eqn. 12.45c, p. 344
      e1e = exp( -2.0/!pi*cotthe*cote   )
      e2e = exp( -1.0/!pi*cotthe2*cote2 )

      chi = 1.0/sqrt(1.0+!pi*tanthe^2)       ; eqn. 12.45a, p. 344
      fg = exp( -2.0 * tan(phi/2.0) )        ; eqn. 12.51, p. 345

      emue0 = chi * ( emu[z] + sine * tanthe * e2e / ( 2.0 - e1e ) )
      imue0 = chi * ( imu[z] + sini * tanthe * e2i / ( 2.0 - e1i ) )

      ; e >= i
      zz = where(e ge i,countee)
      if countee ne 0 then begin
         denom = 2.0 - e1e[zz] - (phi[zz]/!pi)*e1i[zz]
         imue[zz] = chi[zz] * ( imu[z[zz]] + sini[zz] * tanthe[zz] * $
                    ( cosphi[zz]*e2e[zz] + sinphi2_2[zz]*e2i[zz] ) / denom )
         emue[zz] = chi[zz] * ( emu[z[zz]] + sine[zz] * tanthe[zz] * $
                    ( e2e[zz] - sinphi2_2[zz]*e2i[zz] ) / denom )
         sfun[zz] = emue[zz]/emue0[zz] * imu[z[zz]]/imue0[zz] * chi[zz] / $
                    ( 1.0 - fg[zz] + fg[zz]*chi[zz]*imu[z[zz]]/imue0[zz] )
      endif

      ; e < i
      zz = where(e lt i,countee)
      if countee ne 0 then begin
         denom = 2.0 - e1i[zz] - (phi[zz]/!pi)*e1e[zz]
         imue[zz] = chi[zz] * ( imu[z[zz]] + sini[zz] * tanthe[zz] * $
                    ( e2i[zz] - sinphi2_2[zz]*e2e[zz] ) / denom )
         emue[zz] = chi[zz] * ( emu[z[zz]] + sine[zz] * tanthe[zz] * $
                    ( cosphi[zz]*e2i[zz] + sinphi2_2[zz]*e2e[zz] ) / denom )
         sfun[zz] = emue[zz]/emue0[zz] * imu[z[zz]]/imue0[zz] * chi[zz] / $
                    ( 1.0 - fg[zz] + fg[zz]*chi[zz]*emu[z[zz]]/emue0[zz] )
      endif

      twoemu = emue + emue
      twoimu = imue + imue

      if h93 then begin
         r0 = 2.0/(1.0+gamma[z])-1.0
         lnt = replicate(0.0,count)
         zn0 = where(emue ne 0.0)
         if zn0[0] ne -1 then lnt[zn0]=alog((1.0+emue[zn0])/emue[zn0])
         hobs = 1.0/(1.0 - (1.0-gamma[z])*emue*(r0+(1.0-0.5*r0-r0*emue)*lnt))
         lnt = replicate(0.0,count)
         zn0 = where(imue ne 0.0)
         if zn0[0] ne -1 then lnt[zn0]=alog((1.0+imue[zn0])/imue[zn0])
         hsun = 1.0/(1.0 - (1.0-gamma[z])*imue*(r0+(1.0-0.5*r0-r0*imue)*lnt))
      endif else begin
         hobs = (1+twoemu) / (1 + gamma[z] * twoemu)
         hsun = (1+twoimu) / (1 + gamma[z] * twoimu)
      endelse

      if not keyword_set(Pfn) then Pp = reform(p[z,0]) else begin
         Pp = fltarr(n_elements(z))
         for i=0L,n_elements(z)-1 do begin
            call_procedure,Pfn,g[z[i]],reform(p[z[i],*]),F,/radians
            Pp[i] = F
         endfor
      endelse

      bidr[z] = w[z]*imue/(imue+emue)*((1+bscat[z])*Pp+hobs*hsun-1)/(4*!pi)*sfun

   endif

   err=check_math()
   if (err and '1B'x) ne 0 then $ 
      print,'BIDR: Math error detected, code ',err

   return,bidr

end

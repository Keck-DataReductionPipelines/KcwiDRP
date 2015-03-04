;+
; NAME:
;  nic2src
; PURPOSE:
;  Detangle two sources from single NICMOS Grism image data.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  nic2src,obs
; INPUTS:
;  obs - anonymous structure containing the NICMOS data.  This structure
;           is initially created by NICINIT.PRO
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;  com_nic2src  - Used only to communicate between this procedure and its
;                    internal function, nic2src_fn.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1999 Aug 31
;
;-
function nic2src_fn, y, f

common com_nic2src,xgrid,ypsi,yo,hwid,np1,np2

   ; first source, interpolate master profile on input grid.  By fiat
   ;   the profile is centered on 0 and the grid is on this same grid.
   interp,xgrid,ypsi,y,prof1

   ; second source is a bit more complicated.
   dy = yo[1]-yo[0]
   interp,xgrid,ypsi,y-dy,prof2
   z=where(y-dy lt -hwid or y-dy gt hwid,count)
   if count ne 0 then prof2[z]=0.0

;   prof1 = prof1/total(prof1)
;   prof2 = prof2/total(prof2)
;print,n_elements(y),y

   prof1=prof1/np1
   prof2=prof2/np2

   return,[ [prof1], [prof2], [replicate(1.0,n_elements(y))] ]

end

pro nic2src,obs,PSF=in_psf

common com_nic2src,xgrid,ypsi,yo,hwid,np1,np2

   if badpar(obs,8,1,CALLER='NIC2SRC: (obs) ') then return
   if badpar(in_psf,[0,4,7],[0,2],CALLER='NIC2SRC: (PSF) ', $
                               DEFAULT='',RANK=forcepsf) then return

   if forcepsf eq 0 then begin
      if in_psf ne '' then begin
         if exists(in_psf) then begin
            psf = readfits(in_psf,/silent)
            rank = size(psf,/n_dimen)
            if rank ne 2 then begin
               print,'NIC2SRC: Error!  psf in file ',in_psf,' has rank of ', $
                     strn(rank),' but should be 2.'
               return
            endif
            forcepsf = 1
         endif else begin
            print,'NIC2SRC: Error!  psf file ',in_psf,' was not found.  Aborting!'
            return
         endelse
      endif else begin
         psf=''
      endelse
   endif else begin
      psf = in_psf
      forcepsf = 1
   endelse

   if forcepsf and n_elements(obs.xgrid) ne n_elements(psf[*,0]) then begin
      print,'NIC2SRC: Error!  psf array in file ',in_psf,' is the wrong size.'
      return
   endif

   if obs.nobj ne 2 then begin
      print,'NIC2SRC:  Error!  The observation structure has not defined two objects.'
      return
   endif

   setwin,0,xsize=400,ysize=300
   plot,obs.xgrid,obs.ypsi

   ; If this is the first pass of this routine, must create the first
   ;   approximation to the profile.
   if obs.pass eq 0 then begin

      obs.wspec[*,0] = obs.wave
      obs.wspec[*,1] = obs.wave + 0.01152*(obs.xo[1]-obs.xo[0])

      ; Find the "middle" or the peak of the profile.
      zp = where(obs.yps eq max(obs.yps))

      ryps = reverse(obs.yps)
      zp1 = where(ryps eq max(obs.yps))

      shft = zp[0]-zp1[0]
      ryps = shift(ryps,shft)
      if shft gt 0 then begin
         ryps[0:shft-1] = 0.0
      endif else if shft lt 0 then begin
         ryps[n_elements(ryps)+shft:*] = 0.0
      endif

      oplot,obs.xgrid,ryps,color=100

      onep=obs.yps < ryps

      z=where(abs(obs.xgrid) ge 5.0,count)
      if count ne 0 then begin
         onep[z] = 0.0
      endif else begin
         ; force the first two and last two points in the profile to zero
         onep[0:1] = 0.0
         onep[n_elements(onep)-2:n_elements(onep)-1] = 0.0
      endelse

      oplot,obs.xgrid,onep,color=60
      renorm = int_tabulated(obs.xgrid,onep)

      obs.ypsi = onep/renorm

   endif else begin
      oplot,obs.xgrid,obs.yps,color=100
   endelse

   ; Setup some arrays that need to be filled.
   spec0 = fltarr(obs.npts)
   spec1 = fltarr(obs.npts)
   err0  = fltarr(obs.npts)
   err1  = fltarr(obs.npts)
   chisq = fltarr(obs.npts)
   a1    = fltarr(obs.npts)  ; center-of-light (position)
   bck   = fltarr(obs.npts)

   ; Get setup for fitting, install forced psf if used.
   if forcepsf then begin
      ; Must match the position of the imposed psf to that of the current
      ;   numerical psf.

      ; First compute the centroided center of the core of the psf
      z=where(obs.ypsi ge max(obs.ypsi)*0.4)
      range = minmax(obs.xgrid[z])
      z=where(obs.xgrid ge range[0] and obs.xgrid le range[1])
      ix = findgen(n_elements(obs.xgrid))
      nloc = total(ix[z]*obs.ypsi[z])/total(obs.ypsi[z])

      ; Now compute the centroided location of the core of the imposed psf
      z=where(psf[*,1] ge max(psf[*,1])*0.4)
      range = minmax(psf[z,0])
      z=where(psf[*,0] ge range[0] and psf[*,0] le range[1])
      floc = total(ix[z]*psf[z,1])/total(psf[z,1])

;print,'shifting given psf by ',nloc-floc,' interpolated pixels'
      obs.ypsi = sshift(psf[*,1],nloc-floc)
      obs.forcepsf = 1
      yps = obs.ypsi

      oplot,obs.xgrid,yps,color=110

   endif else begin
      obs.forcepsf = 0
   endelse
   ypsi  = obs.ypsi
   xgrid = obs.xgrid
   yo   = obs.yo
   hwid = obs.hwid

   for pick=obs.x1,obs.x2 do begin
      i=pick-obs.x1

      yv = obs.y-obs.yc[pick]
      z=where(yv ge -obs.hwid and yv le obs.hwid)
      yv=yv[z]
      dat = obs.image[pick,z]
      dat=dat[*]

      np1 = 1.0
      np2 = 1.0
      cprof=nic2src_fn(yv,3)
      np1 = total(cprof[*,0])
      np2 = total(cprof[*,1])

      result = svdfit(yv,dat,3,function_name='nic2src_fn',sigma=dr,chisq=chisq0)
      cprof=nic2src_fn(yv,3)

;setwin,12
;plot,yv,cprof[*,0],psym=-5
;oplot,yv,cprof[*,1],psym=-6
;zzzz=''
;read,prompt=strn(total(cprof[*,0]))+' '+strn(total(cprof[*,1]))+' continue?',zzzz

      spec0[i] = result[0]
      spec1[i] = result[1]
      bck[i]   = result[2]
      err0[i]  = dr[0]
      err1[i]  = dr[1]
      chisq[i] = chisq0

      colprof = (dat-cprof[*,1]*result[1]-result[2])/result[0]

      a1[i] = total(colprof*obs.y[z])/total(colprof)

      if i eq 0 then begin
         xp = yv
         yp = colprof
      endif else begin
         xp = [xp,yv]
         yp = [yp,colprof]
      endelse

   endfor

   setwin,1,xsize=400,ysize=300
   plot,xp,yp,psym=3,yr=[-0.05,0.65]

   if not forcepsf then begin
      lowess,xp,(yp<1.0) > (-0.1),0.5,ypsmoo,order=3,newx=obs.xgrid
      lowess,xp,(yp<1.0) > (-0.1),1.0,ypsmoo1,order=1,newx=obs.xgrid
      wprof = exp(-(obs.xgrid/3.0)^2)
      yps = (ypsmoo*wprof + ypsmoo1*(1.0-wprof)) > (-0.1)
   ;   yps = ypsmoo > (-0.1)

      oplot,obs.xgrid,wprof*0.5+0.1,color=40
      oplot,obs.xgrid,(1.0-wprof)*0.5+0.1,color=40

      interp,obs.xgrid,yps,xp,ypfit
      sbad=bytarr(n_elements(yp))
      robomean,yp-ypfit,3.0,0.5,savg,dummy,sstdev,bad=sbad
      zg = where(sbad eq 0)
      xp = xp[zg]
      yp = yp[zg]
      lowess,xp,(yp<1.0) > (-0.1),0.5,ypsmoo,order=3,newx=obs.xgrid
      lowess,xp,(yp<1.0) > (-0.1),1.0,ypsmoo1,order=1,newx=obs.xgrid
      yps = (ypsmoo*wprof + ypsmoo1*(1.0-wprof)) ; > 0.0
   ;   yps = ypsmoo > 0.0

      dyth = 0.01
      ; Scan the left side of the profile from 1/e to edge.  The profile
      ;    must descend, enforce this.
      left = max(where(obs.xgrid lt -1.0))
      for is=left-1,0,-1 do $
         if yps[is]-yps[is+1] gt dyth and yps[is] gt 0.0 then yps[is]=yps[is+1]+dyth

      ; Do the same for the right side of the profile.
      right = min(where(obs.xgrid gt 1.0))
      for is=right+1,n_elements(obs.xgrid)-1 do $
         if yps[is-1]-yps[is] gt dyth and yps[is] gt 0.0 then yps[is]=yps[is-1]+dyth

      z=where(abs(obs.xgrid) ge 5.0,count)
      if count ne 0 then begin
         yps[z] = 0.0
      endif else begin
         ; force the first two and last two points in the profile to zero
         yps[0:1] = 0.0
         yps[n_elements(yps)-2:n_elements(yps)-1] = 0.0
      endelse

   ;   obs.ypsi = (ypsmoo+obs.ypsi)/2.0
   ;   renorm = int_tabulated(xgrid,obs.ypsi)
      renorm = int_tabulated(xgrid,yps)
      yps = yps/renorm
      oplot,obs.xgrid,ypsmoo,color=90
   endif

   oplot,obs.xgrid,obs.ypsi,color=70
   oplot,obs.xgrid,yps,color=60

   setwin,2,xsize=400,ysize=300
   ploterror,obs.wave,spec0,err0

   setwin,3,xsize=400,ysize=300
   ploterror,obs.wave,spec1,err1

   setwin,4,xsize=400,ysize=300
   plot,obs.wave,chisq

   setwin,5,xsize=400,ysize=300

   z=where(abs(obs.yc[obs.x1:obs.x2]-a1) le 0.5)

   coeffs=goodpoly(obs.x[z+obs.x1],a1[z],1,3.0,yfit,newx,newy)
   yc=poly(obs.x,coeffs)

   plot,obs.x[z+obs.x1],obs.yc[z+obs.x1]-a1[z],psym=5 ;,yr=[-1,1]*5.0
   oplot,obs.x,obs.yc-yc

   setwin,6,xsize=400,ysize=300
   plot,obs.wave,bck

   obs.ypsi = yps

   obs.ycnew = yc
   obs.ygn   = a1[obs.xg[0:obs.ngood-1]-obs.x1]
   obs.coeffn = coeffs
   obs.ycnewg = 1

   obs.ospec[*,0] = spec0
   obs.ospec[*,1] = spec1
   obs.oerr[*,0]  = err0
   obs.oerr[*,1]  = err1

   if not forcepsf or obs.pass eq 0 then $
      obs.pass = obs.pass+1

end

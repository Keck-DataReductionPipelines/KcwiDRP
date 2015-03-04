;+
; NAME:
;  kepler1
; PURPOSE:   (one line only)
;  Solve Kepler's Equation (small eccentricity)
; DESCRIPTION:
;  Solve Kepler's Equation. Method by S. Mikkola (1987) Celestial
;     Mechanics, 40 , 329-334. 
;    result from Mikkola then used as starting value for
;       Newton-Raphson iteration to extend the applicability of this
;       function to higher eccentricities, Courtesy of Joern Wilms.
; CATEGORY:
;  Celestial Mechanics
; CALLING SEQUENCE:
;  kepler1,m,ecc,eccanom
; INPUTS:
;  m    - Mean anomaly (radians, can be an array)
;  ecc  - Eccentricity
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  THRESH -  stopping criterion for the Newton Raphson iteration; the
;            iteration stops once abs(E-Eold)<thresh
; OUTPUTS:
;  eccanom - Eccentric Anomaly (radians)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/05/29 - Marc W. Buie, Lowell Observatory.  Ported from fortran routines
;    supplied by Larry Wasserman and Ted Bowell.
;  2002/09/09 -- Joern Wilms, IAA Tuebingen, Astronomie.
;    use analytical values obtained for the low eccentricity case as
;    starting values for a Newton-Raphson method to allow high
;    eccentricity values as well
;  2005/09/14, MWB, modified kepler1 to incoporate Wilms keplereq routine.
;-
pro kepler1,in_m,in_ecc,eccanom,THRESH=thresh

   self='kepler1: '
   if badpar(in_m,[2,3,4,5],[0,1],caller=self+'(m) ', $
                    npts=npts_m) then return
   if badpar(in_ecc,[2,3,4,5],[0,1],caller=self+'(ecc) ', $
                    npts=npts_ecc) then return
   if badpar(thresh,[0,2,3,4,5],0,caller=self+'(THRESH) ', $
                default=1.0d-5) then return

   if npts_ecc eq 1 then begin
      if in_ecc lt 0. or in_ecc ge 1. then begin
         print,self,' Eccentricity must be 0<=ecc<1'
         return
      endif
   endif else begin
      if npts_m ne 1 and npts_m ne npts_ecc then begin
         print,self,' Mean anomaly (m) and eccentricity (ecc) must be the', $
                    ' same length if both'
         print,'are vectors.'
         return
      endif
      z=where(in_ecc lt 0. or ecc ge 1.,count)
      if count ne 0 then begin
         print,self,' Out of range eccentricities detected (must be 0<=ecc<1)'
         return
      endif
   endelse

   ; force to principal value and force to be double precision
   m = prival(double(in_m))
   ecc = double(in_ecc)

   ; convert from 0 to 2pi to -pi to pi
   z=where(m gt !dpi,count)
   if count ne 0 then m[z] -= 2.0d0*!dpi

   aux   =  4.d0*ecc+0.5d0
   alpha = (1.d0-ecc)/aux

   eccanom=replicate(0.0d0,max([npts_m,npts_ecc]))

   if npts_m eq 1 and npts_ecc gt 1 then begin
      mx=replicate(m,npts_ecc)
   endif else begin
      mx=temporary(m)
   endelse
   if npts_ecc eq 1 and npts_m gt 1 then begin
      ecc=replicate(ecc,npts_m)
   endif

   z=where(ecc eq 0.0d0,count)
   ; handle trivial case of circular orbit
   if count ne 0 then begin
      if npts_ecc eq 1 then begin
         eccanom = mx
         return
      endif else begin
         eccanom[z] = mx[z]
      endelse
   endif

   ; handle non-trivial case
   zg=where(ecc ne 0.0d0,countg)
   if countg ne 0 then begin

      beta=mx[zg]/(2.d0*aux[zg])
      aux[zg]=sqrt(beta^2+alpha[zg]^3)

      z=beta+aux[zg]
      zz=where(z le 0.0d0,count)
      if count gt 0 then z[zz]=beta[zz]-aux[zz]

      test=abs(z)^0.3333333333333333d0

      z = test
      zz=where(z lt 0.0d0,count)
      if count gt 0 then z[zz] = -z[zz]

      s0=z-alpha[zg]/z
      s1=s0-(0.078d0*s0^5)/(1.d0+ecc[zg])
      e0=mx[zg]+ecc[zg]*(3.d0*s1-4.d0*s1^3)

      se0=sin(e0)
      ce0=cos(e0)

      f  = e0-ecc[zg]*se0-mx[zg]
      f1 = 1.d0-ecc[zg]*ce0
      f2 = ecc[zg]*se0
      f3 = ecc[zg]*ce0
      f4 = -f2
      u1 = -f/f1
      u2 = -f/(f1+0.5d0*f2*u1)
      u3 = -f/(f1+0.5d0*f2*u2+.16666666666667d0*f3*u2*u2)
      u4 = -f/(f1+0.5d0*f2*u3+.16666666666667d0*f3*u3*u3+.041666666666667d0*f4*u3^3)

      eccanom[zg]=e0+u4

      zd = zg
      niter=0
      repeat begin
         ;; E-e sinE-M
         fe=eccanom[zd]-ecc[zd]*sin(eccanom[zd])-mx[zd]
         ;; f' = 1-e*cosE
         fs=1.-ecc[zd]*cos(eccanom[zd])
         oldval=eccanom[zd]
         eccanom[zd]=oldval-fe/fs
         err = abs(oldval-eccanom[zd])
         zn = where(err gt thresh,countd)
         if countd gt 0 then zd=zd[zn]
         niter++
      endrep until countd eq 0 or niter gt 1000
      if niter gt 1000 then begin
         print,self,'Warning: did not fully converge (',strn(countd),')'
         print,thresh,err[zn],oldval,eccanom[zn]
      endif
      eccanom = prival(eccanom)
      ; convert from 0 to 2pi to -pi to pi
      z=where(eccanom gt !dpi,count)
      if count ne 0 then eccanom[z] -= 2.0d0*!dpi

   endif

end

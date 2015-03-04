;+
; NAME:
;	inst2std
; PURPOSE: (one line)
;	Apply photometric transformation from instrumental to standard mags.
; DESCRIPTION:
;  The formula for applying transformation to a photometric measurement
;  follows the basic formalism (including signs) from Hardie.  A time
;  dependent term has been added.  The formula looks like this:
;
;    m0 = m - kX - n(t-t0)X - k"CX + eC + Z
;
;     where
;        m  = instrumental magnitude
;        k  = extinction coefficient, mag/airmass
;        X  = airmass
;        n  = coefficient of the 1st order expansion of extinction as a
;               function of time
;        t  = Time of observation (in hours)
;        t0 = Reference time for n, time dependent correction is zero at
;               this time, usually is the middle of the observation set.
;        k" = second order extinction coefficient
;        C  = Standard system color of the object
;        e  = color term
;        Z  = zero point
;        m0 = Standard magnitude
;
; CATEGORY:
;	Photometry
; CALLING SEQUENCE:
;	inst2std,jd,am,inst,instsig,color,colorsig, $
;     tran,transig,jdref,std,stdsig
; INPUTS:
;  jd       - Julian date of observation for each entry.
;  am       - Floating point array of the airmass of observations.
;	inst     - Instrumental magnitude
;	instsig  - Uncertainty of the instrumental magnitude
;	color    - Standard system color for object.
;	colorsig - Uncertainty on the standard color
;	tran     - Transformation coefficients (vector)
;                tran(0) = principal extinction coefficient
;                tran(1) = second order extinction coefficient
;                tran(2) = color term
;                tran(3) = zero-point
;                tran(4) = time-dependent extinction term
;  transig  - Uncertainty on the transformation coefficients (vector).
;                (no uncertainty on reference time)
;  jdref    - Time reference point for extinction
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; KEYWORD OUTPUT PARAMETERS:
; OUTPUTS:
;	std      - Standard magnitude.
;	stdsig   - Uncertainty of the standard magnitude.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written: 1992 Mar 31, Marc W. Buie, Lowell Observatory.
;  97/2/10, MWB, total rewrite
;-
pro inst2std,jd,am,inst,instsig,color,colorsig, $
   tran,transig,jdref,std,stdsig

   if n_params() eq 0 then begin
      print,'inst2std,jd,am,inst,instsig,color,colorsig, $'
      print,'   tran,transig,jdref,std,stdsig'
      return
   endif

   if badpar(jd,      5,     1,caller='INST2STD: (jd) ',      npts=n1) then return
   if badpar(am,      [4,5], 1,caller='INST2STD: (am) ',      npts=n2) then return
   if badpar(inst,    [4,5], 1,caller='INST2STD: (inst) ',    npts=n3) then return
   if badpar(instsig, [4,5], 1,caller='INST2STD: (instsig) ', npts=n4) then return
   if badpar(color,   [4,5], 1,caller='INST2STD: (color) ',   npts=n5) then return
   if badpar(colorsig,[4,5], 1,caller='INST2STD: (colorsig) ',npts=n6) then return
   if badpar(tran,    [4,5], 1,caller='INST2STD: (tran) ',    npts=n7) then return
   if badpar(transig, [4,5], 1,caller='INST2STD: (transig) ', npts=n8) then return
   if badpar(jdref,   5,     0,caller='INST2STD: (jdref) '           ) then return

   alln=[n1,n2,n3,n4,n5,n6]
   if min(alln) ne max(alln) then begin
      print,'INST2STD: Error!  jd,am,mag,err,color,colorsig must be the same length.'
      return
   endif

   if n7 ne 5 then begin
      print,'INST2STD: Error!  tran must a 5 element vector.'
      return
   endif

   IF n8 ne 5 THEN BEGIN
      print,'INST2STD: Error!  transig must be a 5 element vector.'
      return
   ENDIF

   ; Compute standard magnitudes for all observations.
   dt = float((jd-jdref)*24.0)
   std = inst - tran[0]*am - tran[4]*am*dt $
                 - tran[1]*am*color + tran[2]*color + tran[3]

   ; Propagate uncertainties.
   stdvarall = instsig^2 + (am*transig[0])^2 + (am*transig[4]*dt)^2 + $
            (am*color*transig[1])^2 + $
            (am*tran[1]*colorsig)^2 + $
            (color*transig[2])^2 + transig[3]^2

   stdsig=sqrt(stdvarall)

end

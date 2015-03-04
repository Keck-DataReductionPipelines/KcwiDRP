;+
; NAME: 
;  lcfun
; PURPOSE: 
;  Compute a lightcurve function (Fourier series plus phase coefficient).
; DESCRIPTION:
;  This is to be used in cases where you already know the fundamental period.
;  The input independent variable is assumed to be reduced to phase of a
;  fundamental period already.  The integer part of the number is not used
;  by the function.
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  lcfun,x,c,f,pder
; INPUTS:
;  x - independent variable (longitude, between 0 and 360)
;  c - fourier series coefficients
;        0 - beta    -> linear phase coefficient
;        1 - a(0)    -> constant term
;        2 - a(1)    -> cos(x2pi)
;        3 - b(1)    -> sin(x2pi)
;        4 - a(2)    -> cos(2x2pi)
;        5 - b(2)    -> sin(2x2pi)
;     and so on for as many elements as in c
; OPTIONAL INPUT PARAMETERS:
;  PHANG - phase angle of each observation, will be saved to common, if
;             not provided will use the last values saved to common block.
;             Angle is in degrees.
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  f    - Evaluated function value
;  pder - (Optional) partial derivatives
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
;   lc_com - one variable, phang, that is used to save the phase angle from
;               one call to the next in case of calling the lcfun or lcfunf
;               from a canned routine that cannot be taught to pass the
;               extra independent variable.
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/10/10, Written by Marc W. Buie, Lowell Observatory
;-
pro lcfun,x,c,f,pder,PHANG=in_phang
   common lc_com,phang

   if n_elements(in_phang) eq 1 or $
      n_elements(x) eq n_elements(in_phang) then phang = in_phang

   beta  = c[0]
   const = c[1]
   nterms = n_elements(c)
   alen = fix((nterms-1)/2)
   if nterms/2*2 ne nterms then blen = alen-1 else blen = alen
   a = c[indgen(alen)*2+2]
   b = c[indgen(blen)*2+3]

   ;Evaluate
   f = const + beta*phang
   for i=0,alen-1 do f = f + a[i]*cos(2.0*!pi*(i+1)*x/360.0)
   for i=0,blen-1 do f = f + b[i]*sin(2.0*!pi*(i+1)*x/360.0)

   ;Need partial?
   if n_params(0) eq 4 then begin
      pder=fltarr(n_elements(x),nterms)
      pder[*,0] = phang
      pder[*,1] = 1.0
      for i=0,alen-1 do pder[*,2*i+2] = cos(2.0*!pi*(i+1)*x/360.0)
      for i=0,blen-1 do pder[*,2*i+3] = sin(2.0*!pi*(i+1)*x/360.0)
   endif
end


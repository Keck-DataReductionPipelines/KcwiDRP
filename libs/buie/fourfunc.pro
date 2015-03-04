;+
; NAME: 
;  fourfunc
; PURPOSE: 
;  Compute a Fourier series function (called by fourfit)
; DESCRIPTION:
;  This is to be used in cases where you already know the fundamental period.
;  The input independent variable is assumed to be reduced to phase of a
;  fundamental period already.  The integer part of the number is not
;  to the function.
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  fourfunc,x,c,f,pder
; INPUTS:
;  x - independent variable (phase, between zero and one)
;  c - fourier series coefficients
;        0 - a(0)    -> constant term
;        1 - a(1)    -> cos(x2pi)
;        2 - b(1)    -> sin(x2pi)
;        3 - a(2)    -> cos(2x2pi)
;        4 - b(2)    -> sin(2x2pi)
;     and so on for as many elements as in c
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  f    - Evaluated function value
;  pder - (Optional) partial derivatives
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/10/10, Written by Marc W. Buie, Lowell Observatory
;-
pro fourfunc,x,c,f,pder
   const = c[0]
   nterms = n_elements(c)
   alen = fix(nterms/2)
   if nterms/2*2 eq nterms then blen = alen-1 else blen = alen
   a = c[indgen(alen)*2+1]
   b = c[indgen(blen)*2+2]

   ;Evaluate
   f = const
   for i=0,alen-1 do f = f + a[i]*cos(2.0*!pi*(i+1)*x)
   for i=0,blen-1 do f = f + b[i]*sin(2.0*!pi*(i+1)*x)

   ;Need partial?
   if n_params(0) eq 4 then begin
      pder=fltarr(n_elements(x),nterms)
      pder[*,0] = 1.0
      for i=0,alen-1 do pder[*,2*i+1] = cos(2.0*!pi*(i+1)*x)
      for i=0,blen-1 do pder[*,2*i+2] = sin(2.0*!pi*(i+1)*x)
   endif
end

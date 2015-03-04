;+
; NAME: 
;  lcfunf
; PURPOSE: 
;  Compute a lightcurve function independent variables.
; DESCRIPTION:
;  This is to be used in cases where you already know the fundamental period.
;  The input independent variable is assumed to be reduced to phase of a
;  fundamental period already.  The integer part of the number is not used
;  by the function.
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  ans = lcfunf(x,m)
; INPUTS:
;  x - independent variable (longitude, between 0 and 360)
;  m - order of fit (see lcfun.pro)
; OPTIONAL INPUT PARAMETERS:
;  PHANG - phase angle of each observation, will be saved to common, if
;             not provided will use the last values saved to common block.
;             Angle is in degrees.
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value - 2-d array with all the independent variables computed.
;                    This is normally used to setup a svdfit or similar
;                    operation.
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
;  95/08/01, cloned from LCFUN by MWB
;-
function lcfunf,x,m,PHANG=in_phang
   common lc_com,phang

   if n_elements(in_phang) eq 1 or $
      n_elements(x) eq n_elements(in_phang) then phang = in_phang

   f = fltarr(n_elements(x),m)

   f[*,0] = phang
   f[*,1] = 1.0
   j = 0
   i = 2
   while (i lt m) do begin
      f[*,i] = cos(2.0*!pi*(j+1)*x/360.0)
      i = i + 2
      j = j + 1
   endwhile
   j = 0
   i = 3
   while (i lt m) do begin
      f[*,i] = sin(2.0*!pi*(j+1)*x/360.0)
      i = i + 2
      j = j + 1
   endwhile

   return,f
end

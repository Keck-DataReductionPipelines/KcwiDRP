;+
; NAME:
;  decparse
; PURPOSE:
;  Convert Declination string to radians.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  dec=decparse(str)
; INPUTS:
;  str - String (or array) to parse as a declination
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  DEGREES  - Flag, if set, indicates the decimal value is in degrees.
;                The default is radians.
;
; OUTPUTS:
;  return value is scalar or vector value of Dec in radians
; KEYWORD OUTPUT PARAMETERS:
;   ERROR - Flag (scalar or vector), set if an error was seen.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  1997/06/04, Written by Marc W. Buie, Lowell Observatory
;  2000/11/9, MWB, removed Str_sep call.
;  2006/05/08, MWB, upgraded loop variable to long.
;  2009/04/26, MWB, converted to use new CVTSIXTY routine
;-
function decparse,str,DEGREES=degrees,ERROR=error

   if keyword_set(degrees) then begin
      mindec =  -90.0d0
      maxdec =   90.0d0
   endif else begin
      mindec = -0.5d0*!dpi
      maxdec =  0.5d0*!dpi
   endelse

   cvtsixty,str,mindec,maxdec,0,['+','-'],answer, $
      degrees=degrees,error=error

   return,answer

end

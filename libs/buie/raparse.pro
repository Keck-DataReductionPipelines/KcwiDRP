;+
; NAME:
;  raparse
; PURPOSE:
;  Convert Right Ascension (RA) string to radians.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  ra=raparse(str)
; INPUTS:
;  str - String (or array) to parse as a right ascension
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  DEGREES  - Flag, if set, indicates the decimal value is in degrees.
;                The default is radians.
;
; OUTPUTS:
;  return value is scalar or vector value of RA in radians
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
;  2009/04/26, MWB, converted to use new CVTSIXTY routine
;-
function raparse,str,DEGREES=degrees,ERROR=error

   if keyword_set(degrees) then begin
      minra =    0.0d0
      maxra =  360.0d0
   endif else begin
      minra =  0.0d0*!dpi
      maxra =  2.0d0*!dpi
   endelse

   cvtsixty,str,minra,maxra,1,['',''],answer,/hours, $
      degrees=degrees,error=error

   return,answer

end

;+
; NAME:
;  haparse
; PURPOSE:
;  Convert Hour Angle (HA) string to radians.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  ha=haparse(str)
; INPUTS:
;  str - String (or array) to parse as an hour angle
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  DEGREES  - Flag, if set, indicates the decimal value is in degrees.
;                The default is radians.
;
; OUTPUTS:
;  return value is scalar or vector value of HA in radians
;    Note - W,w,+ prefixes are all west hour angles (positive)
;           E,e,- prefixes are all east hour angles (negative)
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
;  2002/03/14, Written by Marc W. Buie, Lowell Observatory
;  2009/04/26, MWB, converted to use new CVTSIXTY routine
;-
function haparse,str,DEGREE=degree,ERROR=error

   if keyword_set(degree) then begin
      minha = -180.0d0
      maxha =  180.0d0
   endif else begin
      minha = -1.0d0*!dpi
      maxha =        !dpi
   endelse

   cvtsixty,str,minha,maxha,0,['W','E'],answer,/hours, $
      degrees=degrees,error=error

   return,answer

end

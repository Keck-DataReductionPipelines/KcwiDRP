;+
; NAME:
;    rastr
; PURPOSE: (one line)
;    Convert RA in radians to hours, minutes, and seconds (ASCII string).
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    rastr, radians, places, str, carry
; INPUTS:
;  ra     - Right Ascension, in radians, to be converted to a string.
;             May a vector, in which case the outputs will be vectors.
;  places - output format and precision (see cvtsixty).
;OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;    DEGREES - Flag, if set it means the input is in decimal degrees.
;    SEPCHAR - Separator character for the fields in the position.
;                 Default=':'.  Warning, if you do not use ':' then the
;                 string may not be parsable by raparse.
; OUTPUTS:
;    str   - Output string for the converted right ascension.
;    carry - Flag that indicates ra rolled over after rounding.
; KEYWORD OUTPUT PARAMETERS:
;   ERROR - Flag (scalar or vector), set if an error was seen.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    Right ascension is reduced to the range [0,24) then converted to a string.
;    There are no imbedded blanks.
; MODIFICATION HISTORY:
;  1993/07/15, Doug loucks, Lowell Observatory, July, 1993, ported from the
;         C-Language version written by Marc Buie.
;  1997/07/14, MWB, added places=4
;  1999/01/12, MWB, added places = -8
;  2001/11/04, MWB, fixed bug in places=-7 case
;  2003/05/09, MWB, added SEPCHAR keyword
;  2009/04/26, MWB, converted to use new CVTSIXTY routine
;-
pro rastr, ra, places, str, carry, SEPCHAR=sepchar, DEGREES=degrees, ERROR=error

   if n_params() LT 3 then begin
      ; Display the calling sequence.
      print, 'rastr, radians, places, str, carry'
      return
   endif

   self='RASTR: '

   if badpar(ra,[2,3,4,5],[0,1],CALLER=self+'(ra) ') then return
   if badpar(places,[2,3],0,CALLER=self+'(places) ') then return
   if badpar(sepchar,[0,7],0,CALLER=self+'(SEPCHAR) ',default=':') then return

   cvtsixty,ra,0.0d0,2.0d0*!dpi,1,['',''],str,carry=carry,error=error, $
      degrees=degrees,/hours,places=places,sepchar=sepchar

end

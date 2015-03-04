;+
; NAME:
;  hastr
; PURPOSE:
;  Convert an Hour Angle to a string.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  hastr,ha,places,str
; INPUTS:
;  ha     - scalar or vector, hour angle in radians
;  places - output format and precision (see cvtsixty).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;    DEGREES - Flag, if set it means the input is in decimal degrees.
;    SEPCHAR - Separator character for the fields in the position.
;                 Default=':'.  Warning, if you do not use ':' then the
;                 string may not be parsable by raparse.
; OUTPUTS:
;  str    - formatted string
; KEYWORD OUTPUT PARAMETERS:
;   ERROR - Flag (scalar or vector), set if an error was seen.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/10/09
;  2009/04/26, MWB, converted to use new CVTSIXTY routine
;-
pro hastr,ha,places,str, SEPCHAR=sepchar, DEGREES=degrees, ERROR=error

   if n_params() LT 3 then begin
      ; Display the calling sequence.
      print, 'hastr, radians, places, str'
      return
   endif

   self='HASTR: '

   if badpar(ha,[2,3,4,5],[0,1],CALLER=self+'(ha) ') then return
   if badpar(places,[2,3],0,CALLER=self+'(places) ') then return
   if badpar(sepchar,[0,7],0,CALLER=self+'(SEPCHAR) ',default=':') then return

   cvtsixty,ha,-1.0d0*!dpi,1.0d0*!dpi,0,['W','E'],str,error=error, $
      degrees=degrees,/hours,places=places,sepchar=sepchar

end

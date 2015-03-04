;+
; NAME:
;    decstr
; PURPOSE: (one line)
;    Convert declination in radians to an ASCII string.
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    decstr, declination, places, str
; INPUTS:
;    declination - Declination, in radians, to be converted to a string.  May
;                  a vector, in which case the output will be a vector.
;    places      - Resolution of output string.
;             = -7     nearest hour.                    +DD
;             = -6     nearest 30 minutes.              +DD:(00,30)
;             = -5     nearest 15 minutes.              +DD:(00,15,30,45)
;             = -4     nearest 10 minutes.              +DD:M0
;             = -3     nearest 5 minutes.               +DD:M(0,5)
;             = -2     nearest minute.                  +DD:MM
;             = -1     nearest ten seconds.             +DD:MM:S0
;             =  0     nearest second.                  +DD:MM:SS
;             =  1     nearest tenth of a second.       +DD:MM:SS.s
;             =  2     nearest hundredth of a second.   +DD:MM:SS.ss
;             =  3     nearest thousandth of a second.  +DD:MM:SS.sss
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;    SEPCHAR - Separator character for the fields in the position.
;                 Default=':'.  Warning, if you do not use ':' then the
;                 string may not be parsable by raparse.
; OUTPUTS:
;    str         - Output string for the converted declination.
; KEYWORD OUTPUT PARAMETERS:
;   ERROR - Flag (scalar or vector), set if an error was seen.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    Declination (in the range [-pi/2,pi/2]) is converted to a string.
;    The sign of the declination is always provided and there are no imbedded
;    blanks.  PLACES indicates the reported precision of the string.
; MODIFICATION HISTORY:
;  1993/08/11 - Ported by Doug Loucks, Lowell Observatory, from the
;         C-Language version written by Marc Buie.
;  2003/05/09, MWB, added SEPCHAR keyword
;  2009/04/02, MWB, added NOLIMIT keyword
;  2009/04/26, MWB, converted to use new CVTSIXTY routine
pro decstr, declination, places, str, $
       SEPCHAR=sepchar, DEGREES=degrees, ERROR=error

   if n_params() ne 3 then begin
      ; Display the calling sequence.
      print, 'decstr, declination, places, str'
      return
   endif

   self='DECSTR: '

   if badpar(declination,[2,3,4,5],[0,1],CALLER=self+'(declination) ') then return
   if badpar(places,[2,3],0,CALLER=self+'(places) ') then return
   if badpar(sepchar,[0,7],0,CALLER=self+'(SEPCHAR) ',default=':') then return

   cvtsixty,declination,-0.5d0*!dpi,0.5d0*!dpi,0,['+','-'],str, $
      error=error,degrees=degrees,places=places,sepchar=sepchar

end

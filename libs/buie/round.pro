;+
; NAME:
;    round
; PURPOSE: (one line)
;    round a floating point number.
; DESCRIPTION:
;    rounds a floating point number to the nearest whole number.
; CATEGORY:
;    Mathematical
; CALLING SEQUENCE:
;    round( x )
; INPUTS:
;    x : The number to be rounded.
; OPTIONAL INPUT PARAMETERS:
;    None.
; KEYWORD PARAMETERS:
;    None.
; OUTPUTS:
;    Function value: The rounded number.
; COMMON BLOCKS:
;    None.
; SIDE EFFECTS:
;    None.
; RESTRICTIONS:
;    None.
; PROCEDURE:
;    Applies symmetric rounding to the given number.
; MODIFICATION HISTORY:
;    Ported by Doug Loucks, Lowell Observatory, 1992 Sep, from the routine
;    in pixwt.c written by Marc Buie.
;-
FUNCTION round, x
IF x LT 0 THEN BEGIN
   RETURN, FIX( x - 0.5 )
ENDIF ELSE BEGIN
   RETURN, FIX( x + 0.5 )
ENDELSE
END

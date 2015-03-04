;+
; NAME:
;    radtodms
; PURPOSE: (one line)
;    Convert an angle from radians to degrees, minutes, and seconds.
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    radtodms, radians, sign, deg, min, sec
; INPUTS:
;    radians = Angle expressed in radians.  May be a vector, in which case
;              the outputs will be vectors.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    sign :  Sign associated with the outputs (-1 or +1).
;    deg  :  Degrees.
;    min  :  Minutes.
;    sec  :  Seconds.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    The input declination is broken into four parts.  The sign is carried
; as a separate value, to make angles near zero behave correctly.  There
; are no invalid angles.
; MODIFICATION HISTORY:
;  Copyright (C) 1987, by Marc W. Buie
;  Version dated 87/2/1
;  Ported by Doug Loucks, Lowell Observatory, August 11, 1993, from the
;  C-Language version written by Marc Buie.
;-
PRO radtodms, radians, sign, deg, mm, ss

IF N_PARAMS() NE 5 THEN BEGIN
   ; Display the calling sequence.
   PRINT, 'radtodms, radians, sign, deg, min, sec'
   RETURN
ENDIF

; Allow input to be integer, long, float, or double, and scalar or vector.
IF badpar( radians, [2,3,4,5], [0,1], CALLER='RADTODMS ' ) THEN RETURN

; Save the sign(s) of the input parameter.
sign = FIX( radians GE 0.0 )
i = WHERE( sign EQ 0, count )
IF count NE 0 THEN sign[i] = -1

deg_per_rad = 57.29577951308232D0

; Convert to deg.
fdeg = abs( radians ) * deg_per_rad

; Split into separate parts.
deg = LONG( fdeg )
mm = FIX( ( fdeg - deg ) * 60.0 )
ss = ( fdeg - deg - mm / 60.0 ) * 3600.0

; Adjust minutes, if necessary.  The test for 'greater than, or equal' is
; used because floating-point arithmetic may not yield exactly 60.0.
i = WHERE( ss GE 60.0, count )
IF count NE 0 THEN BEGIN
   ss[i] = 0.0
   mm[i] = mm[i] + 1
ENDIF

; Adjust deg, if necessary.
i = WHERE( mm EQ 60, count )
IF count NE 0 THEN BEGIN
   mm[i] = 0
   deg[i] = deg[i] + 1
ENDIF

END

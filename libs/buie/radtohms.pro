;+
; NAME:
;    radtohms
; PURPOSE: (one line)
;    Convert from radians to hours, minutes, and seconds of right ascension.
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    radtohms, radians, hh, mm, ss
; INPUTS:
;    radians : Angle expressed in radians.  May be a vector, in which case
;              the outputs will be vectors.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    hh : Hours
;    mm : Minutes
;    ss : Seconds
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    The angle is reduced to its principal value then converted to hours.
;    The decimal number of hours is broken up into the three pieces.  There are
;  no invalid input quantities.
;    Calls external function prival.pro.
; MODIFICATION HISTORY:
;    Copyright (C) 1987, by Marc W. Buie
;    Version dated 87/2/1
;    Ported by Doug Loucks, Lowell Observatory, July 28, 1993, from the
;    C-Language version written by Marc Buie.
;-
PRO radtohms, radians, hh, mm, ss

; Check for required parameters.
IF N_PARAMS() NE 4 THEN BEGIN
   ; Display the calling sequence.
   PRINT, 'radtohms, radians, hh, mm, ss'
   RETURN
ENDIF

; Allow input to be integer, long, float, or double, and scalar or vector.
IF badpar( radians, [2,3,4,5], [0,1], CALLER='RADTOHMS ' ) THEN RETURN

hour_per_rad = 12.0d0/!dpi

ra = prival( radians ) * hour_per_rad

hh = FIX( ra )
mm = FIX( (ra - hh) * 60.0 )
ss = ( ra - double(hh) - double(mm)/60.0d0 ) * 3600.0D0
END

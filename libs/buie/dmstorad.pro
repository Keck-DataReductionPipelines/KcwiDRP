;+
; NAME:
;    dmstorad
; PURPOSE: (one line)
;    Convert from degrees, minutes, and seconds to radians.
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    dmstorad, sign, deg, min, sec, radians
; INPUTS:
;    sign : Sign associated with the inputs.  Must be -1, or +1.
;    deg  : Degrees.  No restrictions.
;    min  : Minutes.  0   <= min < 60.
;    sec  : Seconds.  0.0 <= sec < 60.0.
;    If more than one of these are vectors, they must be the same length.
;    A mixture of scalar and vector input parameters is equivalent to all three
; inputs being vectors: The scalar inputs are treated as replicated vectors.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    radians  : Converted angle in radians.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    The input declination is in four parts.  The sign is carried as a separate
; value, to make angles near zero behave correctly.
; MODIFICATION HISTORY:
;    Copyright (C) 1987, by Marc W. Buie
;    Version dated 87/6/3
;    Ported by Doug Loucks, Lowell Observatory, August 12, 1993, from the
; C-Language version written by Marc Buie.
; 2003/07/22, MWB, Relaxed error check to permit 60 in minutes or seconds field.
;-
; ------------------------------------------------------------------------------
; Procedure dmstorad
; ------------------------------------------------------------------------------
PRO dmstorad, sign, ddd, mm, ss, radians

rad_per_deg = 0.0174532925199432957692D0

; Check for correct number of parameters.
IF N_PARAMS() NE 5 THEN BEGIN
   ; Display the calling sequence.
   PRINT, 'dmstorad, sign, deg, min, sec, radians'
   RETURN
ENDIF

; Verify the type and rank of the input parameters.  Allowed types are
; integer, long, float, or double.  Allowed ranks are scalar (0) or
; vector (1).
IF badpar( sign, [2,3,4,5], [0,1], CALLER='DMSTORAD: (sign) ', NPTS=sign_size, $
RANK=sign_rank ) THEN RETURN
;
IF badpar( ddd, [2,3,4,5], [0,1], CALLER='DMSTORAD: (dd) ', NPTS=ddd_size, $
RANK=ddd_rank ) THEN RETURN
;
IF badpar( mm, [2,3,4,5], [0,1], CALLER='DMSTORAD: (mm) ', NPTS=mm_size, $
RANK=mm_rank ) THEN RETURN
;
IF badpar( ss, [2,3,4,5], [0,1], CALLER='DMSTORAD: (ss) ', NPTS=ss_size, $
RANK=ss_rank ) THEN RETURN

check = [ sign_size, ddd_size, mm_size, ss_size ]
z = WHERE( check NE 1, count )
IF count NE 0 THEN BEGIN
   IF MIN( check[z] ) NE MAX( check[z] ) THEN BEGIN
      MESSAGE, 'Vector input parameters must be the same length.', /INFO
      RETURN
   ENDIF
ENDIF

i = WHERE( sign NE -1 AND sign NE 1, count )
IF count NE 0 THEN BEGIN
   MESSAGE, 'Parameter value out of range.  sign must be -1 or +1.', /INFO
   RETURN
ENDIF

i = WHERE( mm LT 0 OR mm GT 60, count )
IF count NE 0 THEN BEGIN
   MESSAGE, 'Parameter value out of range.  0 <= min < 60.', /INFO
   RETURN
ENDIF

i = WHERE( ss LT 0.0 OR ss GT 60.0, count )
IF count NE 0 THEN BEGIN
   MESSAGE, 'Parameter value out of range.  0.0 <= sec < 60.0.', /INFO
   RETURN
ENDIF

deg = ABS( ddd )

radians = deg + ( mm + ss / 60.0 ) / 60.0
radians = sign * radians * rad_per_deg
END

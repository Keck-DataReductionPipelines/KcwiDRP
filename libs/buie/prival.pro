;+
; NAME:
;    prival
; PURPOSE: (one line)
;    Reduce an angle to its principal value (0 $\leq \Theta < 2\pi$).
; DESCRIPTION:
;    This routine uses [0 to 2*PI) as the principal value of an angle.  If
; the angle is negative, 2*PI is added to the remainder of division by 2*PI.
;    If positive, then the value is the remainder of division by 2*PI.  Nothing
; is done to the input value if it is already in range.
;
; CATEGORY:
;    Mathematical
; CALLING SEQUENCE:
;    result = prival( angle )
; INPUTS:
;    angle : Angle to reduce to principal value (double).
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    result = Principal value of angle (double).
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Copyright (C) 1987, by Marc W. Buie
;    Version dated 86/2/1
;    Ported by Doug Loucks, Lowell Observatory, July 28, 1993, from the
; C-language version written by Marc Buie.
;-
FUNCTION prival, angle

l_angle = DOUBLE( angle )
twopi   = !dpi * 2.0d0

n = WHERE( l_angle LT 0.0D0, count )
WHILE count GT 0 DO BEGIN
   l_angle[ n ] = l_angle[ n ] + twopi
   n = WHERE( l_angle LT 0.0D0, count )
ENDWHILE

p = WHERE( l_angle GE twopi, count )
WHILE count GT 0 DO BEGIN
   l_angle[ p ] = l_angle[ p ] - twopi
   p = WHERE( l_angle GE twopi, count )
ENDWHILE

RETURN, l_angle
END

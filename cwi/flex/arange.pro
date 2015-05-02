function arange, angle, center

;+
; This function adjusts angle by increments of 360 degrees such that 
; the center of the 360 degree range is given by center.
; For example, suppose you want an angle in the range -180 to + 180.
; Then use arange(angle,0) since the center of the range is at 0.
; This is useful when subtracting angles.
; For example, if you want angle1-angle2, you must worry about how each
; angle is defined.  If angle1=-179 and angle2=+179, the difference is
; -358 but the correct answer is 2.  This routine corrects this problem:
; use difference=arange(angle1,angle2)-angle2.
;-

   if n_elements(center) EQ 0 then center = 0.
   value = (angle-center+180.) MOD 360.
   neg = where(value LT 0.,count)
   if count GT 0 then value(neg) = value(neg) + 360.
   value = value + center - 180.
   return, value

end

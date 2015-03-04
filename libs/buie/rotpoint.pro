;+
; NAME: 
;  rotpoint
; PURPOSE: 
;  Rotate x,y,z point(s) about arbitrary axis.
; DESCRIPTION:
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  rotpoint,x,y,z,axis,angle,xp,yp,zp
; INPUTS:
;  x     - Cartesian x-coordinate of point.
;  y     - Cartesian y-coordinate of point.
;  z     - Cartesian z-coordinate of point.
;  axis  - Axis to rotate about:
;           'x' or 'X' or 0  - indicates rotation about x-axis
;           'y' or 'Y' or 1  - indicates rotation about y-axis
;           'z' or 'Z' or 2  - indicates rotation about z-axis
;  angle - Angle to rotate by (scalar).  Radians unless /DEG is set.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DEG - flag, if true indicates angle is in degrees, otherwise angle is
;           taken to be in radians.
; OUTPUTS:
;  xp - Rotated x-coordinates.
;  yp - Rotated y-coordinates.
;  zp - Rotated z-coordinates.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written 1/4/95 by Marc W. Buie, Lowell Observatory
;-
pro rotpoint,x,y,z,axis,in_angle,xp,yp,zp, $
      DEG = deg

;Validate input
if badpar(x,[4,5],[0,1,2,3,4,5,6,7,8],CALLER='ROTPOINT (x): ') then return
if badpar(y,[4,5],[0,1,2,3,4,5,6,7,8],CALLER='ROTPOINT (y): ') then return
if badpar(z,[4,5],[0,1,2,3,4,5,6,7,8],CALLER='ROTPOINT (z): ') then return
if badpar(axis,7,0,CALLER='ROTPOINT (axis): ') then return
;if badpar(in_angle,[2,3,4,5],0,CALLER='ROTPOINT (angle): ') then return

if keyword_set(deg) then $
   angle = in_angle/!radeg $
else $
   angle = in_angle

cangle = cos(angle)
sangle = sin(angle)

;Rotation about X-axis
if axis eq 'x' or axis eq 'X' then begin
   xp = x
   yp = y*cangle - z*sangle
   zp = y*sangle + z*cangle

;Rotation about Y-axis
endif else if axis eq 'y' or axis eq 'Y' then begin
   xp =  x*cangle + z*sangle
   yp =  y
   zp = -x*sangle + z*cangle

;Rotation about Z-axis
endif else if axis eq 'z' or axis eq 'Z' then begin
   xp = x*cangle - y*sangle
   yp = x*sangle + y*cangle
   zp = z

;Unrecognized request.
endif else begin
   print,'ROTPOINT: Error, unrecognized axis code'

endelse

end

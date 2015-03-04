;+
; NAME:
;    sint2d
; PURPOSE: (one line)
;    Sinc interpolation of a 2-D array of data.
; DESCRIPTION:
;
; CATEGORY:
;    Numerical
; CALLING SEQUENCE:
;    result = sint2d( x, y, f )
; INPUTS:
;    x, y  : Position of desired function value.
;    f     : Two-D function array.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    Interpolated function value.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    Calls external function sint to interpolate appropriate 1-D slices of
; the 2-D array.
;    Note: For speed, input parameters are not verified.
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, September, 1993.
;-
FUNCTION sint2d, x, y, f

;Get size of input array.
stat = SIZE( f )
xsize = stat[ 1 ]
ysize = stat[ 2 ]

;Half-size of the kernel.
delta = 10

;Compute integer and fractional parts of input position.
ix = FIX( x )
fx = x - ix
iy = FIX( y )
fy = y - iy

yoff = MIN( [ iy, delta ] )
ly   = iy - yoff
hy   = iy + yoff
IF hy GE ysize THEN hy = ysize-1
ny   = hy - ly + 1

vals = FLTARR( ny )
FOR j = 0, ny-1 DO BEGIN
   xoff = MIN( [ ix, delta ] )
   lx = ix - xoff
   hx = ix + xoff
   IF hx GE xsize THEN hx = xsize-1
   r1 = f[ lx : hx, ly+j ]
   x1 = fx + xoff
   vals[j] = sint( x1, r1 )
ENDFOR
RETURN, sint( fy+yoff, vals )

END

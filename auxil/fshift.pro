function fshift,image,delx,dely
;+
;			fshift
;
; Routine to shift an image by non-integer values
;
; CALLING SEQUENCE:
;	results = fshift(image,delx,dely)
;
; INPUTS:
;	image - 2D image to be shifted
;	delx - shift in x (same direction as IDL SHIFT function)
;	dely - shift in y
;
; OUTPUTS:
;	shifted image is returned as the function results
;
; HISTORY:
;	version 2  D. Lindler  May, 1992 - rewritten for IDL version 2
;	19-may-1992	JKF/ACC		- move to GHRS DAF.
;-
;--------------------------------------------------------------------

;
; separate shift into an integer and fraction shift
;
	intx = fix(delx)
	inty = fix(dely)
	fracx = delx - intx
	fracy = dely - inty
	if fracx lt 0 then begin
		fracx = fracx + 1
		intx = intx - 1
	endif
	if fracy lt 0 then begin
		fracy = fracy + 1
		inty = inty - 1
	end

;
; shift by integer portion
;
	x = shift(image,intx,inty)
	if (delx eq 0.0) and (dely eq 0.0) then return,x
;
; use bi-linear interpolation between four pixels
;
	return, x * ((1-fracx)*(1-fracy)) + $
		shift(x,0,1) * ((1-fracx)*fracy) + $
		shift(x,1,0) * (fracx*(1-fracy)) + $
		shift(x,1,1) * fracx*fracy
return,0
end

;+
; NAME:
;	slope
; PURPOSE: (one line)
;	Compute slope of a line using part of the data.
; DESCRIPTION:
;	The left and right hand values are computed from the mean of the
;	input data array from -width to +width about the mid-point of the
;	bin.  The slope is derived from these means and the uncertainty
;	comes from propagating the uncertainties.
; CATEGORY:
;       Function fitting
; CALLING SEQUENCE:
;	coeff=slope(a,b,width,x,y)
; INPUTS:
;	a     - Mid-point of the left hand bin in units of x.
;	b     - Mid-point of the right hand bin in units of x.
;	width - Half-width of each bin.
;	x     - Independant values of x-y relationship.
;	y     - Dependant values to get slope from.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	Return value is a vector, [slope,ds], which is the slope of the
;	line from a to b and ds is the uncertainty on the slope.
; COMMON BLOCKS:
; SIDE EFFECTS:
;	Left, right values and slope are printed to the screen.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory.
;-

function slope,a,b,width,x,y

left  = where( x gt a-width and x lt a+width, nleft)
right = where( x gt b-width and x lt b+width, nright)

bsig = stdev(y[left],blue)
rsig = stdev(y[right],red)

slope = ( red/blue - 1.0 ) / ( b - a ) * 1000.0

ds = abs( ( rsig/blue - bsig*red/blue/blue ) / ( b - a ) * 1000.0 )

print,blue,bsig,red,rsig,format='("Left ",f8.1," +/- ",f6.1,"  Right ",f8.1," +/- ",f6.1)'
print,slope,ds,format='("  Slope is ",f7.4," +/- ",f7.4)'

return,[slope,ds]
end

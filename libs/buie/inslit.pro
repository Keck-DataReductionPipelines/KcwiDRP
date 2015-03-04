;+
;NAME:
;	inslit
;PURPOSE: (one line)
;	Compute flux passing through a slit assuming a gaussian image.
;CATEGORY
;       Spectroscopy
;DESCRIPTION:
;
;CALLING SEQUENCE:
;	inslit,width,height,fwhm,offset
;INPUTS:
;	width  - Width of slit.
;	height - Height of slit.
;	fwhm   - Full-width at half-maximum of the gaussian image.
;	offset - Offset of the image from center of the slit.
;
;	Note: the units are arbitrary but they must be the same within
;	all of the input quantities.  The object is also assumed to
;	be perfectly centered in the vertical direction.
;OUTPUTS:
;	Returned is the fraction of the total gaussian included in the slit.
;REVISION HISTORY:
;	Written 1991 Mar., Marc W. Buie, Lowell Observatory
;COMMENTS:
;-

function inslit,width,height,fwhm,offset

a = width / 2.0
b = height / 2.0
s = fwhm/(sqrt(8.0)*alog(2.0))

slit = ( 1.0 - gaussint( -(a+offset)/s ) - gaussint( -(a-offset)/s ) ) * $
       ( 1.0 - 2.0*gaussint( -b/s ) )

return,slit
end

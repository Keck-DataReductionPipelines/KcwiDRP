function maxmin,array
;+
; NAME:
;	maxmin
; PURPOSE:
;	Return vector [max,min] of input array or vector.
;
; DESCRIPTION:
;	Return a 2 element array giving the maximum and minimum of a vector
;	or array.  This is faster than doing a separate MAX and MIN.
;
; CATEGORY:
;	Utility
; CALLING SEQUENCE:
;	value = maxmin( array )
; INPUTS:
;	array - an IDL numeric scalar, vector or array.
;
; OUTPUTS:
;	value = a two element vector, 
;		value(0) = maximum value of array
;		value(1) = minimum value of array
;
; EXAMPLE:
;	Print the maximum and minimum of an image array, im
; 
;         IDL> print, maxmin( im )
;
; PROCEDURE:
;  The MIN function is used with the MAX keyword.  This function is "opposite"
;  of the MINMAX function and is useful for automatic yrange for plots that
;  need an inverted range (from max to min).
;
; REVISION HISTORY:
;	Cloned from MINMAX by Marc Buie,     July 1995
;    original verision written by W. Landsman,  January, 1990
;-

On_error,2

amin = min( array, MAX = amax)
return, [ amax, amin ]

end

;+
; NAME:
;    sinterp4
; PURPOSE: (one line)
;    Four-fold sinc interpolation of a vector of data.
; DESCRIPTION:
;    This function will do a 4-fold sinc interpolation of the data
;    array that is passed.
; CATEGORY:
;    Numerical
; CALLING SEQUENCE:
;    result = sinterp4( x )
; INPUTS:
;    x     : Input data array.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    The interpolated array is returned as the function value.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Original C-Language version written by: Marc W. Buie, Institute for
; Astronomy, University of Hawaii.  Date: 9 December 1987.
;    Adapted for IDL by Doug Loucks, Lowell Observatory, September 2, 1993.
;-
FUNCTION sinterp4, x

n = N_ELEMENTS( x )

r = FLTARR( 4, n )

r[ 0, * ] = x
r[ 1, * ] = sshift( x, -0.25 )
r[ 2, * ] = sshift( x, -0.50 )
r[ 3, * ] = sshift( x, -0.75 )

RETURN, r[ INDGEN( 4 * n - 3 ) ]

END

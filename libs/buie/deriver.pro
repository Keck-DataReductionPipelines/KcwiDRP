;+
; NAME:
;    deriver
; PURPOSE: (one line)
;    Compute the step-wise derivative of a function of one variable.
; DESCRIPTION:
;      Calculates the step-wise derivative of a function by computing
;  deltaf / deltax.  This requires that f and x be vectors with at least
;  two elements.
;      The uncertainties associated with the dependent variable of the
;  function may be supplied via a keyword, in which case the uncertainties
;  associated with the derivative are returned to the caller via another
;  keyword parameter.
; CATEGORY:
;    Mathematical
; CALLING SEQUENCE:
;    deriver, x, f, newx, fprime, [, FERR=in_ferr, FPERR=out_fperr]
; INPUTS:
;    x    : Independent variable.
;    f    : Dependent variable.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    FERR   = Uncertainty in f (optional input).
;    FPERR  = Uncertainty in fprime (optional output).
; OUTPUTS:
;    newx   : The centered x-values corresponding to fprime.
;    fprime : The computed derivative of the dependent variable.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, July, 1993.
;  1999 Nov 24, Marc W. Buie, Lowell Observatory, changed name to eliminate
;    a naming conflict with DERIVE, an IDL built-in routine.
;-
PRO deriver, x, f, newx, fprime, FERR=in_ferr, FPERR=out_fperr

IF N_PARAMS() NE 4 THEN BEGIN
   ;Print the calling sequence.
   PRINT, 'deriver, x, f, newx, fprime [, FERR=in_ferr, FPERR=out_fperr]'
   RETURN
ENDIF

IF badpar( x, [1,2,3,4,5], 1, CALLER=' DERIV' ) THEN RETURN
IF badpar( f, [1,2,3,4,5], 1, CALLER=' DERIV' ) THEN RETURN

;Check for at least two points in the input arguments.
n = [ N_ELEMENTS( x ), N_ELEMENTS( f ) ]
IF MIN( n ) LT 2 THEN BEGIN
   MESSAGE, 'Input arguments must have at least two points.', /INFO
   RETURN
ENDIF

;Their lengths must be the same.
IF n[0] NE n[1] THEN BEGIN
   MESSAGE, 'Input arguments must be equal in length.'
   RETURN
ENDIF

;Reset n to the number of points in the input arguments.
n = n[0]

;Compute the numerator and denominator vectors.
df = f[1:n-1] - f[0:n-2]
dx = x[1:n-1] - x[0:n-2]

;Then the derivative.  Force the expression to be floating point, so we
;don't loose the fractional part after the divide operation.
fprime = df / FLOAT( dx )

;Now the x-centers vector.  This computes the correct x-values for each
;point in the derivative vector (fprime).
newx = x + ( dx / 2.0 )

IF KEYWORD_SET( in_ferr ) THEN BEGIN
   ;The uncertainties are to be computed and returned (if the caller also
   ;specifies the keyword for output).
   out_fperr = $
   SQRT( in_ferr[1:n-1]*in_ferr[1:n-1] + in_ferr[0:n-2]*in_ferr[0:n-2] )
ENDIF
END

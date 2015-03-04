; NAME:
;	rcgauss_funct
; PURPOSE:
;	Evaluate the sum of a gaussian and a constant.
; DESCRIPTION:
;	Evaluate the sum of a gaussian and a constant and optionally return
;  the value of it's partial derivatives.  Normally, this function is
;  used by curvefit to fit the sum of a line and a varying background
; to actual data.
; CATEGORY:
;	E2 - curve and surface fitting.
; CALLING SEQUENCE:
;	funct,r,a,f,pder
; INPUTS:
;	r = values of independent variable.
;	a = parameters of equation described below.
; OUTPUTS:
;	f = value of function at each r(i).
;
; OPTIONAL OUTPUT PARAMETERS:
;	pder = (n_elements(r),4) array containing the
;		partial derivatives.  p(i,j) = derivative
;		at ith point w/respect to jth parameter.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;	F = A(0)*EXP(-Z^2/2) + A(2) + A(3)*R^2
;	Z = R/A(1)
; MODIFICATION HISTORY:
;	WRITTEN, DMS, RSI, SEPT, 1982.
;	Modified, DMS, Oct 1990.  Avoids divide by 0 if A(1) is 0.
;	Added to Gauss_fit, when the variable function name to
;		Curve_fit was implemented.  DMS, Nov, 1990.
;	Converted to a radial fitting program, MWB, Dec, 1992.
;	Modified from RGAUSSFIT to remove linear portion of function, MWB 1/93
;
PRO	RCGAUSS_FUNCT,R,A,F,PDER
	ON_ERROR,2                        ;Return to caller if an error occurs

	if a[1] ne 0.0 then Z = R/A[1] $	;GET Z
	else z= 10.
	EZ = EXP(-Z^2/2.)*(ABS(Z) LE 7.) ;GAUSSIAN PART IGNORE SMALL TERMS
	F = A[0]*EZ + A[2] ;FUNCTIONS.
	IF N_PARAMS(0) LE 3 THEN RETURN ;NEED PARTIAL?
;
	PDER = FLTARR(N_ELEMENTS(R),3) ;YES, MAKE ARRAY.
	PDER[0,0] = EZ		;COMPUTE PARTIALS
	if a[1] ne 0. then PDER[0,1] = A[0] * EZ * Z^2/A[1]
	PDER[*,2] = 1.
	RETURN
END



function rcgfit, r, y, a
;+
; NAME:
;	rcgfit
;
; PURPOSE:
;	Fit a radial gaussian function to the input data (no linear term).
;
; DESCRIPTION:
; 	Fit the equation y=f(r) where:
;
; 		F(r) = A0*EXP(-z^2/2) + A2 + A3*r^2
; 			and
;		z=r/A2
;
;	A0 = height of exp, A1 = sigma (the width).
;	A2 = constant term, A3 = quadratic term.
; 	The parameters A0, A1, A2 are estimated and then CURVEFIT is 
;	called.  The gaussian is assumed to be centered at r=0.
;
; CATEGORY:
;	Function fitting
;
; CALLING SEQUENCE:
;	Result = RCGAUSSFIT(R, Y [, A])
;
; INPUTS:
;	R:	The independent variable.  R must be a vector.
;	Y:	The dependent variable.  Y must have the same number of points
;		as R.
;
; OUTPUTS:
;	The fitted function is returned.
;
; OPTIONAL OUTPUT PARAMETERS:
;	A:	The coefficients of the fit.  A is a three-element vector as 
;		described under PURPOSE.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;
; PROCEDURE:
;	The initial guess of the intensity is the value of the point 
;	with the smallest r value.  The first point that drops below
;	half of the first is the guess at the width.
;
; MODIFICATION HISTORY:
;	DMS, RSI, Dec, 1983.
;	Rewritten to change initial guess, MWB, Lowell Obs., Dec. 1992.
;	Rewritten to remove background function, MWB, Lowell Obs., Jan. 1993.
;-
;
on_error,2                      ;Return to caller if an error occurs
n = n_elements(y)		;# of points.
idx=sort(r)			;Put all the values in increasing r order.

ymax=y[idx[0]]			;The max occurs closest to zero.
ymin=min(y)			;Take whatever min there is.
a=fltarr(3)			;coefficient vector

dy=ymax-ymin			;diff between extreme and mean
del = dy/exp(1.)+ymin		;1/e value
ewid = r[idx[min(where( y[idx] lt del ))]]	;The r closest to zero below 1/e

a = [dy, ewid, ymin]	;estimates

return,curvefit(r,y,replicate(1.,n),a,sigmaa, $
		function_name = "RCGAUSS_FUNCT") ;call curvefit
end

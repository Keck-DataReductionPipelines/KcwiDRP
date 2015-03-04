; NAME:
;	rfgauss_funct
; PURPOSE:
;	Evaluate the sum of a gaussian and a constant (width fixed in common).
; DESCRIPTION:
;	Evaluate the sum of a gaussian and a constant (width fixed in common)
;	and optionally return the value of it's partial derivatives.
;	Normally, this function is used by curvefit and radp to fit the
;	sum of a line and a varying background to actual data.
;
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
;	pder = (n_elements(r),3) array containing the
;		partial derivatives.  p(i,j) = derivative
;		at ith point w/respect to jth parameter.
; COMMON BLOCKS:
;	rf_gauss   -  Contains the constant gaussian width to use.
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;	F = A(0)*EXP(-Z^2/2) + A(2)
;	Z = R/ewid
; MODIFICATION HISTORY:
;	WRITTEN, DMS, RSI, SEPT, 1982.
;	Modified, DMS, Oct 1990.  Avoids divide by 0 if A(1) is 0.
;	Added to Gauss_fit, when the variable function name to
;		Curve_fit was implemented.  DMS, Nov, 1990.
;	Converted to a radial fitting program, MWB, Dec, 1992.
;	Modified from RGAUSSFIT to remove linear portion of function, MWB 1/93
;
PRO	RFGAUSS_FUNCT,R,A,F,PDER

common rf_guass,ewid

	ON_ERROR,2                        ;Return to caller if an error occurs

	if a[1] ne 0.0 then Z = R/ewid $	;GET Z
	else z= 10.
	EZ = EXP(-Z^2/2.)*(ABS(Z) LE 7.) ;GAUSSIAN PART IGNORE SMALL TERMS
	F = A[0]*EZ + A[1] ;FUNCTIONS.
	IF N_PARAMS(0) LE 3 THEN RETURN ;NEED PARTIAL?
;
	PDER = FLTARR(N_ELEMENTS(R),2) ;YES, MAKE ARRAY.
	PDER[0,0] = EZ		;COMPUTE PARTIALS
	PDER[*,1] = 1.
	RETURN
END



function rfgfit, r, y, fwhm, a

common rf_guass,ewid

;+
; NAME:
;	rfgfit
;
; PURPOSE:
;	Fit a radial gaussian to the input data (no linear term, width fixed).
;
; DESCRIPTION:
; 	Fit the equation y=f(r) where:
;
; 		F(r) = A0*EXP(-z^2/2) + A2
; 			and
;		z=r/A1, A1 is constant
;
;	A0 = height of exp, A1 = sigma (the width).
;	A2 = constant term,
; 	The parameters A0, A1, A2 are estimated and then CURVEFIT is 
;	called.  The gaussian is assumed to be centered at r=0.
;
; CATEGORY:
;	Function fitting
;
; CALLING SEQUENCE:
;	Result = RFGAUSSFIT(R, Y [, A])
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
;	rf_gauss   -  Contains the constant gaussian width to use.
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
a=fltarr(2)			;coefficient vector

dy=ymax-ymin			;diff between extreme and mean

ewid = fwhm/2.35482

a = [dy, ymin]	;estimates

return,curvefit(r,y,replicate(1.,n),a,sigmaa, $
		function_name = "RFGAUSS_FUNCT") ;call curvefit
end

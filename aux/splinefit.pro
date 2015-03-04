FUNCTION SPLINEFIT,X,Y,W,XS,YS,SIGYS,DELY
;+
; NAME:
;	SPLINEFIT
; PURPOSE:
;	Non-linear least squares fit to a cubic spline function of an
;	arbitrary number of nodes.
; CATEGORY:
;	E2 - Curve and Surface Fitting
; CALLING SEQUENCE:
;	YFIT = SPINEFIT(X,Y,W,XS,YS,SIGYS,DELY)
; INPUTS:
;	X = Row vector of independent variables.
;	Y = Row vector of dependent variable, same length as x.
;	W = Row vector of weights, same length as x and y.
;		For no weighting
;		w(i) = 1., instrumental weighting w(i) =
;		1./y(i), etc.
;	XS = Vector containing the x-positions of the spline nodes
;	YS = Vector containing the intial y-position for the spline
;		at each node (same length as XS)
; OPTIONAL INPUTS:
;	DELY = distance to use in computing numerical derivatives
;		with respect to YS values.  The distance is DELY*YS(i)
;		(Default= 0.001)
;
; OUTPUTS:
;	YS = Vector of parameters containing fit.
;	Function result = YFIT = Vector of calculated
;		values. YFIT=CSPLINE(XS,YS,X)
; OPTIONAL OUTPUT PARAMETERS:
;	Sigys = Vector of standard deviations for parameters
;		ys.
;	
; COMMON BLOCKS:
;	NONE.
; RESTRICTIONS:
;	NONE.
; PROCEDURE:
;	Copied from "CURFIT", least squares fit to a non-linear
;	function, pages 237-239, Bevington, Data Reduction and Error
;	Analysis for the Physical Sciences.
;
;	"This method is the Gradient-expansion algorithm which
;	compines the best features of the gradient search with
;	the method of linearizing the fitting function."
;
;	Iterations are perform until the chi square changes by
;	only 0.1% or until 20 iterations have been performed.
;
;	The initial guess of the parameter values should be
;	as close to the actual values as possible or the solution
;	may not converge.
;
; MODIFICATION HISTORY:
;	Modified (D. Neill, Sep. 14) added check for non-finite CHISQR 
;		to prevent infinite loop
;	Modified (D. Lindler, Feb. 87) version of IDL routine CURVEFIT
;	written by DMS, RSI, September, 1982.
;-
	ON_ERROR,2		;RETURN TO CALLER IF ERROR
	YS = 1.*YS		;MAKE PARAMS FLOATING
	NTERMS = N_ELEMENTS(YS)	;# OF PARAMS.
	NFREE = (N_ELEMENTS(Y)<N_ELEMENTS(X))-NTERMS ;Degs of freedom
	IF NFREE LE 0 THEN STOP,'Curvefit - not enough data points.'
; DEFAULT DELY AND TENSION
	IF N_PARAMS(0) LT 7 THEN DELY=0.001
	IF N_PARAMS(0) LT 8 THEN TENSION=1.0
;
	FLAMBDA = 0.001		;Initial lambda
	DIAG = INDGEN(NTERMS)*(NTERMS+1) ;SUBSCRIPTS OF DIAGONAL ELEMENTS
;
	FOR ITER = 1,20 DO BEGIN	;Iteration loop
;
; COMPUTE SPLINE FUNCTION AND DERIV. FOR YS
;
	YFIT=CSPLINE(XS,YS,X)
	PDER=FLTARR(N_ELEMENTS(YFIT),NTERMS)
	FOR I=0,NTERMS-1 DO BEGIN
		NEWYS=YS & NEWYS(I)=YS(I)+DELY*YS(I)
		PDER(0,I)=(CSPLINE(XS,NEWYS,X)-YFIT)/(DELY*YS(I))
	END
;
;		EVALUATE ALPHA AND BETA MATRICIES.
;
	BETA = (Y-YFIT)*W # PDER
	ALPHA = TRANSPOSE(PDER) # (W # (FLTARR(NTERMS)+1)*PDER)
;
	CHISQ1 = TOTAL(W*(Y-YFIT)^2)/NFREE ;PRESENT CHI SQUARED.
;
;	INVERT MODIFIED CURVATURE MATRIX TO FIND NEW PARAMETERS.
;
	REPEAT BEGIN
		C = SQRT(ALPHA(DIAG) # ALPHA(DIAG))
		ARRAY = ALPHA/C
		ARRAY(DIAG) = 1.+FLAMBDA
		ARRAY = INVERT(ARRAY)
		B = YS+ ARRAY/C # TRANSPOSE(BETA) ;NEW PARAMS
		YFIT=CSPLINE(XS,B,X) ;EVALUATE SPLINE
		CHISQR = TOTAL(W*(Y-YFIT)^2)/NFREE ;NEW CHISQR
		FLAMBDA = FLAMBDA*10.	;ASSUME FIT GOT WORSE
		ENDREP UNTIL CHISQR LE CHISQ1 OR NOT FINITE(CHISQR)
;
	FLAMBDA = FLAMBDA/100.	;DECREASE FLAMBDA BY FACTOR OF 10
	YS=B			;SAVE NEW PARAMETER ESTIMATE.
;	PRINT,'ITERATION =',ITER,' ,CHISQR =',CHISQR
;	PRINT,YS
	IF ((CHISQ1-CHISQR)/CHISQ1) LE .001 THEN GOTO,DONE ;Finished?
	ENDFOR			;ITERATION LOOP
;
	PRINT,'CURVEFIT - Failed to converge'
;
DONE:	SIGYS = SQRT(ARRAY(DIAG)/ALPHA(DIAG)) ;RETURN SIGMA'S
	RETURN,YFIT		;RETURN RESULT
END

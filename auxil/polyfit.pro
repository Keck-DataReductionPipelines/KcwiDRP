; $Id: polyfit.pro,v 1.1 2014/09/19 15:51:07 neill Exp $
;+
; NAME: 
;       POLYFIT
;
; PURPOSE:
;	Fit a polynomial to a function using linear least-squares
; NOTE:
;       Twice as fast as POLY_FIT.PRO as tested by Robishaw's 
;       BENCHMARK.PRO due to the miracle of loopless IDL code.
;
;       Uses JohnJohn's FAN.PRO 
;       http://astron.berkeley.edu/~johnjohn/idlprocs/fan.pro
;
; CALLING SEQUENCE:
;       coeffs = polyfit(t, y_of_t, degree [, yfit, dum, covariance=])
; 
; KEYWORDS
; MODIFICATION HISTORY:
;       Written by JohnJohn long ago in ought 1 after attending Carl
;       Heiles' lecture on line fitting.
;-
function polyfit,t,y,deg,yfit,yfit1,covariance=cov,weight=w
;on_error,2
n = n_elements(t)
pow = indgen(deg+1)
powarr = fan(pow,n,/trans)
x =  fan(double(t),deg+1)
xarr = x^powarr
xarrt = transpose(xarr)
if keyword_set(w) then xarr = fan(double(w),deg+1)*x^powarr 

alpha = xarr##xarrt
beta = xarr##(double(y))
cov = invert(alpha)
a = cov##beta
if n_params() eq 4 then yfit = poly(t,a)
if n_params() eq 5 then yfit1 = poly(t,a) ;;Legacy code, kept for compatibility
return,a
end

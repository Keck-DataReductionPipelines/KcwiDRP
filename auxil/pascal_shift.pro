; $Id: pascal_shift.pro,v 1.1 2013/12/12 01:22:39 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PASCAL_SHIFT
;
; PURPOSE:
;	Shift coefficients using Pascal's triangle
;
; CATEGORY:
;	Polynomial fitting
;
; CALLING SEQUENCE:
;	Scoeff = PASCAL_SHIFT(Coef, X0)
;
; INPUTS:
;	Coeff	- Input coefficients
;	X0	- reference x value
;
; RETURNS:
;	Shifted coefficients relative to X0
;
; INPUT KEYWORDS:
;	SILENT	- set to suppress warnings
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	None
;
; PROCEDURE:
;	Uses Pascal's triangle
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Matt Matuszewski
;	2013-DEC-10	Initial Revision
;-
;
function pascal_shift, coeff, x0, silent=silent

pre = 'PASCAL_SHIFT'
version = repstr('$Revision: 1.1 $ $Date: 2013/12/12 01:22:39 $','$','')
q=''
;
; check inputs
s=size(coeff,/dim)
if n_elements(s) gt 1 then begin
	print,pre+' ERROR: Coefficients must be a vector!'
	return,-1
endif
;
; this only works for up to 6th degree polynomials
if s ne 7 then begin
	if s gt 7 then begin
		if not keyword_set(silent) then $
			print,pre+' WARNING: This works with up to 7 coefficients.'
		usecoeff = coeff[0:6]
		fincoeff = coeff - coeff
	endif else begin
		buf = fltarr(7-s)
		usecoeff = [coeff,buf]
		fincoeff = usecoeff - usecoeff
	endelse
endif
;
; okay. so the input coefficients are referenced to the center of the 
; array, so the polynomials have to be adjusted to make sure they relate 
; to the simply indexed rows
;
; p(x) = p0 + p1*(x-x0) + p2*(x-x0)^2 + p3*(x-x0)^3 +p4*(x-x0)^4 +
; p5*(x-x0)^5 + p6*(x-x0)^6
;
; Pascal's triangle is awesome. 
; Expanding all the powers
; p(x) = p0 +
;      - p1*x0 + p1*x
;      + p2*x0^2 - 2*p2*x0*x + p2*x^2
;      - p3*x0^3 + 3*p3*x0^2*x - 3*p3*x0*x^2 + p3*x^3
;      + p4*x0^4 - 4*p4*x0^3*x + 6*p4*x0^2*x^2 - 4*p4*x0*x^3 + p4*x^4
;      - p5*x0^5 + 5*p5*x0^4*x - 10*p5*x0^3*x^2 + 10*p5*x0^2*x^3
;                -5*p5*x0*x^4 + p5*x^5
;      + p6*x0^6 - 6*p6*x0^5*x + 15*p6*x0^4*x^2 - 20*p6*x0^3*x^3 +
;                  15*p6*x0^2*x^4 - 6*p6*x0*x^5 + p6*x^6
;
; Collecting terms of like power:
; p(x) = 
;     (p0 - p1*x0 + p2*x0^2 -p3*x0^3 + p4*x0^4 - p5*x0^5 + p6*x0^6)
;   + (p1 - 2*p2*x0 + 3*p3*x0^2 - 4*p4*x0^3 + 5*p5*x0^4 - 6*p6*x0^5)*x
;   + (p2 - 3*p3*x0 + 6*p4*x0^2 - 10*p5*x0^3 +15 * p6*x0^4)*x^2
;   + (p3 - 4*p4*x0 + 10*p5*x0^2 - 20*p6*x0^3)*x^3
;   + (p4 - 5*p5*x0 + 15*p6*x0^2)*x^4
;   + (p5 - 6*p6*x0)*x^5
;   + (p6)
;
x01 = double(x0)
x02 = x01^2
x03 = x01^3
x04 = x01^4
x05 = x01^5
x06 = x01^6
;
fincoeff[0] = usecoeff[0] - usecoeff[1]*x01 + usecoeff[2]*x02 $
		- usecoeff[3]*x03 + usecoeff[4]*x04 - usecoeff[5]*x05 $
		+ usecoeff[6]*x06

fincoeff[1] = usecoeff[1] - 2.0d*usecoeff[2]*x01 + 3.0d*usecoeff[3]*x02 $
		- 4.0d*usecoeff[4]*x03 + 5.0d*usecoeff[5]*x04 - 6*usecoeff[6]*x05

fincoeff[2] = usecoeff[2] - 3.0d*usecoeff[3]*x01 + 6.0d*usecoeff[4]*x02 $
		-10.0d*usecoeff[5]*x03 + 15.0d*usecoeff[6]*x04

fincoeff[3] = usecoeff[3] - 4.0d*usecoeff[4]*x01 + 10.0d*usecoeff[5]*x02 $
		- 20.0d*usecoeff[6]*x03

fincoeff[4] = usecoeff[4] - 5.0d*usecoeff[5]*x01 + 15.0d*usecoeff[6]*x02

fincoeff[5] = usecoeff[5] - 6.0d*usecoeff[6]*x01

fincoeff[6] = usecoeff[6]
;
; adjust to size
if s lt 7 then $
	fincoeff = fincoeff[0:(s-1)]
;
return,fincoeff
end		; pascal_shift

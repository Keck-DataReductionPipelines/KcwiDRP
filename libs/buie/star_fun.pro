;+
; NAME:
;	star_fun
;
; PURPOSE:
;	Single 2-d gaussian image, support routine for STARFIT.
;
; DESCRIPTION:
;	Evaluate the sum of one 2-d guassian and a 2-d 2nd order polynomial
;	and optionally return the value of it's partial derivatives.
;
; 	Evaluate the equation y=f(i) where:
;
;		X  = I MOD NX
;		Y  = I / NX
;
;		R  = SQRT( (X-A0)^2 + (Y-A1)^2 )
;
;		Z  = R/A3
;
; 		F(I) = A2*EXP(-Z1^2/2) + A4
;
;	Function parameters			     
;       -------------------------------------------- 
;	A0  = X location of center of Pluto,
;	A1  = Y location of center of Pluto,
;	A2  = height of gaussian for Pluto,
;	A3  = the 1/e width of the guassian,
;	A4  = Constant term for the background,
;
; CATEGORY
;       Function fitting
;
; CALLING SEQUENCE:
;	STARFIT_FUN,I,A,F[,PDER]
;
; INPUTS:
;	I = Independent variable, actually, just an index into 2-d array.
;	A = Parameters of equation described above.
;
; OPTIONAL KEYWORD PARAMETERS:
;	XSIZE = width of image
;
; OUTPUTS:
;	F = value of function at each F(I).
;
; OPTIONAL OUTPUT PARAMETERS:
;	PDER = (N_ELEMENTS(I),9) array containing the
;		partial derivatives.  P(I,J) = Derivative
;		at Ith point w/respect to Jth parameter.
; COMMON BLOCKS:
;	starfit_com - contains the x-width of the array stored in A
; SIDE EFFECTS:
;	NONE.
; RESTRICTIONS:
;	NONE.
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1993 January 13
;-
pro star_fun,i,a,f,pder,XSIZE=xsize

common starfit_com, nx

on_error,2

if keyword_set(xsize) then nx = xsize

x    = float(fix(i) mod nx)
y    = float(fix(i) / nx)

rsq  = (x - a[0])^2 + (y - a[1])^2
r    = sqrt( rsq )

if a[3] ne 0.0 then z = r/a[3] else z = 10.

ez = fltarr(n_elements(z))

big = where(abs(z) le 7.,count)
if count ne 0 then ez[big] = exp(-z[big]^2/2.)

f = a[2]*ez + a[4] ; + a(5)*x + a(6)*y ; + a(7)*x^2 + a(8)*y^2

if n_params(0) le 3 then return

pder = fltarr(n_elements(i),n_elements(a))

; Compute partial derivatives

if a[3] ne 0. then begin
   eza32 = ez/a[3]^2
   eza33 = ez/a[3]^3
endif else begin
   eza32 = 0.
   eza33 = 0.
endelse

pder[0,0]  = a[2] * (x-a[0]) * eza32
pder[0,1]  = a[2] * (y-a[1]) * eza32
pder[0,2]  = ez
pder[0,3]  = a[2] * rsq * eza33
pder[0,4]  = 1.

return

end

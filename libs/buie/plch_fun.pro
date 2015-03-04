pro plch_fun,i,a,f,pder,XSIZE=xsize

common plchfit_com, nx, dx,dy

;+
; NAME:
;	plch_fun
;
; PURPOSE:
;	Two 2-d gaussian images, support routine for PLCHFIT.
;
; DESCRIPTION:
;	Evaluate the sum of two 2-d guassian and a 2-d 2nd order polynomial
;	and optionally return the value of it's partial derivatives.
;
; 	Evaluate the equation y=f(i) where:
;
;		X  = I MOD NX
;		Y  = I / NX
;
;		R1 = SQRT( (X-A0)^2 + (Y-A1)^2 )
;		R2 = SQRT( (X-A0-A6)^2 + (Y-A1-A7)^2 )
;
;		Z1 = R1/A3
;		Z2 = R2/A3
;
; 		F(I) = A2*EXP(-Z1^2/2) + A5*EXP(-Z2^2/2) + A4
;
;	Function parameters
;       --------------------------------------------
;	A0  = X location of center of Pluto
;	A1  = Y location of center of Pluto
;	A2  = height of gaussian for Pluto
;	A3  = the 1/e width of the guassian
;	A4  = Constant term for the background
;	A5  = height of gaussian for Charon
;	A6  = X location of center of Charon relative to Pluto
;	A7  = Y location of center of Charon relative to Pluto
;
; CATEGORY
;       Function fitting
; CALLING SEQUENCE:
;	FUNCT,I,A,F[,PDER]
; INPUTS:
;	I = Independent variable, actually, just an index into 2-d array.
;	A = PARAMETERS OF EQUATION DESCRIBED BELOW.
;
; OPTIONAL KEYWORD PARAMETERS:
;	XSIZE = width of image
;
; OUTPUTS:
;	F = value of function at each F(I).
;
; OPTIONAL OUTPUT PARAMETERS:
;	PDER = (N_ELEMENTS(I),8 or 6) array containing the
;		partial derivatives.  P(I,J) = Derivative
;		at Ith point w/respect to Jth parameter.
; COMMON BLOCKS:
;	r2gauss_com - contains the x-width of the array stored in A
; SIDE EFFECTS:
;	NONE.
; RESTRICTIONS:
;	NONE.
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1993 January 13
;	93/07/01, MWB, changed to remove linear and quadratic background
;	   terms, added facility for forcing position of Charon.
;  2000/11/08, MWB, removed use of obsolete ()
;-

on_error,2

if keyword_set(xsize) then nx = xsize

if n_elements(a) eq 8 then begin
   dx = a[6]-a[0]
   dy = a[7]-a[1]
endif

x    = float(fix(i) mod nx)
y    = float(fix(i) / nx)

r1sq = (x - a[0])^2 + (y - a[1])^2
r1   = sqrt( r1sq )

r2sq = (x - (a[0]+dx))^2 + (y - (a[1]+dy))^2
r2   = sqrt( r2sq )

if a[3] ne 0.0 then begin
   z1 = r1/a[3]
   z2 = r2/a[3]
endif else begin
   z1 = 10.
   z2 = 10.
endelse

ez1 = fltarr(n_elements(z1))
ez2 = fltarr(n_elements(z2))

big = where(abs(z1) le 7.,count)
if count ne 0 then ez1[big] = exp(-z1[big]^2/2.)

big = where(abs(z2) le 7.,count)
if count ne 0 then ez2[big] = exp(-z2[big]^2/2.)

f = a[2]*ez1 + a[4] + a[5]*ez2

if n_params(0) le 3 then return

pder = fltarr(n_elements(i),n_elements(a))

; Compute partial derivatives

if a[3] ne 0. then begin
   ez1a32 = ez1/a[3]^2
   ez1a33 = ez1/a[3]^3
   ez2a32 = ez2/a[3]^2
   ez2a33 = ez2/a[3]^3
endif else begin
   ez1a32 = 0.
   ez1a33 = 0.
   ez2a32 = 0.
   ez2a33 = 0.
endelse

pder[*,2]  = ez1
pder[*,3]  = a[2] * r1sq * ez1a33 + a[5] * r2sq * ez2a33
pder[*,4]  = 1.
pder[*,5]  = ez2
if n_elements(a) eq 8 then begin
   pder[*,0]  = a[2] * (x-a[0]) * ez1a32
   pder[*,1]  = a[2] * (y-a[1]) * ez1a32
   pder[*,6]  = a[5] * (x-a[6]) * ez2a32
   pder[*,7]  = a[5] * (y-a[7]) * ez2a32
endif else begin
   pder[*,0]  = a[2] * (x-a[0]) * ez1a32 + a[5] * (x-(a[0]+dx)) * ez2a32
   pder[*,1]  = a[2] * (y-a[1]) * ez1a32 + a[5] * (y-(a[1]+dy)) * ez2a32
endelse

return

end

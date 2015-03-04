;+
; NAME:
;	starfit
;
; PURPOSE:
;	Single gaussian fit to a stellar image.
;
; DESCRIPTION:
; 	Fit the equation y=f(i) where:
;
;		X  = I MOD NX
;		Y  = I / NX
;
;		R  = SQRT( (X-A0)^2 + (Y-A1)^2 )
;
;		Z  = R/A3
;
; 		F(I) = A2*EXP(-Z^2/2) +
;			A4 + A5*X + A6*Y + A7*X^2 + A8*Y^2
;
;	Function parameters				Initial guess
;       --------------------------------------------    --------------------
;	A0  = X location of center of Pluto,		Maximum in image
;	A1  = Y location of center of Pluto,		Maximum in image
;	A2  = height of gaussian for Pluto,		Max - Min from image
;	A3  = the 1/e width of the guassian,		GUESS
;	A4  = Constant term for the background,		0
;	A5  = Linear term in X for the background,	0
;	A6  = Linear term in Y for the background,	0
;	A7  = Quadratic term in X for the background,	0
;	A8  = Quadratic term in Y for the background,	0
;
; CATEGORY:
;	Function fitting
;
; CALLING SEQUENCE:
;  starfit, image, guess, model, a, sigmaa
;
; INPUTS:
;	image  - Input image to be fitted, must be 2-d.
;	guess  - Input information for starting guess.
;		  guess = estimate of the 1/e full width of the seeing.
;
; OUTPUTS:
;	model  - The fitted model image is returned.
;	a      - The final coefficient array.
;	sigmaa - The uncertainties on the coefficients.
;
; OPTIONAL OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;	starfit_com - contains the xwidth of the image.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Obs., 1993 January 13.
;-
;
pro starfit, image, guess, model, a, sigmaa

common starfit_com, nx

on_error,2                      ;Return to caller if an error occurs

if n_params() lt 5 then begin
   print,'starfit,image,guess,model,a,sigmaa'
   return
end

sz = size(image)

if sz[0] ne 2 then message,'Input data must be a two-D array'
if n_elements(guess) ne 1 then message,'Initial guess array must be a scalar value'

nx = sz[1]

y = reform(image,sz[4])

;Get the maximum in the image and its location, this will be the initial guess
;  for Object's location
ymax = max(y)
ymin = min(y)
imax = where(y eq ymax)
xmaxpos = imax[0] mod nx
ymaxpos = imax[0] / nx

;The guess for Object's peak value is the difference between the min and max
dy=ymax-ymin

;Set up the initial coefficient array
;a = [xmaxpos,ymaxpos,dy,guess,ymin,0.,0.,0.,0.]
a = [xmaxpos,ymaxpos,dy,guess,ymin]

;Set up the index array
i = indgen(sz[4])

;Call curvefit to do the acutal fitting.
yfit = curvefit(i,y,replicate(1.,sz[4]),a,sigmaa,function_name="star_fun")

model = reform(yfit,sz[1],sz[2])

end

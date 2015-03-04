pro plchfit, image, guess, model, a, sigmaa, FORCE=force

common plchfit_com, nx, dx, dy

;+
; NAME:
;	plchfit
;
; PURPOSE:
;	Two gaussian fit to an image of the Pluto-Charon system.
;
; DESCRIPTION:
; 	Fit the equation y=f(i) where:
;
;		X  = I MOD NX
;		Y  = I / NX
;
;		R1 = SQRT( (X-A0)^2 + (Y-A1)^2 )
;		R2 = SQRT( (X-A9)^2 + (Y-A10)^2 )
;
;		Z1 = R1/A3
;		Z2 = R2/A3
;
; 		F(I) = A2*EXP(-Z1^2/2) + A5*EXP(-Z2^2/2) +A4
;
;	Function parameters				Initial guess
;       --------------------------------------------    --------------------
;	A0  = X location of center of Pluto,		Maximum in image
;	A1  = Y location of center of Pluto,		Maximum in image
;	A2  = height of gaussian for Pluto,		Max - Min from image
;	A3  = the 1/e width of the guassian,		GUESS(0)
;	A4  = Constant term for the background,		0
;	A5  = height of gaussian for Charon,		A2/7
;	A6  = X location of center of Charon,		A0 + GUESS(1)
;	A7  = Y location of center of Charon,		A1 + GUESS(2)
;
; CATEGORY:
;	Function fitting
;
; CALLING SEQUENCE:
;	plchfit,i,y,numx,guess,model,a,sigmaa
;
; INPUTS:
;	image  - Input image to be fitted, must be 2-d.
;	guess  - Input information for starting guess.
;		  guess(0) = estimate of the 1/e full width of the seeing.
;		  guess(1) = X offset of Charon relative to Pluto.
;		  guess(2) = Y offset of Charon relative to Pluto.
;
; OUTPUTS:
;	model  - The fitted model image is returned.
;	a      - The final coefficient array.
;	sigmaa - The uncertainties on the coefficients.
;
; OPTIONAL OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;	plchfit_com - contains the xwidth of the image.
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
;	93/07/01, MWB, Converted to new fitting function protocol.
;-
;
on_error,2                      ;Return to caller if an error occurs

if n_params() lt 5 then begin
   print,'plchfit,i,y,numx,guess,model,a,sigmaa'
   return
end

sz = size(image)

if sz[0] ne 2 then message,'Input data must be a two-D array'
if n_elements(guess) ne 3 then message,'Initial guess array must contain 3 items'

nx = sz[1]

y = reform(image,sz[4])

;Get the maximum in the image and its location, this will be the initial guess
;  for Pluto's location
ymax = max(y)
ymin = min(y)
imax = where(y eq ymax)
xmaxpl = imax[0] mod nx
ymaxpl = imax[0] / nx

;The guess for Pluto's peak value is the difference between the min and max
di=ymax-ymin

if keyword_set(force) then begin
   ; Set the forced values for dx and dy for Charon
   dx = guess[1]
   dy = guess[2]
   a = [xmaxpl,ymaxpl,di,guess[0],ymin,di/7.]
endif else begin
   ;Set up the initial guess for Charon from the input info.
   xch    = xmaxpl + guess[1]
   ych    = ymaxpl + guess[2]

   ;Set up the initial coefficient array
   a = [xmaxpl,ymaxpl,di,guess[0],ymin,di/7.,xch,ych]
endelse

;Set up the index array
i = indgen(sz[4])

;Call curvefit to do the acutal fitting.
yfit = curvefit(i,y,replicate(1.,sz[4]),a,sigmaa,function_name="plch_fun")

model = reform(yfit,sz[1],sz[2])

end

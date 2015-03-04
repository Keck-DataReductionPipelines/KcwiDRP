;+
; NAME:
;	synstar2
; PURPOSE: (one line)
;	Compute a synthetic (Lorentizian) star image.
; DESCRIPTION:
;	This uses the functional form of a Lorentzian point-spread function
;	as described in Bosh et. al., Icarus, 95, 319-324 (1992).
; CATEGORY:
;       Astronomy
; CALLING SEQUENCE:
;	image = synstar(nx,ny,x,y,flux,power,fwhm)
; INPUTS:
;	nx   - X dimension of image.
;	ny   - Y dimension of image.
;	x    - X location(s) of synthetic star(s).
;	y    - Y location(s) of synthetic star(s).
;	flux - Integrated strength of star(s).
;	power- Exponent for the r dependence (must be greater than 1)
;	fwhm - Full Width at Half Maximum of stellar image.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	return - nx by ny image containing star.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	92/11/05 - Written by Marc W. Buie, Lowell Observatory
;-
function synstar2,nx,ny,x,y,flux,power,fwhm

print,'SYNSTAR2: WARNING this function does not work properly yet.'

   const = 2.0 * power * sin(!pi/power) / !pi / fwhm

   img=fltarr(nx,ny)
   tmp=fltarr(nx,ny)
   if ((n_elements(x) ne n_elements(y)) and $
       (n_elements(x) ne n_elements(flux))) then begin
      print,'SYNSTAR2: x, y, and flux vectors must be the same length'
      return,img
   endif
   if power le 1 then begin
      print,'SYNSTAR2: power must be greater than 1'
      return,img
   endif

   xr = findgen(nx)
   for yr=0,ny-1 do begin
      tmp[*,yr] = (2.0*sqrt((xr-x[0])^2 + (float(yr)-y[0])^2)/fwhm)^power
   endfor
;   z=where(tmp le 80.0,count)
;   if (count ne 0) then img[z]=const/(1.0+tmp[z])
   img=const/(1.0+tmp)

   if (n_elements(x) gt 1) then begin
      for i=1,n_elements(x)-1 do begin
         xr = findgen(nx)
         for yr=0,ny-1 do begin
            tmp[*,yr] = ((xr-x[i])^2 + (float(yr)-y[i])^2)/sigsq
         endfor
;         z=where(tmp le 80.0,count)
;         if (count ne 0) then img[z]=img[z]+exp(-tmp[z])/sigsq/!pi*flux[i]
      img=img+const/(1.0+tmp)
      endfor
   endif

   return,img
end

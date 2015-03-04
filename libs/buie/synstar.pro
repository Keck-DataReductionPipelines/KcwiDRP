;+
; NAME:
;	synstar
; PURPOSE: (one line)
;	Compute a synthetic (gaussian) star image.
; DESCRIPTION:
; CATEGORY:
;	Mathematical
; CALLING SEQUENCE:
;	image = synstar(nx,ny,x,y,flux,sig)
; INPUTS:
;	nx   - X dimension of image.
;	ny   - Y dimension of image.
;	x    - X location(s) of synthetic star(s).
;	y    - Y location(s) of synthetic star(s).
;	flux - Integrated strength of star(s).
;	sig  - 1/e half-width of star image.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	return - nx by ny image containing star.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-
function synstar,nx,ny,x,y,flux,sig
   img=fltarr(nx,ny)
   tmp=fltarr(nx,ny)
   if ((n_elements(x) ne n_elements(y)) and $
       (n_elements(x) ne n_elements(flux))) then begin
      print,'SYNSTAR: x, y, and flux vectors must be the same length'
      return,img
   endif

   xr = findgen(nx)
   sigsq=sig^2
   for yr=0,ny-1 do begin
      tmp[*,yr] = 0.5*((xr-x[0])^2 + (float(yr)-y[0])^2)/sigsq
   endfor
   z=where(tmp le 80.0,count)
   if (count ne 0) then img[z]=exp(-tmp[z])*flux[0]

   if (n_elements(x) gt 1) then begin
      for i=1,n_elements(x)-1 do begin
         xr = findgen(nx)
         sigsq=sig^2
         for yr=0,ny-1 do begin
            tmp[*,yr] = 0.5*((xr-x[i])^2 + (float(yr)-y[i])^2)/sigsq
         endfor
         z=where(tmp le 80.0,count)
         if (count ne 0) then img[z]=img[z]+exp(-tmp[z])*flux[i]
      endfor
   endif

   return,img
end

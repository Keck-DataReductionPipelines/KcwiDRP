;+
; NAME:
;	radp
; PURPOSE: (one line)
;	Extract point-wise radial ``profile'' from image data.
; DESCRIPTION:
;
;	This routine will reorder the image into two 1-d vectors.  One vector
;	is the radial distance from a point and the other vector is the
;	intensity for that distance.  The image data is not modified, every
;	input pixel is returned, just in a different form.
;
;	Optionally, this program will attempt to fit a gaussian to the profile
;	and will return the FWHM of the image.  You can also get the actual
;	fitted coefficients and the computed functional value.  The fitting
;	is done with the procedure rgfit and its associated routine
;	rgauss_funct for computing the function.
;
; CATEGORY:
;	CCD data processing
; CALLING SEQUENCE:
;	radp,image,xcen,ycen,r,i,[fwhm,[coefs,[yfit]]]
; INPUTS:
;	image - Input image to create profile from.
;	xcen  - X-coordinate for r=0
;	ycen  - Y-coordinate for r=0
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;	QONLY - No linear term in fit, just constant and quadratic.
;	CONLY - Guassian and constant only.
;	FZWID - Freeze the full-width half max to this value.
; OUTPUTS:
;	r     - the r coordinates for all input pixels.
;	i     - the input image after reordering to match r vector.
; OPTIONAL OUTPUT PARAMETERS:
;	fwhm  - Return value of the gaussian fit to the profile, in pixels.
;	coefs - Fitted function coefficients. (See rgfit.pro)
;	yfit  - Fitted values.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	92/11/04, Written by Marc W. Buie, Lowell Observatory
;-
pro radp,image,xcen,ycen,r,i,fwhm,coefs,yfit, $
   QONLY=qonly,CONLY=conly,FZWID=fzwid

   if keyword_set(qonly) and keyword_set(conly) then $
      message,'Only one of QONLY and CONLY can be specified'

   stat=size(image)
   r=fltarr(stat[1],stat[2],/nozero)
   i=fltarr(stat[1],stat[2],/nozero)
   len = stat[1]*stat[2]

   x=findgen(stat[1])
   for y=0,stat[2]-1 do begin
      r[*,y] = x
      i[*,y] = fltarr(stat[1])+y
   endfor

   r = sqrt( (r-xcen)^2 + (i-ycen)^2 )
   i = image

   r=reform(r,len,/overwrite)
   i=reform(i,len,/overwrite)

   if (n_params() ge 6) and (n_params() le 8) then begin
      if keyword_set(conly) then begin
         fit_val = rcgfit(r,i,coef)
      endif else if keyword_set(qonly) then begin
         fit_val = rqgfit(r,i,coef)
      endif else if keyword_set(fzwid) then begin
         fit_val = rfgfit(r,i,fzwid,coef)
      endif else begin
         fit_val = rgfit(r,i,coef)
      endelse
      fwhm = coef[1]*2.35482  ; const is 2*sqrt(-2*alog(0.5))
      if n_params() ge 7 then coefs=coef
      if n_params() eq 8 then yfit=fit_val
   endif

end

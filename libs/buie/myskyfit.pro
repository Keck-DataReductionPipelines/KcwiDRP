;+
; NAME:
;  skyfit
; PURPOSE:   (one line only)
;  Determine a 2-d polynomial fit to sky background in an image.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  skyfit,image,skyimage
; INPUTS:
;  image - Array which is the image to be analyzed
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DISPLAY - Flag, if set will generate lots of plotting output.
;  XORDER  - order of fit to X direction (default=0, constant)
;  YORDER  - order of fit to Y direction (default=0, constant)
;  LOWCLIP - fraction of random sample to clip at the low end of the signal.
;              If lowclip=.1 and npts=100, then the 10 lowest values in the
;              random sample are excluded BEFORE the robust mean is computed
;              for the stretch range.  This option will probably be just a bit
;              slower if invoked.  This option will likely be more robust
;              against extreme values in the image.  Default=0.0 (no clipping)
;  HICLIP  - fraction of random sample to clip at the high end of the signal.
;              If hiclip=.9 and npts=100, then the 10 highest values in the
;              random sample are excluded BEFORE the robust mean is computed
;              for the stretch range.  This option will probably be just a bit
;              slower if invoked.  This option will likely be more robust
;              against extreme values in the image.  Default=1.0 (no clipping)
;  NPTS   - Number of pixels to use in fit (default=601)
;  SILENT - Flag, if set will suppress information output to screen.
; OUTPUTS:
;  skyimage - Smooth image of sky in image.
; KEYWORD OUTPUT PARAMETERS:
;  COEFF   - Coefficients of fit
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2004/07/02
;  2005/06/22, MWB, Added error traps
;-
pro myskyfit,image,skyimage,COEFF=coeff,SKYSIG=skysig,SILENT=silent, $
       ORDER=order,HICLIP=hiclip,DISPLAY=display,ERROR=error

   self='MYSKYFIT: '
   if badpar(image,[1,2,3,4,5,12,13,14,15],2, $
             caller=self+'(image): ',dimen=dimen) then return
   if badpar(order,[0,2,3],0, $
             caller=self+'(ORDER): ',default=0  ) then return
   if badpar(lowclip,[0,4,5],0, $
             caller=self+'(LOWCLIP): ',default=0.0) then return
   if badpar(hiclip,[0,4,5],0, $
             caller=self+'(HICLIP): ',default=1.0) then return
   if badpar(display,[0,2,3],0, $
             caller=self+'(DISPLAY): ',default=0  ) then return
   if badpar(silent,[0,2,3],0, $
             caller=self+'(SILENT): ',default=0  ) then return

   error=0

   ; Limit to proper range
   lowclip=(lowclip > 0.0) < 1.0
   hiclip =( hiclip < 1.0) > 0.0

   ; error check
   if lowclip ge hiclip then begin
      print,self,' lowclip must be less than hiclip. Aborting.'
      error=1
      return
   endif

   skyimage = image - image

   xx = findgen(dimen[1])
   for i=0l,dimen[0]-1 do begin
	   y = reform(image[i,*])
	   good = where(finite(y) eq 1, ngood)
	   if ngood gt 10 then begin
		x = xx[good]
		y = y[good]
	   	w = sqrt(y)
		ims_asym,y,mn,sig,wgt,siglim=[2.0,5.0]
		keep = where(wgt eq 1, nkeep)
		if nkeep gt 10 then begin
			x = x[keep]
			y = y[keep]
			w = w[keep]
	   		coeff = poly_fit(x,y,order,measure_err=w)
			yfit = poly(xx,coeff)
	   		skyimage[i,*] = yfit[*]
		endif
	   endif
   endfor

end


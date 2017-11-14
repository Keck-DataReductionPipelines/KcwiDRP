;
; Copyright (c) 2017, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MAKE_SKY
;
; PURPOSE:
;	This procedure creates a sky model from the input image.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_MAKE_SKY, Ppar, Img, Hdr, Gfil, Sky
;
; INPUTS:
;	Ppar	- KCWI_PPAR struct for flat group to combine
;	Img	- Object image with lots of sky pixels!
;	Hdr	- Image header
;	Gfil	- Geometry solution file
;
; OUTPUTS:
;	Sky	- The sky model for subtracting
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Fits the sky with bsplines.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2017-NOV-13	Initial version
;-
pro kcwi_make_sky,ppar,img,hdr,gfil,sky
	;
	; initialize
	pre = 'KCWI_MAKE_SKY'
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; is geometry solved?
	if not file_test(gfil) then begin
		kcwi_print_info,ppar,pre,'no geometry file',/error
		return
	endif
	;
	; read in geometry file
	kgeom = mrdfits(gfil,1,ghdr,/silent)
	if kgeom.status ne 0 then begin
		kcwi_print_info,ppar,pre,'bad geometry solution',/error
		return
	endif
	;
	; read in wavemap image
	wmf = repstr(gfil,'_geom', '_wavemap')
	if file_test(wmf) then begin
		wavemap = mrdfits(wmf,0,wmfh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no wavemap file',/error
		return
	endelse
	;
	; read in slice image
	slf = repstr(gfil,'_geom','_slicemap')
	if file_test(slf) then begin
		slice = mrdfits(slf,0,slfh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no slicemap file',/error
		return
	endelse
	;
	; read in position image
	pof = repstr(gfil,'_geom','_posmap')
	if file_test(pof) then begin
		pos = mrdfits(pof,0,pofh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no posmap file',/error
		return
	endelse
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
	;
	; prepare plots
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	;
	; do fitting
	finiteflux = finite(img)
	q = where(slice ge 0 and slice le 23 and pos ge 0 and $
		  wavemap ge kgeom.waveall0 and wavemap lt kgeom.waveall1 and $
		  finiteflux)
	fluxes=img
	waves=wavemap
	fluxes=img[q]
	waves=wavemap[q]
	owaves=waves
	s=sort(waves)
	waves=waves[s]
	fluxes=fluxes[s]
	plot,waves,smooth(fluxes,250),psym=3,yrange=[0,200]
	stop
	;
	; knots
	n=8000

	bkpt=min(waves)+findgen(n+1)*(max(waves)-min(waves))/n
	print,minmax(bkpt)
	sft0=bspline_iterfit(waves,fluxes,fullbkpt=bkpt,yfit=yfit1,upper=1,lower=1)
	yfit=bspline_valu(owaves,sft0)

	oplot,waves,yfit1,color=colordex('orange')
	sky=img-img
	sky[q]=yfit
	print,"Done!"
	stop
	;
	; output file name
	ofn = sxpar(hdr,'OFNAME')
	sfil = repstr(ofn,'.fits','_sky.fits')
	;
	; update sky header
	shdr = hdr
	sxaddpar,shdr,'HISTORY','  '+pre+' '+systime(0)
	;
	; write out image file
	kcwi_write_image,sky,shdr,sfil,ppar
	;
	return
end	; kcwi_make_sky

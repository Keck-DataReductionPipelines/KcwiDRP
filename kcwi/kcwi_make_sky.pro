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
;	KCWI_MAKE_SKY, Ppar, Img, Hdr, Gfil, Sky, SKY_MASK_FILE=<skymaskfile>
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
;	SKY_MASK_FILE	- a text file containing regions to mask using
;		five white-space separated columns for each region you
;		want to mask: 
;		1 - slice
;		2 - slice position (px) start
;		3 - slice position (px) end
;		4 - wavelength (A) start
;		5 - wavelength (A) end
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
pro kcwi_make_sky,ppar,img,hdr,gfil,sky,sky_mask_file=skymf
	;
	; initialize
	pre = 'KCWI_MAKE_SKY'
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
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
		kcwi_print_info,ppar,pre,'reading wavemap file ',wmf
		wavemap = mrdfits(wmf,0,wmfh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no wavemap file',/error
		return
	endelse
	;
	; read in slice image
	slf = repstr(gfil,'_geom','_slicemap')
	if file_test(slf) then begin
		kcwi_print_info,ppar,pre,'reading slicemap file',slf
		slicemap = mrdfits(slf,0,slfh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no slicemap file',/error
		return
	endelse
	;
	; read in position image
	pof = repstr(gfil,'_geom','_posmap')
	if file_test(pof) then begin
		kcwi_print_info,ppar,pre,'reading posmap file  ',pof
		posmap = mrdfits(pof,0,pofh,/fscale,/silent)
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
	; sky map, if requested
	if keyword_set(sky_mask_file) then begin
	endif
	;
	; prepare plots
	do_plots = (ppar.display ge 1)
	if do_plots then begin
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		th=3
		si=1.7
		ask = ''
	endif
	;
	; do bspline fitting
	;
	; use only finite values
	finiteflux = finite(img)
	;
	; get points mapped to exposed regions on CCD
	q = where(slicemap ge 0 and slicemap le 23 and posmap ge 0 and $
		  wavemap ge kgeom.waveall0 and wavemap lt kgeom.waveall1 and $
		  finiteflux)
	;
	; extract relevant image values
	fluxes=img[q]
	;
	; relevant wavelengths
	waves=wavemap[q]
	;
	; keep output wavelengths
	owaves=waves
	;
	; mask, if requested
	if keyword_set(skymf) gt 0 then begin
		if file_test(skymf) then begin
			readcol,skymf,msli0, msli1, mp0, mp1, mw0, mw1, $
				/silent,comment='#'
			kcwi_print_info,ppar,pre,'Sky mask file read in', $
				skymf
			kcwi_print_info,ppar,pre,'Number of masked regions', $
				n_elements(msli0)
			;
			; relevant positions
			pos = posmap[q]
			;
			; relevant slice numbers
			slice = slicemap[q]
			;
			; count masked pixels
			tmsk = 0L
			for i = 0, n_elements(msli0)-1 do begin
				msk = where(slice ge msli0[i] and $
					slice le msli1[i] and $
					pos ge mp0[i] and pos le mp1[i] and $
					waves ge mw0[i] and waves le mw1[i], $
					nmsk)
				if nmsk gt 0 then begin
					fluxes[msk] = !values.f_nan
					tmsk += nmsk
				endif else $
					kcwi_print_info,ppar,pre, $
					 'empty mask region', i,/warning
			endfor
			finiteflux = finite(fluxes)
			good = where(finiteflux)
			fluxes = fluxes[good]
			waves = waves[good]
			kcwi_print_info,ppar,pre, $
				'Total number of pixels masked',tmsk
		endif else $
			kcwi_print_info,ppar,pre,'Sky mask file not found', $
				skymf,/warning
	endif
	;
	; sort on wavelength
	s=sort(waves)
	waves=waves[s]
	fluxes=fluxes[s]
	;
	; knots
	if kgeom.ifunum ge 3 then $
		n = 8000 $
	else if kgeom.ifunum eq 2 then $
		n = 5000 $
	else if kgeom.ifunum le 1 then $
		n = 2000
	;
	; calculate break points for b splines
	bkpt = min(waves) + findgen(n+1) * (max(waves) - min(waves)) / n
	;
	; log
	kcwi_print_info,ppar,pre,'Nknots, Min, Max breakpoints (A)', $
		n, minmax(bkpt),format='(a,i6,2f13.2)'
	;
	; do bspline fit
	sft0 = bspline_iterfit(waves,fluxes,fullbkpt=bkpt,yfit=yfit1, $
				upper=1,lower=1)
	;
	; get values at original wavelengths
	yfit = bspline_valu(owaves,sft0)

	if do_plots then begin
		fsmo = smooth(fluxes,250)
		mo = moment(fsmo)
		yrng = [0, max([mo[0]+5.*sqrt(mo[1]), max(yfit1)])]
		plot,waves,fsmo,psym=3,title=sxpar(hdr,'OFNAME'), $
			charsi=si, charthi=th, $
			xtitle='Wavelength (A)',xthick=th, /xs, $
			ytitle='Flux (e-)', ythick=th, yrange=yrng
		oplot,waves,yfit1,color=colordex('orange')
		oplot,[kgeom.wavegood0,kgeom.wavegood0],!y.crange, $
			color=colordex('green')
		oplot,[kgeom.wavegood1,kgeom.wavegood1],!y.crange, $
			color=colordex('green')
		kcwi_legend,['DATA','FIT'],charsi=si,charthi=th, $
			color=[colordex('black'), colordex('orange')], $
			linesty=[0,0],/clear,clr_color=!p.background
		if ppar.display ge 2 then read,'next: ',ask
	endif
	;
	; create sky image
	sky = img - img
	sky[q] = yfit
	;
	; output file name
	ofn = sxpar(hdr,'OFNAME')
	sfil = repstr(ofn,'.fits','_sky.fits')
	;
	; update sky header
	shdr = hdr
	sxaddpar,shdr,'HISTORY','  '+pre+' '+systime(0)
	sxaddpar,shdr,'SKYMODEL','T',' sky model image?'
	sxaddpar,shdr,'SKYIMAGE',ofn,' image used for sky model'
	;
	; write out image file
	kcwi_write_image,sky,shdr,sfil,ppar
	;
	return
end	; kcwi_make_sky

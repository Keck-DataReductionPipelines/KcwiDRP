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
;		six white-space separated columns for each region you
;		want to mask: 
;		1 - slice number start
;		2 - slice number end
;		3 - slice position (px) start
;		4 - slice position (px) end
;		5 - wavelength (A) start
;		6 - wavelength (A) end
;
;		OR
;
;	FITS	- set if sky mask is a binary FITS image
;			where 0 = use, 1 - mask, otherwise assume
;			sky mask is a text file as above
;
; PROCEDURE:
;	Fits the sky with bsplines.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2017-NOV-13	Initial version
;	2017-NOV-26	Added fits keyword and re-implemented sky masking
;-
pro kcwi_make_sky,ppar,img,hdr,gfil,sky,sky_mask_file=skymf,fits=fits
	;
	; initialize
	pre = 'KCWI_MAKE_SKY'
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
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
		kcwi_print_info,ppar,pre,'reading wavemap file ',wmf, $
			format='(a,a)'
		wavemap = mrdfits(wmf,0,wmfh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no wavemap file',/error
		return
	endelse
	;
	; read in slice image
	slf = repstr(gfil,'_geom','_slicemap')
	if file_test(slf) then begin
		kcwi_print_info,ppar,pre,'reading slicemap file',slf, $
			format='(a,a)'
		slicemap = mrdfits(slf,0,slfh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no slicemap file',/error
		return
	endelse
	;
	; read in position image
	pof = repstr(gfil,'_geom','_posmap')
	if file_test(pof) then begin
		kcwi_print_info,ppar,pre,'reading posmap file  ',pof, $
			format='(a,a)'
		posmap = mrdfits(pof,0,pofh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no posmap file',/error
		return
	endelse
	;
	; get size
	sm_sz = size(slicemap,/dim)
	;
	; sky masking
	;
	; default is no masking (0 = use, 1 = mask)
	binary_mask = bytarr(sm_sz[0], sm_sz[1])
	;
	; was sky masking requested?
	if keyword_set(skymf) then begin
		;
		; was it supplied as an image?
		if keyword_set(fits) then begin
			;
			; is the image accessible?
			if file_test(skymf) then begin
				binary_mask = mrdfits(skymf,/unsigned,/silent)
				bm_sz = size(binary_mask,/dim)
				;
				; is there a size mis-match?
				if (bm_sz[0] ne sm_sz[0]) or $
				   (bm_sz[1] ne sm_sz[1]) then begin
					kcwi_print_info,ppar,pre, $
				'Sky mask size mis-match: masking disabled', $
					/warning
					binary_mask = bytarr(sm_sz[0], sm_sz[1])
				endif
			;
			; fall back to no masking
			endif else $
				kcwi_print_info,ppar,pre, $
					'Sky mask image not found',skymf, $
					format='(a,a)',/warning
		;
		; was it supplied as a text region file?
		endif else begin
		    ;
		    ; is the text region file accessible?
		    if file_test(skymf) then begin
			;
			; read regions
			readcol,skymf,msli0, msli1, mp0, mp1, mw0, mw1, $
				/silent,comment='#'
			kcwi_print_info,ppar,pre,'Sky mask file read in', $
				skymf
			kcwi_print_info,ppar,pre,'Number of masked regions', $
				n_elements(msli0)
			;
			; loop over regions
			for i = 0, n_elements(msli0)-1 do begin
				msk = where(slicemap ge msli0[i] and $
					    slicemap le msli1[i] and $
					    posmap ge mp0[i] and $
					    posmap le mp1[i] and $
					    wavemap ge mw0[i] and $
					    wavemap le mw1[i], nmsk)
				;
				; set masked pixels
				if nmsk gt 0 then $
					binary_mask[msk] = 1b $
				else	kcwi_print_info,ppar,pre, $
					 	'empty mask region', i,/warning
			endfor
		    endif else $
			kcwi_print_info,ppar,pre, $
				'Sky mask region file not found', $
				skymf, format='(a,a)',/warning
		endelse
	;
	; was sky masking requested?
	endif
	;
	; count masked pixels
	msk = where(binary_mask ne 0, tmsk)
	kcwi_print_info,ppar,pre,'Number of pixels masked',tmsk
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
	; get un-masked points mapped to exposed regions on CCD
	q = where(slicemap ge 0 and slicemap le 23 and posmap ge 0 and $
		  wavemap ge kgeom.waveall0 and wavemap lt kgeom.waveall1 and $
		  finiteflux and binary_mask eq 0)
	;
	; get all points mapped to exposed regions on the CCD (for output)
	qo = where(slicemap ge 0 and slicemap le 23 and posmap ge 0 and $
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
	owaves=wavemap[qo]
	;
	; sort on wavelength
	s=sort(waves)
	waves=waves[s]
	fluxes=fluxes[s]
	;
	; knots
	n = fix(sm_sz[1] * 1.25)
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
	; check result
	if max(yfit1) le 0. then begin
		kcwi_print_info,ppar,pre,'bspline failure',/warn
		if n gt 2000 then begin
			if n eq 5000 then n = 2000
			if n eq 8000 then n = 5000
			;
			; calculate new break points
			bkpt = min(waves) + findgen(n+1) * $
			       (max(waves) - min(waves)) / n
			;
			; log
			kcwi_print_info,ppar,pre, $
				'Nknots, Min, Max breakpoints (A)', $
				n, minmax(bkpt),format='(a,i6,2f13.2)'
			;
			; do bspline fit
			sft0 = bspline_iterfit(waves,fluxes,fullbkpt=bkpt, $
						yfit=yfit1,upper=1,lower=1)
		endif	; n gt 2000
		;
		; still not good
		if max(yfit1) le 0. then $
			kcwi_print_info,ppar,pre, $
				'bspline final failure, sky is zero',/warn
	endif	; max(yfit) le 0.
	;
	; get values at original wavelengths
	yfit = bspline_valu(owaves,sft0)
	;
	; plot, if requested
	if do_plots then begin
		fsmo = smooth(fluxes,250)
		pg = where(waves ge kgeom.wavegood0 and $
			   waves le kgeom.wavegood1, npg)
		if npg gt 3 then begin
			mo = moment(fsmo[pg])
			yrng = [0, max([mo[0]+5.*sqrt(mo[1]), max(yfit1[pg])])]
		endif else begin
			mo = moment(fsmo)
			yrng = [0, max([mo[0]+5.*sqrt(mo[1]), max(yfit1)])]
		endelse
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
	sky[qo] = yfit
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
	if tmsk gt 0 then begin
		sxaddpar,shdr,'SKYMSK','T',' was sky masked?'
		sxaddpar,shdr,'SKYMSKF', skymf, ' sky mask file'
	endif
	;
	; write out image file
	kcwi_write_image,sky,shdr,sfil,ppar
	;
	return
end	; kcwi_make_sky

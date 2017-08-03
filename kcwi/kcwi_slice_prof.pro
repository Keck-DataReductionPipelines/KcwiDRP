;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SLICE_PROF
;
; PURPOSE:
;	This procedure creates a slice profile image from the input
;	data cube.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SLICE_PROF, Kcfg,  Ppar, Prof
;
; INPUTS:
;	Kcfg	- KCWI_CFG struct for the input data cube, preferrably
;			from a sky or dome-flat observation
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Prof	- an array giving the normalized profile response
;
; SIDE EFFECTS:
;	Outputs a fits image of the profile response with same image
;	number root as the input file, but with '_prof' appended. For
;	example, if 'image1234.fits' is pointed to by the input 
;	KCWI_CFG struct, then the output profile image would have the
;	filename 'image1234_prof.fits'.
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	For each slice, sums along wavelength to derive the profile
;	normalized to the average of the good pixel values.  This is
;	then suitable for dividing out the slice profile.
;
; EXAMPLE:
;
; TODO:
;	Make plot scaling more consistent between slices
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-SEP-20	Initial version
;	2013-SEP-23	Uses inclusive wavelengths
;	2013-NOV-12	Put spatial inclusive limits in KCWI_PPAR struct (ppar)
;	2014-APR-08	Check against zero-like values in profile causing overflow
;	2017-AUG-02	Now use mean prof from center
;-
pro kcwi_slice_prof,kcfg,ppar,profs
	;
	; setup
	pre = 'KCWI_SLICE_PROF'
	;
	; check inputs
	if kcwi_verify_cfg(kcfg,/init) ne 0 then return
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
	;
	; is this a sky ? (add check for dome flat later)
	if kcfg.skyobs eq 0 and kcfg.shuffmod eq 0 then begin
		kcwi_print_info,ppar,pre,'not a sky obs',/warning
	endif
	;
	; what kind are we?
	if kcfg.shuffmod eq 1 then $
		kcwi_print_info,ppar,pre,'nod-and-shuffle sky obs',/info
	if kcfg.skyobs eq 1 then $
		kcwi_print_info,ppar,pre,'standard sky obs',/info
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; get output image (in reduced dir)
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_prof',/reduced)
	if file_test(ofil) then begin
		if ppar.clobber ne 1 then begin
			kcwi_print_info,ppar,pre,'output file already exists',ofil,/error
			return
		endif else $
			kcwi_print_info,ppar,pre,'output file will be overwritten',ofil,/warning
	endif
	;
	; check nod-and-shuffle
	if kcfg.nasmask eq 1 then begin
		if kcfg.shuffmod eq 1 then begin
			;
			; read sky cube from nod-and-shuffle image set
			icub = kcwi_read_image(kcfg.imgnum,ppar,'_scube',hdr, $
						/calib,status=stat)
			if stat ne 0 then return
			scub = sqrt(kcwi_read_image(kcfg.imgnum,ppar,'_vcube', $
							hdr,/calib,status=stat))
			if stat ne 0 then return
		endif else begin
			;
			; read sky observation image
			icub = kcwi_read_image(kcfg.imgnum,ppar,'_icube',hdr, $
						/calib,status=stat)
			if stat ne 0 then return
			scub = sqrt(kcwi_read_image(kcfg.imgnum,ppar,'_vcube', $
							hdr,/calib,status=stat))
			if stat ne 0 then return
		endelse
	endif else begin
		;
		; read sky observation image
		icub = kcwi_read_image(kcfg.imgnum,ppar,'_icube',hdr, $
					/calib,status=stat)
		if stat ne 0 then return
		scub = sqrt(kcwi_read_image(kcfg.imgnum,ppar,'_vcube',hdr, $
					/calib,status=stat))
		if stat ne 0 then return
	endelse
	;
	; get size
	sz = size(icub,/dim)
	;
	; default pixel ranges
	y0 = 175
	y3 = sz[2] - 175
	dely = fix( ( y3 - y0 ) / 3. )
	y1 = y0 + dely
	y2 = y1 + dely
	;
	; get wavelength scale
	w0 = sxpar(hdr,'CRVAL3')
	dw = sxpar(hdr,'CDELT3')
	;
	; get wavelength range for only good data
	wall0 = sxpar(hdr,'WAVGOOD0')
	wall1 = sxpar(hdr,'WAVGOOD1')
	;
	; compute good y pixel ranges
	if w0 gt 0. and dw gt 0. and wall0 gt 0. and wall1 gt 0. then begin
		y0 = fix( (wall0 - w0) / dw ) + 20
		y3 = fix( (wall1 - w0) / dw ) - 10
		dely = fix( ( y3 - y0 ) / 3. )
		y1 = y0 + dely
		y2 = y1 + dely
	endif
	;
	; sigma noise limit
	sl = 3.
	;
	; profiles
	profs = fltarr(24,sz[1])
	;
	; good range
	gx0 = 5
	gx1 = sz[1] - gx0 - 1
	px = findgen(sz[1])
	g = where(px ge gx0 and px le gx1, ng)
	;
	; log
	if kcfg.nasmask ne 1 then $
		kcwi_print_info,ppar,pre,':Slice  All:Mn    Sg       Bl:Mn    Sg       Rd:Mn    Sg       Mid:Mn   Sg' $
	else	kcwi_print_info,ppar,pre,':Slice   Mid:Mn   Sg'
	;
	; display status
	doplots = ppar.display
	;
	; loop over slices
	for i=0,23 do begin
		;
		; use middle wavelength sample
		slim = reform(icub[i,*,y1:y2])
		sgim = reform(scub[i,*,y1:y2])
		;
		; don't use noisy data
		b = where(slim lt sl*sgim, nb)
		if nb gt 0 then slim[b] = !values.f_nan
		;
		; profile of slice
		prof = mean(slim,dim=2,/nan)
		;
		; get stats and avoid using edges for fit
		ims,prof[g],mn,sg,wgt
		lim = max([mn/100., 1.e-3])	; limit zero-like values
		;
		; normalization factor
		if i gt 12 or i eq 11 then begin
			nf = prof[g[0]]
		endif else if i lt 11 or i eq 12 then begin
			nf = prof[g[ng-1]]
		endif
		;
		; check for nod-and-shuffle
		if kcfg.nasmask ne 1 then begin
			;
			; full wavelength sample
			slimf = reform(icub[i,*,y0:y3])
			sgimf = reform(scub[i,*,y0:y3])
			badf = where(slimf lt 3.*sgimf, nbf)
			if nbf gt 0 then slimf[badf] = !values.f_nan
			proff = mean(slimf,dim=2,/nan)
			ims,proff[g],mnf,sgf
			;
			; blue end
			slim1 = reform(icub[i,*,y0:y1])
			sgim1 = reform(scub[i,*,y0:y1])
			bad1 = where(slim1 lt 3.*sgim1, nb1)
			if nb1 gt 0 then slim1[bad1] = !values.f_nan
			prof1 = mean(slim1,dim=2,/nan)
			ims,prof1[g],mn1,sg1
			;
			; red end
			slim3 = reform(icub[i,*,y2:y3])
			sgim3 = reform(scub[i,*,y2:y3])
			bad3 = where(slim3 lt 3.*sgim3, nb3)
			if nb3 gt 0 then slim3[bad3] = !values.f_nan
			prof3 = mean(slim3,dim=2,/nan)
			ims,prof3[g],mn3,sg3
			;
			; normalization factors
			if i gt 12 or i eq 11 then begin
				nff = proff[g[0]]
				nf1 = prof1[g[0]]
				nf3 = prof3[g[0]]
			endif else if i lt 11 or i eq 12 then begin
				nff = proff[g[ng-1]]
				nf1 = prof1[g[ng-1]]
				nf3 = prof3[g[ng-1]]
			endif
			;
			; log results
			kcwi_print_info,ppar,pre,'', $
				i,mnf,sgf,mn1,sg1,mn3,sg3,mn,sg, $
				format='(a,i4,2x,8f9.4)'
		endif else $
			kcwi_print_info,ppar,pre,'',i,mn,sg, $
				format='(a,i4,2x,2f9.4)'
		profs[i,*] = (prof>lim)/nf 	; avoid zeros
		;
		q=''
		if doplots ge 2 then begin
			deepcolor
			!p.background=colordex('white')
			!p.color=colordex('black')
			th=2
			si=1.5
			;
			yran = [0.9,1.05]
			;
			plot,prof/nf,xthick=th,ythick=th,charsi=si, $
				charthi=th,thick=th, $
				title='Image # '+strn(kcfg.imgnum)+ $
				', Slice '+strn(i),psym=10, $
				xtitle='Pixel',/xs, $
				ytitle='Avg Int.',yrange=yran,/ys,/nodata
			if kcfg.nasmask ne 1 then begin
				oplot,prof1/nf1,color=colordex('B'), psym=10
				oplot,proff/nff,psym=10
				oplot,prof3/nf3,color=colordex('R'), psym=10
			endif
			oplot,prof/nf,color=colordex('G'),psym=10,thick=th
			oplot,[gx0,gx0],!y.crange,linesty=2
			oplot,[gx1,gx1],!y.crange,linesty=2
			;
			read,'Next slice? (Q-quit plotting, <cr>-next): ',q
		endif
		if strupcase(strmid(q,0,1)) eq 'Q' then doplots = 0
	endfor	; loop over 24 slices
	;
	; update profile image header
	sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
	sxaddpar,hdr,'SLPROF','T',' Slice profile image?'
	sxaddpar,hdr,'SLPROFY0',y1,' low wave pixel for profile'
	sxaddpar,hdr,'SLPROFY1',y2,' high wave pixel for profile'
	sxaddpar,hdr,'SLPROFX0',gx0,' low spatial pixel for profile'
	sxaddpar,hdr,'SLPROFX1',gx1,' high spatial pixel for profile'
	sxaddpar,hdr,'WCSDIM',2
	sxdelpar,hdr,'NAXIS3'
	sxdelpar,hdr,'CTYPE3'
	sxdelpar,hdr,'CUNIT3'
	sxdelpar,hdr,'CNAME3'
	sxdelpar,hdr,'CRPIX3'
	sxdelpar,hdr,'CRVAL3'
	sxdelpar,hdr,'CDELT3'
	;
	; write out profile image file
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_prof',/nodir)
	kcwi_write_image,profs,hdr,ofil,ppar
	return
end

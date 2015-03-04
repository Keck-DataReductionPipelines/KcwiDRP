; $Id: kcwi_slice_rr.pro,v 1.23 2015/02/21 00:18:37 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SLICE_RR
;
; PURPOSE:
;	This procedure creates a slice relative response image from the input
;	data cube.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SLICE_RR, Kcfg,  Ppar, Rr
;
; INPUTS:
;	Kcfg	- KCWI_CFG struct for the input data cube, preferrably
;			from a sky or dome-flat observation
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Rr	- an array giving the normalized relative response
;
; SIDE EFFECTS:
;	Outputs a fits image of the relative response with same image
;	number root as the input file, but with '_rr' appended. For
;	example, if 'image1234.fits' is pointed to by the input 
;	KCWI_CFG struct, then the output rr image would have the
;	filename 'image1234_rr.fits'.
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	For each slice, sums along spatial dimension to derive the normalized 
;	response relative to the reference slice.  This is then suitable for 
;	dividing out the slice relative response.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-NOV-12	Initial Revision
;	2014-APR-08	Check against zero-like values in rresp. causing overlow
;-
pro kcwi_slice_rr,kcfg,ppar,rr
	;
	; setup
	pre = 'KCWI_SLICE_RR'
	version = repstr('$Revision: 1.23 $ $Date: 2015/02/21 00:18:37 $','$','')
	;
	; check inputs
	if kcwi_verify_cfg(kcfg,/init) ne 0 then return
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,version
	;
	; is this a dome flat or twilight flat?
	if strmatch(strtrim(kcfg.imgtype,2),'dflat') eq 0 and $
	   strmatch(strtrim(kcfg.imgtype,2),'tflat') eq 0 then begin
		kcwi_print_info,ppar,pre,'not an rr obs',/warning
	endif
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; get output image (in reduced dir)
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_rr',/reduced)
	if file_test(ofil) then begin
		if ppar.clobber ne 1 then begin
			kcwi_print_info,ppar,pre,'output file already exists',ofil,/error
			return
		endif else $
			kcwi_print_info,ppar,pre,'output file will be overwritten',ofil,/warning
	endif
	;
	; read in image
	;
	; first try profile corrected data cube
	icub = kcwi_read_image(kcfg.imgnum,ppar,'_icubep',hdr,/calib,status=stat)
	;
	; if not try data cube
	if stat ne 0 then $
		icub = kcwi_read_image(kcfg.imgnum,ppar,'_icube',hdr,/calib,status=stat)
	;
	; neither were available
	if stat ne 0 then return
	;
	; get size
	sz = size(icub,/dim)
	;
	; default pixel ranges
	y = findgen(sz[2])
	y0 = 175
	y1 = sz[2] - 175
	;
	; get wavelength scale
	w0 = sxpar(hdr,'CRVAL3')
	dw = sxpar(hdr,'CD3_3')
	crpixw = sxpar(hdr,'CRPIX3')
	;
	; get spatial scale
	s0 = sxpar(hdr,'CRVAL2')
	ds = sxpar(hdr,'CD2_2')
	crpixs = sxpar(hdr,'CRPIX2')
	;
	; get inclusive wavelength range
	wall0 = sxpar(hdr,'WAVGOOD0')
	wall1 = sxpar(hdr,'WAVGOOD1')
	;
	; compute good y pixel ranges
	if w0 gt 0. and dw gt 0. and wall0 gt 0. and wall1 gt 0. then begin
		y0 = fix( (wall0 - w0) / dw ) + 10
		y1 = fix( (wall1 - w0) / dw ) - 10
	endif
	;
	; wavelength scale
	w = w0 + y*dw
	;
	; relative responses
	rrs = fltarr(24,sz[2]) + 1.
	;
	; avoid zero-like values
	lim = 1.e-1
	;
	; good spatial range
	gx0 = ppar.slicex0
	gx1 = ppar.slicex1
	x = indgen(sz[0])
	g = where(x ge gx0 and x le gx1)
	;
	;
	; log results
	kcwi_print_info,ppar,pre,'Resp. Pars: X0, X1, Y0, Y1, Wav0, Wav1', $
		gx0,gx1,y0,y1,w[y0],w[y1],format='(a,4i6,2f9.3)'
	;
	; display status
	doplots = ppar.display
	;
	; get reference slice number
	irs = ppar.refslice >0<23
	;
	; extract reference slice response
	test = reform(icub[gx0:gx1,irs,y0:y1])
	refr = median(test,dim=1)
	;
	; plot results if requested
	q=''
	if doplots ge 2 then begin
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		th=2
		si=1.5
		;
		xran=[wall0-10.,wall1+10.]
		plot,w[y0:y1],refr,xthick=th,ythick=th,charsi=si,charthi=th, $
			title='Img: '+strn(kcfg.imgnum)+' Reference Slice: '+strn(irs), $
			xrange=xran,/xs,xtitle='Wave', $
			ytitle='Avg Int.', /ys
		oplot,[wall0,wall0],!y.crange
		oplot,[wall1,wall1],!y.crange
		;
		read,'Next slice? (Q-quit plotting, <cr>-next): ',q
		if strupcase(strmid(q,0,1)) eq 'Q' then doplots = 0
	endif
	;
	; loop over slices
	for i=0,23 do begin
		;
		; are we the reference slice?
		if i eq irs then begin
			fco = fltarr(6)
			fco[0] = 1.
			nrr = refr/refr
			nfit = nrr
			rlab = ' REF'
		endif else begin
			;
			; full wavelength sample (avoiding edges)
			test  = reform(icub[gx0:gx1,i,y0:y1])
			rr = median(test,dim=1)
			nrr = rr/refr
			fco = polyfit(y[y0:y1],nrr,5)
			nfit = poly(y,fco)
			rrs[i,*] = nfit > lim	; avoid zero-like values that cause overflow
			rlab = ''
		endelse
		;
		; log results
		kcwi_print_info,ppar,pre,'Resp. Fit: Slice#, Coefs',i,fco, $
			format='(a,i4,2x,6g13.5)'
		;
		if doplots ge 2 then begin
			;
			plot,w[y0:y1],nrr,xthick=th,ythick=th,charsi=si, $
				charthi=th, $
				title='Img: '+strn(kcfg.imgnum)+' Slice: '+strn(i)+rlab, $
				xtitle='Wave', xrange=xran,/xs, $
				ytitle='Int/Ref Int',yrange=[0.25,1.50],/ys
			oplot,w,nfit,linesty=5
			oplot,[wall0,wall0],!y.crange
			oplot,[wall1,wall1],!y.crange
			;
			read,'Next slice? (Q-quit plotting, <cr>-next): ',q
		endif
		if strupcase(strmid(q,0,1)) eq 'Q' then doplots = 0
	endfor
	;
	; update rr image header
	sxaddpar,hdr,'COMMENT','  '+pre+' '+version
	sxaddpar,hdr,'SLRR','T',' Slice rr image?'
	sxaddpar,hdr,'SLRRY0',y0,' low wave pixel for rr'
	sxaddpar,hdr,'SLRRY1',y1,' high wave pixel for rr'
	sxaddpar,hdr,'SLRRX0',gx0,' low spatial pixel for rr'
	sxaddpar,hdr,'SLRRX1',gx1,' high spatial pixel for rr'
	;
	; remove old WCS
	sxdelpar,hdr,'NAXIS3'
	sxdelpar,hdr,'CTYPE1'
	sxdelpar,hdr,'CTYPE2'
	sxdelpar,hdr,'CTYPE3'
	sxdelpar,hdr,'CUNIT1'
	sxdelpar,hdr,'CUNIT2'
	sxdelpar,hdr,'CUNIT3'
	sxdelpar,hdr,'CNAME1'
	sxdelpar,hdr,'CNAME2'
	sxdelpar,hdr,'CNAME3'
	sxdelpar,hdr,'CRVAL1'
	sxdelpar,hdr,'CRVAL2'
	sxdelpar,hdr,'CRVAL3'
	sxdelpar,hdr,'CRPIX1'
	sxdelpar,hdr,'CRPIX2'
	sxdelpar,hdr,'CRPIX3'
	sxdelpar,hdr,'CD1_1'
	sxdelpar,hdr,'CD1_2'
	sxdelpar,hdr,'CD2_1'
	sxdelpar,hdr,'CD2_2'
	sxdelpar,hdr,'CD3_3'
	;
	; set wavelength axis WCS values
	sxaddpar,hdr,'WCSDIM',2
	sxaddpar,hdr,'CTYPE1','SPATIAL',' DEC'
	sxaddpar,hdr,'CUNIT1','deg',' DEC units'
	sxaddpar,hdr,'CNAME1','KCWI DEC',' DEC name'
	sxaddpar,hdr,'CRVAL1',s0,' DEC zeropoint'
	sxaddpar,hdr,'CRPIX1',crpixs,' DEC reference pixel'
	sxaddpar,hdr,'CDELT1',ds,' DEC degrees per pixel'
	sxaddpar,hdr,'CTYPE2','AWAV',' Air Wavelengths'
	sxaddpar,hdr,'CUNIT2','Angstrom',' Wavelength units'
	sxaddpar,hdr,'CNAME2','KCWI RR Wavelength',' Wavelength name'
	sxaddpar,hdr,'CRVAL2',w0,' Wavelength zeropoint'
	sxaddpar,hdr,'CRPIX2',crpixw,' Wavelength reference pixel'
	sxaddpar,hdr,'CDELT2',dw,' Wavelength Angstroms per pixel'
	;
	; write out rr image file
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_rr',/nodir)
	kcwi_write_image,rrs,hdr,ofil,ppar
	return
end

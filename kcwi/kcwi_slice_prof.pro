; $Id: kcwi_slice_prof.pro,v 1.27 2015/02/21 00:18:37 neill Exp $
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
;-
pro kcwi_slice_prof,kcfg,ppar,profs
	;
	; setup
	pre = 'KCWI_SLICE_PROF'
	version = repstr('$Revision: 1.27 $ $Date: 2015/02/21 00:18:37 $','$','')
	;
	; check inputs
	if kcwi_verify_cfg(kcfg,/init) ne 0 then return
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,version
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
			icub = kcwi_read_image(kcfg.imgnum,ppar,'_scube',hdr,status=stat)
			if stat ne 0 then return
		endif else begin
			;
			; read sky observation image
			icub = kcwi_read_image(kcfg.imgnum,ppar,'_icube',hdr,status=stat)
			if stat ne 0 then return
		endelse
		;
		; get size
		sz = size(icub,/dim)
		dely = fix(sz[2]/3)
		;
		; default pixel ranges
		y0 = dely + 10
		y3 = y0 + dely - 10
	endif else begin
		;
		; read sky observation image
		icub = kcwi_read_image(kcfg.imgnum,ppar,'_icube',hdr,/calib,status=stat)
		if stat ne 0 then return
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
	endelse
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
	; profiles
	profs = fltarr(sz[0],24)
	;
	; good range
	gx0 = ppar.slicex0
	gx1 = ppar.slicex1
	x = indgen(sz[0])
	g = where(x ge gx0 and x le gx1)
	;
	; log
	if kcfg.nasmask ne 1 then $
		kcwi_print_info,ppar,pre,':Slice   Bl:Mn    Sg       Gr:Mn    Sg       Rd:Mn    Sg       All:Mn   Sg' $
	else	kcwi_print_info,ppar,pre,':Slice   All:Mn   Sg'
	;
	; display status
	doplots = ppar.display
	;
	; loop over slices
	for i=0,23 do begin
		;
		; full wavelength sample (avoiding edges)
		test  = reform(icub[*,i,y0:y3])
		prof  = total(test,2)/float(n_elements(test[0,*]))
		mo = moment(prof[g])
		mn = mo[0]
		sg = sqrt(mo[1])
		lim = max([mn/100., 1.e-3])	; limit zero-like values
		;
		; check for nod-and-shuffle
		if kcfg.nasmask ne 1 then begin
			;
			; three wavelength samples
			test1 = reform(icub[*,i,y0:y1])
			test2 = reform(icub[*,i,y1:y2])
			test3 = reform(icub[*,i,y2:y3])
			;
			prof1 = total(test1,2)/float(n_elements(test1[0,*]))
			prof2 = total(test2,2)/float(n_elements(test2[0,*]))
			prof3 = total(test3,2)/float(n_elements(test3[0,*]))
			mo1 = moment(prof1[g])
			mn1 = mo1[0]
			sg1 = sqrt(mo1[1])
			mo2 = moment(prof2[g])
			mn2 = mo2[0]
			sg2 = sqrt(mo2[1])
			mo3 = moment(prof3[g])
			mn3 = mo3[0]
			sg3 = sqrt(mo3[1])
			kcwi_print_info,ppar,pre,'',i,mn1,sg1,mn2,sg2,mn3,sg3,mn,sg, $
				format='(a,i4,2x,8f9.4)'
		endif else $
			kcwi_print_info,ppar,pre,'',i,mn,sg, $
				format='(a,i4,2x,2f9.4)'
		profs[*,i] = (prof>lim)/mn 	; avoid zeros
		;
		q=''
		if doplots ge 2 then begin
			deepcolor
			!p.background=colordex('white')
			!p.color=colordex('black')
			th=2
			si=1.5
			;
			if kcfg.nasmask ne 1 then $
				yran=[min( [min([prof[g],prof1[g],prof2[g],prof3[g]]), mn-1.] ), $
			      	      max( [max([prof[g],prof1[g],prof2[g],prof3[g]]), mn+1.] )] $
			else	yran=[min( [min(prof[g]), mn-1.] ), max( [max(prof[g]), mn+1.] ) ]
			;
			plot,prof,xthick=th,ythick=th,charsi=si,charthi=th, $
				title='Image # '+strn(kcfg.imgnum)+', Slice '+strn(i),psym=10, $
				xtitle='Pixel',/xs, $
				ytitle='Avg Int.',yrange=yran,/ys
			oplot,!x.crange,[mn,mn],linesty=1
			if kcfg.nasmask ne 1 then begin
				oplot,prof1,color=colordex('B'),psym=10
				oplot,prof2,color=colordex('G'),psym=10
				oplot,prof3,color=colordex('R'),psym=10
			endif
			oplot,[gx0,gx0],!y.crange
			oplot,[gx1,gx1],!y.crange
			;
			read,'Next slice? (Q-quit plotting, <cr>-next): ',q
		endif
		if strupcase(strmid(q,0,1)) eq 'Q' then doplots = 0
	endfor	; loop over 24 slices
	;
	; update profile image header
	sxaddpar,hdr,'COMMENT','  '+pre+' '+version
	sxaddpar,hdr,'SLPROF','T',' Slice profile image?'
	sxaddpar,hdr,'SLPROFY0',y0,' low wave pixel for profile'
	sxaddpar,hdr,'SLPROFY1',y3,' high wave pixel for profile'
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

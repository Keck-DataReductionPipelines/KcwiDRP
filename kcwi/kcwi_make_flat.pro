; $Id: kcwi_make_flat.pro,v 1.21 2015/02/21 00:18:39 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MAKE_FLAT
;
; PURPOSE:
;	This procedure creates a median-stacked cflat from a list of flat
;	image numbers given in KCWI_PPAR struct, then creates a flat image
;	that will take out the pixel-to-pixel variations.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_MAKE_FLAT, Ppar
;
; INPUTS:
;	Ppar	- KCWI_PPAR struct for flat group to combine
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Uses list of flat frames to construct a master flat frame that
;	is median stacked.  Then fits each column to take out low
;	frequency structure and leave the pixel-to-pixel structure.
;
; EXAMPLE:
;
;	ppar = KCWI_PPAR_READ('mflat_0.ppar')
;	KCWI_MAKE_FLAT,ppar
;
; TODO:
;	check for dark subtracted flats
;	create master variance and mask images
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-29	Initial version
;	2013-SEP-10	Preliminary calculation of variance and mask images
;	2013-SEP-16	use ppar to pass pipeline params
;-
pro kcwi_make_flat,ppar
	;
	; version
	version = repstr('$Revision: 1.21 $ $Date: 2015/02/21 00:18:39 $','$','')
	;
	; initialize
	pre = 'KCWI_MAKE_FLAT'
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,version
	;
	; are there flats listed?
	if strlen(ppar.cflats) le 0 then begin
		kcwi_print_info,ppar,pre,'no flats entered',/error
		return
	endif
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; get image list for this group
	rangepar,ppar.cflats,fnums
	nf = n_elements(fnums)
	;
	; get image number format
	i_fmt = '(i0'+strn(ppar.fdigits)+')'
	;
	; get filename root
	root = ppar.froot
	;
	; log number of images
	kcwi_print_info,ppar,pre,'combining this many images',nf
	;
	; read first image
	ffil = reddir + root + string(fnums[0],form=i_fmt)+'_int.fits'
	if file_test(ffil) then begin
		im = mrdfits(ffil,0,hdr,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'Image file not found',ffil, $
			format='(a,a)',/error
		return
	endelse
	;
	; get size
	sz = size(im,/dim)
	nx = sz[0]
	ny = sz[1]
	;
	; set up stacks
	stack = fltarr(nf,nx,ny)
	nstack = 1
	stack[0,*,*] = im
	mflat = im		; single image
	;
	; how many images?
	if nf eq 2 then begin
		;
		; read second image
		ffil = reddir + root + string(fnums[1],form=i_fmt)+'_int.fits'
		if file_test(ffil) then begin
			im = mrdfits(ffil,0,fhdr,/fscale,/silent)
		endif else begin
			kcwi_print_info,ppar,pre,'Image file not found', $
				ffil,/error
			return
		endelse
		;
		; make sure the sizes match
		sz = size(im,/dim)
		if sz[0] ne nx or sz[1] ne ny then begin
			kcwi_print_info,ppar,pre,'Size mismatch',ffil,/error
			return
		endif
		;
		; simple average
		mflat = ( mflat + im ) / 2.0
	endif else if nf ge 3 then begin
		;
		; loop over remaining images
		for i=1,nf-1 do begin
	    		;
	    		; read next image
			ffil = reddir + root + string(fnums[i],form=i_fmt)+'_int.fits'
			if file_test(ffil) then begin
				im = mrdfits(ffil,0,fhdr,/fscale,/silent)
			endif else begin
				kcwi_print_info,ppar,pre,'File not found', $
					ffil,/error
				return
			endelse
			;
			; make sure sizes match
			sz = size(im,/dim)
			if sz[0] ne nx or sz[1] ne ny then begin
				kcwi_print_info,ppar,pre,'Size mismatch', $
					ffil,/error
				return
			endif
			;
			; insert into stack
			stack[i,*,*] = im
		endfor	; loop over remaining images
		;
		; create master flat from median stack
		mflat = fltarr(nx,ny)
		for yi=0,ny-1 do for xi=0,nx-1 do $
			mflat[xi,yi] = median(stack[*,xi,yi])
	endif
	;
	; now fit master flat
	kcwi_fit_flat,mflat,hdr,ppar,flato
	;
	; update master flat header
	sxaddpar,hdr,'COMMENT','  '+pre+' '+version
	sxaddpar,hdr,'NMEDIAN',nf, $
		' number of images used for stack'
	sxaddpar,hdr,'MASTFLAT','T',' master flat image?'
	sxaddpar,hdr,'FLATLST',ppar.cflats, $
		' range list of image numbers for stack'
	;
	; write out image file
	kcwi_write_image,flato,hdr,ppar.masterflat,ppar
	;
	return
end

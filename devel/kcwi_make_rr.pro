;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MAKE_RR
;
; PURPOSE:
;	This procedure creates a median-stacked master relative response 
;	image from a list of rr image numbers given in KCWI_PPAR struct
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_MAKE_RR, Ppar
;
; INPUTS:
;	Ppar	- KCWI_PPAR struct for rr group to combine
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Uses list of rr frames to construct a master rr frame that
;	is median stacked.
;
; EXAMPLE:
;
;	ppar = KCWI_PPAR_READ('mrr_0.ppar')
;	KCWI_MAKE_RR,ppar
;
; TODO:
;	check for dark subtracted rrs
;	create master variance and mask images
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-JUL-29	Initial version
;-
pro kcwi_make_rr,ppar
	;
	; version
	version = repstr('$Revision: v0.2.10-40-g60c9d44 $ $Date: Fri May 15 10:35:48 2015 -0700 $','$','')
	;
	; initialize
	pre = 'KCWI_MAKE_RR'
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,version
	;
	; are there rrs listed?
	if strlen(ppar.rrs) le 0 then begin
		kcwi_print_info,ppar,pre,'no relative responses entered',/error
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
	rangepar,ppar.rrs,rnums
	nr = n_elements(rnums)
	;
	; get image number format
	i_fmt = '(i0'+strn(ppar.fdigits)+')'
	;
	; get filename root
	root = ppar.froot
	;
	; log number of images
	kcwi_print_info,ppar,pre,'combining this many images',nr
	;
	; read first image
	rfil = reddir + root + string(rnums[0],form=i_fmt)+'_int.fits'
	if file_test(rfil) then begin
		im = mrdfits(rfil,0,hdr,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'Image file not found',rfil, $
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
	mrr = im		; single image
	;
	; how many images?
	if nf eq 2 then begin
		;
		; read second image
		rfil = reddir + root + string(fnums[1],form=i_fmt)+'_int.fits'
		if file_test(rfil) then begin
			im = mrdfits(rfil,0,fhdr,/fscale,/silent)
		endif else begin
			kcwi_print_info,ppar,pre,'Image file not found', $
				rfil,/error
			return
		endelse
		;
		; make sure the sizes match
		sz = size(im,/dim)
		if sz[0] ne nx or sz[1] ne ny then begin
			kcwi_print_info,ppar,pre,'Size mismatch',rfil,/error
			return
		endif
		;
		; simple average
		mrr = ( mrr + im ) / 2.0
	endif else if nf ge 3 then begin
		;
		; loop over remaining images
		for i=1,nf-1 do begin
	    		;
	    		; read next image
			rfil = reddir + root + string(fnums[i],form=i_fmt)+'_int.fits'
			if file_test(rfil) then begin
				im = mrdfits(rfil,0,fhdr,/fscale,/silent)
			endif else begin
				kcwi_print_info,ppar,pre,'File not found', $
					rfil,/error
				return
			endelse
			;
			; make sure sizes match
			sz = size(im,/dim)
			if sz[0] ne nx or sz[1] ne ny then begin
				kcwi_print_info,ppar,pre,'Size mismatch', $
					rfil,/error
				return
			endif
			;
			; insert into stack
			stack[i,*,*] = im
		endfor	; loop over remaining images
		;
		; create master rr from median stack
		mrr = fltarr(nx,ny)
		for yi=0,ny-1 do for xi=0,nx-1 do $
			mrr[xi,yi] = median(stack[*,xi,yi])
	endif
	;
	; update master rr header
	sxaddpar,hdr,'COMMENT','  '+pre+' '+version
	sxaddpar,hdr,'NMEDIAN',nr, $
		' number of images used for stack'
	sxaddpar,hdr,'MASTRR','T',' master rr image?'
	sxaddpar,hdr,'RRLST',ppar.rrs, $
		' range list of image numbers for stack'
	;
	; write out image file
	kcwi_write_image,mrr,hdr,ppar.masterrr,ppar
	;
	return
end

; $Id: kcwi_make_dark.pro,v 1.15 2015/02/21 00:18:37 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MAKE_DARK
;
; PURPOSE:
;	This procedure creates a median-stacked dark from a list of dark
;	image numbers given in KCWI_PPAR struct PPAR.DARKS.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_MAKE_DARK, Ppar
;
; INPUTS:
;	Ppar	- KCWI_PPAR struct for dark group to combine
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Uses list of dark frames to construct a master dark frame that
;	is median stacked.
;
; EXAMPLE:
;
;	ppar = KCWI_PPAR_READ('mdark_0.ppar')
;	KCWI_MAKE_DARK,ppar
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-29	Initial version
;	2013-SEP-14	Use ppar to pass params, added var and msk image creation
;-
pro kcwi_make_dark,ppar
	;
	; version
	version = repstr('$Revision: 1.15 $ $Date: 2015/02/21 00:18:37 $','$','')
	;
	; initialize
	pre = 'KCWI_MAKE_DARK'
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,version
	;
	; are there darks listed?
	if strlen(ppar.darks) le 0 then begin
		kcwi_print_info,ppar,pre,'no darks entered',/error
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
	rangepar,ppar.darks,dnums
	nd = n_elements(dnums)
	;
	; get image number format
	i_fmt = '(i0'+strn(ppar.fdigits)+')'
	;
	; get image file root
	root = ppar.froot
	;
	; are there enough?
	if nd ge ppar.mingroupdark then begin
		;
		; log number of images
		kcwi_print_info,ppar,pre,'combining this many images',nd
	    	;
	    	; read first image
		dfil = reddir + root + string(dnums[0],form=i_fmt)+'_int.fits'
		if file_test(dfil) then begin
			im = mrdfits(dfil,0,hdr,/fscale,/silent)
		endif else begin
			kcwi_print_info,ppar,pre,'Image file not found', $
				dfil,/error
			return
		endelse
		; get size
		sz = size(im,/dim)
		nx = sz[0]
		ny = sz[1]
	    	;
	    	; read first variance image
		vfil = reddir + root + string(dnums[0],form=i_fmt)+'_var.fits'
		if file_test(vfil) then begin
			var = mrdfits(vfil,0,varhdr,/fscale,/silent)
		endif else begin
			kcwi_print_info,ppar,pre,'Variance file not found', $
				vfil,/warning
			var = fltarr(sz)
		endelse
	    	;
	    	; read first mask image
		mfil = reddir + root + string(dnums[0],form=i_fmt)+'_msk.fits'
		if file_test(mfil) then begin
			msk = mrdfits(mfil,0,mskhdr,/silent)
		endif else begin
			kcwi_print_info,ppar,pre,'Mask file not found', $
				mfil,/warning
			msk = intarr(sz)
		endelse
		;
		; set up stack
		stack  = fltarr(nd,nx,ny)
		vstack = fltarr(nd,nx,ny)
		mstack = fltarr(nd,nx,ny)
		nstack = 1
		stack[0,*,*] = im
		vstack[0,*,*] = var
		mstack[0,*,*] = msk
		;
		; loop over remaining images
		for i=1,nd-1 do begin
	    		;
	    		; read next image
			dfil = reddir + root + string(dnums[i],form=i_fmt)+'_int.fits'
			if file_test(dfil) then begin
				im = mrdfits(dfil,0,dhdr,/fscale,/silent)
			endif else begin
				kcwi_print_info,ppar,pre,'File not found', $
					dfil,/error
				return
			endelse
			;
			; make sure sizes match
			sz = size(im,/dim)
			if sz[0] ne nx or sz[1] ne ny then begin
				kcwi_print_info,ppar,pre,'Size mismatch', $
					dfil,/error
				return
			endif
	    		;
	    		; read next variance image
			vfil = reddir + root + string(dnums[i],form=i_fmt)+'_var.fits'
			if file_test(vfil) then begin
				var = mrdfits(vfil,0,varhdr,/fscale,/silent)
			endif else begin
				kcwi_print_info,ppar,pre, $
					'Variance file not found',vfil,/warning
				var = fltarr(sz)
			endelse
	    		;
	    		; read next mask image
			mfil = reddir + root + string(dnums[i],form=i_fmt)+'_msk.fits'
			if file_test(mfil) then begin
				msk = mrdfits(mfil,0,mskhdr,/silent)
			endif else begin
				kcwi_print_info,ppar,pre, $
					'Mask file not found',mfil,/warning
				msk = intarr(sz)
			endelse
			;
			; insert into stack
			stack[i,*,*] = im
			vstack[i,*,*] = var
			mstack[i,*,*] = msk
		endfor	; loop over remaining images
		;
		; create master dark from median stack
		mdark = fltarr(nx,ny)
		for yi=0,ny-1 do for xi=0,nx-1 do $
			mdark[xi,yi] = median(stack[*,xi,yi])
		;
		; create master variance from median stack
		mvar = fltarr(nx,ny)
		for yi=0,ny-1 do for xi=0,nx-1 do $
			mvar[xi,yi] = median(vstack[*,xi,yi])/float(nd)
		;
		; create master mask from stack
		mmsk = intarr(nx,ny)
		;
		; Calculate threshhold for setting mask.
		; Since we are using the median, we don't
		; mask it unless all but two pixels are
		; already masked.
		mskthr = (nd-2) > 1
		for yi=0,ny-1 do for xi=0,nx-1 do $
			mmsk[xi,yi] = (total(mstack[*,xi,yi]) gt mskthr) ? 1 : 0
		;
		; update master flat header
		sxaddpar,hdr,'COMMENT','  '+pre+' '+version
		sxaddpar,hdr,'NMEDIAN',nd, $
			' number of images used for stack'
		sxaddpar,hdr,'MASTDARK','T',' master dark image?'
		sxaddpar,hdr,'DARKLST',ppar.darks, $
			' range list of image numbers for stack'
		;
		; write out master dark
		kcwi_write_image,mdark,hdr,ppar.masterdark,ppar
		;
		; update master variance header
		sxaddpar,varhdr,'COMMENT','  '+pre+' '+version
		sxaddpar,varhdr,'NMEDIAN',nd, $
			' number of images used for stack'
		sxaddpar,varhdr,'MASTDARK','T',' master dark image?'
		sxaddpar,varhdr,'DARKLST',ppar.darks, $
			' range list of image numbers for stack'
		;
		; master variance file name
		rute = strmid(ppar.masterdark,0,strpos(ppar.masterdark,'.fit'))
		mastervar = rute + '_var.fits'
		;
		; write out master variance
		kcwi_write_image,mvar,varhdr,mastervar,ppar
		;
		; update master mask header
		sxaddpar,mskhdr,'COMMENT','  '+pre+' '+version
		sxaddpar,mskhdr,'NMEDIAN',nd, $
			' number of images used for stack'
		sxaddpar,mskhdr,'MASTDARK','T',' master dark image?'
		sxaddpar,mskhdr,'DARKLST',ppar.darks, $
			' range list of image numbers for stack'
		;
		; master variance file name
		mastermsk = rute + '_msk.fits'
		;
		; write out master variance
		kcwi_write_image,mmsk,mskhdr,mastermsk,ppar
	endif else $
		kcwi_print_info,ppar,pre,'too few dark frames',nd,/error
	;
	return
end

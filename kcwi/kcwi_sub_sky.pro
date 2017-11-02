;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SUB_SKY
;
; PURPOSE:
;	This procedure subtracts the sky from the input cube.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SUB_SKY, Ppar, Hdr, cub, msk
;
; INPUTS:
;	Ppar	- KCWI_PPAR struct for flat group to combine
;	Hdr	- Data cube header
;	cub	- Data cube output from kcwi_stage6rr
;	msk	- Mask cube output from kcwi_stage6rr
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Fits the sky with rejection and subtracts it from cub, also
;	masks rejected pixels (object)
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2018-NOV-01	Initial version
;-
pro kcwi_sub_sky,ppar,hdr,cub,msk
	;
	; initialize
	pre = 'KCWI_SUB_SKY'
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	sz = size(cub,/dimen)
	if n_elements(sz) ne 3 then begin
		kcwi_print_info,ppar,pre,'A 3-D cube is required input',/error
		return
	endif
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
	;
	; sky cube
	scub = cub - cub
	shdr = hdr
	;
	; x vector
	x = findgen(sz[0]*sz[1])
	;
	; y trim
	yt = 2
	;
	; loop over wavelengths
	for iw = 0, sz[2]-1 do begin
		;
		; assemble sky vector to fit
		svec = fltarr(sz[0]*sz[1])
		vvec = fltarr(sz[0]*sz[1])
		mvec = bytarr(sz[0]*sz[1])
		for is = 0,sz[0]-1 do begin
			svec[is*sz[1]+yt:(is+1)*sz[1]-(yt+1)] = $
				reform(cub[is,yt:sz[1]-(yt+1),iw])>1.e-4
			mvec[is*sz[1]+yt:(is+1)*sz[1]-(yt+1)] = $
				reform(msk[is,yt:sz[1]-(yt+1),iw])
		endfor
		ims_asym,svec,sky,sig,wgt,siglim=[2.0,3.0]
		rej = where(wgt eq 0, nrej)
		if nrej gt 0 then $
			mvec[rej] = mvec[rej] + 4b
		for is = 0,sz[0]-1 do begin
			scub[is,*,iw] = sky ;[is*sz[1]:(is+1)*sz[1]-1]
			msk[is,*,iw] = mvec[is*sz[1]:(is+1)*sz[1]-1]
		endfor
	endfor
	;
	; now subtract
	cub = cub - scub
	;
	; update sky header
	sxaddpar,shdr,'HISTORY','  '+pre+' '+systime(0)
	;
	; get output file
	ofn = sxpar(hdr,'OFNAME')
	outf = repstr(ofn,'.fits','_sky.fits')
	;
	; write out image file
	kcwi_write_image,scub,shdr,outf,ppar
	;
	return
end	; kcwi_sub_sky

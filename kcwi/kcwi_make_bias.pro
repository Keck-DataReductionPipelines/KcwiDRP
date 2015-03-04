; $Id: kcwi_make_bias.pro,v 1.26 2014/10/29 17:06:51 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MAKE_BIAS
;
; PURPOSE:
;	This procedure creates a median-stacked bias from a list of bias
;	image numbers given in KCWI_PPAR struct PPAR.BIASES.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_MAKE_BIAS, ppar
;
; INPUTS:
;	ppar	- KCWI_PPAR struct for bias group to combine
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Uses list of bias frames to construct a master bias frame that
;	is median stacked.
;
; EXAMPLE:
;
;	This will create a master bias frame.
;
;	Ppar = KCWI_READ_PPAR('redux/mbias_0.ppar')
;	KCWI_MAKE_BIAS,Ppar
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-APR-22	Initial version
;	2013-MAY-08	remove SKIP1 keyword and now use KCWI_PPAR as input
;	2013-JUL-03	added logging
;	2013-SEP-14	use ppar to pass loglun
;-
pro kcwi_make_bias,ppar
	;
	; version
	version = repstr('$Revision: 1.26 $ $Date: 2014/10/29 17:06:51 $','$','')
	;
	; initialize
	pre = 'KCWI_MAKE_BIAS'
	;
	; check input ppar
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,version
	;
	; are there biases listed?
	if strlen(ppar.biases) le 0 then begin
		kcwi_print_info,ppar,pre,'no biases entered',/error
		return
	endif
	;
	; get image list
	rangepar,ppar.biases,bnums
	nb = n_elements(bnums)
	;
	; are there enough?
	if nb ge ppar.mingroupbias then begin
		;
		; log number of images
		kcwi_print_info,ppar,pre,'combining this many images',nb
		;
		; set up image stack
		im = kcwi_read_raw(ppar,bnums[0],header=hdr,/silent)
		nx = sxpar(hdr,'NAXIS1')
		ny = sxpar(hdr,'NAXIS2')
		stack = intarr(nb,nx,ny)
		stack[0,*,*] = im
		nstack = 1
		for i=1,nb-1 do begin
			im = kcwi_read_raw(ppar,bnums[i],header=bhdr, $
					/silent)
			if strtrim(sxpar(bhdr,'IMGTYPE'),2) eq 'bias' then begin
				stack[nstack,*,*] = im
				nstack = nstack + 1
			endif else begin
				kcwi_print_info,ppar,pre,'not a bias image', $
					bnums[i],/warning
				bnums[i] = -1
			endelse
		endfor
		;
		; adjust stack if needed
		if nstack lt nb then begin
			stack = stack[0:(nstack-1),*,*]
			kcwi_print_info,ppar,pre, $
				'stack adjusted for missing bias'
			rangepar,bnums[where(bnums ge 0)],rl
		endif
		;
		; print report if requested
		kcwi_print_info,ppar,pre,'Master Bias has '+strn(nstack)+ $
			' images'
		;
		; create master bias from median stack
		mbias = fltarr(nx,ny)
		for yi=0,ny-1 do for xi=0,nx-1 do $
			mbias[xi,yi] = median(stack[*,xi,yi])
		;
		; get read noise for each amplifier
		;
		; first, get ccd geometry
		kcwi_map_ccd,hdr,asec,bsec,csec,tsec,namps=namps,verbose=ppar.verbose
		mbias_rn = fltarr(namps)
		for ia = 0, namps - 1 do begin
			x0 = csec[ia,0,0]
			x1 = csec[ia,0,1]
			y0 = csec[ia,1,0]
			y1 = csec[ia,1,1]
			noise = reform(stack[1,x0:x1,y0:y1] - $
				       stack[2,x0:x1,y0:y1])
			ims,noise,mn,sg
			;
			; get gain
			gain = sxpar(hdr,'GAIN'+strn(ia+1))
			mbias_rn[ia] = gain * sg/sqrt(2)
			;
			; log
			kcwi_print_info,ppar,pre,'Amp'+strn(ia+1)+ $
				' Read noise in e-',mbias_rn[ia]
			;
			; update header
			sxaddpar,hdr,'BIASRN'+strn(ia+1),mbias_rn[ia], $
				' amp'+strn(ia+1)+' RN in e- from bias'
		endfor
		;
		; update master bias header
		sxaddpar,hdr,'COMMENT','  '+pre+' '+version
		sxaddpar,hdr,'NMEDIAN',nstack, $
			' number of images used for median stack'
		sxaddpar,hdr,'MASTBIAS','T', ' master bias image?'
		sxaddpar,hdr,'BIASLST',ppar.biases, $
			' range list of image numbers for stack'
		if ppar.biasskip1 then $
			sxaddpar,hdr,'BIASKP1','T', ' skip first bias image?' $
		else	sxaddpar,hdr,'BIASKP1','F', ' skip first bias image?'
		;
		; write out image file
		ofile = ppar.masterbias
		kcwi_write_image,mbias,hdr,ofile,ppar
	endif else $
		kcwi_print_info,ppar,pre,'too few bias frames',nb,/error
	;
	return
end

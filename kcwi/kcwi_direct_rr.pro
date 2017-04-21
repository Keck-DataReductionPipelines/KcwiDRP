;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_DIRECT_RR
;
; PURPOSE:
;	This procedure creates a direct relative response image from the input
;	direct image.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_DIRECT_RR, Kcfg,  Ppar, Rr
;
; INPUTS:
;	Kcfg	- KCWI_CFG struct for the input data cube, preferrably
;			from a direct image arc observation
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Rr	- an array giving the normalized relative response
;
; SIDE EFFECTS:
;	Outputs a fits image of the direct relative response with same image
;	number root as the input file, but with '_drr' appended. For
;	example, if 'image1234.fits' is pointed to by the input 
;	KCWI_CFG struct, then the output drr image would have the
;	filename 'image1234_drr.fits'.
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Normalizes the input image which is assumed to be uniformly
;	illuminated.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-NOV-12	Initial Revision
;	2014-APR-08	Check against zero-like values in rresp. causing overlow
;	2017-APR-21	Modified from kcwi_slice_rr.pro for direct images
;-
pro kcwi_direct_rr,kcfg,ppar,rr
	;
	; setup
	pre = 'KCWI_DIRECT_RR'
	;
	; check inputs
	if kcwi_verify_cfg(kcfg,/init) ne 0 then return
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
	;
	; is this a direct arc?
	if strmatch(strtrim(kcfg.imgtype,2),'arc') eq 0 or $
	   strpos(kcfg.obstype,'direct') lt 0 then begin
		kcwi_print_info,ppar,pre,'not a drr obs',/warning
	endif
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; get output image (in reduced dir)
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_drr',/reduced)
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
	rrfil = kcwi_get_imname(ppar,kcfg.imgnum,'_img',/reduced)
	;
	; input not available
	if not file_test(rrfil) then begin
		kcwi_print_info,ppar,pre,'No drr image available',/error
		return
	endif
	;
	; read in image
	img = mrdfits(rrfil,0,hdr,/fscale,/silent)
	;
	; get size
	sz = size(img,/dim)
	;
	; get median value
	medv = median(img)
	drr = img / medv
	;
	; log results
	kcwi_print_info,ppar,pre,'Resp. Med Value: ', medv, $
		format='(a,f9.3)'
	;
	; update drr image header
	sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
	sxaddpar,hdr,'DRR','T',' Direct rr image?'
	sxaddpar,hdr,'DRRNVAL',medv,' Image normalized by this value'
	;
	; write out drr image file
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_drr',/nodir)
	kcwi_write_image,drr,hdr,ofil,ppar
	return
end

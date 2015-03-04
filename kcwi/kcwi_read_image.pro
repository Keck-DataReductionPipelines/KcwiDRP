; $Id: kcwi_read_image.pro,v 1.6 2015/02/21 00:18:38 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_READ_IMAGE
;
; PURPOSE:
;	Read 2-d or 3-d image and header
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_READ_IMAGE( Kcfg, Ppar, Tail, Hdr)
;
; Returns:
;	2-d or 3-d image, or -1 if image file not found
;
; INPUTS:
;	Kcfg	- KCWI_CFG struct for a single observation.  If this is a
;			array, will only read first image
;	Ppar	- KCWI_PPAR struct specifying inputs and logging flags
;	Tail	- string specifying tail, e.g. '_int', '_icube'
;
; OUTPUTS:
;	Hdr	- FITS header for image
;
; KEYWORDS:
;	MASTER	- set to read a master image (bias, dark, flat)
;	CALIB	- set to read image from ppar.caldir directory instead of ppar.indir
;	STATUS	- returns 0, for no problems, 1 for file not found, 2 for 
;			input dir not accessible, and 3 for other problem
;
; SIDE EFFECTS:
;	None
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-SEP-20	Initial version
;	2014-APR-03	Added master and calib keywords
;-
function kcwi_read_image,imgnum,ppar,tail,hdr, $
	master=master,calib=calib,status=status,help=help
	;
	; setup
	pre = 'KCWI_READ_IMAGE'
	version = repstr('$Revision: 1.6 $ $Date: 2015/02/21 00:18:38 $','$','')
	status = 3
	;
	; help request
	if keyword_set(help) then begin
		print,pre+': Info - Usage: Img = '+pre+'( Kcfg, Ppar, Tail, Hdr)'
		print,pre+': Info - Keywords: STATUS=STATUS, /HELP'
		return,-1
	endif
	;
	; check structs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return,-1
	;
	; check inputs
	if n_params(0) lt 3 then begin
		if not keyword_set(calib) then begin
			kcwi_print_info,ppar,pre,'use kcwi_read_raw to read raw images',/error
			return,-1
		endif else tail = ''
	endif
	;
	; input dir
	if keyword_set(calib) then $
		idir = kcwi_expand_dir(ppar.caldir) $
	else	idir = kcwi_expand_dir(ppar.reddir)
	if not file_test(idir,/directory,/executable,/read) then begin
		kcwi_print_info,ppar,pre,'input directory not accessible',idir,/error
		status = 2
		return,-1
	endif
	;
	; image number and root image name strings
	;
	; are we a master file? (mbias, mdark, mflat)
	if keyword_set(master) then begin
		if strlen(master) gt 0 then begin
			rutstr = master
		endif else begin
			kcwi_print_info,ppar,pre,'invalid master file root',/error
			status = 3
			return,-1
		endelse
	;
	; regular image file
	endif else begin
		rutstr = ppar.froot
	endelse
	imgstr = string(imgnum,'(i0'+strn(ppar.fdigits)+')')
	;
	; get input filename
	ifil = idir + rutstr + imgstr + tail + '.fits'
	;
	; check if it exists
	if file_test(ifil) then begin
		;
		; check for type of read:
		; masks are binary images
		if strpos(tail,'msk') ge 0 then begin
			img = mrdfits(ifil,0,hdr,/silent)
			kcwi_print_info,ppar,pre,'read mask file with no scaling',ifil,format='(a,a)'
		;
		; everything else should be floats
		endif else begin
			img = mrdfits(ifil,0,hdr,/silent,/fscale)
			kcwi_print_info,ppar,pre,'read image file with /fscale',ifil,format='(a,a)'
		endelse
		status = 0
	;
	; report non-existence
	endif else begin
		kcwi_print_info,ppar,pre,'file not found',ifil,/error
		status = 1
		img = -1
	endelse
	;
	;
	return,img
end

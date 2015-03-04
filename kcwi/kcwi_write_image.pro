; $Id: kcwi_write_image.pro,v 1.8 2015/02/21 00:18:38 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_WRITE_IMAGE
;
; PURPOSE:
;	Write input 2-d or 3-d image to disk.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_WRITE_IMAGE, Img, Hdr, FileName, Ppar
;
; INPUTS:
;	Img	- 2-d or 3-d image
;	Hdr	- FITS header for image
;	FileName- filename for output used with Ppar.reddir to specify full
;		output path
;	Ppar	- KCWI_PPAR struct specifying outputs and logging flags
;
; KEYWORDS:
;	LOGLUN	- the logfile logical unit
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Outputs image files in output directory specified by the
;	KCWI_PPAR struct Ppar.  Updates logfile if LOGLUN specified
;	and outputs to screen if Ppar.verbose is set to 1.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-17	Initial version
;	2013-SEP-11	Now use ppar.loglun for logging
;-
pro kcwi_write_image,img,hdr,filename,ppar,iscale=iscale,help=help
	;
	; setup
	pre = 'KCWI_WRITE_IMAGE'
	version = repstr('$Revision: 1.8 $ $Date: 2015/02/21 00:18:38 $','$','')
	;
	; help request
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', Img, Hdr, FileName, Ppar'
		print,pre+': Info - Keywords: LOGLUN=<logfile_lunit>, /HELP'
		return
	endif
	;
	; check ppar struct
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; check inputs
	odir = kcwi_expand_dir(ppar.reddir)
	if not file_test(odir,/directory,/executable,/write) then begin
		print,pre+': Error - Output directory not accessible: ',odir
		return
	endif
	;
	; get output filename
	ofil = odir + filename
	;
	; check if it exists
	if file_test(ofil) then begin
		;
		; clobber it, if requested
		if ppar.clobber eq 1 then begin
			file_delete,ofil,verbose=ppar.verbose
			kcwi_print_info,ppar,pre,'deleted existing file',ofil,format='(a,a)'
		;
		; or skip writing and report
		endif else begin
			kcwi_print_info,ppar,pre,'existing file undisturbed',ofil,format='(a,a)'
			return
		endelse
	endif
	;
	; time stamp
	get_date,dstr,/time
	sxaddpar,hdr,'KDRPDATE',dstr,' KCWI DRP image write date'
	;
	; write it out
	mwrfits,img,ofil,hdr,iscale=iscale
	;
	; log
	kcwi_print_info,ppar,pre,'wrote image file',ofil,format='(a,a)'
	;
	return
end

;
; Copyright (c) 2014, California Institute of Technology. All rights reserved.
;+
; NAME:
;	KCWI_WRITE_GEOM
;
; PURPOSE:
;	Writes out the KCWI_GEOM struct as a FITS file
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_WRITE_GEOM,Ppar,Kgeom
;
; INPUTS:
;	Kgeom	- KCWI_GEOM struct
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;	TEST	- only write out if kgeom.status=0 (good fit)
;
; OUTPUTS:
;	None.
;
; PROCEDURE:
;	Uses the tag kgeom.geomfile to write out the struct as a FITS
;	table file.  Checks if ppar.clobber is set and takes appropriate
;	action.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-SEP-11	Initial Revision
;	2017-JUN-29	Converted to writing FITS instead of save file
;-
;
pro kcwi_write_geom,ppar,kgeom, test=test
;
; startup
pre = 'KCWI_WRITE_GEOM'
q = ''
;
; check inputs
if n_params(0) lt 1 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', Ppar, Kgeom'
	return
endif
;
; Check structs
if kcwi_verify_geom(kgeom,/init) ne 0 then return
if kcwi_verify_ppar(ppar,/init) ne 0 then return
;
; check fit status
if keyword_set(test) and kgeom.status ne 0 then begin
	kcwi_print_info,ppar,pre,'Kgeom fit no good, nothing written.',/error
	return
endif
;
; write it out
; check if it exists already
if file_test(kgeom.geomfile) then begin
	;
	; clobber it, if requested
    	if ppar.clobber eq 1 then begin
		file_delete,kgeom.geomfile,verbose=ppar.verbose
		kcwi_print_info,ppar,pre,'deleted existing geom file', $
			kgeom.geomfile,format='(a,a)'
	endif else begin
		kcwi_print_info,ppar,pre, $
			'existing geom file undisturbed', $
			kgeom.geomfile,format='(a,a)'
		return
	endelse
endif
;
; get header
if n_elements(tag_names(kgeom)) lt 90 then $
	hdr = headfits(kgeom.arcfname) $	; direct
else	hdr = headfits(kgeom.cbarsfname)	; dispersed
sxaddpar,hdr,'SIMPLE','F'
sxdelpar,hdr,'NAXIS'
sxdelpar,hdr,'NAXIS1'
sxdelpar,hdr,'NAXIS2'
sxdelpar,hdr,'BITPIX'
sxdelpar,hdr,'BZERO'
sxdelpar,hdr,'BSCALE'
;
; write it out if we get here
mwrfits,kgeom,kgeom.geomfile
;
; update header
modfits,kgeom.geomfile,0,hdr
;
; log
kcwi_print_info,ppar,pre,'wrote geom file',kgeom.geomfile,format='(a,a)'
;
return
end

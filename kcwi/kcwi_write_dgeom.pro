;
; Copyright (c) 2016, California Institute of Technology. All rights reserved.
;+
; NAME:
;	KCWI_WRITE_DGEOM
;
; PURPOSE:
;	Writes out the KCWI_DGEOM struct as an IDL save file
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_WRITE_DGEOM,Ppar,Kdgeom
;
; INPUTS:
;	Kdgeom	- KCWI_DGEOM struct
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;	TEST	- only write out if kdgeom.status=0 (good fit)
;
; OUTPUTS:
;	None.
;
; PROCEDURE:
;	Uses the tag kdgeom.dgeomfile to write out the struct as an IDL
;	save file.  Checks if ppar.clobber is set and takes appropriate
;	action.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-SEP-11	Initial Revision
;-
;
pro kcwi_write_dgeom,ppar,kdgeom, test=test
;
; startup
pre = 'KCWI_WRITE_DGEOM'
q = ''
;
; check inputs
if n_params(0) lt 1 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', Ppar, Kdgeom'
	return
endif
;
; Check structs
if kcwi_verify_geom(kdgeom,/init) ne 0 then return
if kcwi_verify_ppar(ppar,/init) ne 0 then return
;
; check fit status
if keyword_set(test) and kdgeom.status ne 0 then begin
	kcwi_print_info,ppar,pre,'Kdgeom fit no good, nothing written.',/error
	return
endif
;
; write it out
; check if it exists already
if file_test(kdgeom.dgeomfile) then begin
	;
	; clobber it, if requested
    	if ppar.clobber eq 1 then begin
		file_delete,kdgeom.dgeomfile,verbose=ppar.verbose
		kcwi_print_info,ppar,pre,'deleted existing dgeom file', $
			kdgeom.dgeomfile,format='(a,a)'
	endif else begin
		kcwi_print_info,ppar,pre, $
			'existing dgeom file undisturbed', $
			kdgeom.dgeomfile,format='(a,a)'
		return
	endelse
endif
;
; write it out if we get here
save,kdgeom,filename=kdgeom.dgeomfile
kcwi_print_info,ppar,pre,'wrote dgeom file',kdgeom.dgeomfile,format='(a,a)'
;
return
end

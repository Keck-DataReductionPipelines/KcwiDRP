; $Id: kcwi_write_geom.pro,v 1.1 2014/09/11 18:26:47 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights reserved.
;+
; NAME:
;	KCWI_WRITE_GEOM
;
; PURPOSE:
;	Writes out the KCWI_GEOM struct as an IDL save file
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
;	Uses the tag kgeom.geomfile to write out the struct as an IDL
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
pro kcwi_write_geom,ppar,kgeom, test=test
;
; startup
pre = 'KCWI_WRITE_GEOM'
version = repstr('$Revision: 1.1 $ $Date: 2014/09/11 18:26:47 $','$','')
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
; write it out if we get here
save,kgeom,filename=kgeom.geomfile
kcwi_print_info,ppar,pre,'wrote geom file',kgeom.geomfile,format='(a,a)'
;
return
end

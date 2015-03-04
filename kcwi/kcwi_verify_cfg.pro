; $Id: kcwi_verify_cfg.pro,v 1.5 2013/10/31 18:06:04 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_VERIFY_CFG
;
; PURPOSE:
;	This function verifies the input KCWI_CFG struct.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_VERIFY_CFG(Kcfg)
;
; INPUTS:
;	Kcfg	- array of struct KCWI_CFG
;
; RETURNS:
;	The status of the input KCWI_CFG struct:
;	0	- verified without problems
;	1	- a malformed KCWI_CFG struct was passed
;
; KEYWORDS:
;	INITIALIZED - set to check if KCWI_CFG is initialized
;	SILENT	- set to silence output
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-08	Initial version
;	2013-SEP-14	Added initialized keyword
;-
function kcwi_verify_cfg,kcfg,initialized=initialized,silent=silent
	;
	; setup
	pre = 'KCWI_VERIFY_CFG'
	;
	; check input
	stat = 0
	sz = size(kcfg)
	if sz[0] ne 1 or sz[1] lt 1 or sz[2] ne 8 then begin
		if not keyword_set(silent) then $
			print,pre+': Error - malformed KCWI_CFG struct array'
		stat = 1
	endif else begin
		if keyword_set(initialized) then begin
			test = n_elements(kcfg)
			if total(kcfg.initialized) ne test then begin
				print,pre+': Error - KCWI_CFG struct not initialized'
				stat = 1
			endif
		endif
	endelse
	;
	return,stat
end

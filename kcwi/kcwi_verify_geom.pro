; $Id: kcwi_verify_geom.pro,v 1.2 2013/09/16 20:45:12 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_VERIFY_GEOM
;
; PURPOSE:
;	This function verifies the input KCWI_GEOM struct.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_VERIFY_GEOM(Kgeom)
;
; INPUTS:
;	Kgeom	- KCWI_GEOM struct
;
; RETURNS:
;	The status of the input KCWI_GEOM struct:
;	0	- verified without problems
;	1	- a malformed or uninitialized KCWI_GEOM struct was passed
;
; KEYWORDS:
;	INITIALIZED - set to check if KCWI_GEOM struct is initialized
;	SILENT	- set to silence output
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-SEP-15	Initial version
;-
function kcwi_verify_geom,kgeom,initialized=initialized,silent=silent
	;
	; setup
	pre = 'KCWI_VERIFY_GEOM'
	;
	; check input
	stat = 0
	sz = size(kgeom)
	if sz[0] ne 1 or sz[1] lt 1 or sz[2] ne 8 then begin
		if not keyword_set(silent) then $
			print,pre+': Error - malformed KCWI_GEOM struct'
		stat = 1
	endif else begin
		if keyword_set(initialized) then begin
			if kgeom.initialized ne 1 then begin
				if not keyword_set(silent) then $
					print,pre+': Error - KCWI_GEOM struct not initialized, run KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS first'
				stat = 1
			endif
		endif
	endelse
	;
	return,stat
end

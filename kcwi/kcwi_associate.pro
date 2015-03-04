; $Id: kcwi_associate.pro,v 1.6 2013/11/06 00:05:28 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_ASSOCIATE
;
; PURPOSE:
;	This function returns the indices of the KCWI_CFG array that
;	is closest in time to the target KCWI_CFG scalar input.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_ASSOCIATE( KCFG, TCFG )
;
; INPUTS:
;	Kcfg	- array of struct KCWI_CFG for a given group
;	Tcfg	- target scalar struct KCWI_CFG to match
;	Ppar	- pipeline parameters KCWI_PPAR struct
;
; Returns:
;	Index of the Kcfg entry that is closest in time to the target Tcfg
;
; INPUT KEYWORDS:
;	AFTER	- match the closest in time after epoch of target
;	BEFORE	- match the closest in time before epoch of target
;
; OUTPUT KEYWORD:
;	COUNT	- set to get extra screen output
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;	Compares target Julian date given by Tcfg to Julian dates for
;	group contained in Kcfg and finds the entry with the smallest
;	time offset compared to the target.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-08	Initial version
;	2013-OCT-31	Now returns matched KCWI_CFG struct
;	2013-NOV-01	Added AFTER/BEFORE keywords
;-
function kcwi_associate, kcfg, tcfg, ppar, $
	after=after, before=before, count=count
	;
	; setup
	pre = 'KCWI_ASSOCIATE'
	count = 0
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init,/silent) ne 0 then begin
		kcwi_print_info,ppar,pre,'Ppar - malformed KCWI_PPAR struct',/error
		match = -1
	endif
	if kcwi_verify_cfg(kcfg,/init,/silent) ne 0 then begin
		kcwi_print_info,ppar,pre,'Search - malformed KCWI_CFG struct array',/error
		match = -1
	endif
	if kcwi_verify_cfg(tcfg,/init,/silent) ne 0 then begin
		kcwi_print_info,ppar,pre,'Target - malformed KCWI_CFG struct array',/error
		match = -1
	endif
	if tcfg.juliandate le 0. then begin
		kcwi_print_info,ppar,pre,'target date not set',/error
		match = -1
	endif
	if total(kcfg.juliandate) le 0. then begin
		kcwi_print_info,ppar,pre,'group dates not set',/error
		match = -1
	endif
	;
	; check after match
	if keyword_set(after) then begin
		offs = kcfg.juliandate - tcfg.juliandate
		a = where(offs ge 0., na)
		if na gt 0 then begin
			offs = offs[a]
			match = (where(offs eq min(offs)))[0]
		endif else begin
			kcwi_print_info,ppar,pre,'no after match',/error
			match = -1
		endelse
	;
	; check before match
	endif else if keyword_set(before) then begin
		offs = tcfg.juliandate - kcfg.juliandate
		b = where(offs ge 0., nb)
		if nb gt 0 then begin
			offs = offs[b]
			match = (where(offs eq min(offs)))[0]
		endif else begin
			kcwi_print_info,ppar,pre,'no before match',/error
			match = -1
		endelse
	;
	; get offsets
	endif else begin
		offs = abs(kcfg.juliandate - tcfg.juliandate)
		match = (where(offs eq min(offs)))[0]
	endelse
	;
	if match[0] ge 0 then begin
		count = 1
		return,kcfg[match]
	endif else return,match
end

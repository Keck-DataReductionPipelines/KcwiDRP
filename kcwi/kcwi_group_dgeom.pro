;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_GROUP_DGEOM
;
; PURPOSE:
;	This procedure groups continuum bars (cbars) and arcs in the KCWI_CFG 
;	struct for a given night.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_GROUP_DGEOM, Kcfg, Ppar, Ccfg, Acfg
;
; INPUTS:
;	Kcfg	- array of struct KCWI_CFG for a given directory
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Ccfg	- a KCWI_CFG struct vector with one entry for each arcbars from
;			a calibration set
;	Acfg	- a KCWI_CFG struct vector with one entry for each arc from
;			a calibration set
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Updates Ppar struct with number of geometry groups, number of
;	arcs and number continuum bars (ndirect, ndarcs).
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-APR-01	Initial version
;-
pro kcwi_group_dgeom, kcfg, ppar, ccfg, acfg
	;
	; setup
	pre = 'KCWI_GROUP_DGEOM'
	;
	; check input
	if kcwi_verify_cfg(kcfg) ne 0 then return
	if kcwi_verify_ppar(ppar) ne 0 then return
	;
	; required calibration image types
	bg = where(kcfg.imgtype eq 'arcbars' and $
		strpos(kcfg.obstype,'direct') ge 0, nbar)
	ag = where(kcfg.imgtype eq 'arc' and $
		strpos(kcfg.obstype,'direct') ge 0, narc)
	;
	; check if we have required types
	if narc le 0 or nbar le 0 then begin
		;
		; record results
		ppar.ndarcs = narc
		ppar.ndirect = 0
		if narc le 0 then $
			kcwi_print_info,ppar,pre,'no arcs found!',/error
		if nbar le 0 then $
			kcwi_print_info,ppar,pre,'no arcbars found!',/error
		return
	endif
	;
	; get config records
	cfg = kcfg[bg]
	afg = kcfg[ag]
	;
	; collect matches
	bmatch = lonarr(nbar) - 1L
	amatch = lonarr(nbar) - 1L
	;
	; loop over bars images and gather direct geom pairs
	for i=0,nbar-1 do begin
		;
		; loop over arcs and find a good match
		for j=0,narc-1 do begin
			;
			; check configuration
			tcfg = kcwi_match_cfg(afg[j],cfg[i],ppar,count=nm,/silent)
			;
			; check for match
			if nm eq 1 then begin
				;
				; are we close in image number?
				if abs(cfg[i].imgnum - afg[j].imgnum) lt 2 then begin
					bmatch[i] = i
					amatch[i] = j
				endif	; end check if we are close in image number
			endif	; end check for match
		endfor	; end loop over arcs
	endfor	; end loop over cbars
	;
	; get the good ones
	good = where(bmatch ge 0, ndirect)
	;
	; collect the good calibs
	if ndirect gt 0 then begin
		;
		; cbars
		ccfg = cfg[bmatch[good]]
		;
		; arcs
		acfg = afg[amatch[good]]
	endif else begin
		acfg = -1
		ccfg = -1
		kcwi_print_info,ppar,pre,'no direct geom image sets found',/warning
	endelse
	;
	; record results
	ppar.ndarcs = ndirect
	ppar.ndirect = ndirect
	if ndirect gt 0 then ppar.dgeomexists = 1
	;
	return
end

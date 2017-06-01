;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_GROUP_GEOM
;
; PURPOSE:
;	This procedure groups continuum bars (cbars) and arcs in the KCWI_CFG 
;	struct for a given night.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_GROUP_GEOM, Kcfg, Ppar, Ccfg, Acfg
;
; INPUTS:
;	Kcfg	- array of struct KCWI_CFG for a given directory
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Ccfg	- a KCWI_CFG struct vector with one entry for each cbars from
;			a calibration set
;	Acfg	- a KCWI_CFG struct vector with one entry for each arc from
;			a calibration set
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Updates Ppar struct with number of geometry groups, number of
;	arcs and number continuum bars (ngeom, narcs, nbars).
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-APR-01	Initial version
;-
pro kcwi_group_geom, kcfg, ppar, ccfg, acfg
	;
	; setup
	pre = 'KCWI_GROUP_GEOM'
	ppar.nggrps = 0
	;
	; check input
	if kcwi_verify_cfg(kcfg) ne 0 then return
	if kcwi_verify_ppar(ppar) ne 0 then return
	;
	; required calibration image types
	bg = where(kcfg.imgtype eq 'cbars' and $
		strpos(kcfg.obstype,'direct') lt 0, nbar)
	ag = where(kcfg.imgtype eq 'arc' and $
		strpos(kcfg.obstype,'direct') lt 0, narc)
	;
	; check if we have required types
	if narc le 0 or nbar le 0 then begin
		;
		; record results
		if narc le 0 then $
			kcwi_print_info,ppar,pre,'no arcs found!',/error
		if nbar le 0 then $
			kcwi_print_info,ppar,pre,'no bars found!',/error
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
	lamp   = strarr(nbar)
	;
	; loop over bars images and gather geom pairs
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
				if abs(cfg[i].imgnum - afg[j].imgnum) lt 4 then begin
					bmatch[i] = i
					if strpos(cfg[i].gratid,'BH') ge 0 or $
					   strpos(cfg[i].gratid,'BL') ge 0 or $
					   strpos(cfg[i].gratid,'BM') ge 0 then begin
						if afg[j].lmp1stat eq 1 and afg[j].lmp1shst eq 1 then begin
							amatch[i] = j
							lamp[i] = afg[j].lmp1nam
						endif else $
							if amatch[i] lt 0 then amatch[i] = j
					endif else begin
						if afg[j].lmp0stat eq 1 and afg[j].lmp0shst eq 1 then begin
							amatch[i] = j
							lamp[i] = afg[j].lmp0nam
						endif else $
							if amatch[i] lt 0 then amatch[i] = j
					endelse
				endif	; end check if we are close in image number
			endif	; end check for match
		endfor	; end loop over arcs
	endfor	; end loop over cbars
	;
	; get the good ones
	good = where(bmatch ge 0, ngeom)
	;
	; collect the good calibs
	if ngeom gt 0 then begin
		kcwi_print_info,ppar,pre,'Number of geom groups',ngeom
		;
		; cbars
		ccfg = cfg[bmatch[good]]
		;
		; arcs
		acfg = afg[amatch[good]]
		;
		; lamps
		lamp = lamp[good]
		for i=0,ngeom-1 do $
			kcwi_print_info,ppar,pre,'BarImg, Geom config, lamp', $
				ccfg[i].imgnum, $
				', '+(kcwi_cfg_string(ccfg[i]))[0] +', '+lamp[i], $
				format='(a,i7,a)'
	endif else begin
		acfg = -1
		ccfg = -1
		kcwi_print_info,ppar,pre,'no geom groups found',/warning
	endelse
	;
	; report number of geom groups
	ppar.nggrps = ngeom
	;
	return
end

;
; Copyright (c) 2017, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MATCH_GEOM
;
; PURPOSE:
;	Match input Kcfg to geom from input list Gfiles
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_MATCH_GEOM(Gfiles, Kcfg)
;
; INPUTS:
;	Gfiles		- string array list of *_geom.save files
;	Kcfg		- KCWI_CFG struct giving configuration of target
;
; KEYWORDS:
;
; OUTPUTS:
;	Full file specification of alternate geometry file.
;
; SIDE EFFECTS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2018-JUN-28	Initial version
;-
function kcwi_match_geom, gfiles, kcfg
	;
	; setup
	cfile = ''
	;
	; matching status
	mstat = intarr(n_elements(gfiles))
	tstmp = dblarr(n_elements(gfiles))
	;
	; are we direct?
	if strpos(kcfg.obstype,'direct') ge 0 then $
		do_direct = (1 eq 1) $
	else	do_direct = (1 eq 0)
	;
	; loop over input list of geom files
	for i=0,n_elements(gfiles)-1 do begin
		restore,gfiles[i]
		match = 0
		;
		; check for direct geometry
		if do_direct then $
			do_geom = (kdgeom.status eq 0) $
		else	do_geom = (kgeom.status eq 0)
		;
		; is our geom file good?
		if do_geom then begin
			;
			; check the direct items for matching
			if do_direct then begin
				;
				; get timestamp
				tstmp[i] = kdgeom.timestamp
				;
				; match items
				if kcfg.xbinsize eq kdgeom.xbinsize then $
					match += 1
				if kcfg.ybinsize eq kdgeom.ybinsize then $
					match += 1
				if abs(kcfg.camang-kdgeom.camang) le .05 then $
					match += 1
				if kcfg.ifunum eq kdgeom.ifunum then $
					match += 1
				;
				; if the all match, then record it
				if match ge 4 then mstat[i] = 1
			;
			; check the dispersed items for matching
			endif else begin
				;
				; get timestamp
				tstmp[i] = kgeom.timestamp
				;
				; match items
				if kcfg.xbinsize eq kgeom.xbinsize then $
					match += 1
				if kcfg.ybinsize eq kgeom.ybinsize then $
					match += 1
				if strtrim(kcfg.gratid,2) eq $
				   strtrim(kgeom.gratid,2) then $
					match += 1
				if abs(kcfg.grangle-kgeom.grangle) le .05 then $
					match += 1
				if kcfg.filtnum eq kgeom.filtnum then $
					match += 1
				if abs(kcfg.camang-kgeom.camang) le .05 then $
					match += 1
				if kcfg.ifunum eq kgeom.ifunum then $
					match += 1
				;
				; if the all match, then record it
				if match ge 7 then mstat[i] = 1
			endelse
		endif	; our geometry is good
	endfor	; loop over geom files
	;
	; check for matches
	g = where(mstat eq 1, ng)
	;
	; just got one
	if ng eq 1 then begin
		cfile = gfiles[g]
	;
	; more than one, take most recent
	endif else if ng gt 1 then begin
		gfs = gfiles[g]
		tss = tstmp[g]
		s = sort(tss)
		gfs = gfs[s]
		cfile = gfs[ng-1]
	endif
	return,cfile
end	; kcwi_match_geom

; $Id: kcwi_match_cfg.pro,v 1.19 2014/10/27 19:47:13 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MATCH_CFG
;
; PURPOSE:
;	This function returns the indices within Kcfg of images that
;	match the target image Tcfg
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_MATCH_CFG( KCFG, TCFG )
;
; INPUTS:
;	Kcfg	- array of struct KCWI_CFG for a given data set
;	Tcfg	- target struct KCWI_CFG to match
;	Ppar	- pipeline parameter KCWI_PPAR struct
;	Tlist	- list of KCWI_CFG struct tag names to match
;		   Defaults to ['XBINSIZE','YBINSIZE','AMPMODE',
;				'GRATID','GRATPOS','FILTER',
;				'FM4POS','CAMPOS','FOCPOS']
;
; Returns:
;	Kcfg entries that match the target configuration, -1 if none found.
;
; INPUT KEYWORDS:
;	SILENT	- set to suppress warning and error messages (for testing)
;	IMGTYPE	- set to KCWI image type to restrict match to: bias, dark, etc.
;	OBJECT	- set this to match target object or set to string to match
;			object name in string, implies IMGTYPE='object'
;	CWI	- set this to return the match closest in sky coordinate to the target
;	TIME	- set this to return the match closest in time to the target
;	AFTER	- set this to return the match closest in time after the target
;	BEFORE	- set this to return the match closest in time before the target
;
; OUTPUT KEYWORDS:
;	COUNT	- the number of matched records
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;	Compares target configuration defined by Tcfg to configurations for
;	group contained in Kcfg and finds each entry that matches the
;	target configuration.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-07	Initial version
;-
function kcwi_match_cfg, kcfg, tcfg, ppar, tlist, $
	imgtype=imgtype, object=object, $
	cwi=cwi, time=time, after=after, before=before, $
	count=count, silent=silent
	;
	; setup
	pre = 'KCWI_MATCH_CFG'
	count=0
	name=''
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init,/silent) ne 0 then begin
		kcwi_print_info,ppar,pre,'Ppar error - malformed KCWI_PPAR struct',/error
		return,-1
	endif
	if kcwi_verify_cfg(kcfg,/init,/silent) ne 0 then begin
		kcwi_print_info,ppar,pre,'Search Error - malformed KCWI_CFG struct array',/error
		return,-1
	endif
	if kcwi_verify_cfg(tcfg,/init,/silent) ne 0 then begin
		kcwi_print_info,ppar,pre,'Target Error - malformed KCWI_CFG struct array',/error
		return,-1
	endif
	;
	; check object name keyword
	if keyword_set(object) then begin
		;
		; did we specify an object in the keyword?
		if size(object,/type) eq 7 then $
			tobj = strtrim(object,2) $
		else	tobj = strtrim(tcfg.object,2)	; no? use target object
	endif	else	tobj = ''
	;
	; how big is our pool?
	ncfg = n_elements(kcfg)
	;
	; do we have a tag list
	if n_elements(tlist) le 0 then begin
		mtags = ['XBINSIZE','YBINSIZE','AMPMODE','GRATID','GRATPOS','FILTER', $
			 'FM4POS','CAMPOS','FOCPOS']
	endif else begin
		mtags = strupcase(tlist)
	endelse
	nmatch = n_elements(mtags)
	tnum = intarr(nmatch)
	;
	; check input tag list
	tags = tag_names(tcfg)
	for i=0,nmatch-1 do begin
		t = where(strmatch(tags,mtags[i]) eq 1, nt)
		if nt le 0 then begin
			kcwi_print_info,ppar,pre,'Unrecognized tag',mtags[i],/error
			return,-1
		endif
		tnum[i] = t[0]
	endfor
	;
	; set up match status
	mstat = intarr(ncfg)
	;
	; set up time deltas
	tdelt = dblarr(ncfg)
	;
	; set up coord deltas
	cdelt = dblarr(ncfg)
	;
	; loop over pool
	for i=0,ncfg-1 do begin
		;
		; assume we match
		match = (1 eq 1)
		;
		; loop over match tags
		for j=0,nmatch-1 do begin
			;
			; check for non-matching condition
			if kcfg[i].(tnum[j]) ne tcfg.(tnum[j]) then begin
				match = (1 eq 0)
			endif
		endfor
		;
		; we have a tag match
		if match then begin
			;
			; tag match result
			mstat[i] = 1
			;
			; check image type keyword
			if keyword_set(imgtype) then begin
				if strmatch(strtrim(kcfg[i].imgtype,2),imgtype) ne 1 then $
					mstat[i] = 0
			endif
			;
			; should we test for a target object?
			if strlen(tobj) gt 0 then begin
				;
				; is our test case an object image?
				if strmatch(strtrim(kcfg[i].imgtype,2),'object') eq 1 then begin
					;
					; does the string appear?
					if strpos(kcfg[i].object,tobj) lt 0 then $
						mstat[i] = 0
				endif else mstat[i] = 0
			endif
		endif
		;
		; time offset
		tdelt[i] = tcfg.juliandate - kcfg[i].juliandate
		;
		; sky coord offset
		if kcfg[i].ra ge 0. and kcfg[i].dec ge -90. then $
			gcirc,2,tcfg.ra,tcfg.dec,kcfg[i].ra,kcfg[i].dec,dis $
		else	dis = -99.
		cdelt[i] = dis
	endfor
	;
	; record tag matches
	mcfgi = where(mstat eq 1,count)
	;
	; do we have any matches?
	if count gt 0 then begin
		;
		; get minimum tdelt
		mintdelt = min(abs(tdelt[mcfgi]))
		;
		; check cwi keyword
		if keyword_set(cwi) then begin
			;
			; coord match only good for a limited time
			good = where(abs(tdelt) lt 0.07d, ngood)
			;
			; do we have good points in time window?
			if ngood gt 0 then begin
				;
				; get minimum coord offset within window
				mincdelt = min(cdelt[good])
				;
				; get closest in sky coords within time window
				mcfgi = where(mstat eq 1 and cdelt ge 0. and cdelt eq mincdelt, count)
				;
				; no singular match?: use closest in time instead
				if count ne 1 then $
					mcfgi = where(mstat eq 1 and abs(tdelt) eq mintdelt, count)
			;
			; no good points in time window?: use closest in time instead
			endif else begin
				mcfgi = where(mstat eq 1 and abs(tdelt) eq mintdelt, count)
			endelse
		;
		; now check time keyword
		endif else if keyword_set(time) then begin
			mcfgi = where(mstat eq 1 and abs(tdelt) eq mintdelt, count)
		;
		; before keyword
		endif else if keyword_set(before) then begin
			before = where(mstat eq 1 and tdelt gt 0, nbefore)
			if nbefore gt 0 then begin
				ttdel = min(tdelt[before])
				mcfgi = where(mstat eq 1 and tdelt eq ttdel, count)
			;
			; none before
			endif else count = 0
		;
		; after keyword
		endif else if keyword_set(after) then begin
			after = where(mstat eq 1 and tdelt lt 0, nafter)
			if nafter gt 0 then begin
				ttdel = max(tdelt[after])
				mcfgi = where(mstat eq 1 and tdelt eq ttdel, count)
			;
			; none after
			endif else count = 0
		endif
	endif	; do we have any matches?
	;
	; check status
	if count le 0 then begin 
		mcfg = -1
		if not keyword_set(silent) then $
			kcwi_print_info,ppar,pre,'No matches to image,type',tcfg.imgnum,tcfg.imgtype, $
				format='(a,i9,2x,a)',/error
	endif else mcfg = kcfg[mcfgi]
	;
	return,mcfg
end	; KCWI_MATCH_CFG

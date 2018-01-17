;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_FIND_STDS
;
; PURPOSE:
;	This function finds the standard star observations within
;	the input configuration structure.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_FIND_STDS( Kcfg,  Ppar, Nstds)
;
; INPUTS:
;	Kcfg	- KCWI_CFG struct array
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Nstds	- How many standard star observations were found?
;
; RETURNS:
;	The indices of the observations within Kcfg that are standard star
;	observations.
;
; SIDE EFFECTS:
;	None.
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Gets a list of standard star reference spectra in !KCWI_DATA directory
;	and compares the names to the object names in Kcfg configuration
;	struct to determine which are standard star observations.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-NOV-05	Initial Revision
;-
function kcwi_find_stds,kcfg,ppar,nstds
	;
	; setup
	pre = 'KCWI_FIND_STDS'
	q=''
	nstds = 0
	stds = -1
	;
	; check inputs
	if kcwi_verify_cfg(kcfg,/init) ne 0 then return,stds
	if kcwi_verify_ppar(ppar,/init) ne 0 then return,stds
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return,stds
	endif
	;
	; test standard star directory
	if file_test(ddir+'stds',/directory,/read) ne 1 then begin
		kcwi_print_info,ppar,pre,'Standard star reference dir inaccessable, returning',/error
		return,stds
	endif
	;
	; number of observations
	nobs = n_elements(kcfg)
	;
	; set up a status array
	stdstat = intarr(nobs)
	;
	; get observation names
	obnames = strcompress(strlowcase(strtrim(kcfg.targname,2)),/remove)
	obstat = strcmp(strtrim(kcfg.imgtype,2),'object')
	;
	; get list of standard star reference spectra
	reffiles = file_search(ddir+'stds/*.fit*',count=nrefs)
	;
	; get names from file names
	fdecomp,reffiles,disk,dir,reflist,ext
	;
	; loop over observations
	for i=0,nobs-1 do begin
		;
		; are we a science object?
		if obstat[i] then begin
			;
			; get canonical standard name
			sname = kcwi_std_name(obnames[i])
			;
			; are we in the ref list?
			yes = where(strcmp(sname,reflist) eq 1, nyes)
			if nyes ge 1 then stdstat[i] = 1
		endif	; are we a science object?
	endfor	; loop over observations
	;
	; get the standards, if any
	stds = where(stdstat eq 1, nstds)
	;
	; log results
	kcwi_print_info,ppar,pre,'Found this many standard star observations', $
		nstds,form='(a,i4)'
	;
	return,stds
end	; kcwi_find_stds

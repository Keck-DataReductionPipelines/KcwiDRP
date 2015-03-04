; $Id: kcwi_find_stds.pro,v 1.2 2015/02/21 00:18:36 neill Exp $
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
	version = repstr('$Revision: 1.2 $ $Date: 2015/02/21 00:18:36 $','$','')
	q=''
	nstds = 0
	stds = -1
	;
	; check inputs
	if kcwi_verify_cfg(kcfg,/init) ne 0 then return,stds
	if kcwi_verify_ppar(ppar,/init) ne 0 then return,stds
	;
	; log
	kcwi_print_info,ppar,pre,version
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
	; get list of standard star reference spectra
	reflist = file_search(ddir+'stds/*.fit*',count=nrefs)
	;
	; get names from file names
	for i=0,nrefs-1 do begin
		fdecomp,reflist[i],disk,dir,name,ext
		reflist[i] = name
	endfor
	;
	; get observation names
	obnames = strlowcase(strtrim(kcfg.object,2))
	obstat = strcmp(strtrim(kcfg.imgtype,2),'object')
	;
	; set up a status array
	stdstat = intarr(n_elements(kcfg))
	;
	; loop over reference list
	for i=0,nrefs-1 do begin
		t = where(obstat eq 1 and strcmp(obnames,reflist[i]) eq 1, nt)
		if nt gt 0 then stdstat[t] = 1
	endfor
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

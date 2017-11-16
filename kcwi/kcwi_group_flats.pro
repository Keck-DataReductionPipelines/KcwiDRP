;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_GROUP_FLATS
;
; PURPOSE:
;	This procedure groups flats in the KCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_GROUP_FLATS, Kcfg, Ppar, Fcfg
;
; INPUTS:
;	Kcfg	- array of struct KCWI_CFG for a given directory
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Fcfg	- a KCWI_CFG struct vector with one entry for each flat group
;
; KEYWORDS:
;	None
;
; SIDE EFFECTS:
;	Outputs pipeline parameter file in ODIR for each flat group.
;
; PROCEDURE:
;	Finds flat images by inspecting the imgtype tags in Kcfg and
;	groups contiguous flat images.  Returns a KCWI_CFG struct vector
;	with one element for each flat group which is used to associate 
;	the flat groups with other observations.
;
; EXAMPLE:
;	Group flat images from directory 'night1/' and put the resulting
;	ppar files in 'night1/redux/':
;
;	KCFG = KCWI_READ_CFGS('night1/')
;	KCWI_GROUP_FLATS, KCFG, PPAR, FCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-29	Initial version
;	2013-SEP-09	Added loglun keyword
;	2013-SEP-13	Now use KCWI_PPAR struct for parameters
;	2017-NOV-15	Group all flats
;-
pro kcwi_group_flats, kcfg, ppar, fcfg
	;
	; setup
	pre = 'KCWI_GROUP_FLATS'
	ppar.nfgrps = 0
	;
	; instantiate and init a KCWI_CFG struct for the flat groups
	F = {kcwi_cfg}
	fcfg = struct_init(F)
	;
	; check input
	if kcwi_verify_cfg(kcfg) ne 0 then return
	if kcwi_verify_ppar(ppar) ne 0 then return
	;
	; get flat lists
	cflats = where(strpos(kcfg.imgtype,'cflat') ge 0, ncflats)
	dflats = where(strpos(kcfg.imgtype,'dflat') ge 0, ndflats)
	tflats = where(strpos(kcfg.imgtype,'tflat') ge 0, ntflats)
	;
	; get groups for each
	ngroups = 0
	;
	; cont flats
	if ncflats gt 0 then begin
		rangepar,cflats,cflist
		cfgroups = strsplit(cflist,',',/extract,count=ncgroups)
		if ncgroups gt 0 then begin
			fgroups = cfgroups
			ngroups += ncgroups
		endif
	endif
	;
	; dome flats
	if ndflats gt 0 then begin
		rangepar,dflats,dflist
		dfgroups = strsplit(dflist,',',/extract,count=ndgroups)
		if ndgroups gt 0 then begin
			if ngroups gt 0 then begin
				fgroups = [fgroups,dfgroups]
			endif else begin
				fgroups = dfgroups
			endelse
			ngroups += ndgroups
		endif
	endif
	;
	; twilight flats
	if ntflats gt 0 then begin
		rangepar,tflats,tflist
		tfgroups = strsplit(tflist,',',/extract,count=ntgroups)
		if ntgroups gt 0 then begin
			if ngroups gt 0 then begin
				fgroups = [fgroups,tfgroups]
			endif else begin
				fgroups = tfgroups
			endelse
			ngroups += ntgroups
		endif
	endif
	;
	; if we have flat groups, set them up
	if ngroups gt 0 then begin
		;
		; setup KCWI_CFG struct for groups
		fcfg = replicate(fcfg, ngroups)
		;
		; report number of flat groups
		ppar.nfgrps = ngroups
		kcwi_print_info,ppar,pre,'Number of flat groups',ngroups
		;
		; loop over flat groups
		for i=0,ngroups-1 do begin
			;
			; fresh copy of KCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			rangepar,fgroups[i],flist
			nims = n_elements(flist)
			imnums = kcfg[flist].imgnum
			rangepar,imnums,rl
			;
			; copy params
			f = flist[0]
			tags = tag_names(fcfg)
			for it = 0, n_elements(tags)-1 do $
				fcfg[i].(it) = kcfg[f].(it)
			;
			; set parameters
			pp.cflats		= rl
			fcfg[i].grouplist	= rl
			fcfg[i].nimages		= nims
			;
			; configuration
			fcfg[i].imgtype		= kcfg[f].imgtype
			;
			; use first image in group
			gi = kcfg[f].imgnum
			grt = strmid(kcfg[f].obsfname,0, $
				     strpos(kcfg[f].obsfname,'.fit'))
			;
			; files and directories
			pp.masterflat		= grt + '_mflat.fits'
			pp.ppfname		= grt + '_mflat.ppar'
			;
			fcfg[i].groupnum	= gi
			fcfg[i].groupfile	= pp.masterflat
			fcfg[i].grouppar	= pp.ppfname
			;
			; status
			pp.nfgrps		= 1
			pp.initialized		= 1
			pp.progid		= pre
			fcfg[i].initialized	= 1
			;
			; write out ppar file
			kcwi_write_ppar,pp
		endfor	; loop over flat groups
	endif else $
		kcwi_print_info,ppar,pre,'no flat frames found',/warning
	;
	return
end

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
;	2019-FEB-04	More robust grouping
;-
pro kcwi_group_flats, kcfg, ppar, fcfg
	;
	; setup
	pre = 'KCWI_GROUP_FLATS'
	ppar.nfgrps = 0
	max_ngroups = 20
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
	fcfg = replicate(fcfg,max_ngroups)
	;
	; cont flats
	if ncflats gt 0 then begin
		cfcfg = kcfg[cflats]
		cp = 0
		while cp lt ncflats do begin
			m = kcwi_match_cfg(cfcfg,cfcfg[cp],ppar)
			;
			; fresh copy of KCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			nims = n_elements(m)
			imnums = m.imgnum
			rangepar,imnums,rl
			;
			; copy params
			tags = tag_names(cfcfg[cp])
			for it = 0, n_elements(tags)-1 do $
				fcfg[ngroups].(it) = cfcfg[cp].(it)
			;
			; set parameters
			pp.cflats		= rl
			fcfg[ngroups].grouplist	= rl
			fcfg[ngroups].nimages	= nims
			;
			; configuration
			fcfg[ngroups].imgtype	= cfcfg[cp].imgtype
			;
			; use first image in group
			gi = cfcfg[cp].imgnum
			grt = strmid(cfcfg[cp].obsfname,0, $
				     strpos(cfcfg[cp].obsfname,'.fit'))
			;
			; files and directories
			pp.masterflat		= grt + '_mflat.fits'
			pp.ppfname		= grt + '_mflat.ppar'
			;
			fcfg[ngroups].groupnum	= gi
			fcfg[ngroups].groupfile	= pp.masterflat
			fcfg[ngroups].grouppar	= pp.ppfname
			;
			; status
			pp.nfgrps		= 1
			pp.initialized		= 1
			pp.progid		= pre
			fcfg[ngroups].initialized	= 1
			;
			; write out ppar file
			kcwi_write_ppar,pp
			;
			ngroups += 1
			cp += nims
		endwhile
	endif
	;
	; dome flats
	if ndflats gt 0 then begin
		dfcfg = kcfg[dflats]
		cp = 0
		while cp lt ndflats do begin
			m = kcwi_match_cfg(dfcfg,dfcfg[cp],ppar)
			;
			; fresh copy of KCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			nims = n_elements(m)
			imnums = m.imgnum
			rangepar,imnums,rl
			;
			; copy params
			tags = tag_names(dfcfg[cp])
			for it = 0, n_elements(tags)-1 do $
				fcfg[ngroups].(it) = dfcfg[cp].(it)
			;
			; set parameters
			pp.cflats		= rl
			fcfg[ngroups].grouplist	= rl
			fcfg[ngroups].nimages	= nims
			;
			; configuration
			fcfg[ngroups].imgtype	= dfcfg[cp].imgtype
			;
			; use first image in group
			gi = dfcfg[cp].imgnum
			grt = strmid(dfcfg[cp].obsfname,0, $
				     strpos(dfcfg[cp].obsfname,'.fit'))
			;
			; files and directories
			pp.masterflat		= grt + '_mflat.fits'
			pp.ppfname		= grt + '_mflat.ppar'
			;
			fcfg[ngroups].groupnum	= gi
			fcfg[ngroups].groupfile	= pp.masterflat
			fcfg[ngroups].grouppar	= pp.ppfname
			;
			; status
			pp.nfgrps		= 1
			pp.initialized		= 1
			pp.progid		= pre
			fcfg[ngroups].initialized	= 1
			;
			; write out ppar file
			kcwi_write_ppar,pp
			;
			ngroups += 1
			cp += nims
		endwhile
	endif
	;
	; twilight flats
	if ntflats gt 0 then begin
		tfcfg = kcfg[tflats]
		cp = 0
		while cp lt ntflats do begin
			m = kcwi_match_cfg(tfcfg,tfcfg[cp],ppar)
			;
			; fresh copy of KCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			nims = n_elements(m)
			imnums = m.imgnum
			rangepar,imnums,rl
			;
			; copy params
			tags = tag_names(tfcfg[cp])
			for it = 0, n_elements(tags)-1 do $
				fcfg[ngroups].(it) = tfcfg[cp].(it)
			;
			; set parameters
			pp.cflats		= rl
			fcfg[ngroups].grouplist	= rl
			fcfg[ngroups].nimages	= nims
			;
			; configuration
			fcfg[ngroups].imgtype	= tfcfg[cp].imgtype
			;
			; use first image in group
			gi = tfcfg[cp].imgnum
			grt = strmid(tfcfg[cp].obsfname,0, $
				     strpos(tfcfg[cp].obsfname,'.fit'))
			;
			; files and directories
			pp.masterflat		= grt + '_mflat.fits'
			pp.ppfname		= grt + '_mflat.ppar'
			;
			fcfg[ngroups].groupnum	= gi
			fcfg[ngroups].groupfile	= pp.masterflat
			fcfg[ngroups].grouppar	= pp.ppfname
			;
			; status
			pp.nfgrps		= 1
			pp.initialized		= 1
			pp.progid		= pre
			fcfg[ngroups].initialized	= 1
			;
			; write out ppar file
			kcwi_write_ppar,pp
			;
			ngroups += 1
			cp += nims
		endwhile
	endif
	;
	; if we have flat groups, set them up
	if ngroups gt 0 then begin
		;
		; setup KCWI_CFG struct for groups
		fcfg = fcfg[0:(ngroups-1)]
		;
		; report number of flat groups
		ppar.nfgrps = ngroups
		kcwi_print_info,ppar,pre,'Number of flat groups',ngroups
	endif else begin
		fcfg = fcfg[0]
		kcwi_print_info,ppar,pre,'no flat frames found',/warning
	endelse
	;
	return
end

; $Id: kcwi_group_flats.pro,v 1.14 2015/02/21 00:18:37 neill Exp $
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
;-
pro kcwi_group_flats, kcfg, ppar, fcfg
	;
	; setup
	pre = 'KCWI_GROUP_FLATS'
	version = repstr('$Revision: 1.14 $ $Date: 2015/02/21 00:18:37 $','$','')
	;
	; instantiate and init a KCWI_CFG struct for the flat groups
	F = {kcwi_cfg}
	fcfg = struct_init(F)
	;
	; check input
	if kcwi_verify_cfg(kcfg) ne 0 then return
	if kcwi_verify_ppar(ppar) ne 0 then return
	;
	; get flat list
	flats = where(strpos(kcfg.imgtype,'cflat') ge 0, nflats)
	;
	; if we have flats, group them
	if nflats gt 0 then begin
		;
		; create range list of all flats
		rangepar,flats,flist
		;
		; get flat groups split by comma
		fgroups = strsplit(flist,',',/extract,count=ngroups)
		;
		; record number of groups
		ppar.nfgrps = ngroups
		ppar.flatexists = 1
		;
		; setup KCWI_CFG struct for groups
		fcfg = replicate(fcfg, ngroups)
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
			; set parameters
			pp.cflats		= rl
			fcfg[i].grouplist	= rl
			fcfg[i].nimages		= nims
			;
			; get date and coords from first flat in series
			f = flist[0]
			fcfg[i].juliandate	= kcfg[f].juliandate
			fcfg[i].date		= kcfg[f].date
			fcfg[i].ra		= kcfg[f].ra
			fcfg[i].dec		= kcfg[f].dec
			;
			; configuration
			fcfg[i].imgtype		= 'cflat'
			fcfg[i].naxis		= kcfg[f].naxis
			fcfg[i].naxis1		= kcfg[f].naxis1
			fcfg[i].naxis2		= kcfg[f].naxis2
			fcfg[i].binning		= kcfg[f].binning
			fcfg[i].xbinsize	= kcfg[f].xbinsize
			fcfg[i].ybinsize	= kcfg[f].ybinsize
			fcfg[i].ampmode		= kcfg[f].ampmode
			fcfg[i].nasmask		= kcfg[f].nasmask
			fcfg[i].gratid		= kcfg[f].gratid
			fcfg[i].gratpos		= kcfg[f].gratpos
			fcfg[i].filter		= kcfg[f].filter
			fcfg[i].fm4pos		= kcfg[f].fm4pos
			fcfg[i].campos		= kcfg[f].campos
			fcfg[i].focpos		= kcfg[f].focpos
			;
			; use first image number in group
			fi = kcfg[f].imgnum
			;
			; files and directories
			pp.masterflat		= 'mflat_' + $
				string(fi,'(i0'+strn(pp.fdigits)+')') + '.fits'
			pp.ppfname		= 'mflat_' + $
				string(fi,'(i0'+strn(pp.fdigits)+')') + '.ppar'
			;
			fcfg[i].groupnum	= fi
			fcfg[i].groupfile	= pp.masterflat
			fcfg[i].grouppar	= pp.ppfname
			;
			; status
			pp.initialized		= 1
			pp.progid		= pre+': '+version
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

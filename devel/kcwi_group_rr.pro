;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_GROUP_RR
;
; PURPOSE:
;	This procedure groups relative response images in the KCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_GROUP_RR, Kcfg, Ppar, Fcfg
;
; INPUTS:
;	Kcfg	- array of struct KCWI_CFG for a given directory
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Rcfg	- a KCWI_CFG struct vector with one entry for each rr group
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Outputs pipeline parameter file in ODIR for each rr group.
;
; PROCEDURE:
;	Finds rr images by inspecting the imgtype tags in Kcfg and
;	groups contiguous rr images.  Returns a KCWI_CFG struct vector
;	with one element for each rr group which is used to associate 
;	the rr groups with other observations.
;
; EXAMPLE:
;	Group rr images from directory 'night1/' and put the resulting
;	ppar files in 'night1/redux/':
;
;	KCFG = KCWI_READ_CFGS('night1/')
;	KCWI_GROUP_RR, KCFG, PPAR, RCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-JUL-29	Initial version
;-
pro kcwi_group_rr, kcfg, ppar, rcfg
	;
	; setup
	pre = 'KCWI_GROUP_RR'
	version = repstr('$Revision: v0.2.10-40-g60c9d44 $ $Date: Fri May 15 10:35:48 2015 -0700 $','$','')
	;
	; instantiate and init a KCWI_CFG struct for the rr groups
	R = {kcwi_cfg}
	rcfg = struct_init(R)
	;
	; check input
	if kcwi_verify_cfg(kcfg) ne 0 then return
	if kcwi_verify_ppar(ppar) ne 0 then return
	;
	; get rr list
	rrs = where(strcmp(kcfg.imgtype,'dflat') eq 1, nrrs)
	;
	; if we have rrs, group them
	if nrrs gt 0 then begin
		;
		; create range list of all rrs
		rangepar,rrs,rlist
		;
		; get rr groups split by comma
		rgroups = strsplit(rlist,',',/extract,count=ngroups)
		;
		; record number of groups
		ppar.nrgrps = ngroups
		ppar.rrexists = 1
		;
		; setup KCWI_CFG struct for groups
		rcfg = replicate(rcfg, ngroups)
		;
		; loop over rr groups
		for i=0,ngroups-1 do begin
			;
			; fresh copy of KCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			rangepar,rgroups[i],rlist
			nims = n_elements(rlist)
			imnums = kcfg[rlist].imgnum
			rangepar,imnums,rl
			;
			; set parameters
			pp.rrs			= rl
			rcfg[i].grouplist	= rl
			rcfg[i].nimages		= nims
			;
			; get date from first rr in series
			r = rlist[0]
			rcfg[i].juliandate	= kcfg[r].juliandate
			rcfg[i].date		= kcfg[r].date
			;
			; configuration
			rcfg[i].imgtype		= 'dflat'
			rcfg[i].naxis		= kcfg[r].naxis
			rcfg[i].naxis1		= kcfg[r].naxis1
			rcfg[i].naxis2		= kcfg[r].naxis2
			rcfg[i].binning		= kcfg[r].binning
			rcfg[i].xbinsize	= kcfg[r].xbinsize
			rcfg[i].ybinsize	= kcfg[r].ybinsize
			rcfg[i].ampmode		= kcfg[r].ampmode
			rcfg[i].nasmask		= kcfg[r].nasmask
			rcfg[i].gratid		= kcfg[r].gratid
			rcfg[i].gratpos		= kcfg[r].gratpos
			rcfg[i].filter		= kcfg[r].filter
			rcfg[i].fm4pos		= kcfg[r].fm4pos
			rcfg[i].campos		= kcfg[r].campos
			rcfg[i].focpos		= kcfg[r].focpos
			;
			; use first image number in group
			ri = kcfg[r].imgnum
			;
			; files and directories
			pp.masterrr		= 'mrr_' + strn(ri) + '.fits'
			pp.ppfname		= 'mrr_' + strn(ri) + '.ppar'
			;
			rcfg[i].groupnum	= ri
			rcfg[i].groupfile	= pp.masterrr
			rcfg[i].grouppar	= pp.ppfname
			;
			; status
			pp.initialized		= 1
			pp.progid		= pre+': '+version
			rcfg[i].initialized	= 1
			;
			; write out ppar file
			kcwi_write_ppar,pp
		endfor	; loop over rr groups
	endif else $
		kcwi_print_info,ppar,pre,'no rr frames found',/warning
	;
	return
end

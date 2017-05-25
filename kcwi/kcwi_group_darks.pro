;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_GROUP_DARKS
;
; PURPOSE:
;	This procedure groups biases in the KCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_GROUP_DARKS, Kcfg, Ppar, Dcfg
;
; INPUTS:
;	Kcfg	- array of struct KCWI_CFG for a given night
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Dcfg	- array of struct KCWI_CFG, one for each dark group
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Outputs pipeline parameter file in ODIR for each dark group.
;
; PROCEDURE:
;	Finds dark images by inspecting the imgtype tags in Kcfg and
;	gropus contiguous dark images.  Returns a KCWI_CFG struct vector
;	with one element for each dark group which is used to associate 
;	the dark groups with other observations.
;
; EXAMPLE:
;	Group dark images from directory 'night1/' and put the resulting
;	ppar files in 'night1/redux/':
;
;	KCFG = KCWI_NIGHT_READ('night1/')
;	KCWI_GROUP_DARKS, KCFG, PPAR, DCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-03	Initial version
;	2013-SEP-09	Added loglun keyword
;	2013-SEP-13	Now use KCWI_PPAR struct for parameters
;-
pro kcwi_group_darks, kcfg, ppar, dcfg
	;
	; setup
	pre = 'KCWI_GROUP_DARKS'
	ppar.ndgrps = 0
	;
	; instantiate and init a KCWI_CFG struct for the dark groups
	D = {kcwi_cfg}
	dcfg = struct_init(D)
	;
	; check inputs
	if kcwi_verify_cfg(kcfg) ne 0 then return
	if kcwi_verify_ppar(ppar) ne 0 then return
	;
	; get dark groups
	dg = where(strmatch(kcfg.imgtype,'dark') eq 1, ndg)
	;
	; if we have darks, group them
	if ndg gt 0 then begin
		;
		; set up for group counting
		maxgrps = 100
		maxmemb = 50
		groups = lonarr(maxgrps,maxmemb) - 1l
		gind = 0
		p = 0
		;
		; set up first group
		gcfg = kcfg[dg[0]]
		groups[gind,p] = dg[0]
		last = gcfg.imgnum
		p += 1
		;
		; loop over dark images and gather groups
		for i=1,ndg-1 do begin
			;
			; check exposure time, binning, ccdmode, ampmode,
			; gainmul and sequence
			if abs(kcfg[dg[i]].telapse - gcfg.telapse) gt 0.01 or $
			       kcfg[dg[i]].xbinsize ne gcfg.xbinsize or $
			       kcfg[dg[i]].ybinsize ne gcfg.ybinsize or $
			       kcfg[dg[i]].ccdmode ne gcfg.ccdmode or $
			       kcfg[dg[i]].gainmul ne gcfg.gainmul or $
			       (kcfg[dg[i]].imgnum - last) ne 1 or $
			       strcmp(kcfg[dg[i]].ampmode,gcfg.ampmode) ne 1 then begin
				;
				; new group
				gind += 1
				p = 0
				;
				; check for group overflow
				if gind ge maxgrps then begin
					kcwi_print_info,ppar,pre,'dark group overflow',gind,/error
					return
				endif
				;
				; first member of new group
				gcfg = kcfg[dg[i]]
				groups[gind,p] = dg[i]
				last = gcfg.imgnum
				p += 1
			endif else begin
				;
				; next member of group
				gcfg = kcfg[dg[i]]
				groups[gind,p] = dg[i]
				last = kcfg[dg[i]].imgnum
				p += 1
				;
				; check for member overflow
				if p ge maxmemb then begin
					kcwi_print_info,ppar,pre,'dark group member overflow',p,/error
					return
				endif
			endelse
		endfor
		;
		; number of groups
		ngroups = gind + 1
		;
		; setup KCWI_CFG struct for groups
		dcfg = replicate(dcfg, ngroups)
		;
		; loop over dark groups
		g = 0	; good group counter
		for i=0,ngroups-1 do begin
			;
			; fresh copy of KCWI_PPAR struct
			pp = ppar
			;
			; get indexes for this group
			dlist = reform(groups[i,*])
			good = where(dlist ge 0, nmem)
			dlist = dlist[good]
			nims = n_elements(dlist)
			;
			; do we have enough for a group?
			if nims ge pp.mingroupdark then begin
				imnums = kcfg[dlist].imgnum
				rangepar,imnums,rl
				pp.darks		= rl
				dcfg[g].grouplist	= rl
				dcfg[g].nimages		= nims
				;
				; get date from first dark in series
				d = dlist[0]
				dcfg[g].juliandate	= kcfg[d].juliandate
				dcfg[g].date		= kcfg[d].date
				;
				; configuration
				dcfg[g].imgtype		= 'dark'
				dcfg[g].xposure		= kcfg[d].xposure
				dcfg[g].telapse		= kcfg[d].telapse
				dcfg[g].exptime		= kcfg[d].exptime
				dcfg[g].naxis		= kcfg[d].naxis
				dcfg[g].naxis1		= kcfg[d].naxis1
				dcfg[g].naxis2		= kcfg[d].naxis2
				dcfg[g].binning		= kcfg[d].binning
				dcfg[g].xbinsize	= kcfg[d].xbinsize
				dcfg[g].ybinsize	= kcfg[d].ybinsize
				dcfg[g].ampmode		= kcfg[d].ampmode
				dcfg[g].ccdmode		= kcfg[d].ccdmode
				dcfg[g].gainmul		= kcfg[d].gainmul
				;
				; use first image number in group
				gi = kcfg[d].imgnum
				;
				; files and directories
				pp.masterdark		= 'mdark_' + $
					string(gi,'(i0'+strn(pp.fdigits)+')') +$
								'.fits'
				pp.ppfname		= 'mdark_' + $
					string(gi,'(i0'+strn(pp.fdigits)+')') +$
								'.ppar'
				;
				dcfg[g].groupnum	= gi
				dcfg[g].groupfile	= pp.masterdark
				dcfg[g].grouppar	= pp.ppfname
				;
				; status
				pp.ndgrps		= 1
				pp.initialized		= 1
				pp.progid		= pre
				dcfg[g].initialized	= 1
				;
				; write out ppar file
				kcwi_write_ppar,pp
				;
				; increment group counter
				g = g + 1
			endif	; do we have enough images?
		endfor	; loop over dark groups
		;
		; all groups failed
		if g le 0 then begin
			;
			; return an uninitialized, single KCWI_CFG struct
			dcfg = dcfg[0]
			kcwi_print_info,ppar,pre,'no dark groups with >= ', $
				ppar.mingroupdark, ' images.',/warning
		;
		; some groups failed
		endif else if g lt ngroups then begin
			;
			; trim KCWI_CFG struct to only good groups
			dcfg = dcfg[0:(g-1)]
			kcwi_print_info,ppar,pre,'removing ', ngroups - g, $
				' dark groups with < ', ppar.mingroupdark, $
				' images.', format='(a,i3,a,i3,a)'
		endif	; otherwise, we are OK as is
		;
		; report number of dark groups
		ppar.ndgrps = g
		kcwi_print_info,ppar,pre,'Number of dark groups',g
	;
	; no dark frames found
	endif else $
		kcwi_print_info,ppar,pre,'no dark frames found',/warning
	;
	return
end

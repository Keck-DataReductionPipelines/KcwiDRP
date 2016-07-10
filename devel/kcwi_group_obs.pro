;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_GROUP_OBS
;
; PURPOSE:
;	This procedure groups objects in the KCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_GROUP_OBS, KCFG, INDIR, ODIR, OCFG
;
; OPTIONAL INPUTS:
;	Kcfg	- array of struct KCWI_CFG for a given directory
;	Indir	- input directory (string) defaults to current dir
;	Odir	- output directory (string) defaults to current dir
;
; OUTPUTS:
;	Ocfg	- a KCWI_CFG struct vector with one entry per object image
;
; KEYWORDS:
;	COUNT	- number of good groups found
;	VERBOSE	- set to get extra screen output
;
; SIDE EFFECTS:
;	outputs pipeline parameter file in ODIR for each object group.
;
; PROCEDURE:
;	Finds object images by inspecting the imgtype tags in Kcfg and
;	groups with nearby (in time) calibration images.  Returns a KCWI_CFG
;	struct vector with one element for each object group which is used 
;	to associate the object groups with calibration files.
;
; EXAMPLE:
;	Group image data in directory 'night1' and put resulting pipeline
;	parameter files in 'night1/redux':
;
;	KCFG = KCWI_READ_CFGS('night1/')
;	KCWI_GROUP_OBS, KCFG, 'night1/', 'night1/redux/', OCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-APR-16	Initial version
;-
pro kcwi_group_obs, kcfg, indir, odir, ocfg, count=count, $
	filespec=filespec, verbose=verbose
	;
	; setup
	pre = 'KCWI_GROUP_OBS'
	;
	; check inputs
	if kcwi_verify_cfg(kcfg) ne 0 then return
	;
	; instantiate and init a KCWI_CFG struct for the object groups
	O = {kcwi_cfg}
	ocfg = struct_init(O)
	count = 0
	;
	; instantiate a and init ppar structure for the pipeline parameters
	A = {kcwi_ppar}
	ppar = struct_init(A)
	;
	; get object list
	objects = where(strpos(kcfg.imgtype,'object') ge 0, nobj)
	;
	; test object names
	good = where(strlen(strtrim(kcfg[objects].object,2)) gt 0, ngood)
	if ngood le 0 then begin
		print,pre+': Header Keyword (OBJECT) Error - no valid objects'
		return
	endif
	;
	; get unique object id's
	sobs = strtrim(kcfg[good].object,2)
	sobs = sobs[sort(sobs)]
	sobs = sobs[uniq(sobs)]
	nuobs = n_elements(sobs)
	;
	; maintain a list of unique object/configuration groups
	uocgrps = ['']
	uocpars = [ppar]
	;
	; loop over each unique id
	for i=0,nuobs-1 do begin
		;
		; this object's observations
		t=where(strmatch(strtrim(kcfg.object,2),sobs[i]) eq 1 and $
			strmatch(strtrim(kcfg.imgtype,2),'object') eq 1, nt)
		if nt gt 0 then begin
			;
			; loop over this object's observations
			pnum = 0
			skynum = 0
			for j=0,nt-1 do begin
				p = t[j]	; pointer into config struct array
				;
				; this allows us to skip grouped observations of same object
				if done[p] eq 0 then begin
					;
					; new copy of pipeline parameter struct
					pp = ppar
					;
					; output file name
					pp.ppfname = sobs[i]+'_'+strn(pnum)+'.ppar'
					;
					; set observation parameters
					pp.object	= kcfg[p].object
					pp.observer	= kcfg[p].observer
					pp.imgtype	= 'object'
					pp.date		= kcfg[p].date
					pp.juliandate	= kcfg[p].juliandate
					pp.rawdir	= indir
					pp.reddir	= odir
					;
					; group observations of same object by configuration
					same = kcwi_match_cfg(kcfg,kcfg[p],object=sobs[i],count=nsame)
					;
					; mark object/config group completed
					done[same] = 1
					;
					; are there one or more sky observations?
					sky = where(kcfg[same].skyobs ne 0, nsky)
					if nsky gt 0 then begin
						pp.skyexists = 1
						rangepar,kcfg[same[sky]].imgnum,rl
						pp.sky = rl
						if nsky lt nsame then $
							remove,sky,same
					endif else begin
						pp.skyexists = 0
						pp.sky = ''
						if kcfg[p].shuffmod eq 0 then $
							print,pre+': Warning - no sky frame for: ',pp.ppfname
					endelse
					rangepar,kcfg[same].imgnum,rl
					pp.npims = n_elements(same)
					pp.imnum = rl
					;
					; record configuration
					pp.binning 	= kcfg[p].binning
					pp.xbinsize	= kcfg[p].xbinsize
					pp.ybinsize	= kcfg[p].ybinsize
					pp.shuffmod	= kcfg[p].shuffmod
					pp.nasmask	= kcfg[p].nasmask
					pp.gratid	= kcfg[p].gratid
					pp.gratpos	= kcfg[p].gratpos
					pp.filter	= kcfg[p].filter
					pp.fm4pos	= kcfg[p].fm4pos
					pp.campos	= kcfg[p].campos
					pp.focpos	= kcfg[p].focpos
					;
					; dark and bias lists
					;
					; check biases
					if nbgrps gt 0 then begin
						b = kcwi_associate(bcfg,kcfg[p])
						if b ge 0 then begin
							pp.biasexists	= 1
							pp.biases	= bcfg[b].grouplist
							pp.masterbias	= bcfg[b].groupfile
						endif
					endif
					;
					; check darks
					if ndgrps gt 0 then begin
						d = kcwi_associate(dcfg,kcfg[p])
						if d ge 0 then begin
							pp.darkexists	= 1
							pp.darks	= dcfg[d].grouplist
							pp.masterdark	= dcfg[d].groupfile
						endif
					endif
					;
					; group continuum flats by configuration
					cflats = kcwi_match_cfg(kcfg,kcfg[p],imgtype='cflat',count=ncflats)
					if ncflats gt 0 then begin
						rangepar,kcfg[cflats].imgnum,rl
						pp.cflats = rl
						pp.flatexists = 1
					endif else begin
						pp.cflats = ''
						pp.flatexists = 0
						print,pre+': Warning - no continuum flats for: ', $
							pp.ppfname
					endelse
					;
					; group continuum bars by configuration
					cbars = kcwi_match_cfg(kcfg,kcfg[p],imgtype='cbars',count=ncbars)
					if ncbars gt 0 then begin
						rangepar,kcfg[cbars].imgnum,rl
						pp.cbars = rl
					endif else begin
						pp.cbars = ''
						print,pre+': Warning - no continuum bars for: ', $
							pp.ppfname
					endelse
					;
					; group arcs by configuration
					arcs = kcwi_match_cfg(kcfg,kcfg[p],imgtype='arc',count=narcs)
					if narcs gt 0 then begin
						rangepar,kcfg[arcs].imgnum,rl
						pp.arcs = rl
					endif else begin
						pp.arcs = ''
						print,pre+': Warning - no arcs for: ',pp.ppfname
					endelse
					;
					; group arc bars by configuration
					arcbars = kcwi_match_cfg(kcfg,kcfg[p],imgtype='arcbars',count=narcbars)
					if narcbars gt 0 then begin
						rangepar,kcfg[arcbars].imgnum,rl
						pp.arcbars = rl
					endif else begin
						pp.arcbars = ''
						print,pre+': Warning - no arc bars for: ', $
							pp.ppfname
					endelse
					;
					; we have initialized the ppar structure
					pp.initialized	= 1
					;
					; check for sky obs
					if nsky gt 0 then begin
						ppsky = pp
						ppsky.npims = nsky
						ppsky.imnum = ppsky.sky
						ppsky.ppfname = sobs[i]+'_sky_'+strn(skynum)+'.ppar'
						;
						; record in list
						grpsky = string(ppsky.object,ppsky.npims,ppsky.gratid,ppsky.filter, $
						     ppsky.shuffmod,ppsky.nasmask,1, $
						     ppsky.fm4pos,ppsky.gratpos,ppsky.campos,ppsky.focpos, $
						     ppsky.biasexists,ppsky.darkexists,ppsky.flatexists, $
						     ppsky.skyexists,ppsky.binning,ppsky.xbinsize,ppsky.ybinsize, $
						     ppsky.ppfname, $
					format = '(a-16,1x,i3,1x,a4,1x,a4,1x,3i1,1x,4i7,1x,4i1,1x,3i1,1x,a)')
						skynum = skynum + 1
						uocgrps = [ uocgrps, grpsky ]
						uocpars = [ uocpars, ppsky ]
					endif
					;
					; do we still have regular observations after removing the skies?
					if nsky lt nsame then begin
						pp.ppfname = sobs[i]+'_'+strn(pnum)+'.ppar'
						;
						; record in list
						grp = string(pp.object,pp.npims,pp.gratid,pp.filter, $
						     pp.shuffmod,pp.nasmask,0, $
						     pp.fm4pos,pp.gratpos,pp.campos,pp.focpos, $
						     pp.biasexists,pp.darkexists,pp.flatexists, $
						     pp.skyexists,pp.binning,pp.xbinsize,pp.ybinsize, $
						     pp.ppfname, $
					format = '(a-16,1x,i3,1x,a4,1x,a4,1x,3i1,1x,4i7,1x,4i1,1x,3i1,1x,a)')
						;
						; increment number of ppar files for this object
						pnum = pnum + 1
						uocgrps = [ uocgrps, grp ]
						uocpars = [ uocpars, pp ]
					endif
				endif	; are we done?
			endfor	; loop over this object's observations
		endif else print,pre+': Error - no observations found for: ', sobs[i]
	endfor	; loop over unique object names
	;
	; trim unique object/configuration (uoc) lists
	uocgrps = uocgrps[1:*]
	uocpars = uocpars[1:*]
	nuocg = n_elements(uocgrps)
	print,'Found '+strn(nuocg)+' unique object/configuration groups:'
	print,'SET  OBJECT           NIM  GRT  FLT NSS     FM4   GRAT    CAM    FOC  CAL BIN PPFNAME'
	print,'---  ---------------- --- ---- ---- --- ------- ------ ------ ------ ---- --- -------'
	forprint,indgen(nuocg),uocgrps,form='(i03,2x,a)'
	;
	; loop and get resp and flux cal associations
	;print,'Associate response and flux cal observations to targets'
	;print,'Enter SET triples: object response flux, <cr> to quit'
	;rec = 'start'
	;while strlen(rec) gt 0 do begin
	;	read,'N N N: ',rec
	;	if strlen(rec) gt 0 then begin
	;		io = fix(gettok(rec,' '))
	;		ir = fix(gettok(rec,' '))
	;		ic = fix(rec)
	;		uocpars[io].resp_ppf = uocpars[ir].ppfname
	;		uocpars[io].fcal_ppf = uocpars[ic].ppfname
	;	endif
	;endwhile
	for i=0,nuocg-1 do $
		kcwi_write_ppar,uocpars[i]
	;
	return
end

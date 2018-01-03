;
; Copyright (c) 2017, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_ALT_CALS
;
; PURPOSE:
;	Find calibration files in an alternate directory.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_ALT_CALS(Kcfg, Adir, Ppar)
;
; INPUTS:
;	Kcfg		- KCWI_CFG struct giving configuration of target
;	Adir		- Alternate calibration source directory
;	Ppar		- KCWI_PPAR struct for pipeline params
;
; KEYWORDS:
;
; OUTPUTS:
;	Full file specification of alternate calibration file.
;
; SIDE EFFECTS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2018-JUN-27	Initial version
;-
function kcwi_alt_cals, kcfg, adir, ppar, $
	bias=bias, dark=dark, flat=flat, geom=geom, dgeom=dgeom, afile=afile, $
	drr=drr, std=std
	;
	; setup
	pre = 'KCWI_ALT_CALS'
	cfile = ''
	afile = ''
	;
	; verify inputs
	if kcwi_verify_cfg(kcfg,/init) ne 0 then return,cfile
	if kcwi_verify_ppar(ppar,/init) ne 0 then return,cfile
	;
	; test adir
	if not file_test(adir,/directory,/read) then begin
		kcwi_print_info,ppar,pre,'Alt cal dir not accessable: '+adir, $
			/error
		return,cfile
	endif
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
	;
	; bias
	if keyword_set(bias) then begin
		kcwi_print_info,ppar,pre,'Searching for mbiases in '+adir
		cfgs = kcwi_read_cfgs(adir, filespec='*_mbias.fit*', count=nf)
		;
		; found some biases
		if nf gt 0 then begin
			;
			; matching criteria
			tlist = ['CCDMODE','AMPMODE','XBINSIZE','YBINSIZE', $
				 'GAINMUL','NAXIS1','NAXIS2']
			mcfg = kcwi_match_cfg(cfgs,kcfg,ppar,tlist,count=nm)
			;
			; found some matches
			if nm gt 0 then begin
				;
				; only one?
				if nm eq 1 then begin
					cfile = mcfg.obsdir + mcfg.obsfname
				;
				; more than one, use closest ccd temperature to match
				endif else begin
					tdel = abs(mcfg.tmpa1 - kcfg.tmpa1)
					t = (where(tdel eq min(tdel)))[0]
					cfile = mcfg[t].obsdir + mcfg[t].obsfname
				endelse
			endif else $
				kcwi_print_info,ppar,pre, $
					'No matching biases found'
		endif else $
			kcwi_print_info,ppar,pre,'No alternate biases found'
	;
	; darks
	endif else if keyword_set(dark) then begin
		kcwi_print_info,ppar,pre,'Searching for mdarks in '+adir
		cfgs = kcwi_read_cfgs(adir, filespec='*_mdark.fit*', count=nf)
		;
		; found some darks
		if nf gt 0 then begin
			;
			; matching criteria
			tlist = ['CCDMODE','AMPMODE','XBINSIZE','YBINSIZE','GAINMUL']
			mcfg = kcwi_match_cfg(cfgs,kcfg,ppar,tlist,count=nm)
			;
			; found some matches
			if nm gt 0 then begin
				;
				; only one?
				if nm eq 1 then begin
					cfile = mcfg.obsdir + mcfg.obsfname
				;
				; more than one, use closest exposure time to match
				endif else begin
					tdel = abs(mcfg.exptime - kcfg.exptime)
					tind = where(tdel eq min(tdel),ntind)
					;
					; same exposure time, use ccd temperature to match
					if ntind gt 1 then begin
						zcfg = mcfg[tind]
						zdel = abs(zcfg.tmpa1 - kcfg.tmpa1)
						zind = (where(zdel eq min(zdel)))[0]
						mcfg = zcfg[zind]
					endif else $
						mcfg = mcfg[tind]
					cfile = mcfg.obsdir + mcfg.obsfname
				endelse
			endif else $
				kcwi_print_info,ppar,pre, $
					'No matching darks found'
		endif else $
			kcwi_print_info,ppar,pre,'No alternate darks found'
	;
	; flats
	endif else if keyword_set(flat) then begin
		kcwi_print_info,ppar,pre,'Searching for mflats in '+adir
		cfgs = kcwi_read_cfgs(adir, filespec='*_mflat.fit*', count=nf)
		;
		; found some flats
		if nf gt 0 then begin
			;
			; matching criteria
			tlist = ['CCDMODE','AMPMODE','XBINSIZE','YBINSIZE', $
				 'GAINMUL','GRATID','GRANGLE', 'FILTNUM', $
				 'CAMANG','IFUNUM']
			mcfg = kcwi_match_cfg(cfgs,kcfg,ppar,tlist, $
						imgtype='cflat',count=nm)
			;
			; found some matches
			if nm gt 0 then begin
				;
				; only one?
				if nm eq 1 then begin
					cfile = mcfg.obsdir + mcfg.obsfname
				;
				; more than one, use closest bench temperature to match
				endif else begin
					tdel = abs(mcfg.tmpa8 - kcfg.tmpa8)
					t = (where(tdel eq min(tdel)))[0]
					cfile = mcfg[t].obsdir + mcfg[t].obsfname
				endelse
			endif else $
				kcwi_print_info,ppar,pre, $
					'No matching flats found'
		endif else $
			kcwi_print_info,ppar,pre,'No alternate flats found'
	;
	; geom files
	endif else if keyword_set(geom) then begin
		kcwi_print_info,ppar,pre,'Searching for geom files in '+adir
		cfgs = kcwi_read_cfgs(adir, filespec='*_geom.fit*', count=nf)
		;
		; found some geom files
		if nf gt 0 then begin
			;
			; matching criteria
			tlist = ['XBINSIZE','YBINSIZE','GRATID','GRANGLE', $
				 'FILTNUM','CAMANG','IFUNUM']
			mcfg = kcwi_match_cfg(cfgs,kcfg,ppar,tlist,count=nm)
			;
			; found some matches
			if nm gt 0 then begin
				;
				; only one?
				if nm eq 1 then begin
					cfile = mcfg.obsdir + mcfg.obsfname
				;
				; more than one, use closest bench temperature to match
				endif else begin
					tdel = abs(mcfg.tmpa8 - kcfg.tmpa8)
					t = (where(tdel eq min(tdel)))[0]
					cfile = mcfg[t].obsdir + mcfg[t].obsfname
				endelse
				;
				; extract cbars, arcs filenames
				geom = mrdfits(cfile,1,chdr)
				cfile = geom.cbarsfname
				afile = geom.arcfname
			endif else $
				kcwi_print_info,ppar,pre, $
					'No matching geom files found'
		endif else $
			kcwi_print_info,ppar,pre,'No alternate geom files found'
	;
	; direct geom files
	endif else if keyword_set(dgeom) then begin
		kcwi_print_info,ppar,pre,'Searching for dgeom files in '+adir
		cfgs = kcwi_read_cfgs(adir, filespec='*_dgeom.fit*', count=nf)
		;
		; found some dgeom files
		if nf gt 0 then begin
			;
			; matching criteria
			tlist = ['XBINSIZE','YBINSIZE','CAMANG','IFUNUM']
			mcfg = kcwi_match_cfg(cfgs,kcfg,ppar,tlist,count=nm)
			;
			; found some matches
			if nm gt 0 then begin
				;
				; only one?
				if nm eq 1 then begin
					cfile = mcfg.obsdir + mcfg.obsfname
				;
				; more than one, use closest bench temperature to match
				endif else begin
					tdel = abs(mcfg.tmpa8 - kcfg.tmpa8)
					t = (where(tdel eq min(tdel)))[0]
					cfile = mcfg[t].obsdir + mcfg[t].obsfname
				endelse
			endif else $
				kcwi_print_info,ppar,pre, $
					'No matching dgeom files found'
		endif else $
			kcwi_print_info,ppar,pre,'No alternate dgeom files found'
	;
	; direct relative response files
	endif else if keyword_set(drr) then begin
		kcwi_print_info,ppar,pre,'Searching for drrs in '+adir
		cfgs = kcwi_read_cfgs(adir, filespec='*_drr.fit*', count=nf)
		;
		; found some direct relative response files
		if nf gt 0 then begin
			;
			; matching criteria
			tlist = ['CCDMODE','AMPMODE','XBINSIZE','YBINSIZE', $
				 'GAINMUL','CAMANG','IFUNUM']
			mcfg = kcwi_match_cfg(cfgs,kcfg,ppar,tlist,count=nm)
			;
			; found some matches
			if nm gt 0 then begin
				;
				; only one?
				if nm eq 1 then begin
					cfile = mcfg.obsdir + mcfg.obsfname
				;
				; more than one, use closest bench temperature to match
				endif else begin
					tdel = abs(mcfg.tmpa8 - kcfg.tmpa8)
					t = (where(tdel eq min(tdel)))[0]
					cfile = mcfg[t].obsdir + mcfg[t].obsfname
				endelse
			endif else $
				kcwi_print_info,ppar,pre, $
					'No matching drrs found'
		endif else $
			kcwi_print_info,ppar,pre,'No alternate drrs found'
	;
	; inverse sensitivity files
	endif else if keyword_set(std) then begin
		kcwi_print_info,ppar,pre,'Searching for invsens files in '+adir
		cfgs = kcwi_read_cfgs(adir, filespec='*_invsens.fit*', count=nf)
		;
		; found some ivnsens files
		if nf gt 0 then begin
			;
			; matching criteria
			tlist = ['CCDMODE','AMPMODE','XBINSIZE','YBINSIZE', $
				 'GAINMUL','GRATID','GRANGLE', 'FILTNUM', $
				 'CAMANG','IFUNUM']
			mcfg = kcwi_match_cfg(cfgs,kcfg,ppar,tlist,count=nm)
			;
			; found some matches
			if nm gt 0 then begin
				;
				; only one?
				if nm eq 1 then begin
					cfile = mcfg.obsdir + mcfg.obsfname
				;
				; more than one, use closest bench temperature to match
				endif else begin
					tdel = abs(mcfg.tmpa8 - kcfg.tmpa8)
					t = (where(tdel eq min(tdel)))[0]
					cfile = mcfg[t].obsdir + mcfg[t].obsfname
				endelse
			endif else $
				kcwi_print_info,ppar,pre, $
					'No matching invsens files found'
		endif else $
			kcwi_print_info,ppar,pre,'No alternate invsens files found'
	endif
	return,cfile
end	; kcwi_alt_cals

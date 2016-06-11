; $Id$
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SET_GEOM
;
; PURPOSE:
;	This procedure uses the input KCWI_CFG struct to set the basic
;	parameters in the KCWI_GEOM struct.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SET_GEOM, Kgeom, Kcfg
;
; INPUTS:
;	Kgeom	- Input KCWI_GEOM struct.
;	Kcfg	- Input KCWI_CFG struct for a given observation.
;	Ppar	- Input KCWI_PPAR struct.
;
; KEYWORDS:
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Sets the following tags in the KCWI_GEOM struct according to the
;	configuration settings in KCWI_CFG.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-13	Initial version
;	2014-AUG-14	Added CWI Yellow grating
;-
pro kcwi_set_geom,kgeom,ikcfg,ppar, help=help
	;
	; setup
	pre = 'KCWI_SET_GEOM'
	;
	; help request
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', Kgeom, Kcfg, Ppar'
		return
	endif
	;
	; verify Kgeom
	ksz = size(kgeom)
	if ksz[2] eq 8 then begin
		if kgeom.initialized ne 1 then begin
			print,pre+': Error - KCWI_GEOM struct not initialized.'
			return
		endif
	endif else begin
		print,pre+': Error - malformed KCWI_GEOM struct'
		return
	endelse
	;
	; verify Kcfg
	if kcwi_verify_cfg(ikcfg,/silent) ne 0 then begin
		print,pre+': Error - malformed KCWI_CFG struct'
		return
	endif
	;
	; verify Ppar
	psz = size(ppar)
	if psz[2] eq 8 then begin
		if ppar.initialized ne 1 then begin
			print,pre+': Error - KCWI_PPAR struct not initialized.'
			return
		endif
	endif else begin
		print,pre+': Error - malformed KCWI_PPAR struct'
		return
	endelse
	;
	; take singleton of KCWI_CFG
	kcfg = ikcfg[0]
	;
	; check image type
	if strtrim(strupcase(kcfg.imgtype),2) ne 'CBARS' then begin
		kcwi_print_info,ppar,pre,'cbars images are the geom reference files, this file is of type',kcfg.imgtype,/error
		return
	endif
	;
	; get output geom file name
	odir = ppar.reddir
	kgeom.geomfile = ppar.reddir + $
	    strmid(kcfg.obsfname,0,strpos(kcfg.obsfname,'_int')) + '_geom.save'
    	;
    	; set basic configuration parameters
	kgeom.gratid = kcfg.gratid
	kgeom.gratnum = kcfg.gratnum
	kgeom.filter = kcfg.filter
	kgeom.filtnum = kcfg.filtnum
	kgeom.campos = kcfg.campos
	kgeom.grenc = kcfg.grenc
	kgeom.grangle = kcfg.grangle
	kgeom.gratanom = kcfg.gratanom
	kgeom.xbinsize = kcfg.xbinsize
	kgeom.ybinsize = kcfg.ybinsize
	kgeom.nx = kcfg.naxis1
	kgeom.ny = kcfg.naxis2
	kgeom.x0out = 30 / kgeom.xbinsize
	kgeom.goody0 = 10
	kgeom.goody1 = kgeom.ny - 10
	kgeom.trimy0 = 0
	kgeom.trimy1 = kgeom.ny
	kgeom.ypad = 1400 / kgeom.ybinsize
	kgeom.nasmask = kcfg.nasmask
	if kcfg.nasmask eq 1 then begin
		kgeom.goody0 = kcfg.nsobjr0 + 18
		kgeom.goody1 = kcfg.nsobjr1 - 18
		kgeom.trimy0 = kcfg.nsobjr0 - 18
		kgeom.trimy1 = kcfg.nsobjr1 + 18
		kgeom.ypad = 0
	endif
	;
	; get noise model
	rdnoise = 0.
	;
	; sum over amp inputs
	switch kcfg.nvidinp of
		4: rdnoise = rdnoise + kcfg.biasrn4
		3: rdnoise = rdnoise + kcfg.biasrn3
		2: rdnoise = rdnoise + kcfg.biasrn2
		1: rdnoise = rdnoise + kcfg.biasrn1
	endswitch
	;
	; take average
	rdnoise /= float(kcfg.nvidinp)
	kgeom.rdnoise = rdnoise
	;
	; wavelength numbers default from header
	kgeom.cwave = kcfg.cwave
	kgeom.wave0out = kcfg.wave0	
	kgeom.wave1out = kcfg.wave1
	kgeom.dwout = kcfg.dwav
	;
	; reference spectrum
	kgeom.refspec = ppar.datdir+ppar.atlas
	kgeom.reflist = ppar.datdir+ppar.linelist
	kgeom.refname = ppar.atlasname
	;
	; default to no cc offsets
	kgeom.ccoff = fltarr(24)
	;
	; check resolution and dispersion
	if strtrim(kcfg.gratid,2) eq 'BH3' then begin
		kgeom.resolution = 0.15
		kgeom.wavran = 560.
		kgeom.ccwn = 260./kgeom.ybinsize
		kgeom.rho = 2.80d
		kgeom.slant = -5.0d
		kgeom.lastdegree = 4
		kgeom.ccoff[0:4] = -277.0
		;
		; output disperison
		kgeom.dwout = 0.095 * float(kcfg.ybinsize)
	endif
	;
	; spatial scales
	kgeom.pxscl = 0.00004048d0	; deg/unbinned pixel
	kgeom.slscl = 0.00037718d0	; deg/slice
	if kcfg.ifunum eq 2 then begin
		kgeom.slscl = kgeom.slscl/2.d0
	endif else if kcfg.ifunum eq 3 then begin
		kgeom.slscl = kgeom.slscl/4.d0
	endif
	;
	; check central wavelength
	if kgeom.cwave le 0. then begin
		kcwi_print_info,ppar,pre,'No central wavelength found',/error
		return
	endif
	;
	; now check ppar values which override defaults
	if ppar.dw gt 0. then $
		kgeom.dwout = ppar.dw
	if ppar.wave0 gt 0. then $
		kgeom.wave0out = ppar.wave0
	if ppar.wave1 gt 0. then $
		kgeom.wave1out = ppar.wave1
	;
	; print log of values
	kcwi_print_info,ppar,pre,'Data cube output Disp (A/px), Wave0 (A): ', $
		kgeom.dwout,kgeom.wave0out,format='(a,f8.3,f9.2)'
	;
	return
end

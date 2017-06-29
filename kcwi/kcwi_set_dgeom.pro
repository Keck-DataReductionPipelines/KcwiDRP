;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SET_DGEOM
;
; PURPOSE:
;	This procedure uses the input KCWI_CFG struct to set the basic
;	parameters in the KCWI_DGEOM struct.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SET_DGEOM, Kdgeom, iKcfg
;
; INPUTS:
;	Kdgeom	- Input KCWI_DGEOM struct.
;	iKcfg	- Input KCWI_CFG struct for a given observation.
;	Ppar	- Input KCWI_PPAR struct.
;
; KEYWORDS:
;	None
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Sets the following tags in the KCWI_DGEOM struct according to the
;	configuration settings in KCWI_CFG.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2016-NOV-09	Initial version
;-
pro kcwi_set_dgeom,kdgeom,ikcfg,ppar,help=help
	;
	; setup
	pre = 'KCWI_SET_DGEOM'
	;
	; help request
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', Kdgeom, Kcfg, Ppar'
		return
	endif
	;
	; verify Kdgeom
	if kcwi_verify_geom(kdgeom,/init) ne 0 then return
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
	if strtrim(strupcase(kcfg.imgtype),2) ne 'ARC' then begin
		kcwi_print_info,ppar,pre,'arc images are the direct geom reference files, this file is of type',kcfg.imgtype,/error
		return
	endif
	;
	; get output dgeom file name
	odir = ppar.reddir
	kdgeom.geomfile = ppar.reddir + $
	    strmid(kcfg.obsfname,0,strpos(kcfg.obsfname,'_int')) + '_dgeom.fits'
    	;
    	; set basic configuration parameters
	kdgeom.arcimgnum = kcfg.imgnum
    	kdgeom.ifunum = kcfg.ifunum
	kdgeom.ifunam = kcfg.ifunam
	kdgeom.filter = kcfg.filter
	kdgeom.filtnum = kcfg.filtnum
	kdgeom.campos = kcfg.campos
	kdgeom.camang = kcfg.camang
	kdgeom.xbinsize = kcfg.xbinsize
	kdgeom.ybinsize = kcfg.ybinsize
	kdgeom.nx = kcfg.naxis1
	kdgeom.ny = kcfg.naxis2
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
	kdgeom.rdnoise = rdnoise
	;
	; spatial scales
	kdgeom.pxscl = 0.00004048d0	; deg/unbinned pixel
	kdgeom.slscl = 0.00037718d0	; deg/slice, Large slicer
	;
	; set IFU-specific params
	case kdgeom.ifunum of
		1: begin	; Large slicer
			kdgeom.do_gauss = 0
			kdgeom.minpix = 6 / kdgeom.ybinsize
		end
		2: begin	; Medium slicer
			if kdgeom.ybinsize eq 2 then begin	; 2x2 binning
				kdgeom.do_gauss = 1
				kdgeom.minpix = 3
			endif else begin			; 1x1 binning
				kdgeom.do_gauss = 0
				kdgeom.minpix = 6
			endelse
			kdgeom.slscl = kdgeom.slscl/2.d0
		end
		3: begin	; Small slicer
			kdgeom.do_gauss = 1
			if kdgeom.ybinsize eq 2 then $
				kdgeom.minpix = 1 $
			else	kdgeom.minpix = 2
			kdgeom.slscl = kdgeom.slscl/4.d0
		end
		else: begin
			kcwi_print_info,ppar,pre,'Unknown slicer ID', $
				kdgeom.ifunum,/error
			return
		end
	endcase
	;
	; log our update of the geom struct
	kdgeom.progid = pre
	kdgeom.timestamp = systime(1)
	;
	return
end

; $Id: kcwi_solve_geom.pro,v 1.4 2015/02/21 00:18:36 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SOLVE_GEOM
;
; PURPOSE:
;	Solve the wavelength solutions for each arc spectrum
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SOLVE_GEOM,Spec, Kgeom, Ppar
;
; INPUTS:
;	Spec	- a array of arc spectra produced by KCWI_EXTRACT_ARCS
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;
; SIDE EFFECTS:
;	Modifies KCWI_GEOM struct by calculating new control points that
;	take into account the wavelength solution.
;	NOTE: sets KGEOM.STATUS to 0 if fitting succeeded, otherwise sets to
;	1 or greater depending on reason for failure (see kcwi_solve_arcs.pro).
;
; PROCEDURE:
;	Find the wavelength solution of the reference bar arc and then
;	propogate it to the other bars.  Record the wavelength solution
;	in the wavelength control points in Kgeom.
;
; EXAMPLE:
;	Define the geometry from a 'cbars' image and use it to extract and 
;	display the spectra from an 'arc' image from the same calibration
;	sequence.
;
;	cbars = mrdfits('image7142_int.fits',0,chdr)
;	kcwi_trace_cbars,cbars,Kgeom,/centroid
;	arc = mrdfits('image7140_int.fits',0,ahdr)
;	kcwi_extract_arcs,arc,kgeom,arcspec
;	kcwi_solve_geom,arcspec,kgeom,ppar
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-SEP-18	Initial Revision
;-
;
pro kcwi_solve_geom,spec,kgeom,ppar, fitdisp=fitdisp, help=help
;
; startup
pre = 'KCWI_SOLVE_GEOM'
version = repstr('$Revision: 1.4 $ $Date: 2015/02/21 00:18:36 $','$','')
q = ''
;
; check inputs
if n_params(0) lt 2 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', ArcSpec, Kgeom'
	return
endif
;
; Check structs
if kcwi_verify_geom(kgeom,/init) ne 0 then return
if kcwi_verify_ppar(ppar,/init) ne 0 then return
;
; check spec
ssz = size(spec)
if ssz[0] ne 2 or ssz[2] ne 120 then begin
	kcwi_print_info,ppar,pre,'Input spec array malformed, run KCWI_EXTRACT_ARCS first.',/error
	return
endif
;
; plot file
p_fmt = '(i0'+strn(ppar.fdigits)+')'
plfil = ppar.reddir+'wave_cb' + string(kgeom.cbarsimgnum,p_fmt) + $
		       '_arc' + string(kgeom.arcimgnum,p_fmt)
;
; solve arc spectra
kcwi_solve_arcs,spec,kgeom,ppar,/tweak,plot_file=plfil
;
; solve transformation on slice-by-slice basis
kcwi_solve_slices,ppar,kgeom
;
return
end

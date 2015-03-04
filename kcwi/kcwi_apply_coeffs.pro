; $Id: kcwi_apply_coeffs.pro,v 1.11 2014/09/22 16:58:21 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_APPLY_COEFFS
;
; PURPOSE:
;	Applies wavelength solution coefficients to control points
;	for the given arc bar spectrum.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_APPLY_COEFFS,Kgeom,Barno,Coeffs
;
; INPUTS:
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Barno	- which bar spectrum to apply solution to
;
; INPUT KEYWORDS:
;	VERBOSE - extra output
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Updates Kgeom XW and YW tags to have appropriate values for input
;	wavelength solution coefficients.
;
; PROCEDURE:
;	Uses reference bar solution to define wavelength zeropoint and
;	dispersion.  Applies wavelength solution to original output control
;	points (Kgeom.[xo,yo]) to derive real wavelength coordinates of
;	each, then subtracts the wavelength zeropoint and divides by the
;	reference dispersion to generate psuedo wavelength pixel coordinates.
;
; EXAMPLE:
;	Define the geometry from a 'cbars' image and use it to extract and 
;	display the spectra from an 'arc' image from the same calibration
;	sequence.
;
;	cbars = mrdfits('image7142_int.fits',0,chdr)
;	kcwi_trace_cbars,cbars,Kgeom,/centroid
;	arc = mrdfits('image7140_int.fits',0,ahdr)
;	kcwi_extract_arcs,arc,kgeom,arcspec,/verbose
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-31	Initial Revision
;	2013-SEP-26	Uses reference slice for output control points
;-
;
pro kcwi_apply_coeffs,kgeom,barno,coeffs, $
	verbose=verbose, help=help
;
; startup
pre = 'KCWI_APPLY_COEFFS'
version = repstr('$Revision: 1.11 $ $Date: 2014/09/22 16:58:21 $','$','')
q = ''
;
; check inputs
if n_params(0) lt 3 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', Kgeom, Barno, Coeffs'
	return
endif
;
; Check Kgeom
ksz = size(kgeom)
if ksz[2] eq 8 then begin
	if kgeom.initialized ne 1 then begin
		print,pre+': Error - Kgeom struct not initialized.'
		return
	endif
endif else begin
	print,pre+': Error - Kgeom not legal, run KCWI_TRACE_CBARS and KCWI_EXTRACT ARCS first.'
	return
endelse
;
; check reference solution
if total(kgeom.rbcoeffs) eq 0. or kgeom.rbcoeffs[0] eq 0. or $
	 kgeom.rbcoeffs[1] eq 0. then begin
	print,pre+': Error - Kgeom reference bar coefficients not set, run KCWI_SOLVE_ARCS first.'
	return
endif
;
; check Barno
if barno lt 0 or barno gt 119 then begin
	print,pre+': Error - Bar number out of range (0-119): ',barno
	return
endif
;
; reference bar in same slice as kgeom.refbar
refbar = (barno mod 5)
;
; get control points
t=where(kgeom.bar eq barno and kgeom.xi gt 0.)
;
; spatial axis
; use reference slice, but adjust to left edge
refoutx = kgeom.refoutx - min(kgeom.refoutx) + kgeom.x0out
xo = refoutx[refbar]
;
; wavelength axis
yo = kgeom.yo[t]
;
; get reference wavelength
wave0 = kgeom.wave0out
;
; apply coeffs
xw = xo						; nothing to apply
yw = ( poly(yo,coeffs) - wave0 ) / kgeom.dwout	; apply wave soln.
;
; insert into kgeom
kgeom.xw[t] = xw
kgeom.yw[t] = yw
;
; insert fit coeffs for this bar
fo = kgeom.bfitord
kgeom.bfitcoeffs[0:fo,barno] = coeffs[0:fo]
;
; Kgeom timestamp
kgeom.timestamp = systime(1)
;
return
end

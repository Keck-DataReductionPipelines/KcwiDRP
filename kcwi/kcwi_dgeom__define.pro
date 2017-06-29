;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_DGEOM__DEFINE
;
; PURPOSE:
;	This procedure defines the structure for all the information for 
;	KCWI direct mode geometric transformations.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	kdgeom = {KCWI_DGEOM}
;
; INPUTS:
;	None
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	defines the KCWI_DGEOM data structure
;
; PROCEDURE:
;	Provide the automatic structure definition for the KCWI direct mode
;	geometric transformation structure KCWI_DGEOM.
;
; EXAMPLE:
;	Instantiate and initialize a single dgeom struct for KCWI:
;
;	kdgeom = {kcwi_dgeom}
;	kdgeom = struct_init(kdgeom)
;
; NOTES:
;	Keep structure name on a line by itself (see struct_init.pro).
;	Keep tag names 15 chars or less.
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2016-NOV-09	Initial version
;-
pro kcwi_dgeom__define
;
tmp = { kcwi_dgeom, $
;
; output filename
	geomfile:'', $		; output save file for dgeom struct
;
; calibration images
	cbarsimgnum:0l, $	; cbars image number (0 - 9999)
	cbarsjd:0.d0, $		; cbars image julian date
	cbarsfname:'', $	; cbars image file name
	arcimgnum:0l, $		; arc image number
	arcjd:0.d0, $		; arc image julian date
	arcfname:'', $		; arc image file name
;
; IFU properties
	ifunum: 0, $		; Slicer number (1-3, Large, Medium, Small)
	ifunam: '', $		; Slicer name ("Large", etc.)
;
; filter properties
	filter:'', $		; filter id
	filtnum:-1, $		; filter number
;
; encoder positions
	campos:0L, $		; Camera encoder position
	camang:0.0d, $		; Camera articulation angle in degrees
;
; pixel scale
	pxscl:0.00008096d0, $	; degrees per unbinned spatial pixel
;
; slice scale
	slscl:0.00075437d0, $	; degrees per slice
;
; IFU rotator offset
	rotoff:0.0, $		; degrees from rotator PA
;
; CCD binning
	xbinsize:1, $		; binning in x (pixels)
	ybinsize:1, $		; binning in y (pixels)
;
; CCD size
	nx:0, $			; number of x pixels in image
	ny:0, $			; number of y pixels in image
;
; CCD readnoise
	rdnoise:3., $		; e-/pixel
;
; minimum pixels for arc finding
	minpix:6, $		; minimum number of illuminated pix in arc col
;
; slice angles
	angles:fltarr(24), $	; angle in degrees of each slice image
;
; ref slice for x offsets
	refslice:11, $		; reference slice (0 - 23, default is 11)
;
; x offsets
	xoff:fltarr(24), $	; x offset as determined from bars image
;
; starting x position of arc slice image
	x0:fltarr(24), $	; starting x pos in image of each slice (pixels)
;
; ending x position of arc slice image
	x1:fltarr(24), $	; ending x pos in image of each slice (pixels)
;
; starting y position of arc slice image
	y0:fltarr(24), $	; starting y pos in image of each slice (pixels)
;
; ending y position of arc slice image
	y1:fltarr(24), $	; ending y pos in image of each slice (pixels)
;
; width of arc slice image
	wid:fltarr(24), $	; width of arc slice image in pixels
;
; do we fit with a gaussian (or use double erf?)
	do_gauss:0, $		; 0 - no, 1 - yes
;
; Status
	initialized:0, $	; 0 - no, 1 - yes
	status:-1, $		; 0 - good fit, else not good
	progid:'', $		; program that last modified the dgeom file
;
; timestamp
	timestamp:0.d0 $	; timestamp for struct (unix seconds)
	}
end

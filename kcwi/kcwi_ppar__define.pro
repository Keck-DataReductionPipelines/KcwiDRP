;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_PPAR__DEFINE
;
; PURPOSE:
;	This procedure defines the structure containing all the parameters
;	for the KCWI Data Reduction Pipeline.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	kpp = {KCWI_PPAR}
;
; INPUTS:
;	None
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	defines the KCWI_PPAR data structure
;
; PROCEDURE:
;	Provide the automatic structure definition for the KCWI pipeline
;	parameter structure KCWI_PPAR.
;
; EXAMPLE:
;	Generate a Ppfile called 'm82.ppar' for the red side images
;	in directory 'night1' and put it in 'night1/redux':
;
;	KCWI_INIT,'M82.ppar','night1','night1/redux','red*.fit*',ppar=KPP
;
;	KPP will be initialized to have the correct pipeline parameters for
;	the inputs to KCWI_INIT.
;
; NOTES:
;	Keep structure name on a line by itself (see struct_init.pro).
;	Keep tag names 15 chars or less.
;	A tag name with (rangelist) in the comments is a range list with
;	dashes separating the limits of the range and commas separating
;	individual range lists.  The 'x' character can be used to exclude
;	a number from the range.  Examples:
;		'1234-1244'		from 1234 to 1244 (eleven numbers)
;		'1234-1244x1240'	same but excluding 1240 (ten numbers)
;		'1234-1244,1334-1344'	twenty-two numbers
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-APR-10	Initial version
;	2013-MAY-16	Removed bias section tags (not needed)
;	2014-MAR-21	Added cleancoeffs switch (default - 1, on)
;	2014-APR-05	Added froot and fdigits tags
;-
pro kcwi_ppar__define
;
tmp = { kcwi_ppar, $
;
; output filename
	ppfname:'kcwi.ppar', $	; pipeline parameter set name, e.g. 'm81.ppar'
	prfname:'kcwi.proc', $	; processing control file name, e.g. 'm81.proc'
;
; processing directories
	rawdir:'./', $		; directory containing raw images
	reddir:'./redux/', $	; directory to contain reduced images
	caldir:'./calib/', $	; directory containing calibration images
	datdir:'./', $		; directory containing ancillary data
	curdir:'./', $		; current directory
;
; processing files
	filespec:'image????.fit*', $	; input file spec
	froot:'kcwi', $		; filename root string
	fdigits:5, $		; number of digits in image numbers
;
; processing switches
	crzap:1, $		; remove cosmic rays?
	nassub:1, $		; perform nod-and-shuffle subtraction?
	saveintims:0, $		; save intermediate images?
	includetest:0, $	; include test ims in processing?
	clobber:0, $		; overwrite existing output images?
	verbose:0, $		; extra output?
	display:0, $		; display diagnostic plots?
	saveplots:1, $		; save diagnostic plots?
	cleancoeffs:1, $	; clean wavelength coeffs of errant bars?
	waveiter:0, $		; use iterative method to fit wavelengths?
;
; Bias processing
	biasskip1:0, $		; skip first bias frame? (for CWI)
	mingroupbias:5, $	; minimum number of bias frames per group
	masterbias:'', $	; master bias filename
	biases:'', $		; rangelist of bias image numbers
	readnoise:3.0, $	; default readnoise in e-
	nbgrps:0, $		; number of bias groups
;
; Overscan subtraction
	minoscanpix:70,$	; minimum overscan pixels required
	oscanbuf:20, $		; overscan edge buffer
;
; Dark frame subtraction
	mingroupdark:3, $	; minimum number of dark frames per group
	masterdark:'', $	; master dark filename
	darks:'', $		; rangelist of dark image numbers
	ndgrps:0, $		; number of dark groups
;
; Flat processing
	masterflat:'', $	; master flat filename
	cflats:'', $		; rangelist of cont flat image numbers
	nfgrps:0, $		; number of flat groups
;
; Geometry processing
	geomcbar:'', $		; master continuum bars filename
	geomarc:'', $		; master arc filename
	nggrps:0, $		; number of geom groups
;
; Direct mode processing
	dgeombar:'', $		; master direct image arc filename
	dgeomarc:'', $		; master direct image arc filename
	ndggrps:0, $		; number of direct geom groups
;
; Wavelength processing
	taperfrac:0.2, $	; cosine bell taper fraction for x-correlation
	pkdel:2.0, $		; match thresh in fraction of resolution
	atlas:'', $		; wavelength atlas
	atlasname:'', $		; wavelength atlas name
;
; Override header RA, Dec reference pixels
	crpix1:-1., $		; RA reference spatial pixel
	crpix2:-1., $		; DEC reference slice pixel
	crpix3:-1., $		; Wavelength reference pixel
;
; Slice profile processing
	slicex0:30, $		; safe lower spatial limit for slices (unbin px)
	slicex1:144, $		; safe upper spatial limit for slices (unbin px)
	psfwid:30, $		; nominal window for point src. (unbin px)
	masterprof:'', $	; master profile image filename
;
; Sky processing
	mastersky:'', $		; master sky image filename
;
; Relative response processing
	refslice:11, $		; reference slice
	masterrr:'', $		; master relative response filename
;
; Standard star calibration
	masterstd:'', $		; master standard star filename
;
; Output data cube overrides - use these to override default values
	wave0:-9., $		; wavelength minimum in Angstroms
	wave1:-9., $		; wavelength maximum in Angstroms
	dw:-9., $		; wavelength step in Angstroms/pixel
;
; ppar state
	initialized:0, $	; initialized? 0 - no, 1 - yes
	progid:'', $		; program that created the ppar file
;
; logging
	loglun:-1, $		; logical unit of log file (set in program)
;
; timestamp
	timestamp:0.d0 $	; timestamp for struct (unix seconds)
	}
end

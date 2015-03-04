; $Id: kcwi_ppar__define.pro,v 1.51 2015/02/21 00:18:39 neill Exp $
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
	lnfname:'kcwi.link', $	; processing links file name, e.g. 'm81.link'
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
	froot:'image', $	; filename root string
	fdigits:4, $		; number of digits in image numbers
;
; Image numbers
	npims:0, $		; number of primary images
	imnum:'', $		; object image numbers (rangelist)
;
; processing switches
	biasskip1:0, $		; skip first bias frame? (for CWI)
	crzap:1, $		; remove cosmic rays?
	nassub:1, $		; perform nod-and-shuffle subtraction?
	saveintims:0, $		; save intermediate images?
	includetest:0, $	; include test ims in processing?
	clobber:0, $		; overwrite existing output images?
	verbose:0, $		; extra output?
	display:0, $		; display diagnostic plots?
	saveplots:1, $		; save diagnostic plots?
	cleancoeffs:1, $	; clean wavelength coeffs of errant bars?
;
; Bias processing
	mingroupbias:5, $	; minimum number of bias frames per group
	biasexists:0, $		; bias frames exists?
	masterbias:'', $	; master bias filename
	biases:'', $		; List of bias images (rangelist)
	nbgrps:0, $		; number of bias groups
	readnoise:3.0, $	; default readnoise in e-
;
; Overscan subtraction
	minoscanpix:70,$	; minimum overscan pixels required
	oscanbuf:20, $		; overscan edge buffer
;
; Dark frame subtraction
	mingroupdark:3, $	; minimum number of dark frames per group
	darkexists:0, $		; dark frame exists?
	masterdark:'', $	; master dark filename
	darks:'', $		; List of dark images (rangelist)
	ndgrps:0, $		; number of dark groups
;
; Flat processing
	flatexists:0, $		; flat frame exists?
	masterflat:'', $	; master flat filename
	cflats:'', $		; List of continuum flat images (rangelist)
	nfgrps:0, $		; number of flat groups
;
; Geometry processing
	geomexists:0, $		; geometry frames exist?
	ncbars:0, $		; number of continuum bars images
	cbars:'', $		; List of continuum bars images (rangelist)
;
; Wavelength processing
	narcs:0, $		; number of arc images
	arcs:'', $		; List of arc images (rangelist)
	arcbars:'', $		; List of arc bars images (rangelist)
	pksig:1.5, $		; significance of peaks to find in atlas
	pkdel:0.75, $		; how close to match arc and atlas lines (Ang)
	pkiso:2.5, $		; isolation of peaks in arcs (Ang)
	atlas:'thar.fits', $	; wavelength atlas
	atlasname:'ThAr', $	; wavelength atlas name
	linelist:'thar_list.txt', $	; wavelength line list
;
; Override header RA, Dec reference pixels
	crpix1:-1., $		; RA reference spatial pixel
	crpix2:-1., $		; DEC reference slice pixel
	crpix3:-1., $		; Wavelength reference pixel
;
; Slice profile processing
	slicex0:15, $		; safe lower spatial limit for slices
	slicex1:72, $		; safe upper spatial limit for slices
	profexists:0, $		; slice profile frame exists?
	nprofs:0, $		; number of slice profile images
	profs:'', $		; List of slice profile images
;
; Relative response processing
	refslice:11, $		; reference slice
	rrexists:0, $		; relative response frame exists?
	masterrr:'', $		; master relative response filename
	nrgrps:0, $		; number of relative response groups
	nrrs:0, $		; number of relative response images
	rrs:'', $		; List of relative response images (rangelist)
;
; Output data cube overrides - use these to override default values
	wave0:-9., $		; wavelength minimum in Angstroms
	wave1:-9., $		; wavelength maximum in Angstroms
	dw:-9., $		; wavelength step in Angstroms/pixel
;
; Sky processing
	nskys:0, $		; number of sky images
	skyexists:0, $		; sky observation exists?
	skys:'', $		; List of sky observations (rangelist)
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

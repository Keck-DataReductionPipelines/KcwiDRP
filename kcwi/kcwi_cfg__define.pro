; $Id$
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_CFG__DEFINE
;
; PURPOSE:
;	This procedure defines the structure for all the observation
;	information for KCWI raw data read from the raw headers.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	kcfg = {KCWI_CFG}
;
; INPUTS:
;	None
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	defines the KCWI_CFG data structure
;
; PROCEDURE:
;	Provide the automatic structure definition for the KCWI raw header
;	observation structure KCWI_CFG.
;
; EXAMPLE:
;	Instantiate and initialize a single cfg struct for KCWI:
;
;	kcfg = {kcwi_cfg}
;	kcfg = struct_init(kcfg)
;
; NOTES:
;	Keep structure name on a line by itself (see struct_init.pro).
;	Keep tag names 15 chars or less.
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-03	Initial version
;-
pro kcwi_cfg__define
;
tmp = { kcwi_cfg, $
;
; Observation properties
	observer:'', $		; observer
	telescop:'', $		; telescope
	instrume:'', $		; instrument
	object:'', $		; object name
	datepclr:'', $		; UT start of observation (YYYY-MM-DDTHH:MM:SS)
	daterend:'', $		; UT end of observation (YYYY-MM-DDTHH:MM:SS)
	ra:-99.d0, $		; RA
	dec:-99.d0, $		; Dec
	epoch:-99.d0, $		; Coordinate epoch
	rotpa:-999.d0, $	; Rotator PA
	xposure:-9.0, $		; shutter open duration (s)
	telapse:-9.0, $		; dark current duration (s)
	airmass:-1.0, $		; Airmass

;
; Configuration properties
	caltype:'', $		; calibration type: bias, dark, arc, etc.
	frameno:0l, $		; image number
	skyobs:0, $		; sky observation? 0 - object, 1 - sky
	shuffmod:0, $		; is this a Nod & Shuffle observation?
	bgratnam:'', $		; Blue grating id
	bgratnum:0, $		; Blue graing number
	bgrangle:0., $		; Blue grating angle (degrees)
	bgrenc:0l, $		; Blue grating rotator encoder steps
	bfiltnam:'', $		; Blue filter id
	bfiltnum:0, $		; Blue filter number
	bartang:0., $		; Blue articulation angle
	bartenc:0l, $		; Blue cmaera articulation encoder steps
	bfocpos:0l, $		; Blue focus stage encoder steps
	bfocus:0., $		; Blue focus (mm)
	bnasname:'', $		; Blue mask position name
	bnaspos:0, $		; Blue mask position
	ifunum:0, $		; Slicer number (0-5, -1=unknown)
	ifunam:'', $		; Slicer name ("Small", etc.)
	cwave:0., $		; central wavelength (Ang)
	gratanom:0., $		; grating angle anomoly (degrees)
	wave0:0., $		; blue end  of wavelength range (Ang)
	wave1:0., $		; red end of wavelength range (Ang)
	dwav:0., $		; average dispersion (Ang/pix)
;
; CCD properties
	biasrn1:3., $		; bias read noise for amp 1 in electrons/pixel
	biasrn2:3., $		; bias read noise for amp 2 in electrons/pixel
	biasrn3:3., $		; bias read noise for amp 3 in electrons/pixel
	biasrn4:3., $		; bias read noise for amp 4 in electrons/pixel
	gain1:0.145, $		; gain for amp 1 in electrons/DN
	gain2:0.145, $		; gain for amp 2 in electrons/DN
	gain3:0.145, $		; gain for amp 3 in electrons/DN
	gain4:0.145, $		; gain for amp 4 in electrons/DN
	nampsxy:'1 1', $	; number of amplifiers in x and y
	ampmode:'LL', $		; amplifier mode: LL,LR,UL,UR,DUP,DLO,QUAD
	nvidinp:0, $		; number of amps (video inputs)
;
; Image geometry properties
	naxis:0, $		; number of data axes
	naxis1:0, $		; length of data axis 1
	naxis2:0, $		; length of data axis 2
;
; Nod-and-shuffle rows (in trimmed image)
	nsskyr0:0, $		; Sky region row 0 (bottom, pix)
	nsskyr1:0, $		; Sky region row 1 (top, pix)
	nsobjr0:0, $		; Object region row 0 (bottom, pix)
	nsobjr1:0, $		; Object region row 1 (top, pix)
;
; Wavelength values for reduced data
	crval3:0., $		; wavelength zeropoint
	crpix3: 0., $		; wavelength reference pixel
	cdelt3: 0., $		; wavelength dispersion Ang/px
;
; Derived properties
	juliandate:0.d0, $	; Julian date of observation
	date:'', $		; UT start of observation (YYYY-MM-DDTHH:MM:SS)
	binning:0, $		; is image binned?
	xbinsize:1, $		; binning in x
	ybinsize:1, $		; binning in y
;
; Input file properties
	obsfname:'', $		; input observation FITS file name (sans dir)
	obsdir:'', $		; input directory for observation
	obstype:'', $		; observation type: 'zero', 'cal', 'obj', 'std'
	imgnum:0L, $		; image number
;
; Instrument state
	imgtype:'', $		; observation type: bias, dark, arc, etc.
	gratid:'', $		; Grating id name
	gratnum:-1, $		; Gratind id number
	grangle:0., $		; Grating angle (degrees)
	grenc:0l, $		; Grating rotator encoder steps
	filter:'', $		; Filter id name
	filtnum:-1, $		; filter id number
	camang:0., $		; Camera articulation angle (degrees)
	campos:0l, $		; Camera articulation encoder steps
	focpos:0l, $		; Camera focus encoder steps
	focus:0., $		; Camera focus (mm)
	nasmask:0, $		; is the Nod & Shuffle mask deployed?
;
; Master group properties
	groupnum:-1l, $		; group image number
	nimages:0, $		; number of images in group
	grouplist:'', $		; range list of images in group
	groupfile:'', $		; filename of grouped image
	grouppar:'', $		; parameter file for grouped image
;
; Status
	initialized:0, $	; 0 - no, 1 - yes
;
; timestamp
	timestamp:0.d0 $	; timestamp for struct (unix seconds)
	}
end

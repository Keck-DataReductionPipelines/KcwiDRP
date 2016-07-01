; $Id$
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_CENTRAL_WAVE
;
; PURPOSE:
;	Given the grating installed in KCWI and camera articulation
;	angle and grating angle, return the approximate central 
;	wavelength falling onto the PCWI CCD.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_CENTRAL_WAVE(GratingNum, GratingAngle, CameraAngle)
;
; INPUTS:
;       GratingNum	- KCWI grating number (1 - 5)
;	GratingAngle	- Grating angle in degrees (0 - 360)
;	CameraAngle	- Camera articulation angle (-5 - 105)
;
; KEYWORDS:
;       pwave	- the peak wavelength in angstroms
;
; RETURNS:
;	Approximate central wavelength on the PCWI CCD. Returns a negative 
;	value if the program encountered an error.
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;	Uses formulae provided by Mat Matuszewski to calculate central and
;	peak wavelengths.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Matt Matuszewski (matmat@caltech.edu)
;	2013-AUG-12	Initial version
;	2014-AUG-14	Added Yellow grating
;	2014-SEP-10	Added MEDREZ (Richardson) grating
;	2016-JUN-30	Kludged from pcwi_central_wave
;-
function kcwi_central_wave, gid, grangle, camang, pwave=pwave, $
                           help=help

pre = 'KCWI_CENTRAL_WAVE'
wave = -1.

if n_params(0) lt 3 or keyword_set(help) then begin
	print,pre+': Info - Usage: Wave = '+pre+'( GratNum, GratAngle, CamAngle)'
        print,pre+': Info - GratNum is 1-BH3, 2-BL, 3-BH2, 4-BM, 5-BH1'
        print,pre+': Info - GratAngle is the grating angle in degrees'
        print,pre+': Info - CamAngle is the camera articulation angle in degrees'
        print,pre+': Info - wavelength returned in Angstroms'
	return,wave
endif

case gid of 
	1: begin	; BH3
		corang = 180.0
		rho = 2.8017
		d0 = 7.714e-2
		d1 = 1.344e-4
	end
	2: begin	; BL
		corang = 0.0
		rho = 0.870
		d0 = 9.392e-2
		d1 = 3.896e-5
	end
	3: begin	; BH2
		corang = 180.0
		rho = 3.2805
		d0 = 8.721e-2
		d1 = 1.560e-4
	end
	4: begin	; BM
		corang = 0.0
		rho = 1.901
		d0 = 8.315e-2
		d1 = 9.123e-5
	end
	5: begin	; BH1
		corang = 180.0
		rho = 3.3
		d0 = 1.e-2
		d1 = 1.e-4
	end
	ELSE: begin
		print,pre+': Error - Unrecognized grating requested: '+ $
			strn(gid)+'. Returing -1.'
		return,wave
	end; else
endcase

alpha = grangle - (corang + 13.0)
beta = camang - alpha

pwave = ( sin( alpha*!DTOR ) - d0 ) / d1
cwave = ( ( sin( alpha*!DTOR ) + sin( beta*!DTOR ) ) / rho ) * 10000.
        
return,cwave
end

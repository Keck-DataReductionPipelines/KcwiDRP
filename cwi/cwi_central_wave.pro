; $Id: cwi_central_wave.pro,v 1.6 2014/09/10 21:13:34 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	CWI_CENTRAL_WAVE
;
; PURPOSE:
;	Given the grating installed in CWI and camera articulation
;       reading (RP) return the approximate central wavelength falling
;       onto the CWI CCD.
;	
;
; CATEGORY:
;	Data reduction for the Cosmic Web Imager (CWI).
;
; CALLING SEQUENCE:
;	Result = CWI_CENTRAL_WAVE(Grating,Stage_Position,/DISPLAY)
;
; INPUTS:
;       Grating		- String value identifying the installed CWI grating, 
;				either "RED", "YELLOW", or "BLUE"
;       Stage_Position	- Integer value indicating the CWI
;				articulation stage position (RP)
;
; KEYWORDS:
;       DISPLAY		- display the curve fit and the estimated location.
;
; RETURNS:
;	Approximate central wavelength on the CWI CCD. Returns a negative 
;	value if the program encountered an error.
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;	Uses pretabulated wavelength and stage position data interpolated 
;	with a second degree polynomial to compute the approximate central 
;	wavelength of the setting. 
;
; EXAMPLE:
;
;      wave = cwi_central_wave("blue",1000000)
;
; MODIFICATION HISTORY:
;	Written by:	Matt Matuszewski (matmat@caltech.edu)
;	2013-AUG-12	Initial version
;	2014-AUG-14	Added Yellow grating
;	2014-SEP-10	Added MEDREZ (Richardson) grating
;-
function cwi_central_wave, grating, stage_position, grat_position, $
                           display = display, $
                           help=help

pre = 'CWI_CENTRAL_WAVE'
wave = -1.

if n_params(0) lt 2 or keyword_set(help) then begin
	print,pre+': Info - Usage: Wave = '+pre+'( Grating, Stage_Position, Grating_Position )'
        print,pre+': Info - Grating is string "BLUE", "YELLOW", "RED", or "MEDREZ"'
        print,pre+': Info - Stage_Position is CAMPOS encoder keyword'
        print,pre+': Info - Grating_Position is GRATPOS encoder keyword'
        print,pre+': Info - wavelength returned in Angstroms'
	return,wave
endif

; convert to uppercase
grat = strtrim(strupcase(grating),2)

; make sure position is float
rp = stage_position * 1.0

; plot setups
xrng = [-1700000., 550000.]

case grat of 
	"BLUE":begin
		waves=[5527., 5207., 5005., 4984., 4935., 4862., 4680.]
		rps=[-1300000., -700000., -350000., -315000., -235000., $
			-115000., 180000.]
	end
	"YELLOW":begin
		waves=[ 6481., 6320., 6150., 5968., 5785., 5590.0 ]
		rps=[ -1000000., -750000., -500000., -250000., 0., 250000.]
	end
	"RED":begin
		waves=[7045., 6895., 6740., 6640., 6562., 6500.]
		rps=[-350000., -180000., 0, 116000., 180000., 250000.]
	end
	"MEDREZ":begin
		waves=[4950., 5400., 5900., 6300., 6580.]
		rps=[-242500., -246500., -250500, -254500., -256500.]
		rp = grat_position * 1.0
		xrng = [-240000.,-260000.]
	end
	ELSE: begin
		print,pre+': Error - Unrecognized grating requested: '+ $
			grating+'. Returing -1.'
		return,wave
	end; else
endcase
        
; generate the fit
fit = poly_fit(rps,waves,2)

; estimate the central wave
wave = fit[0]+fit[1]*rp+fit[2]*rp^2
        
; was there a request for the display?
if keyword_set(display) then begin
	rpp = indgen(10000)*(550000.0+1700000)/10000.0-1700000.0
	wv = fit[0]+fit[1]*rpp+fit[2]*rpp^2
	window,0,xsize=800,ysize=600,xpos=50,ypos=300
	plot,rpp,wv,xrange=xrng,/ys,xtitle="RP",ytitle="A", $
		linestyle=0,charsize=1.5,/xs
	oplot,rps,waves,psym=1,symsize=1.5,color=1000,thick=2.0
	plots,rp,wave,psym=2,symsize=1.5,color=65000,thick=2.0
endif; display

return,wave
end

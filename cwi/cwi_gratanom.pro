; $Id: cwi_gratanom.pro,v 1.4 2014/09/10 15:42:44 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights reserved
;+
; NAME:
;       CWI_GRATANOM
;
; PURPOSE:
;       Given a grating, a grating position, and a wavelength returns the 
;	tilt in degrees from the nominal grating angle.
;
; CATEGORY:
;	Data reduction for the Cosmic Web Imager (CWI).
;
; CALLING SEQUENCE:
;       Result = CWI_GRATANOM(Grating, Grating_Position, Central_Wavelength)
;
; INPUTS:
;	Grating		- String value identifying the installed CWI grating,
;				either "RED", "YELLOW", or "BLUE"
;	Grating_Position - longint value of the grating encoder
;	Central_Wavelength - float value of the calculated central wavelength,
;				from cwi_central_wave.pro
;
; RETURNS:
;	Grating anomoly in degrees.
;
; EXAMPLE:
;	IDL> print,cwi_gratanom('BLUE',5470,4983.0)
;		 -2.5059096
;	IDL> print,cwi_gratanom('BLUE',-3005,5104.0)
;		0.013733995
;
; MODIFICATION HISTORY:
;	Written by:	Matt Matuszewski (matmat@caltech.edu)
;	2014-AUG-15	Shamelessly kludged from cwi_setting.pro by
;			J.D. Neill (neill@srl.caltech.edu)
;       2014-AUG-24     Added yellow grating values (MM)
;- 
function cwi_gratanom,gratid,gratpos,wavelength
;
; setup
pre='CWI_GRATANOM'
version = repstr('$Revision: 1.4 $ $Date: 2014/09/10 15:42:44 $','$','')
;
; the fits are for angstrom/1000.0
wv = wavelength/1000.0d
;
; articulation stage limits.
pstagelimit = 290000.0
mstagelimit = -1700000.0
;
; blue grating data. Valid on 131118
bluwv = [ 4790., 5005., 5207., 5527.]/1000.0
blugra = [6000.,0000.,-6000.,-14500.]
blucam = [0.,-350000.,-700000.,-1300000.]
;
; yellow grating data. Valid 140823 (MM)
yelwv = [ 6481., 6320., 6150., 5968., 5785.0, 5590.0 ]/1000.0
yelgra = [-15000.0, -10000.0, -7000.0, -2500.0, 2000.0, 6200.0]
yelcam = [-1000000.0, -750000.0, -500000.0, -250000.0, 0.0, 250000.0]
;
; red grating data. Valid on 131118
redwv = [6500., 6562, 6640, 6740, 6895, 7045]/1000.0
redgra = [8000., 5500, 4400, 3000, 0, -3000]
redcam = [250000., 180000, 116000, 0, -180000, -350000]
;
; Process blue grating. 
bcamfit = poly_fit(bluwv,blucam,2)
bgrafit = poly_fit(bluwv,blugra,2)

bcam = bcamfit[0] + bcamfit[1]*wv + bcamfit[2]*wv^2.0
bgra = bgrafit[0] + bgrafit[1]*wv + bgrafit[2]*wv^2.0
;
; Process yellow grating. 
ycamfit = poly_fit(yelwv,yelcam,2)
ygrafit = poly_fit(yelwv,yelgra,2)

ycam = ycamfit[0] + ycamfit[1]*wv + ycamfit[2]*wv^2.0
ygra = ygrafit[0] + ygrafit[1]*wv + ygrafit[2]*wv^2.0
;
; Process red grating. 
rcamfit = poly_fit(redwv,redcam,2)
rgrafit = poly_fit(redwv,redgra,2)

rcam = rcamfit[0] + rcamfit[1]*wv + rcamfit[2]*wv^2.0
rgra = rgrafit[0] + rgrafit[1]*wv + rgrafit[2]*wv^2.0
;
; get appropriate anomoly assuming roughly 2000 cnts/degree
anom = 0.
case gratid of
	"BLUE"  : anom = (gratpos - bgra)/2000.
	"YELLOW": anom = (gratpos - ygra)/2000.
	"RED"   : anom = (gratpos - rgra)/2000.
	ELSE	: anom = 0.
endcase

return,anom
end; cwi_gratanom

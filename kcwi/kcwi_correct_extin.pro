; $Id: kcwi_correct_extin.pro,v 1.2 2015/02/07 17:05:04 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_CORRECT_EXTIN
;
; PURPOSE:
;	This procedure corrects for atmospheric extinction
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_CORRECT_EXTIN, Img, Hdr, Ppar
;
; INPUTS:
;	Img	- Data cube with wavelength as third axis
;	Hdr	- Header containing airmass and wavelength params
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; KEYWORDS:
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Corrects the input data cube in place.
;
; PROCEDURE:
;	Reads airmass, telescope, and wavelength parameters from header.
;	Based on telescope, selects appropriate coefficients and resamples
;	and scales based on airmass.  Converts to a ratio and applies to cube.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-SEP-23	Initial version
;-
pro kcwi_correct_extin,img,hdr,ppar
;
; setup
pre = 'KCWI_CORRECT_EXTIN'
version = repstr('$Revision: 1.2 $ $Date: 2015/02/07 17:05:04 $','$','')
;
; get airmass
air = sxpar(hdr,'airmass')
;
; check instrument
telescope = sxpar(hdr,'telescop')
;
; get Keck extinction coefficients
if strpos(telescope,'Keck') ge 0 then begin
;
; get Palomar extinction coefficients (Hayes & Latham, 1975)
; units are:
;	exwl - wavelengths in Angstroms
;	exma - extinction in magnitudes per airmass
endif else if strpos(telescope,'Hale5m') ge 0 then begin
	exwl = [3200., 3250., 3300., 3350., 3390., 3448., 3509., 3571., 3636., $
		3704., 3862., 4036., 4167., 4255., 4464., 4566., 4785., 5000., $
		5263., 5556., 5840., 6055., 6435., 6790., 7100., 7550., 7780., $
		8090., 8370., 8708., 9832., 10255., 10610., 10795., 10870.]
	exma = [1.058, 0.911, 0.826, 0.757, 0.719, 0.663, 0.617, 0.575, 0.537, $
		0.500, 0.428, 0.364, 0.325, 0.302, 0.256, 0.238, 0.206, 0.183, $
		0.164, 0.151, 0.140, 0.133, 0.104, 0.084, 0.071, 0.061, 0.055, $
		0.051, 0.048, 0.044, 0.036,  0.034,  0.032,  0.032,  0.031]
endif else begin
	kcwi_print_info,ppar,pre,'No extinction curve for telescope', $
		telescope,format='(a,1x,a)',/warning
endelse
;
; get object wavelengths
sz = size(img,/dim)
owls = findgen(sz[2]) * sxpar(hdr,'cd3_3') + sxpar(hdr,'crval3')
;
; resample extinction curve
linterp,exwl,exma,owls,oexma
;
; convert to flux ratio
flxr = 10.^(oexma*air*0.4)
;
; apply to data cube
for ix=0,sz[0]-1l do for iy=0,sz[1]-1l do $
	img[ix,iy,*] = img[ix,iy,*] * flxr
;
; average flux ratio
flrmo = moment(flxr)
;
; update header
sxaddpar,hdr,'COMMENT','  '+pre+' '+version
sxaddpar,hdr,'EXTCOR','T',' extinction corrected?'
sxaddpar,hdr,'AVEXCOR',flrmo[0],' average extin. correction (flux ratio)'
;
return
end	; kcwi_correct_extin

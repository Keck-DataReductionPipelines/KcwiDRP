;
; Copyright (c) 2018, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SKY_TWEAK
;
; PURPOSE:
;	This procedure generates allows the user to interactively tweak
;	the sky scaling.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SKY_TWEAK, Ppar, Img, Sky, ImgHdr, SkyHdr, Scl
;
; INPUTS:
;	Ppar	- KCWI_PPAR struct
;	Img	- Object image to sky subtract
;	Sky	- Sky model image
;	ImgHdr	- Header for object image
;	SkyHdr	- Header for sky image
;
; KEYWORDS:
;
; OUTPUTS:
;	Scl	- Tweak scaling factor for sky
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2018-JAN-19	Initial version
;-
pro kcwi_sky_tweak, ppar, img, sky, ihdr, shdr, scl
;
; setup
pre = 'KCWI_SKY_TWEAK'
q = ''
;
; get default scale
obtime = sxpar(ihdr,'XPOSURE')
sktime = sxpar(shdr,'XPOSURE')
;
; verify exposure times
if obtime le 0. or sktime le 0. then begin
	kcwi_print_info,ppar,pre,'bad exposure times (obj, sky)', $
		obtime, sktime, format='(a,2f9.2)'
	scl = 1.0
endif	else	scl = obtime / sktime
print,''
print,'Current scale value: ',scl
;
; initial correction
dimg = img - sky * scl
;
; number string test
st = '^[-+]?([0-9]+\.?[0-9]*|\.[0-9]+)([eEdD][-+]?[0-9]+)?$'
;
; loop until done
while q ne 'Q' do begin
	!quiet=1
	print,'Examine sky-subtracted image and then'
	print,'quit kctv to enter new scale value.'
	kctv,dimg,header=ihdr,/block
	!quiet=0
	read,'new scale value (q - quit): ',q
	if stregex(strtrim(q,2),st,/bool) then begin
		scl = float(strtrim(q,2))
		dimg = img - sky * scl
	endif else $
		q = strupcase(strtrim(q,2))
endwhile
;
return
end

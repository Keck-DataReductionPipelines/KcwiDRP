; $Id: kcwi_parse_dates.pro,v 1.1 2013/05/02 18:53:26 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_PARSE_DATES
;
; PURPOSE:
;	This function reads the array of header date values and returns
;	an array of julian dates.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_PARSE_DATES( HDR_DATES )
;
; INPUTS:
;	hdr_dates - a string array of dates of form YYYY-MM-DDTHH:MM:SS.SSS
;
; KEYWORDS:
;	VERBOSE - set this to get extra screen output
;
; RETURNS:
;	a double array of julian dates corresponding to the input dates.
;
; PROCEDURE:
;	Parses each date and derives the julian date.
;
; EXAMPLE:
;
;	read one header and get it's corresponding unix time:
;	hdr=headfits('image1234.fits')
;	date = sxpar(hdr,'DATE')
;	jd = kcwi_parse_dates(date)
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-02	Initial version
;-
function kcwi_parse_dates,hdr_dates,verbose=verbose
;
; initialize
	pre = 'KCWI_PARSE_DATES'
;
; set up output array
	nd = n_elements(hdr_dates)
	jds = dblarr(nd)
;
; loop over elements
	for i=0,nd-1 do begin
		;
		; parse each element
		it = hdr_dates[i]
		yr = fix(gettok(it,'-'))
		mo = fix(gettok(it,'-'))
		dy = fix(gettok(it,'T'))
		hr = fix(gettok(it,':'))
		mi = fix(gettok(it,':'))
		se = float(it)
		;
		; convert to julian date
		jds[i] = julday(mo, dy, yr, hr, mi, se)
	endfor
	;
	return,jds
end

pro radec_parse,irastr,idecstr,delim,rad,decd
;
; radec_parse	- Parse strings of ra and dec and convert to decimal dgrees.
;
; INPUTS:
;	irastr	- a string of ra in hours minutes seconds delimited by DELIM
;	idecstr	- a string of dec in deg minutes seconds delimted by DELIM
;	delim	- a one character delimiter for rastr and decstr
;
; OUTPUTS:
;	rad	- ra in decimal degrees
;	decd	- dec in decimal degrees
;
; Don Neill, Columbia University, June 27, 1996
;
; parse ra string
rastr = strtrim(strcompress(irastr),2)	; get rid of extra white space
rastr = strsplit(rastr,delim,/extract,count=n)	; separate into components
ra0_h = double(rastr(0))
if n gt 1 then $
	ra0_m = double(rastr(1)) $
else	ra0_m = 0.d0
if n gt 2 then $
	ra0_s = double(rastr(2)) $
else	ra0_s = 0.d0
;
; parse dec string
decstr = strtrim(strcompress(idecstr),2) ; get rid of extra white space
decstr = strsplit(decstr,delim,/extract,count=n) ; separate into components
;
; check for leading, space separated sign
if decstr(0) eq '-' or decstr(0) eq '+' then begin
	dec0_d = abs( double(decstr(1)) )
	if n gt 2 then $
		dec0_m = double(decstr(2)) $
	else	dec0_m = 0.d0
	if n gt 3 then $
		dec0_s = double(decstr(3)) $
	else	dec0_s = 0.d0
endif else begin
;
; no leading unattached sign
	dec0_d = abs( double(decstr(0)) )
	if n gt 1 then $
		dec0_m = double(decstr(1)) $
	else	dec0_m = 0.d0
	if n gt 2 then $
		dec0_s = double(decstr(2)) $
	else	dec0_s = 0.d0
endelse
;
; convert to decimal degrees
rad = 15.d0 * (ra0_h + ra0_m / 60.d0 + ra0_s / 3600.d0)
decd= dec0_d + dec0_m / 60.d0 + dec0_s / 3600.d0
;
; check for negative declinations
if ( strpos(string(decstr(0)),'-') ge 0 ) then decd = -decd
;
return
end	; radec_parse

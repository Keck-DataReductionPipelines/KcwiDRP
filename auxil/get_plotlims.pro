function get_plotlims,data,errs, $
	pad_fraction=pad_fraction,magnitudes=magnitudes,minzero=minzero, $
	log=log
;+
;
; get_plotlims - get range of values suitable for plots
;
; INPUTS:
;	data, errs 	- data and errors
;
; KEYWORDS:
;	pad_fraction	- fraction to pad the range with
;	magnitudes	- set to invert the range, appropriate for magnitudes
;	minzero		- set to use 0. as the first limit (not for magnitudes)
;	log		- set if axis will be displayed as logarithmic
;-
if keyword_set(pad_fraction) then $
	pf = pad_fraction $
else	pf = 0.05	; default 5% padding
;
; set errs to zero if not supplied
if n_params(0) lt 2 then $
	errs = data-data
;
xmax = max(data+errs,/nan)
xmin = min(data-errs,/nan)
rang = xmax - xmin
pad  = rang * pf
if keyword_set(log) then $
	pad = (alog10(xmax)-alog10(xmin))*pf
;
if keyword_set(magnitudes) then $
	lims = [xmax+pad,xmin-pad] $
else	begin
	if keyword_set(minzero) then $
		lims = [0.,xmax+pad] $
	else	lims = [xmin-pad,xmax+pad]
endelse
;
; log
if keyword_set(log) then $
	lims = [10.^(alog10(xmin)-pad), 10.^(alog10(xmax)+pad)]
;
return,lims
;
end

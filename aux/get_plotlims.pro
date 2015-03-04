function get_plotlims,data,errs, $
	pad_fraction=pad_fraction,magnitudes=magnitudes,minzero=minzero
;+
; $Id: get_plotlims.pro,v 1.3 2014/08/28 07:15:11 neill Exp $
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
;-
if keyword_set(pad_fraction) then $
	pf = pad_fraction $
else	pf = 0.05	; default 5% padding
;
; set errs to zero if not supplied
if n_params(0) lt 2 then $
	errs = data-data
;
xmax = max(data+errs)
xmin = min(data-errs)
rang = xmax - xmin
pad  = rang * pf
;
if keyword_set(magnitudes) then $
	lims = [xmax+pad,xmin-pad] $
else	begin
	if keyword_set(minzero) then $
		lims = [0.,xmax+pad] $
	else	lims = [xmin-pad,xmax+pad]
endelse
;
return,lims
;
end

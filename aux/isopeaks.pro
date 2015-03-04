;+
; NAME:
;      ISOPEAKS
;
;
; PURPOSE:
;      Find isolated peaks in a vector (spectrum) which lie
;      NSIG above the standard deviation of all peaks in
;      the spectrum
;
; CALLING SEQUENCE:
;      result = isopeaks(y, pkgap, nsig [,count=npk])
;
;
; INPUTS:
;      Y - Vector (usually a spectrum) in which you want to 
;          locate peaks.
;  PKGAP - Gap in pixels between neighbors
;   NSIG - Number of sigma above the standard deviation of 
;          all peaks to search.
;
; OUTPUTS:
;
; RESULT - Vector holding the indices of the peaks in Y
;
;
; OPTIONAL OUTPUTS:
;
;    NPK - The number of peaks located
;
; NOTES:
;
;    NSIG is NOT the number of sigma above the noise in the spectrum. 
;    It's instead a measure of the significance of a peak. First, all
;    peaks are located. Then the standard deviation of the peaks is 
;    calculated using ROBUST_SIGMA (see Goddard routines online). Then
;    peaks which are NSIG above the sigma of all peaks are selected.
; 
; EXAMPLE:
;
; IDL> y = randomn(seed,2000)
; IDL> pk = isopeaks(y,20.,2)
; IDL> plot,y
; IDL> oplot,pk,y[pk],ps=2
;
; MODIFICATION HISTORY:
; $Id: isopeaks.pro,v 1.1 2014/03/25 23:19:35 neill Exp $
;
;-
function isopeaks,y,pkgap,nsig,level=level,count=npk
on_error,2
;
; find all peaks
d0 = y - shift(y,1)
d1 = y - shift(y,-1)
pk = where(d0 gt 0 and d1 gt 0,npk)
;
; level cutoff takes precidence
if keyword_set(level) then begin
	good = where(y[pk] gt level, npk)
	pk = pk[good]
endif else begin
;
; sigma thresh
	if n_elements(nsig) gt 0 then begin
		yp = y[pk]
		mn = robust_mean(yp,4)
		sig = robust_sigma(yp)
		good = where(yp gt mn + nsig*sig, npk)
		if npk gt 0 then pk = pk[good] else pk = -1
	endif
endelse
;
; now get isolated peaks
valid = bytarr(npk)+1b
one = fltarr(npk)+1.0
;
rows = one##pk
cols = pk##one
;
diff = (rows-cols)
;
badpks = where(abs(diff) lt pkgap and abs(diff) gt 0., nbad)
;
if nbad gt 0 then valid[badpks mod npk] = 0b

good = where(valid eq 1, npk)

if npk gt 0 then pk = pk[good] else pk = -1

return,pk
end


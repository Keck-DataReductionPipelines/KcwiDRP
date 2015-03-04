;+
; NAME:
;      CLNPEAKS
;
;
; PURPOSE:
;      Find clean peaks in a vector (spectrum) which lie
;      NSIG above the standard deviation of all peaks in
;      the spectrum and are within one sigma of the average
;      width.
;
; CALLING SEQUENCE:
;      result = clnpeaks(x, y, del, pkgap, nsig [,count=npk])
;
;
; INPUTS:
;      X - Vector of wavelength-like values for Y
;      Y - Vector (usually a spectrum) in which you want to 
;          locate peaks.
;    DEL - expected width of lines in pixels
;  PKGAP - Gap in pixels between neighbors
;   NSIG - Number of sigma above the standard deviation of 
;          all peaks to search.
;
; OUTPUTS:
;
; RESULT - Vector holding the X values (wavelengths) of good peaks
;
; OPTIONAL INPUT KEYWORDS:
;
; ESTIMATE - set to the estimated width
;
; OPTIONAL OUTPUT KEYWORDS:
;
;    NPK - The number of peaks located
; NCLEANED - number of peaks cleaned by width
;
; NOTES:
;
;    NSIG is NOT the number of sigma above the noise in the spectrum. 
;    It's instead a measure of the significance of a peak. First, all
;    peaks are located. Then the standard deviation of the peaks is 
;    calculated using ROBUST_SIGMA (see Goddard routines online). Then
;    peaks which are NSIG above the sigma of all peaks are selected.
; 
; MODIFICATION HISTORY:
; $Id: clnpeaks.pro,v 1.2 2014/03/29 00:07:59 neill Exp $
;	2014-MAR-28	Added /ESTIMATE keyword
;
;-
function clnpeaks,x,y,del,pkgap,nsig, $
	level=level,count=npk,ncleaned=ncln,estimate=estimate
on_error,2
;
cen = -1.
npk = 0
ncln = 0
;
; check inputs
np = n_elements(y)
if n_elements(x) ne np then begin
	print,'ERROR: X and Y must be vectors of the same size'
	return,cen
endif
;
; estimate?
if keyword_set(estimate) then $
	esw = double(estimate) $
else	esw = 1.d
;
; get isolated peaks
pks = isopeaks(y,pkgap,nsig,level=level,count=npk)
;
; do we have peaks to fit?
if npk gt 0 then begin
	;
	; centroid and width
	cen = dblarr(npk)
	wid = dblarr(npk)
	;
	; loop over peaks
	for j=0,npk-1 do begin
		suby = y[pks[j]-del>0:pks[j]+del<np-1]
		subx = x[pks[j]-del>0:pks[j]+del<np-1]
		if keyword_set(estimate) then $
			yfit = mpfitpeak(subx,suby,a,nterms=5,/silent, $
				estimate=[y[pks[j]],x[pks[j]],esw]) $
		else 	yfit = mpfitpeak(subx,suby,a,nterms=5,/silent)
		cen[j] = a[1]
		wid[j] = a[2]
	endfor
	;
	; get distribution of widths
	mom = moment(wid)
	;
	; look for those within 1-sigma of mean
	good = where(abs(wid-mom[0])/sqrt(mom[1]) lt 1.0, ngood)
	ncln = npk - ngood
	;
	; extract the good ones
	if ngood gt 0 then begin
		npk = ngood
		cen = cen[good]
	;
	; oops! we cleaned 'em all!
	endif else begin
		npk = 0
		cen = -1.
	endelse
endif
;
return,cen
end

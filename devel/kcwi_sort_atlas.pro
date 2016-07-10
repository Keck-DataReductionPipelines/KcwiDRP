;+
; kcwi_sort_atlas - get good atlas lines and output list sorted by wavelength
;
; INPUTS:
;	w,f	- wavelenth, flux of atlas spectrum
;	atw	- wavelengths of atlas lines
;	wran	- two element vector giving wavelngth range of interest
;	res	- spectral resolution in Angstroms
;	nlines	- how many lines to search for
;
; OUTPUTS:
;	satw	- list of sorted atlas line wavelengths
;-
pro kcwi_sort_atlas,w,f,atw,wran,res,nlines,satw
	g = where(atw ge wran[0] and atw le wran[1], ng)
	if ng lt nlines then begin
		print,'not enough lines: ',ng
		return
	endif
	gatw = atw[g]
	fatw = fltarr(ng)
	for i=0,ng-1 do begin
		fin = (where(w ge gatw[i]))[0]
		fatw[i] = f[fin]
	endfor
	;
	s=(reverse(sort(fatw)))[0:(nlines-1)]
	satw = gatw[s]
	satw = satw(sort(satw))
	;
	for i=1,nlines-1 do begin
		if satw[i] gt 0 then begin
			if satw[i]-satw[i-1] lt 2.*res then begin
				satw[i] = -1.
				satw[i-1] = -1.
			endif
		endif
	endfor
	;
	good = where(satw gt 0.)
	satw = satw[good]
	;
	return
end

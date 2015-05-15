; $Id$
;
; Copyright (c) 2013, California Institute of Technology. All rights reserved.
;+
; NAME: KCWI_GET_LINES
;
; PURPOSE:
;	Get emission line centroids from input spectrum
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_GET_LINES, Wave, ArcSpec, Xcen
;
; INPUTS:
;	Wave	- wavelength vector for input spectrum
;	ArcSpec	- flux vector from calibration arc spectrum
;
; INPUT KEYWORDS:
;	WIDTH	- typical width of a single line in pixels
;	THRESHHOLD - flux threshhold, defaults to 3-sigma above background
;
; OUTPUTS:
;	Xcen	- Wavelength centroids of all lines above THRESHHOLD
;	Pkint	- Peak intensity for each line
;
; SIDE EFFECTS:
;	None.
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-26	Initial Revision
;	2013-NOV-21	Now sort list on pixel position
;	2013-DEC-15	Now checks each line to be sure it is in atlas list
;-
pro kcwi_get_lines,wave,spec,xcen,pkint,kgeom,watl,barno, $
	pxwindow=pxwindow,width=width,sigthresh=sigthresh
	;
	; initialize
	xcen = [-1.]
	pkint = [-1.]
	;
	; dimensions
	npix = n_elements(spec)
	pixpos = findgen(npix)
	w0 = poly(pixpos+kgeom.baroff[barno],kgeom.rbcoeffs)
	natl=n_elements(watl)
	;
	; width
	if keyword_set(width) then $
		wid = width $
	else	wid = 3
	;
	; window
	if keyword_set(pxwindow) then $
		pxwin = pxwindow $
	else	pxwin = 100
	;
	; get thresshold
	if keyword_set(sigthresh) then $
		sigth = sigthresh $
	else	sigth = 10.
	;
	; get sky, skysig
	mmm,spec,sky,skysig
	;
	; sometimes mmm fails
	if skysig lt 0. then $
		ims_asym,spec,sky,skysig,siglim=[2.5,3.0]
	thr = sky + skysig * sigth
	;
	; get ranges
	t=where(spec gt thr, nt)
	;
	; check
	if nt lt 5 then begin
		print,pre+': Error - threshhold too high, not enough lines available'
		return
	endif
	;
	; get packets
	rangepar,t,rstr
	sta = strsplit(rstr,',',/extract,count=np)
	;
	; loop over packets
	for i=0,np-1 do begin
		;
		; get packet indices
		rangepar,sta[i],pindx
		;
		; avoid the ends
		if min(pindx) gt wid+1 and $
		   max(pindx) lt npix-(wid+1) then begin
			;
			; get the peak within the line width
			mxint = max(spec[pindx])
			ipk   = (where(spec[pindx] eq mxint))[0] + min(pindx)
			smeas = spec[ipk-wid:ipk+wid]
			wmeas = wave[ipk-wid:ipk+wid]
			xx    = cntrd1d(wmeas,smeas)
			;
			; make sure it's an atlas line
			wxx   = poly(xx+kgeom.baroff[barno],kgeom.rbcoeffs)
			offs  = abs(watl - wxx)
			if min(offs) lt 1.5 then begin
				xcen  = [xcen, cntrd1d(wmeas,smeas)]
				pkint = [pkint, mxint]
			endif else begin
				xcen  = [xcen, -1.]
				pkint = [pkint, -1.]
			endelse
		endif
	endfor
	;
	; get rid of baddies
	good = where(xcen gt 0., ngood)
	if ngood gt 0 then begin
		xcen = xcen[good]
		pkint = pkint[good]
	endif else begin
		print,pre+': Error - no good centroids.'
		xcen = -1.
		pkint = -1.
		return
	endelse
	ncen = n_elements(xcen)
	;
	; sort on intensity
	srt   = reverse(sort(pkint))
	pkint = pkint[srt]
	xcen  =  xcen[srt]
	;
	; use status
	use = intarr(ncen) + 1
	;
	; now apply window
	for i=0,ncen-1 do begin
		t0 = xcen[i] - pxwin/2
		t1 = xcen[i] + pxwin/2
		test = where(xcen gt t0 and xcen lt t1, ntest)
		if max(pkint[test]) gt pkint[i] then begin
			use[i] = 0
		endif else begin
			if ntest gt 1 then begin
				rats = pkint[test]/pkint[i]
				t = where(rats lt 1. and rats gt 0.5, nrats)
				if nrats gt 0 then use[i] = 0
			endif
		endelse
	endfor
	;
	; get good lines
	good = where(use eq 1, ngood)
	if ngood gt 0 then begin
		xcen  = xcen[good]
		pkint = pkint[good]
		;
		; now sort on position
		spix  = sort(xcen)
		xcen  = xcen[spix]
		pkint = pkint[spix]
	endif else begin
		print,pre+': Error - no good centroids.'
		xcen  = -1.
		pkint = -1.
	endelse
	;
	return
end


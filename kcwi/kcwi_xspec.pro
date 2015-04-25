; $Id: kcwi_xspec.pro | Fri Apr 24 15:25:00 2015 -0700 | Don Neill  $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_XSPEC
;
; PURPOSE:
;	Cross-correlate two spectra and calculate pixel offset and peak value.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_XSPEC, Spec0, Spec1, Ppar, Offset, Value
;
; INPUTS:
;	Spec0, Spec1	- Spectra to cross-correlate (must have same size)
;	Ppar		- KCWI_PPAR pipeline paramter struct
;
; OUTPUTS:
;	Offset	- pixel offset as determined by cross-correlation
;	Value	- peak value of cross correlation
;
; INPUT KEYWORDS:
;	CENTRAL - set to fractional width of window w.r.t. spectra 
;			(def: 10. = 1/10th of spectra width)
;	MEDIAN	- set to subtract medians of spectra
;	MEAN	- set to subtract means of spectra
;	MIN	- set to subtract minima of spectra
;	SHIFT	- value to subtract from offset (def: 0)
;	NEIGHBORHOOD	- range around peak (def: 5)
;	PAD	- set to zero-pad spectra on each end by 5%
;	PLOT	- allow plotting if ppar.display higher than 2
;	LABEL	- label for plot
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Matt Matuszewski
;	2013-DEC-05	Initial Revision
;	2013-DEC-10	Matt updates
;	2014-MAR-28	DN - added CENTRAL keyword and associated code
;-
pro kcwi_xspec, spec0, spec1, ppar, offset, value, $
	central=central, median=median, mean=mean, min=min, shift=shift, $
	neighborhood = neighborhood, pad = pad, plot = plot, label = label, $
	status=status
;
; startup
pre = 'KCWI_XSPEC'
offset = !values.f_nan
value = !values.f_nan
status = 0
q=''
;
; check inputs
if kcwi_verify_ppar(ppar) ne 0 then $
	ppar = { kcwi_ppar }
;
; number of pixels in target spectrum
nsp = n_elements(spec0)
;
; do we match?
if nsp ne n_elements(spec1) then begin
	kcwi_print_info,ppar,pre, $
		'The spectra have unequal numbers of elements',/error
	status = 1
	return
endif
;
; check keywords
if keyword_set(mean) then begin
	mo0 = moment(spec0)
	mo1 = moment(spec1)
	mean0 = mo0[0]
	mean1 = mo1[0]
	spec0 = spec0 - mean0
	spec1 = spec1 - mean1
endif				; mean

if keyword_set(median) then begin
	med0 = median(spec0)
	med1 = median(spec1)
	spec0 = spec0 - med0
	spec1 = spec1 - med1
endif				; median

if keyword_set(min) then begin
	min0 = min(spec0) > 0.
	min1 = min(spec1) > 0.
	spec0 = spec0 - min0
	spec1 = spec1 - min1
endif				; min

if keyword_set(pad) then begin
	buffer = fltarr(n_elements(spec0)/20)*0.0
	spec0 = [buffer,spec0,buffer]
	spec1 = [buffer,spec1,buffer]
	nsp = n_elements(spec0)
endif				; pad

if keyword_set(neighborhood) then $
	neigh = neighborhood $
else	neigh = 5		; neighborhood
if neigh lt 2 then neigh = 5
  
if keyword_set(shift) then $
	shift = nsp/2 $
else	shift = 0		; shift

if keyword_set(central) then begin
	if central gt 1 then $	; central search window
		cent = central $
	else	cent = 10.
endif
;
; Fast Fourier
fft0 = fft(spec0,1)
fft1 = conj(fft(spec1,1))
;
; cross-correlation
xcor = fft(fft0*fft1,-1)
xcor = real_part(shift(xcor* conj(xcor),shift))
xvals = findgen(nsp)
;
; do we look for central peak?
if keyword_set(central) then begin
	;
	; find peaks in cross-correlation
	pks = peaks(xcor,3.,count=npk)
	;
	; what if we don't find any peaks?
	if npk lt 0 then begin
		;
		; this isn't good
		status = 1
		;
		; just take peak cross-correlation value
		value = sqrt(max(xcor,mi))
	;
	; got enough good peaks
	endif else begin
		;
		; find central peaks
		diff = abs(pks - (nsp/2.))
		cpks = where(diff lt nsp/cent,ncpks)
		;
		; do we have any central peaks?
		if ncpks le 0 then begin
			;
			; this isn't good
			status = 1
			;
			; just take peak cross-correlation value
			value = sqrt(max(xcor,mi))
		;
		; we have some central peaks
		endif else begin
			;
			; get largest
			pks = pks[cpks]
			t = (where(xcor[pks] eq max(xcor[pks])))[0]
			mi = pks[t]
			value = sqrt(xcor[mi])
		endelse	; got central peaks
	endelse	; got enough good peaks
;
; don't look for central peak
endif else begin
	;
	; just take max cross-correlation value
	value = sqrt(max(xcor,mi))
endelse
;
; region around peak
subxcor = xcor[mi-neigh>0:mi+neigh<(nsp-1)]
subxvals = xvals[mi-neigh>0:mi+neigh<(nsp-1)]
;
; fit peak
coef = poly_fit(subxvals,subxcor,2,yfit=yf)
offset = -coef[1]/2.0d/coef[2]-shift
;
; plot
if keyword_set(plot) and ppar.display ge 3 then begin
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.multi=[0,1,2]
	plot,spec0/max(spec0),charsi=1.5,charthi=2,thick=2,title=label, $
		xthick=2,/xs, $
		ythick=2,ytitle='SPEC'
	kcwi_legend,['Spec0','Spec1'],charsi=1.5,charthi=2,thick=[2,2], $
		color=[colordex('black'),colordex('blue')], $
		linesty=[0,0],box=0,/clear,clr_color=!p.background
	oplot,spec1/max(spec1),color=colordex('blue'),thick=2
	plot,xcor,charsi=1.5,charthi=2,thick=2, $
		xthick=2,xtitle='PIX',/xs, $
		ythick=2,ytitle='CCOR',/ylog, $
		title='Offset = '+strtrim(string(offset,format='(f9.3)'),2) + $
		', Peak = '+strtrim(string(value,format='(g13.3)'),2)
	oplot,[mi,mi],10.^!y.crange
	read,'Next? (Q-quit plotting, <cr> - next): ',q
	if strupcase(strmid(q,0,1)) eq 'Q' then plot = (1 eq 0)
	!p.multi=0
endif
;
return 
end	; kcwi_xspec

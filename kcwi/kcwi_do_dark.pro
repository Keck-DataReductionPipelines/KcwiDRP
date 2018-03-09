;
; Copyright (c) 2017, California Institute of Technology. All rights reserved.
;
;+
; NAME:
;	KCWI_DO_DARK
;
; PURPOSE:
;	Determine if input image has dark current signatures.
;
; CALLING SEQUENCE:
;	Result = KCWI_DO_DARK( Im, Hdr, Ppar)
;
; INPUTS:
;	Im	- '_int' image to dark subtract
;	Hdr	- image header
;	Ppar	- Pipline parameter struct
;
; KEYWORDS:
;	INTERACT - set to make routine interactive
;
; RETURNS:
;	True (1) if dark current signatures are significant,
;	False (0) if no dark current signatures are detected.
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2017-12-18	Initial version
;-
function kcwi_do_dark, im, hdr, ppar, interact=interact
;
; setup
pre = 'KCWI_DO_DARK'
q=''
retval = (1 eq 0)	; default return value
;
; test ppar input
if kcwi_verify_ppar(ppar,/init) ne 0 then begin
	print,pre+': Error pipeline parameter struct not initialized'
	return, retval
endif
;
; get binning
binstr = sxpar(hdr,'BINNING')
xbin = fix(gettok(binstr,','))
ybin = fix(binstr)
;
; test regions
contlo = [1840, 2000] / ybin
contmi = [2000, 2160] / ybin
conthi  = [2160, 2320] / ybin
xran = [1900, 2098] / ybin
;
; test vector
tvec = median(im[xran[0]:xran[1], *], dim=1)
;
; fit
res = poly_fit(findgen(n_elements(tvec)),tvec,5,yfit=tfit)
;
; subtract
tsub = tvec - tfit
;
; smooth
if ybin le 1 then $
	tsmo = smooth(tsub,11) $
else	tsmo = smooth(tsub,5)
;
; calculate test moments
lmo = moment(tsmo[contlo[0]:contlo[1]])
mmo = moment(tsmo[contmi[0]:contmi[1]])
hmo = moment(tsmo[conthi[0]:conthi[1]])
;
; get average sigma on both sides
tsig = 1.5 * ( (sqrt(lmo[1]) + sqrt(hmo[1])) / 2.0 )
;
; test
if sqrt(mmo[1]) gt tsig then retval = (1 eq 1)
;
; plot title includes results of test
if retval then begin
	tlab = 'Dark Signature Test: YES'
	kcwi_print_info,ppar,pre,'DS Test (drk?, msig, tsig)','YES', $
		sqrt(mmo[1]), tsig, format='(a,a,1x,2f8.4)'
endif else begin
	tlab = 'Dark Signature Test: NO'
	kcwi_print_info,ppar,pre,'DS Test (drk?, msig, tsig)','NO ', $
		sqrt(mmo[1]), tsig, format='(a,a,1x,2f8.4)'
endelse
;
; display test plot
if interact or ppar.saveplots ge 2 then begin
	;
	; set up plotting
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	plot,tsmo,title=tlab, $
		xran=[contlo[0],conthi[1]],xtitle='Y (row) px', $
		ytitle='Flux - Fit (e-)'
	oplot,[contlo[0],contlo[0]],!y.crange,linesty=2
	oplot,[contlo[1],contlo[1]],!y.crange,linesty=2
	oplot,[contmi[0],contmi[0]],!y.crange,linesty=2
	oplot,[contmi[1],contmi[1]],!y.crange,linesty=2
	oplot,[conthi[0],conthi[0]],!y.crange,linesty=2
	oplot,[conthi[1],conthi[1]],!y.crange,linesty=2
	;
	; LEVEL 2 output
	if ppar.saveplots ge 2 then begin
		imgnum = sxpar(hdr,'FRAMENO')
		fn = kcwi_get_imname(ppar,imgnum,'_darksig',/reduced)
		plotfn = repstr(fn,'.fits','.png')
		write_png,plotfn,tvrd(/true)
		kcwi_print_info,ppar,pre,'saved plot to',plotfn,form='(a,a)'
	endif
	;
	; make interactive?
	if interact then $
		read,'next: ',q
endif
;
return, retval
end	; function kcwi_do_dark


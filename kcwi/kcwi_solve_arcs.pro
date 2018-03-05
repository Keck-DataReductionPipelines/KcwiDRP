;
; Copyright (c) 2013-2017, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SOLVE_ARCS
;
; PURPOSE:
;	Solve the wavelength solutions for each bar of the arc spectrum
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SOLVE_ARCS, Specs, Cntcoeff, Kgeom, Ppar
;
; INPUTS:
;	Specs	- a array of arc spectra produced by KCWI_EXTRACT_ARCS
;	Cntcoeff - Central fit coefficients (from kcwi_fit_center.pro)
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;	PLOT_FILE	- if set to a string, will be used to produce
;				postscript output of diagnostic plots
;
; SIDE EFFECTS:
;	Modifies KCWI_GEOM struct by updating the bar coeffiecients and
;	wavelength fit order and calculating new control points that take 
;	into account the wavelength solution.
;	NOTE: sets KGEOM.STATUS to 0 if fitting succeeded, otherwise sets to
;	1 or greater depending on reason for failure (see code below).
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Matt Matuszewski
;	2017-MAY-10	Initial Revision
;-
;
pro kcwi_solve_arcs, specs, cntcoeff, kgeom, ppar, plot_file=plot_file

pre = 'KCWI_SOLVE_ARCS'
q=''
;
; check status of central fit
if kgeom.status gt 0 then begin
	kcwi_print_info,ppar,pre,'Bad central solutions, cannot solve arcs',/error
	return
endif
;
; do we want to display stuff?
do_plots = (ppar.display ge 2 or ppar.saveplots ge 3)
interact = (ppar.display ge 3)
;
; set up plots
if ppar.display ge 1 or ppar.saveplots ge 2 then begin
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	th=2.0
	si=1.5
endif
;
; which image number
imgnum = kgeom.arcimgnum
;
; which slicer?
ifunum = kgeom.ifunum
ifunam = kgeom.ifunam
;
; which grating? 
grating = kgeom.gratid
;
; which filter? 
filter = kgeom.filter
;
; image label
imglab = 'Img # '+strn(imgnum)+' ('+kgeom.refname+') Sl: '+strtrim(ifunam,2)+ $
	' Fl: '+strtrim(filter,2)+' Gr: '+strtrim(grating,2)
;
; is this N+S mask in?
nasmask = kgeom.nasmask
;
; central wavelength?
cwvl = kgeom.cwave
;
; canonical resolution in Angstroms?
resolution = kgeom.atsig
;
; log info
kcwi_print_info,ppar,pre,systime(0)
kcwi_print_info,ppar,pre,'img, grat, filt, nasmsk, cwave', $
	imgnum,grating,filter,nasmask,cwvl, $
	format='(a,i6,2x,a-8,2x,a-8,i4,f12.3)'
;
; last degree is for full ccd tweaked fits
degree = kgeom.lastdegree
;
; which is the reference bar?
refbar = kgeom.refbar
;
; input spectrum dimensions
specsz = size(specs,/dim)
;
; set up array with zero point in the center
x0 = specsz[0]/2
;xvals = dindgen(specsz[0])-x0
xvals = dindgen(specsz[0])
;
; start with central fit coefficients
twkcoeff = cntcoeff
;
; keep track of final sigmas
sigmas = dblarr(120)
;
; keep track of individual bar fits
barstat = intarr(120)
barrej  = intarr(120)
;
; keep track of observed versus atlas comparison
fwaves = fltarr(120,1000)
dwaves = fltarr(120,1000)
;
; keep track of reference wavelengths and matched pixel positions
rwaves = fltarr(120,1000)
xcents = fltarr(120,1000)
;
; x range of spectra to consider
if nasmask then begin
	minrow = (1*specsz[0])/3
	maxrow = (2*specsz[0])/3
endif else begin
	lastrow = 50
	minrow = lastrow
	maxrow = specsz[0]-lastrow-1
endelse				; no nasmask
ftype = 'Std'
;
; find wavelength range
; and pascal shift coeffs
mnwvs = fltarr(120)
mxwvs = fltarr(120)
for b=0,119 do begin
	twkcoeff[*,b] = pascal_shift(cntcoeff[*,b],x0,/silent)
	waves = poly(xvals,twkcoeff[*,b])
	mnwvs[b] = min(waves)
	mxwvs[b] = max(waves)
endfor
;
minwav = min(mnwvs)
maxwav = max(mxwvs)
;
; do a continuum subtraction
if nasmask then sectors = 6 else sectors = 16
;
div = (maxrow-minrow)/sectors
;
xv = fltarr(sectors)
yv = fltarr(sectors)
for b = 0, 119 do begin
	;
	; get continuum point in each sector
	for sec = 0,sectors-1 do begin
		;
		; We may need this for low-res gratings!
		mn = min(specs[minrow+sec*div:minrow+(sec+1)*div,b],mi)
		xv[sec] = mi+minrow+sec*div
		;mn = median(specs[minrow+sec*div:minrow+(sec+1)*div,b])
		;xv[sec] = minrow+sec*div + div/2
		yv[sec] = mn
	endfor
	;
	; fit sector sky points
	res = poly_fit(xv,yv,3,yfit=yf)
	;
	xvs = findgen(specsz[0])
	specs[*,b] -= res[0]+res[1]*xvs+res[2]*xvs^2+res[3]*xvs^3
	;
endfor	; b
; 
; use reference bar
b = refbar
;
; fill out the arrays we are working with.
subxvals = xvals[minrow:maxrow]
subyvals = smooth(reform(specs[minrow:maxrow,b]),3)
subwvals = poly(subxvals,twkcoeff[*,b])
;
; find good peaks in object spectrum
;smooth_width = fix(resolution/abs(twkcoeff[1,b]))>4	; in pixels
smooth_width = 4
;peak_width   = fix(smooth_width*2.5)		; for fitting peaks
peak_width = fix(resolution/abs(twkcoeff[1,b]))>4	; in pixels
;slope_thresh = 0.7*smooth_width/resolution/100.0	; more severe for object
slope_thresh = 0.7*smooth_width/2./100.0	; more severe for object
kcwi_print_info,ppar,pre,'using a peak_width (px) of', $
	peak_width,format='(a,1x,i5)'
;
; set to get the most lines
ampl_thresh = 0.
spec_cent = findpeaks(subwvals,subyvals,smooth_width,slope_thresh, $
	ampl_thresh,peak_width,count=spec_npks,avsg=avwsg)
avwfwhm = avwsg * 2.355
kcwi_print_info,ppar,pre,'starting with ',spec_npks,' lines.'
kcwi_print_info,ppar,pre,'avg line width (A)',avwsg,form='(a,1x,f9.3)'
kcwi_print_info,ppar,pre,'avg line FWHM (A)',avwfwhm,form='(a,1x,f9.3)'
;
; load the reference atlas spectrum.
kcwi_read_atlas,kgeom,ppar,refspec,refwvl,refdisp
;
; first trim to the grating bandpass (and a buffer of 100 Ang). 
qwvs = where(refwvl gt minwav-100. and $
	     refwvl lt maxwav+100.,nqwvs)
if nqwvs eq 0 then begin
	kcwi_print_info,ppar,pre,'No wavelength overlap with atlas spectrum',/error
	kgeom.status=5
	return
endif
refspec = refspec[qwvs]
refwvl = refwvl[qwvs]
kcwi_print_info,ppar,pre,'atlas disp (A/pix)',refdisp,form='(a,1x,f9.3)'
;
; set spectra fitting width
; BM, BL are well sampled, but other
; gratings need a bigger window
if strpos(grating,'BH') ge 0 or strpos(grating,'BM') ge 0 then $
	fwid = avwfwhm $
else	fwid = avwsg
;
; generate an Atlas line list
;
; setup wavelength fitting arrays
refxs = fltarr(spec_npks)-1.
refws = fltarr(spec_npks)-1.
refwd = fltarr(spec_npks)-1.
refof = fltarr(spec_npks)-1.
fi = 0		; fit line index
;
; examine lines
for pp = 0,spec_npks-1 do begin
	is_good = (1 eq 1)
	!p.multi=[0,2,1]
	;
	; fit Atlas peak in wavelength
	ffs = get_line_window(refwvl,refspec,spec_cent[pp],count=nffs)
	if nffs gt 5 then begin
		!quiet = 1	; supress curvefit error messages
		yf = gaussfit(refwvl[ffs],refspec[ffs],a,nterms=3)
		!quiet = 0	; turn error messages back on
		pka = (where(refspec[ffs] eq max(refspec[ffs])))[0]
		xoff = abs(refwvl[ffs[pka]] - a[1])/refdisp
		woff = abs(spec_cent[pp]-a[1])
		wrat = a[2]/fwid
		if woff gt 5. or xoff gt 1.5 or wrat gt 1.1 then $
			is_good = (1 eq 0)
		at_pk_wl = a[1]
		if is_good and interact then begin
		  wvs = where(refwvl gt spec_cent[pp]-30. and $
			      refwvl lt spec_cent[pp]+30.)
		  plot,refwvl[wvs],refspec[wvs],psym=10,title='Atlas', $
			xtitle='Wave(A)',/xs,ytitle='Flux'
		  oplot,refwvl[ffs],yf,color=colordex('G')
		endif
	endif else begin
		is_good = (1 eq 0)
		xoff = -1.
		woff = -1.
		wrat = 0.
	endelse
	;
	; fit Spec peak in X pixels
	ffs = get_line_window(subwvals,subyvals,spec_cent[pp],count=nffs)
	if nffs gt 5 then begin
		!quiet = 1	; supress curvefit error messages
		yf = gaussfit(subxvals[ffs],subyvals[ffs],a,nterms=3)
		!quiet = 0	; turn error messages back on
		pka = (where(subyvals[ffs] eq max(subyvals[ffs])))[0]
		xoff = abs(subxvals[ffs[pka]] - a[1])
		xrat = a[2]/(avwsg/abs(twkcoeff[1,b]))
		if xrat gt 1.25 or not finite(a[1]) or a[2] gt nffs then $
			is_good = (1 eq 0)
		sp_pk_x = a[1]
		if is_good and interact then begin
		  wvs = where(subwvals gt spec_cent[pp]-30. and $
			      subwvals lt spec_cent[pp]+30.)
		  plot,subxvals[wvs],subyvals[wvs],psym=10,title='Spec', $
			xtitle='Wave(A)',/xs,ytitle='Flux'
		  oplot,subxvals[ffs],yf,color=colordex('G')
		endif
	endif else begin
		is_good = (1 eq 0)
		xoff = -1.
		xrat = 0.
	endelse
	;
	; are we good?
	if is_good then begin
	   if interact then begin
		q = ''
		read,'<cr> - skip, f - fit: ',q
		if strupcase(strtrim(q,2)) eq 'F' then begin
			refxs[fi] = sp_pk_x
			refws[fi] = at_pk_wl
			refwd[fi] = wrat
			refof[fi] = woff
			fi += 1
		endif else if strupcase(strtrim(q,2)) eq 'Q' then begin
			interact = (1 eq 0)
		endif
	    endif else begin
			refxs[fi] = sp_pk_x
			refws[fi] = at_pk_wl
			refwd[fi] = wrat
			refof[fi] = woff
			fi += 1
	    endelse
	endif	; are we good?
endfor	; examine lines
;
; reset interact
interact = (ppar.display ge 2)
;
; get fit lines
good = where(refws gt 0., ngood)
refws = refws[good]
refxs = refxs[good]
refwd = refwd[good]
refof = refof[good]
;
; preliminary fit
newcoeff = poly_fit(refxs,refws,degree,yfit=fitw)
;
; get residual stats
diff = refws - fitw
ims, diff, rmn, rsg, rwgt
bad = where(rwgt le 0, nbad)
;
; clean outliers
if nbad gt 0 then begin
	good = where(rwgt gt 0, ngood)
	refws = refws[good]
	refxs = refxs[good]
	refwd = refwd[good]
	refof = refof[good]
	newcoeff = poly_fit(refxs,refws,degree,yfit=fitw)
endif else ngood = n_elements(refws)
diff = refws - fitw
;
; account for cleaning zero-point wavelength
mo = moment(diff)
diff = diff - mo[0]
;
; final RMS
mo = moment(diff)
sigmas[b] = sqrt(mo[1])
;
; store for later plots
fwaves[b,0:(ngood-1)] = fitw
dwaves[b,0:(ngood-1)] = diff
rwaves[b,0:(ngood-1)] = refws
xcents[b,0:(ngood-1)] = refxs
barstat[b] = 0
barrej[b] = nbad
;
; log stats
kcwi_print_info,ppar,pre,ftype+' Fit: Bar#,Npks,RMS,Coefs', $
	b,ngood,sigmas[b],newcoeff[0:degree], $
	format='(a,2i5,2x,f7.3,f9.2,f8.4,'+strn((degree-1)>1)+'g13.5)'
;
; store resulting coeffs
twkcoeff[*,b] = 0.
twkcoeff[0:degree,b] = newcoeff
;
; plot residuals
if do_plots then begin
	!p.multi=0
	yran = get_plotlims(diff)
	if abs(newcoeff[1]) gt 0.4 then $
		yrng = [yran[0]<(-0.4),yran[1]>0.4] $
	else	yrng = [yran[0]<(-0.2),yran[1]>0.2]
	plot,refws,diff,psym=4,charsi=si,charthi=th,thick=th, $
		title=imglab+' (bar '+strn(b)+' REF)', $
		xthick=th, xtitle='Wavelength (A)', $
		ythick=th, ytitle='Atlas - Spec (A)', yrange=yrng, /ys
	oplot,!x.crange,[0,0], linesty=2, color=colordex('blue')
	oplot,!x.crange,[sigmas[b],sigmas[b]],linesty=1,color=colordex('blue')
	oplot,!x.crange,-[sigmas[b],sigmas[b]],linesty=1,color=colordex('blue')
	kcwi_legend,["RMS = "+string(sigmas[b],form='(f7.3)')+' Ang', $
		     'NMatch = '+strn(ngood), $
		     'NRej = '+strn(nbad)],charsi=si,charthi=th
endif
if interact then $
	read,'next: ',q
;
; write out atlas line list
atllist = ppar.reddir+ppar.froot+string(imgnum,'(i0'+strn(ppar.fdigits)+')')+'_atlas.txt'
openw,al,atllist,/get_lun
printf,al,'# '+pre+': Atlas lines (Ang) printed on '+systime(0)
printf,al,'# Atlas filename: '+kgeom.refspec
printf,al,'# Cont Bars filename: '+kgeom.cbarsfname
printf,al,'# Arc filename: '+kgeom.arcfname
printf,al,'# Reference bar: '+string(refbar)
printf,al,'# RMS of fit: '+string(rsg,form='(f9.3)')
printf,al,'# Avg Spec Width (A): '+string(avwsg,form='(f9.3)')
printf,al,'# Atlas disp (A/px): '+string(refdisp,form='(f9.3)')
printf,al,'#    RefWave     SpecPix    FitResid     LWidRat     LOffRat'
for j=0,ngood-1 do printf,al,refws[j],refxs[j],diff[j], $
			     refwd[j],refof[j],format='(5f12.3)'
free_lun,al
kcwi_print_info,ppar,pre,'Atlas lines written to',atllist,format='(a,a)'
;
; now loop over other bars
for b = 0,119 do begin
	;
	; setup wavelength fitting arrays
	fitxs = refxs - refxs
	;
	; skip refbar (already done)
	if b ne refbar then begin
		;
		; fill out the arrays we are working with.
		subxvals = xvals[minrow:maxrow]
		subyvals = smooth(reform(specs[minrow:maxrow,b]),3)
		subwvals = poly(subxvals,twkcoeff[*,b])
		;
		; loop over reference lines
		for pp = 0,n_elements(refws)-1 do begin
			;
			; fit Spec peak
			ffs = get_line_window(subwvals,subyvals,refws[pp], $
						count=nffs)
			if nffs gt 5 then begin
				!quiet = 1	; supress curvefit error msgs
				yf = gaussfit(subxvals[ffs],subyvals[ffs],a, $
						nterms=3)
				!quiet = 0	; turn error messages back on
				if finite(a[1]) and a[2] lt nffs then $
					fitxs[pp] = a[1]
			endif
		endfor	; loop over reference lines
		;
		; get fit lines
		good = where(fitxs gt 0., ngood)
		fitws = refws[good]
		fitxs = fitxs[good]
		;
		; preliminary fit
		newcoeff = poly_fit(fitxs,fitws,degree,yfit=fitw)
		;
		; get residual stats
		diff = fitws - fitw
		ims, diff, rmn, rsg, rwgt
		bad = where(rwgt le 0, nbad)
		;
		; clean outliers
		if nbad gt 0 then begin
			good = where(rwgt gt 0, ngood)
			fitws = fitws[good]
			fitxs = fitxs[good]
			newcoeff = poly_fit(fitxs,fitws,degree,yfit=fitw)
		endif else ngood = n_elements(fitws)
		diff = fitws - fitw
		;
		; account for cleaning zero-point wavelength
		mo = moment(diff)
		diff = diff - mo[0]
		;
		; final RMS
		mo = moment(diff)
		sigmas[b] = sqrt(mo[1])
		;
		; store for later plots
		fwaves[b,0:(ngood-1)] = fitw
		dwaves[b,0:(ngood-1)] = diff
		rwaves[b,0:(ngood-1)] = fitws
		xcents[b,0:(ngood-1)] = fitxs
		;
		; log stats
		kcwi_print_info,ppar,pre,ftype+' Fit: Bar#,Npks,RMS,Coefs', $
			b,ngood,sigmas[b],newcoeff[0:degree], $
			format='(a,2i5,2x,f7.3,f9.2,f8.4,'+strn((degree-1)>1)+'g13.5)'
		;
		; store resulting coeffs
		twkcoeff[*,b] = 0.
		twkcoeff[0:degree,b] = newcoeff
		;
		; plot residuals
		if interact then begin
			yran = get_plotlims(diff)
			if abs(newcoeff[1]) gt 0.4 then $
				yrng = [yran[0]<(-0.4),yran[1]>0.4] $
			else	yrng = [yran[0]<(-0.2),yran[1]>0.2]
			plot,fitws,diff,psym=4,charsi=si,charthi=th,thick=th, $
				title=imglab+' (bar '+strn(b)+')', $
				xthick=th, xtitle='Wavelength (A)', $
				ythick=th, ytitle='Atlas - Spec (A)',yrange=yrng,/ys
			oplot,!x.crange,[0,0],linestyle=2,color=colordex('blue')
			oplot,!x.crange,[sigmas[b],sigmas[b]],linesty=1, $
				color=colordex('blue')
			oplot,!x.crange,-[sigmas[b],sigmas[b]],linesty=1, $
				color=colordex('blue')
			kcwi_legend,['RMS = '+string(sigmas[b],form='(f7.3)')+' Ang',$
				     'NMatch = '+strn(ngood), $
	     			     'NRej = '+strn(nbad)],charsi=si,charthi=th
			read,'Next? (Q - quit prompting for plots, <cr> - next): ',q
			if strupcase(strmid(q,0,1)) eq 'Q' then interact = (1 eq 0)
		endif	; do_plots
    	endif	; b ne refbar
endfor	; b
;
; our final coefficients
fincoeff = twkcoeff
;
; get wavelength ranges
y0wvs = fltarr(120)	; good wavelengths
y1wvs = fltarr(120)
t0wvs = fltarr(120)	; trim wavelengths
t1wvs = fltarr(120)
ym0wvs = fltarr(120)	; masked good wavelengths
ym1wvs = fltarr(120)
tm0wvs = fltarr(120)	; masked trim wavelengths
tm1wvs = fltarr(120)
for b=0,119 do begin
	y0wvs[b] = poly(kgeom.goody0,fincoeff[*,b])
	y1wvs[b] = poly(kgeom.goody1,fincoeff[*,b])
	t0wvs[b] = poly(kgeom.trimy0,fincoeff[*,b])
	t1wvs[b] = poly(kgeom.trimy1,fincoeff[*,b])
	ym0wvs[b] = poly(kgeom.goodmy0,fincoeff[*,b])
	ym1wvs[b] = poly(kgeom.goodmy1,fincoeff[*,b])
	tm0wvs[b] = poly(kgeom.trimmy0,fincoeff[*,b])
	tm1wvs[b] = poly(kgeom.trimmy1,fincoeff[*,b])
endfor
y0max = max(y0wvs)
y0min = min(y0wvs)
y1max = max(y1wvs)
y1min = min(y1wvs)
trim0 = min(t0wvs)
trim1 = max(t1wvs)
ym0max = max(ym0wvs)
ym0min = min(ym0wvs)
ym1max = max(ym1wvs)
ym1min = min(ym1wvs)
trimm0 = min(tm0wvs)
trimm1 = max(tm1wvs)
;
; check for negative dispersion
if trim0 gt trim1 then begin
	trim0 = min(t1wvs)
	trim1 = max(t0wvs)
	trimm0 = min(tm1wvs)
	trimm1 = max(tm0wvs)
endif
;
wavgood0 = min([y0max,y1max])
wavgood1 = max([y0min,y1min])
wavall0  = min([y0min,y1min])
wavall1  = max([y0max,y1max])
wavgoodm0 = min([ym0max,ym1max])
wavgoodm1 = max([ym0min,ym1min])
wavallm0  = min([ym0min,ym1min])
wavallm1  = max([ym0max,ym1max])
;
; mid-wavelength
wavmid = total([wavgood0,wavgood1,wavall0,wavall1])/4.
;
; now let's update kgeom struct
;
; check bar statuses
kgeom.status = max(barstat)
if kgeom.status gt 0 then begin
	kcwi_print_info,ppar,pre,'Some bars had issues, check log for details',/warning
	;
	; if N&S, then this was just for info, carry on
	if kgeom.nasmask then $
		kgeom.status=0
endif
;
; order of the fits
kgeom.bfitord = degree+1
;
; reference bar coeff's
kgeom.rbcoeffs = fincoeff[*,refbar]
;
; inclusive, all, and trim wavelengths
kgeom.waveall0 = wavall0
kgeom.waveall1 = wavall1
kgeom.wavegood0 = wavgood0
kgeom.wavegood1 = wavgood1
kgeom.waveallm0 = wavallm0
kgeom.waveallm1 = wavallm1
kgeom.wavegoodm0 = wavgoodm0
kgeom.wavegoodm1 = wavgoodm1
kgeom.wavemid = wavmid
;
; make sure wavelengths are on fiducial scale and
; zeropoint is integer number of bins from fiducial wl
ndels = long((trim0 - kgeom.wavefid)/kgeom.dwout)
kgeom.wave0out = kgeom.wavefid + float(ndels) * kgeom.dwout
ndels = long((trim1 - kgeom.wavefid)/kgeom.dwout)
kgeom.wave1out = kgeom.wavefid + float(ndels) * kgeom.dwout
;
; log values
kcwi_print_info,ppar,pre,'min,max good wavelengths',kgeom.wavegood0,kgeom.wavegood1, $
	format='(a,2f9.2)'
kcwi_print_info,ppar,pre,'min,max inclusive wavelengths',kgeom.waveall0,kgeom.waveall1, $
	format='(a,2f9.2)'
kcwi_print_info,ppar,pre,'min,max trim wavelengths',kgeom.wave0out,kgeom.wave1out, $
	format='(a,2f9.2)'
kcwi_print_info,ppar,pre,'middle wavelength',kgeom.wavemid,format='(a,f9.2)'
;
; set fit rms values
mom = moment(sigmas)
kgeom.avewavesig = mom[0]
kgeom.stdevwavesig = sqrt(mom[1])
;
; log results
kcwi_print_info,ppar,pre,'ave/stddev bar wavelength sigma (Ang)', $
	mom[0],sqrt(mom[1]),format='(a,f7.3,2x,f7.3)'
kcwi_print_info,ppar,pre,'min/max bar wavelength sigma (Ang)', $
	minmax(sigmas), format='(a,f7.3,2x,f7.3)'
outliers = where(abs(sigmas-mom[0]) gt 3.*sqrt(mom[1]), noutliers)
if noutliers gt 0 then $
	kcwi_print_info,ppar,pre,'> 3sig outlier bars present: Imgnum, Bars', $
		imgnum,outliers,format='(a,i7,2x,'+strn(noutliers)+'i5)', $
		/warning
;
; central dispersion
disp = dblarr(120)
;
; data coefficients 
dcoeff = dblarr(9)
;
; now apply all the coeffs and calculate central dispersion
for b=0,119 do begin
	;
	; update coeffs in kgeom struct
	dcoeff = fincoeff[*,b]
	kcwi_apply_coeffs,kgeom,b,dcoeff
	;
	; calculate central dispersion
	subxvals = xvals[minrow:maxrow]
	subwvals = poly(subxvals,fincoeff[*,b])
	;
	for d=0,degree do dcoeff[d] = d*fincoeff[d,b]
	dcoeff = shift(dcoeff,-1)
	xv = interpol(subxvals,subwvals,cwvl)
	disp[b] = poly(xv,dcoeff)
endfor			; b
;
; stamp the KCWI_GEOM struct
kgeom.progid = pre
kgeom.timestamp = systime(1)
;
; now let's make some plots!
;
; plot diagnostics of arc fits
kcwi_plot_arcfits,specs,kgeom,ppar,cntcoeff,fincoeff,sigmas,fwaves,dwaves, $
	/tweak,plot_file=plot_file,ftype=ftype
;
; finally, let's make a diagnostic summary plot
if ppar.display ge 1 or ppar.saveplots ge 2 then begin
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	tlab = imglab+', '+ftype
	;
	; get fit rms status
	mom = moment(sigmas)
	fitrmslab = strtrim(string(mom[0],format='(f9.3)'),2) + ' +- ' + $
	      strtrim(string(sqrt(mom[1]),format='(f9.3)'),2)
	yrngs = get_plotlims(sigmas)
	!p.multi=[0,1,2]
	plot,sigmas,psym=4,charsi=si,charthi=th,thick=th,title=tlab+' Fit <RMS>: '+fitrmslab, $
		xthick=th,xtitle='Bar #',xrange=[-1,120],/xs, $
		ythick=th,ytitle='RMS (Ang)',yrange=yrngs,/ys
	oplot,!x.crange,[mom[0],mom[0]],linesty=5,thick=th
	oplot,!x.crange,[mom[0]+sqrt(mom[1]),mom[0]+sqrt(mom[1])],linesty=1,thick=th
	oplot,!x.crange,[mom[0]-sqrt(mom[1]),mom[0]-sqrt(mom[1])],linesty=1,thick=th
	kcwi_oplot_slices
	plot,disp,psym=4,charsi=si,charthi=th,thick=th, $
		title=tlab+' Dispersion @ '+string(cwvl,"(f8.2)")+' Ang', $
		xthick=th,xtitle='Bar #',xrange=[-1,120],/xs, $
		ythick=th,ytitle='Ang/pix',yrange=yrngd,/ys
	kcwi_oplot_slices
	if ppar.saveplots ge 2 then begin
		plotfn = kcwi_get_imname(ppar,kgeom.arcimgnum, $
			'_final_fit',/reduced)
		plotfn = repstr(plotfn,'.fits','.png')
		write_png,plotfn,tvrd(/true)
		kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
	endif
	if ppar.display ge 2 then read,'next: ',q
endif
!p.multi=0
;
return
end		; kcwi_solve_arcs

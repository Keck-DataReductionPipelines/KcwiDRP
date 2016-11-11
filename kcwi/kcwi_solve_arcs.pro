;
; Copyright (c) 2013-2016, California Institute of Technology. All rights
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
;	KCWI_SOLVE_ARCS, Specs, Kgeom, Ppar
;
; INPUTS:
;	Specs	- a array of arc spectra produced by KCWI_EXTRACT_ARCS
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;	TWEAK	- set to iteratively adjust wavelength solution to improve 
;			fit in outer thirds of wavelength range: for 
;			nod-and-shuffle images only one iteration is performed
;			and only to assess rms of fit (no actual tweak is done).
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
;	2013-DEC-10	Initial Revision
;	2013-DEC-12	Changes to make the cross-correlation more robust
;	2014-MAR-19	Use KCWI_CLEAN_COEFFS to fix errant bars
;	2014-MAR-20	Adjusts peak match thresh if no peaks matched, regardless of iteration number
;	2014-MAR-28	Use central ccor peak for initial wave soln.
;	2014-MAR-28	Check for scattered light in initial offset calc.
;	2014-APR-07	Changed to a function returning status of fit
;	2014-APR-07	Uses preliminary solution to determine wave range instead of filter
;	2014-APR-07	Uses parabola fit to CC peak when no zero derivs are found
;	2014-SEP-11	Converted function to pro with status in kgeom.status
;	2014-SEP-16	Single iteration mode for assessing rms of fits (N&S)
;	2014-NOV-06	Put stats (rms) after all tweaking done
;	2015-APR-22	Rework of peak finding
;-
;
pro kcwi_solve_arcs, specs, kgeom, ppar, tweak=tweak, plot_file=plot_file

pre = 'KCWI_SOLVE_ARCS'
q=''
;
; do we want to display stuff? 
display = (ppar.display ge 2)
;
if ppar.display ge 1 then begin
	deepcolor
	!p.multi=0
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
resolution = kgeom.resolution * float(kgeom.ybinsize)
;
; log info
kcwi_print_info,ppar,pre,systime(0)
kcwi_print_info,ppar,pre,'img, grat, filt, nasmsk, cwave', $
	imgnum,grating,filter,nasmask,cwvl, $
	format='(a,i6,2x,a-8,2x,a-8,i4,f12.3)'
;
; do initial fit of central third of ccd
kcwi_fit_center,specs,kgeom,ppar,cntcoeff
;
; check status
if kgeom.status gt 0 then begin
	kcwi_print_info,ppar,pre,'Cannot solve arcs',/error
	return
endif
;
; plot results
if ppar.display ge 1 then begin
	window,0,title='kcwi_solve_arcs'
	!p.multi=[0,1,2]
	si = 1.5
	ys = reform(cntcoeff[0,*])
	yrng = get_plotlims(ys)
	plot,cntcoeff[0,*],psym=4,charsi=si,charthi=th,thick=th, $
		title = imglab+' Central Fit Coef0', $
		xthick=th,xtitle='Bar #', xrange=[-1,120],/xs, $
		ythick=th,ytitle='Ang',yrange=yrng,/ys
	kcwi_oplot_slices
	ys = reform(cntcoeff[1,*])
	yrng = get_plotlims(ys)
	plot,cntcoeff[1,*],psym=4,charsi=si,charthi=th,thick=th, $
		title = imglab+' Central Fit Coef1', $
		xthick=th,xtitle='Bar #', xrange=[-1,120],/xs, $
		ythick=th,ytitle='Ang/px',yrange=yrng,/ys
	kcwi_oplot_slices
	if ppar.display ge 3 or (ppar.display ge 2 and nasmask) then $
		read,'next: ',q
	!p.multi=0
endif
;
; we will be initially using a third degree fit to the peak positions:
degree = 3
;
; last degree is for full ccd tweaked fits
lastdegree = kgeom.lastdegree
;
; which is the reference bar?
refbar = kgeom.refbar
;
; load the reference atlas spectrum.
kcwi_read_atlas,kgeom,ppar,refspec,refwvl,refdisp
;
; input spectrum dimensions
specsz = size(specs,/dim)
;
; set up array with zero point in the center
x0 = specsz[0]/2
xvals = dindgen(specsz[0])-x0
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
; for tweaking: array of border fractions to ignore.
if nasmask then begin
	edgespace = [ 0.34 ]
	minrow = (1*specsz[0])/3
	maxrow = (2*specsz[0])/3
	ftype = 'Central'
endif else begin
	edgespace = [ 0.25, 0.18, 0.12, 0.06, 0.00]
	lastrow = 50
	minrow = lastrow
	maxrow = specsz[0]-lastrow-1
	ftype = 'FullCCD'
endelse				; no nasmask
;
; find wavelength range
mnwvs = fltarr(120)
mxwvs = fltarr(120)
for b=0,119 do begin
	waves = poly(xvals,cntcoeff[*,b])
	mnwvs[b] = min(waves)
	mxwvs[b] = max(waves)
endfor
;
minwav = min(mnwvs)
maxwav = max(mxwvs)
;
; adjust higher order terms, if requested
; do a continuum subtraction if tweaking
if keyword_set(tweak) then begin
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

endif	; this is part of the tweak.
;
; --- this is where the tweak happens, if requested
; this is how we do it:
; currently, we are not going to be tweaking nod-and-shuffle
; We assume that the initial solution is pretty good and only needs to
; be refined toward the edges of the CCD. 
; We step through  successively larger windows starting with the inner
; most 1/3 of the CCD 
if keyword_set(tweak) then begin
	;
	; get number of iterations
	niter = n_elements(edgespace)
	if niter eq 1 then $
		kcwi_print_info,ppar,pre,'using '+strn(niter)+' iteration to measure RMS of initial central fit' $
	else	kcwi_print_info,ppar,pre,'using '+strn(niter)+' iterations to tweak higher order terms'
	;
	kcwi_print_info,ppar,pre,'Spectral line finding/matching thresh (frac. of res.): PkDel', $
		ppar.pkdel,format='(a,2x,f7.3)'
	;
	; start with central fit coefficients
	twkcoeff = cntcoeff
	;
	; reference spectrum and wavelengths
	twk_reference_spectrum = refspec
	twk_reference_wavelengths = refwvl
	;
	; first trim to the grating bandpass (and a buffer of 100 Ang). 
	qwvs = where(twk_reference_wavelengths gt minwav-100. and $
		     twk_reference_wavelengths lt maxwav+100.,nqwvs)
	if nqwvs eq 0 then begin
		kcwi_print_info,ppar,pre,'No wavelength overlap with atlas spectrum',/error
		kgeom.status=5
		return
	endif
	twk_reference_spectrum = twk_reference_spectrum[qwvs]
	twk_reference_wavelengths = twk_reference_wavelengths[qwvs]
	;
	; plot if requested
	if ppar.display ge 3 then begin
		xarng = get_plotlims([minwav,maxwav],pad=0.2)
		;
		; if plotting diagnostics, just focus on first iteration (central third)
		if ppar.display ge 3 then begin
			dwav = (maxwav - minwav)/3.
			xarng = get_plotlims([minwav+dwav,maxwav-dwav],pad=0.3)
			atitle = 'Atlas ('+kgeom.refname+') Spectrum Lines (Central 3rd)'
		endif else $
			atitle = 'Atlas ('+kgeom.refname+') Spectrum Lines'
		plot,twk_reference_wavelengths,twk_reference_spectrum, title=atitle, $
			thick=th,charsi=si,charthi=th, $
			xthick=th,xtitle='Wave(A)',xrange=xarng,/xs, $
			ythick=th,ytitle='Flux', $
			yrange=[min(twk_reference_spectrum)>10., $
				max(twk_reference_spectrum)+0.05*max(twk_reference_spectrum)],/ys,/ylog
	endif
	;
	; let's find the peaks in the reference spectrum.
	smooth_width = fix(resolution/refdisp)>4	; in pixels
	slope_thresh = 0.003
	fthr = 0.05	; start at 5% of max
	ampl_thresh  = max(twk_reference_spectrum)*fthr
	twk_ref_cent = findpeaks(twk_reference_wavelengths,twk_reference_spectrum, $
			smooth_width,slope_thresh,ampl_thresh,count=twk_ref_npks)
	newlines = twk_ref_npks
	oldlines = newlines
	;
	; find where noise starts to contaminate line list
	while (newlines - oldlines) lt 10 and fthr gt 0.01 do begin
		fthr -= 0.01
		oldlines = newlines
		ampl_thresh  = max(twk_reference_spectrum)*fthr
		twk_ref_cent = findpeaks(twk_reference_wavelengths,twk_reference_spectrum, $
			smooth_width,slope_thresh,ampl_thresh,count=twk_ref_npks)
		newlines = twk_ref_npks
	endwhile
	fthr += 0.01
	ampl_thresh  = max(twk_reference_spectrum)*fthr
	twk_ref_cent = findpeaks(twk_reference_wavelengths,twk_reference_spectrum, $
		smooth_width,slope_thresh,ampl_thresh,count=twk_ref_npks)
	;
	if twk_ref_npks le lastdegree then begin
		kcwi_print_info,ppar,pre,'Not enough good atlas points found',twk_ref_npks,/error
		kgeom.status=6
		return
	endif
	;
	if twk_ref_npks lt 50 then $
		kcwi_print_info,ppar,pre,'Atlas wavelength coverage may be limited: N lines < 50',/warn

	kcwi_print_info,ppar,pre,'Atlas threshhold in percent of max',fix(fthr*100.)
	kcwi_print_info,ppar,pre,'Number of clean atlas lines found',twk_ref_npks,$
		format='(a,i4)'
	kcwi_print_info,ppar,pre,'Matching width in Angstroms',ppar.pkdel*refdisp,$
		format='(a,f6.4)'
	;
	if ppar.display ge 3 then begin
		for j=0,twk_ref_npks-1 do begin
			oplot,[twk_ref_cent[j],twk_ref_cent[j]],10.^!y.crange,color=colordex('blue'),thick=1.0
		endfor
		oplot,[minwav,minwav],10.^!y.crange,color=colordex('green'),thick=th,linesty=5
		oplot,[maxwav,maxwav],10.^!y.crange,color=colordex('green'),thick=th,linesty=5
		kcwi_legend,['PeakFit','WavRng'],linesty=[0,5],thick=[1.0,th], $
			color=[colordex('blue'),colordex('green')],/clear,charsi=si,charthi=th
		kcwi_legend,['Thr='+string(fix(fthr*100.),form='(i02)')+'%'], $
			/clear,/right, charthi=th,charsi=si
	endif
	;
	; at this point we have the peaks we want in the atlas spectrum. 
	;
	; write out atlas line list
	atllist = ppar.reddir+ppar.froot+string(imgnum,'(i0'+strn(ppar.fdigits)+')')+'_atlas.txt'
	openw,al,atllist,/get_lun
	printf,al,'# '+pre+': Atlas lines (Ang) printed on '+systime(0)
	for j=0,twk_ref_npks-1 do printf,al,twk_ref_cent[j],format='(f12.3)'
	free_lun,al
	kcwi_print_info,ppar,pre,'Atlas lines written to',atllist,format='(a,a)'
	;
	; open file for output of object line list
	objlist = ppar.reddir+ppar.froot+string(imgnum,'(i0'+strn(ppar.fdigits)+')')+'_object.txt'
	openw,al,objlist,/get_lun
	printf,al,'# '+pre+': Object lines (Ang) printed on '+systime(0)
	printf,al,'#     AtlWl           Ypx      Bar'
	;
	; now pop up a diagnostic window if requested
	ddisplay = (ppar.display ge 3)
	if ddisplay then $
		window,1,title='kcwi_solve_arcs(2)'
	;
	; now loop over the iterations and over the bars
	;
	if nasmask and niter gt 1 then $
		kcwi_print_info,ppar,pre,'tweaking not recommended when the NS mask is in',/warning
	;
	centerpoints = intarr(niter)
	leftpoints = centerpoints
	rightpoints = centerpoints
	;
	for iter = 0,niter-1 do begin
		; if this is the last iteration then we should see the full CCD
		; at this point, we want switch to last degree.
		if iter eq niter-1 and not nasmask then degree = lastdegree
		; what region of the ccd are  we fitting in this iteration?
		twk_minrow = fix(specsz[0]*edgespace[iter])
		twk_maxrow = fix(specsz[0]*(1-edgespace[iter]))<specsz[0]-1
		nrows = twk_maxrow-twk_minrow+1
		; 
		for b=0, 119 do begin
			; fill out the arrays we are working with.
			subxvals = xvals[twk_minrow:twk_maxrow]
			subyvals = smooth(reform(specs[twk_minrow:twk_maxrow,b]),3)
			subwvals = poly(subxvals,twkcoeff[*,b])
			;
			; plot if needed
			if ddisplay and iter eq 0 then begin
				wset,1
				plot,subwvals,subyvals>1., title='Object Spectrum Lines (Central 3rd): Bar '+strn(b), $
					thick=th,charsi=si,charthi=th, $
					xthick=th,xtitle='Wave(A)',xrange=xarng,/xs, $
					ythick=th,ytitle='Flux', $
					yrange=[min(subyvals)>1.,max(subyvals)+0.05*max(subyvals)],/ys,/ylog
			endif
			;
			; find good peaks in object spectrum
			smooth_width = fix(resolution/abs(twkcoeff[1,b]))>4	; in pixels
			peak_width   = fix(smooth_width*1.5)			; for fitting peaks
			slope_thresh = 0.7*smooth_width/2./100.0		; more severe for object
			fthr = 0.15						; start at 15% of max
			ampl_thresh  = max(subyvals) * fthr
			twk_spec_cent = findpeaks(subwvals,subyvals,smooth_width,slope_thresh, $
				ampl_thresh,peak_width,count=twk_spec_npks)
			newlines = twk_spec_npks
			oldlines = newlines
			;
			; find where the noise starts to contaminate the line list
			while (newlines - oldlines) lt 10 and fthr gt 0.01 do begin
				oldlines = newlines
				fthr -= 0.01
				ampl_thresh = max(subyvals) * fthr
				twk_spec_cent = findpeaks(subwvals,subyvals,smooth_width,slope_thresh, $
					ampl_thresh,peak_width,count=twk_spec_npks)
				newlines = twk_spec_npks
			endwhile
			;
			; put threshhold back above the noise
			fthr += 0.02
			ampl_thresh  = max(subyvals) * fthr
			twk_spec_cent = findpeaks(subwvals,subyvals,smooth_width,slope_thresh, $
				ampl_thresh,peak_width,count=twk_spec_npks)
			;
			; test final line list
			if twk_spec_npks gt 0 then begin
				;
				; at this point we have the catalog of good reference points (from
				; before) and list of good points in this bar. We have to associate
				; them.
				one_ref = fltarr(twk_ref_npks)+1.0
				one_spec = fltarr(twk_spec_npks)+1.0

				diff = abs((twk_spec_cent##one_ref) - (one_spec##twk_ref_cent))
				;
				mn = min(diff,dim=1,mi)
				;
				; here we match the peaks to one another. 
				pkd = ppar.pkdel > 1.0
				;
				; never let this get smaller than a single atlas pixel
				pkm = pkd*refdisp	; match thresh in Angstroms
				matchedpeaks = where(mn lt pkm, nmatchedpeaks)
				;
				; for first iteration, make sure we have enough peaks
				; also handle if we have no matched peaks
				if iter eq 0 or nmatchedpeaks eq 0 then begin
					orig_nmp = nmatchedpeaks
					while nmatchedpeaks lt 5 and pkd lt 2. do begin
						;
						; open up the match criterion
						pkd += 0.5
						;
						; try again
						pkm = pkd*refdisp
						matchedpeaks = where(mn lt pkm, nmatchedpeaks)
					endwhile
					;
					; report any adjustments
					if pkd ne ppar.pkdel then begin
						print,''
						print,'Bar: ',b,', pkdel updated to ',pkd, '; ',orig_nmp, $
							' --> ',nmatchedpeaks,' peaks', $
							format='(a,i3,a,f5.2,a,i2,a,i3,a)'
					endif
				endif
			;
			; no matched peaks
			endif else begin
				nmatchedpeaks = 0
				kcwi_print_info,ppar,pre,'No good peaks in arc for Bar # '+strn(b),/warning
			endelse
			;
			if nmatchedpeaks le 0 then begin
				kcwi_print_info,ppar,pre,'No peaks matched in Bar # '+strn(b),/warning
				barstat[b]=9
				goto, errbar
			endif
			;
			; plot matches, if requested
			if ddisplay and iter eq 0 then begin
				for pp = 0,n_elements(twk_spec_cent)-1 do $
					oplot,[twk_spec_cent[pp],twk_spec_cent[pp]], $
						10.^!y.crange,color=colordex('R')
				for pp = 0,nmatchedpeaks-1 do $
					oplot,[twk_spec_cent[matchedpeaks[pp]],twk_spec_cent[matchedpeaks[pp]]], $
						10.^!y.crange,color=colordex('G')
			endif
			;
			if nmatchedpeaks le degree then begin
				print,' '
				kcwi_print_info,ppar,pre,'Not enough peaks matched in Bar # '+strn(b)+': '+strn(nmatchedpeaks),/warning
				barstat[b]=9
				goto, errbar
			endif
			;
			twk_ref_idx = mi[matchedpeaks] mod twk_ref_npks
			twk_spec_idx = matchedpeaks
			;
			targetw = twk_ref_cent[twk_ref_idx]
			;
			; check if subwvals is monotonic (cspline requires this)
			testw = subwvals(sort(subwvals))
			diff = subwvals - testw
			junk = where(diff ne 0., ndiff)
			if ndiff gt 0 then begin
				kcwi_print_info,ppar,pre,'wavelengths not monotonic',/warn
				initx = twk_spec_cent[twk_spec_idx]
			endif else	initx = cspline(subwvals,subxvals,twk_spec_cent[twk_spec_idx])
			;
			; we need reverse coefficients.
			fitdegree = degree < (nmatchedpeaks-1)
			newcoeff = poly_fit(initx,targetw,fitdegree,yfit=fitw)
			;
			; get residual stats
			ims,(targetw - fitw),rmn,rsg,rwgt
			bad = where(rwgt le 0, nbad)
			;
			; reject baddies
			nrej = 0
			while nbad gt 0 do begin
				;
				; count the bad guys
				nrej += nbad
				;
				; overplot them if we are displaying
				if ddisplay and iter eq 0 then begin
					for pp = 0,nbad-1 do $
						oplot,[targetw[bad[pp]],targetw[bad[pp]]], $
							10.^!y.crange,color=colordex('B')
				endif
				;
				; get the good guys
				good = where(rwgt eq 1, nmatchedpeaks)
				;
				; check to be sure we have some left
				if nmatchedpeaks le 0 then begin
					kcwi_print_info,ppar,pre,'All points rejected',/error
					barstat[b]=10
					goto, errbar
				endif
				;
				; clean fit data
				initx = initx[good]
				targetw = targetw[good]
				;
				; re-fit
				fitdegree = degree < (nmatchedpeaks-1)
				newcoeff = poly_fit(initx,targetw,fitdegree,yfit=fitw)
				;
				; check for more baddies
				ims,(targetw - fitw),rmn,rsg,rwgt
				bad = where(rwgt le 0, nbad)
			endwhile
			;
			; record results
			print,string(13B),iter+1,niter,b,nmatchedpeaks,nrej, $
				format='($,a1,"Iteration: ",i2," of ",i2," Bar:",i3,"  Valid peaks:",i3,"  Rejected peaks:",i3)'
			;
			; plot legend and query user
			if ddisplay and iter eq 0 then begin
				kcwi_legend,['Good','NoAtlas','Reject'],linesty=[0,0,0], $
					/bottom,/clear, charthi=th,charsi=si, $
					color=[colordex('G'),colordex('R'),colordex('B')]
				kcwi_legend,['Thr='+string(fix(fthr*100.),form='(i02)')+'%'], $
					/clear,/right, charthi=th,charsi=si
				print,''
				read,'Next? (Q - quit plotting, <cr> - next): ',q
				if strupcase(strmid(q,0,1)) eq 'Q' then ddisplay = (1 eq 0)
			endif
			;
			; store for later
			rwaves[b,0:(nmatchedpeaks-1)] = targetw
			xcents[b,0:(nmatchedpeaks-1)] = initx
			;
			; write out
			if iter eq niter-1 then begin
				for j=0,nmatchedpeaks-1 do $
					printf,al,targetw[j],initx[j],b,format='(f12.3,f15.3,i7)'
			endif
			;
			if nmatchedpeaks ge degree+2 then begin
				twkcoeff[*,b] = 0
				twkcoeff[0:fitdegree,b] = newcoeff
			endif
			;
			; if we get here, bar fit was good
			barstat[b] = 0
			barrej[b] = nrej
			;
			; target for bars with problems
			errbar:
		endfor			; b
		;
		; report iteration
		print,string(13B),format='($,a1)'
		kcwi_print_info,ppar,pre,"Iteration "+string(iter+1,"(i2)")+" of "+ $
			string(niter,"(i2)")+" done.          "
		;
		; now clean each slice of outlying bars
		; don't bother for a single iteration
		if kgeom.bclean and niter gt 1 then begin
			;
			; only go interactive on last iteration
			if iter lt niter-1 then $
				clnplot = 0 $
			else	clnplot = 1
			if ppar.display ge 3 and clnplot then $
				window,2,title='kcwi_clean_coeffs'
			kcwi_clean_coeffs,twkcoeff,degree,ppar,plot=clnplot
			if ppar.display ge 3 and clnplot then $
				wdelete,2
		endif
	endfor; iter
	;
	; done tweaking
	print,''
	print,'Done tweaking: now get final stats.'
endif; tweak
; endtweak
;
; close object line output list
free_lun,al
kcwi_print_info,ppar,pre,'Object lines written to',objlist,format='(a,a)'
;
; use the tweaked coefficients, if asked to.
if keyword_set(tweak) and niter gt 1 then $
	usecoeff = twkcoeff $
else	usecoeff = cntcoeff
;
; if we have tweaked at all let's get the final status
if keyword_set(tweak) then begin
	;
	; loop over the bars
	for b=0,119 do begin
		;
		; get output coeffs
		scoeff = pascal_shift(reform(usecoeff[*,b]),x0,/silent)
		;
		; get ref waves and corresponding object x values
		targetw = rwaves[b,*]
		initx = xcents[b,*]
		;
		; matched peaks are where we have a good ref wave
		matchedpeaks = where(targetw gt 0., nmatchedpeaks)
		if nmatchedpeaks gt 2 then begin
			;
			; get the good ones
			targetw = targetw[matchedpeaks]
			initx = initx[matchedpeaks]
			;
			; ref - observed wavelengths
			finwaves = poly(initx,usecoeff[*,b])
			cmpdiff = targetw-finwaves
			;
			; account for cleaning zero-point wavelength
			mom = moment(cmpdiff)
			cmpdiff = cmpdiff - mom[0]
			;
			; final RMS
			mom = moment(cmpdiff)
			sigmas[b] = sqrt(mom[1])
			;
			; store for later plots
			fwaves[b,0:(nmatchedpeaks-1)] = finwaves
			dwaves[b,0:(nmatchedpeaks-1)] = cmpdiff
			;
			; log stats
			kcwi_print_info,ppar,pre,ftype+' Fit: Bar#,Npks,RMS,Cdisp,Coefs', $
				b,nmatchedpeaks,sigmas[b],usecoeff[1,b],scoeff[0:degree], $
				format='(a,2i5,2x,f7.3,f8.4,f9.2,f8.4,'+strn((degree-1)>1)+'g13.5)'
			;
			; for your viewing pleasure
			if display then begin
				;
				; back to main plot window
				wset,0
				if nasmask then begin
					dwav = (maxwav - minwav)/3.
					xrng = get_plotlims([minwav+dwav,maxwav-dwav],pad=0.3)
				endif else	xrng = [minwav,maxwav]
				yran = get_plotlims(cmpdiff)
				if abs(usecoeff[1,refbar]) gt 0.4 then $
					yrng = [yran[0]<(-0.4),yran[1]>0.4] $
				else	yrng = [yran[0]<(-0.2),yran[1]>0.2]
				plot,finwaves,cmpdiff,psym=4,charsi=si,charthi=th,thick=th, $
					xthick=th,xtitle='Wave(Ang)',xrange=xrng,/xs, $
					ythick=th,ytitle='Resid(Ang)',yrange=yrng,/ys, $
					title=imglab+', Bar: '+string(b,"(i3)") + $
					', Slice: '+string(fix(b/5),"(i2)")
				oplot,!x.crange,[0,0],linestyle=2,color=colordex('blue')
				oplot,!x.crange,[sigmas[b],sigmas[b]], $
					linesty=1,color=colordex('blue')
				oplot,!x.crange,-[sigmas[b],sigmas[b]], $
					linesty=1,color=colordex('blue')
				kcwi_legend,["RMS = "+string(sigmas[b],form='(f7.3)')+' Ang', $
					     'NMatch = '+strn(nmatchedpeaks), $
					     'NRej = '+strn(barrej[b])], $
					     charsi=si,charthi=th
				read,'Next? (Q - quit plotting, <cr> - next): ',q
				if strupcase(strmid(q,0,1)) eq 'Q' then display = (1 eq 0)
			endif	; display
		endif	else $	; nmatchedpeaks gt 2
			kcwi_print_info,ppar,pre,'Not enough matched peaks for bar',b,nmatchedpeaks,/error
	endfor	; loop over bars to get final stats
endif	; have we done any tweaking?
;
; clean up
if ppar.display ge 3 then wdelete,1
;
; our final coefficients
fincoeff = dblarr(9,120)
;
; shift solution reference to pixel 0 instead of center
for b=0,119 do $
	fincoeff[*,b] = pascal_shift(usecoeff[*,b],x0,/silent)
;
; good bars
bargood = where(barstat eq 0, nbargood)
if nbargood le 0 then begin
	kcwi_print_info,'No good bar fits',/error
	kgeom.status = 10
	return
endif
;
; bad bars
barbad = where(barstat gt 0, nbarbad)
if nbarbad gt 0 then $
	sigmas[barbad] = -1.
;
; get wavelength ranges
y0wvs = fltarr(120)	; good wavelengths
y1wvs = fltarr(120)
t0wvs = fltarr(120)	; trim wavelengths
t1wvs = fltarr(120)
for b=0,119 do begin
	wy0 = poly(kgeom.goody0,fincoeff[*,b])
	y0wvs[b] = wy0
	wy1 = poly(kgeom.goody1,fincoeff[*,b])
	y1wvs[b] = wy1
	wt0 = poly(kgeom.trimy0,fincoeff[*,b])
	t0wvs[b] = wt0
	wt1 = poly(kgeom.trimy1,fincoeff[*,b])
	t1wvs[b] = wt1
endfor
;
; for central fit only, all bars must be used
if nasmask then begin
	y0max = max(y0wvs)
	y0min = min(y0wvs)
	y1max = max(y1wvs)
	y1min = min(y1wvs)
	trim0 = min(t0wvs)
	trim1 = max(t1wvs)
endif else begin
	y0max = max(y0wvs[bargood])
	y0min = min(y0wvs[bargood])
	y1max = max(y1wvs[bargood])
	y1min = min(y1wvs[bargood])
	trim0 = min(t0wvs[bargood])
	trim1 = max(t1wvs[bargood])
endelse
;
; check for negative dispersion
if trim0 gt trim1 then begin
	trim0 = min(t1wvs[bargood])
	trim1 = max(t0wvs[bargood])
endif
;
wavgood0 = min([y0max,y1max])
wavgood1 = max([y0min,y1min])
wavall0  = min([y0min,y1min])
wavall1  = max([y0max,y1max])
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
if keyword_set(tweak) then begin
	mom = moment(sigmas[bargood])
	kgeom.avewavesig = mom[0]
	kgeom.stdevwavesig = sqrt(mom[1])
	;
	; log results
	outliers = where(abs(sigmas[bargood]-mom[0]) gt 3.*sqrt(mom[1]), noutliers)
	kcwi_print_info,ppar,pre,'average bar wavelength sigma (Ang)',mom[0],' +- ',sqrt(mom[1]),$
		format='(a,f7.3,a,f7.3)'
	kcwi_print_info,ppar,pre,'min/max bar wavelength sigma (Ang)',minmax(sigmas[bargood]), $
		format='(a,f7.3,2x,f7.3)'
	if noutliers gt 0 then $
		kcwi_print_info,ppar,pre,'> 3sig outlier bars present, may want to tweak ppar.pkdel: Imgnum, Bars', $
			imgnum,outliers,format='(a,i7,2x,'+strn(noutliers)+'i5)',/warning
endif
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
	tweak=tweak,plot_file=plot_file
;
; finally, let's make a diagnostic summary plot
if ppar.display ge 1 then begin
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	tlab = imglab+', '+ftype
	if keyword_set(tweak) then begin
		;
		; get fit rms status
		mom = moment(sigmas[bargood])
		fitrmslab = strtrim(string(mom[0],format='(f9.3)'),2) + ' +- ' + $
		      strtrim(string(sqrt(mom[1]),format='(f9.3)'),2)
		yrngs = get_plotlims(sigmas[bargood])
		!p.multi=[0,1,2]
		plot,sigmas,psym=4,charsi=si,charthi=th,thick=th,title=tlab+' Fit <RMS>: '+fitrmslab, $
			xthick=th,xtitle='Bar #',xrange=[-1,120],/xs, $
			ythick=th,ytitle='RMS (Ang)',yrange=yrngs,/ys
		oplot,!x.crange,[mom[0],mom[0]],linesty=5,thick=th
		oplot,!x.crange,[mom[0]+sqrt(mom[1]),mom[0]+sqrt(mom[1])],linesty=1,thick=th
		oplot,!x.crange,[mom[0]-sqrt(mom[1]),mom[0]-sqrt(mom[1])],linesty=1,thick=th
		if nbarbad gt 0 then $
			oplot,barbad,replicate(mom[0],nbarbad),psym=7,thick=th
		kcwi_oplot_slices
	endif else begin
		yrngd = get_plotlims(disp)
		!p.multi=0
	endelse
	plot,disp,psym=4,charsi=si,charthi=th,thick=th, $
		title=tlab+' Dispersion @ '+string(cwvl,"(f8.2)")+' Ang', $
		xthick=th,xtitle='Bar #',xrange=[-1,120],/xs, $
		ythick=th,ytitle='Ang/pix',yrange=yrngd,/ys
	if nbarbad gt 0 then $
		oplot,barbad,disp[barbad],psym=7
	kcwi_oplot_slices
	if ppar.display ge 2 then read,'next: ',q
endif
!p.multi=0
;
return
end		; kcwi_solve_arcs

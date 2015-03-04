; $Id: kcwi_solve_arcs.pro,v 1.79 2015/01/24 01:08:32 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
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
; TODO:
;	modularize ref line finding so program can tweak pkiso if resid large
;	get sigma of non-tweaked solution with ref lines
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
;-
;
pro kcwi_solve_arcs, specs, kgeom, ppar, tweak=tweak, plot_file=plot_file

pre = 'KCWI_SOLVE_ARCS'
version = repstr('$Revision: 1.79 $ $Date: 2015/01/24 01:08:32 $','$','')
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
; which grating? 
grating = kgeom.gratid
;
; which filter? 
filter = kgeom.filter
;
; image label
imglab = 'Img # '+strn(imgnum)+' Fl: '+strtrim(filter,2)+' Gr: '+strtrim(grating,2)
;
; is this N+S mask in?
nasmask = kgeom.nasmask
;
; central wavelength?
cwvl = kgeom.cwave
;
; canonical resolution?
resolution = kgeom.resolution
;
; log info
kcwi_print_info,ppar,pre,version,/info
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
;
; keep track of observed versus atlas comparison
fwaves = fltarr(120,100)
dwaves = fltarr(120,100)
;
; keep track of reference wavelengths and matched pixel positions
rwaves = fltarr(120,100)
xcents = fltarr(120,100)
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
			mn = min(specs[minrow+sec*div:minrow+(sec+1)*div],mi)
			xv[sec] = mi+minrow+sec*div
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
	pksig = ppar.pksig		; which peaks to find? 
	pkdel = ppar.pkdel*resolution	; peak matching delta required in Angstroms
	pkiso = ppar.pkiso*resolution	; isolation gap between peaks required in Angstroms
	kcwi_print_info,ppar,pre,'Spectral line finding/matching params: PkSig, PkDel, PkIso', $
		pksig,pkdel,pkiso,format='(a,2x,3f7.3)'
	;
	; start with central fit coefficients
	twkcoeff = cntcoeff
	;
	; reference spectrum and wavelengths
	twk_reference_spectrum = refspec
	twk_reference_wavelengths = refwvl
	;
	; first trim to the filter bandpass. 
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
	if ppar.display ge 2 then begin
		xarng = get_plotlims([minwav,maxwav],pad=0.2)
		;
		; if plotting diagnostics, just focus on first iteration (central third)
		if ppar.display ge 3 then begin
			dwav = (maxwav - minwav)/3.
			xarng = get_plotlims([minwav+dwav,maxwav-dwav],pad=0.3)
		endif
		plot,twk_reference_wavelengths,twk_reference_spectrum, title='Atlas Spectrum Lines', $
			thick=th,charsi=si,charthi=th, $
			xthick=th,xtitle='Wave(A)',xrange=xarng,/xs, $
			ythick=th,ytitle='Flux', $
			yrange=[min(twk_reference_spectrum)>10., $
				max(twk_reference_spectrum)+0.05*max(twk_reference_spectrum)],/ys,/ylog
	endif
	;
	; let's find the peaks in the reference spectrum.
	; adjust pksig to get more atlas lines
	twk_ref_cent = clnpeaks(twk_reference_wavelengths,twk_reference_spectrum, $
		(resolution*2.)/refdisp,pkiso/refdisp,pksig*0.1,count=twk_ref_npks, $
		 estimate=resolution)
	;
	if twk_ref_npks eq 0 then begin
		kcwi_print_info,ppar,pre,'No good atlas points found',/error
		kgeom.status=6
		return
	endif
	kcwi_print_info,ppar,pre,'Number of clean atlas lines found',twk_ref_npks,$
		format='(a,i4)'
	;
	if ppar.display ge 2 then begin
		for j=0,twk_ref_npks-1 do begin
			oplot,[twk_ref_cent[j],twk_ref_cent[j]],10.^!y.crange,color=colordex('blue'),thick=1.0
		endfor
		oplot,[minwav,minwav],10.^!y.crange,color=colordex('green'),thick=th,linesty=5
		oplot,[maxwav,maxwav],10.^!y.crange,color=colordex('green'),thick=th,linesty=5
		kcwi_legend,['PeakFit','WavRng'],linesty=[0,5],thick=[1.0,th], $
			color=[colordex('blue'),colordex('green')],/clear,charsi=si,charthi=th
	endif
	;
	; at this point we have the peaks we want in the atlas spectrum. 
	;
	; now pop up a diagnostic window if requested
	ddisplay = (ppar.display ge 3)
	if ddisplay then $
		window,1,title='Object Spectrum'
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
				plot,subwvals,subyvals, title='Object Spectrum Lines: Bar '+strn(b), $
					thick=th,charsi=si,charthi=th, $
					xthick=th,xtitle='Wave(A)',xrange=xarng,/xs, $
					ythick=th,ytitle='Flux', $
					yrange=[min(subyvals)>1.,max(subyvals)+0.05*max(subyvals)],/ys,/ylog
			endif
			;
			; JDN (2014-MAR-28) sez:
			; You might be tempted to replace this section with
			; the modular routine clnpeaks.pro, but this will
			; slow things down considerably, so don't.
			twk_spec_pks = peaks(subyvals,pksig,count=twk_spec_npks)
			if twk_spec_npks le 0 then begin
				kcwi_print_info,ppar,pre,'Not enough spec peaks for bar ',b, $
					format='(a,i6)',/warning
				barstat[b]=7
				goto, errbar
			endif
			if ddisplay and iter eq 0 then $
				oplot,subwvals[twk_spec_pks],subyvals[twk_spec_pks],psym=6,symsi=2.5
			;
			twk_spec_valid = bytarr(twk_spec_npks)+1b
			npksig = twk_spec_npks
			;
			; find peaks that are too close to other peaks. 
			one = fltarr(twk_spec_npks)+1.0
			rows = one##subwvals[twk_spec_pks]
			cols = subwvals[twk_spec_pks]##one
			diff = abs(rows-cols)
			badpks = where(diff lt pkiso and diff gt 0, nbadpks)
			if nbadpks gt 0 then twk_spec_valid[badpks mod twk_spec_npks] = 0b
			; and reject the peaks that are too close.
			qgoodpks = where(twk_spec_valid eq 1, ngoodpeaks)
			if ngoodpeaks eq 0 then begin
				kcwi_print_info,ppar,pre,'Not enough good spec peaks for bar ',b, $
					format='(a,i6)',/warning
				barstat[b]=7
				goto, errbar
			endif
			twk_spec_npks = ngoodpeaks
			twk_spec_pks = twk_spec_pks[qgoodpks]
			twk_spec_valid = twk_spec_valid[qgoodpks]
			if ddisplay and iter eq 0 then $
				oplot,subwvals[twk_spec_pks],subyvals[twk_spec_pks],psym=4,symsi=2.0
			;
			; set up variables
			twk_spec_cent = dblarr(ngoodpeaks)
			twk_spec_width = dblarr(ngoodpeaks)
			delta = abs(fix(2.5*kgeom.resolution/twkcoeff[1,b]))
			; now loop over the peaks that are good and fit them
			for pk=0,ngoodpeaks-1 do begin
				subspec = subyvals[twk_spec_pks[pk]-delta>0:twk_spec_pks[pk]+delta<nrows-1]
				subwave = subwvals[twk_spec_pks[pk]-delta>0:twk_spec_pks[pk]+delta<nrows-1]
				res = mpfitpeak(subwave,subspec,a,nterms=5,estimate=[subyvals[twk_spec_pks[pk]], $
						subwvals[twk_spec_pks[pk]],0.6], /silent)
				twk_spec_cent[pk] = a[1]
				twk_spec_width[pk] = a[2]
			endfor				; pk
			;
			; keep lines with widths within one sigma of average
			twk_spec_moment = moment(twk_spec_width)
			sigmawid = abs(twk_spec_width-twk_spec_moment[0])/sqrt(twk_spec_moment[1])
			goodsigma = where(sigmawid lt 1.0, ngoodsigma)
			;
			; is anyone left?
			if ngoodsigma eq 0 then begin
				kcwi_print_info,ppar,pre,'No good spec points for bar ',b, $
					format='(a,i6)',/warning
				barstat[b]=8
				goto, errbar
			endif
			;
			if ddisplay and iter eq 0 then begin
				;print,'n, width, sigwid, cwave'
				;forprint,indgen(n_elements(twk_spec_width)),twk_spec_width,sigmawid,twk_spec_cent,format='(i4,3f9.3)'
				oplot,subwvals[twk_spec_pks[goodsigma]],subyvals[twk_spec_pks[goodsigma]],psym=2,symsi=1.5
			endif
			twk_spec_npks = ngoodsigma
			twk_spec_pks = twk_spec_pks[goodsigma]
			twk_spec_cent = twk_spec_cent[goodsigma]
			twk_spec_width = twk_spec_width[goodsigma]
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
			pkm = pkdel
			matchedpeaks = where(mn lt pkm, nmatchedpeaks)
			;
			; for first iteration, make sure we have enough peaks
			; also if we have no matched peaks
			if iter eq 0 or nmatchedpeaks eq 0 then begin
				orig_nmp = nmatchedpeaks
				while nmatchedpeaks lt 5 and pkm lt pkiso do begin
					;
					; open up the match criterion
					pkm = pkm + 0.25
					;
					; try again
					matchedpeaks = where(mn lt pkm, nmatchedpeaks)
				endwhile
				;
				; report any adjustments
				if pkm ne pkdel then begin
					print,''
					print,'Bar: ',b,', pkdel updated to ',pkm, '; ',orig_nmp, $
						' --> ',nmatchedpeaks,' peaks', $
						format='(a,i3,a,f5.2,a,i2,a,i3,a)'
				endif
			endif
			; print,"matched "+string(nmatchedpeaks)+" peaks."
			if nmatchedpeaks le 0 then begin
				kcwi_print_info,ppar,pre,'No peaks matched in Bar # '+strn(b)+': '+strn(nmatchedpeaks),/warning
				barstat[b]=9
				goto, errbar
			endif
			print,string(13B),iter+1,niter,b,nmatchedpeaks, $
				format='($,a1,"Iteration: ",i2," of ",i2," Bar:",i3,"  Valid peaks:",i3)'
			;
			; plot matches, if requested
			if ddisplay and iter eq 0 then begin
				for pp = 0,n_elements(twk_spec_cent)-1 do $
					oplot,[twk_spec_cent[pp],twk_spec_cent[pp]], $
						10.^!y.crange,color=colordex('R')
				for pp = 0,nmatchedpeaks-1 do $
					oplot,[twk_spec_cent[matchedpeaks[pp]],twk_spec_cent[matchedpeaks[pp]]], $
						10.^!y.crange,color=colordex('G')
				kcwi_legend,['PkSig','PkIso','Wid'],psym=[6,4,2],symsi=[2.5,2.0,1.5], $
					charthi=th,charsi=si,box=0
				kcwi_legend,['PkDel','Fail'],linesty=[0,0],/bottom,/clear, $
					color=[colordex('G'),colordex('R')],charthi=th,charsi=si
				kcwi_legend,['NPkSig: '+strn(npksig),'NPkDel: '+strn(nmatchedpeaks)],/right, $
					charthi=th,charsi=si,/bottom,/clear
				;print,'n, del, cwave'
				;forprint,indgen(n_elements(mn)),mn,twk_spec_cent,form='(i4,2x,2f9.3)'
				print,''
				read,'Next? (Q - quit plotting, <cr> - next): ',q
				if strupcase(strmid(q,0,1)) eq 'Q' then ddisplay = (1 eq 0)
			endif
			;
			if nmatchedpeaks le 2 then begin
				kcwi_print_info,ppar,pre,'Not enough peaks matched in Bar # '+strn(b)+': '+strn(nmatchedpeaks),/warning
				barstat[b]=9
				goto, errbar
			endif
			;
			twk_ref_idx = mi[matchedpeaks] mod twk_ref_npks
			twk_spec_idx = matchedpeaks
			;
			targetw = twk_ref_cent[twk_ref_idx]
			initx = cspline(subwvals,subxvals,twk_spec_cent[twk_spec_idx])
			;
			; store for later
			rwaves[b,0:(nmatchedpeaks-1)] = targetw
			xcents[b,0:(nmatchedpeaks-1)] = initx
			;
			; we need reverse coefficients.
			fitdegree = degree < (nmatchedpeaks-1)
			newcoeff = poly_fit(initx,targetw,fitdegree)
			;
			if nmatchedpeaks ge degree+2 then begin
				twkcoeff[*,b] = 0
				twkcoeff[0:fitdegree,b] = newcoeff
			endif
			;
			; if we get here, bar fit was good
			barstat[b] = 0
			;
			; target for bars with problems
			errbar:
		endfor			; b
		;
		; report iteration
		print,string(13B),format='($,a1)'
		kcwi_print_info,ppar,pre,"Iteration "+string(iter+1,"(i2)")+" of "+ string(niter,"(i2)")+" done."
		;
		; now clean each slice of outlying bars
		; don't bother for a single iteration
		if ppar.cleancoeffs and niter gt 1 then begin
			;
			; only go interactive on last iteration
			if iter lt niter-1 then $
				clnplot = 0 $
			else	clnplot = 1
			wset,0
			kcwi_clean_coeffs,twkcoeff,degree,ppar,plot=clnplot
		endif
	endfor; iter
	;
	; done tweaking
	print,''
	print,'Done tweaking: now get final stats.'
endif; tweak
; endtweak
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
				kcwi_legend,["RMS = "+string(sigmas[b],form='(f7.3)')+' Ang'], $
					charsi=si,charthi=th
				read,'Next? (Q - quit plotting, <cr> - next): ',q
				if strupcase(strmid(q,0,1)) eq 'Q' then display = (1 eq 0)
			endif	; display
		endif	else $	; nmatchedpeaks gt 2
			kcwi_print_info,ppar,pre,'not enough matched peaks',nmatchedpeaks,/error
	endfor	; loop over bars to get final stats
endif	; have we done any tweaking?
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
		kcwi_print_info,ppar,pre,'> 3sig outlier bars present, may want to tweak ppar.pkiso: Imgnum, Bars', $
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
kgeom.progid = pre + ' ' + version
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

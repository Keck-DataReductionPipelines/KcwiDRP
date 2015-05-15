; $Id$
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SOLVE_THAR
;
; PURPOSE:
;	Solve the wavelength solutions for each bar of the ThAr arc spectrum
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SOLVE_THAR, Specs, Kgeom, Ppar
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
;-
;
pro kcwi_solve_thar, specs, kgeom, ppar, tweak=tweak, plot_file=plot_file

pre = 'KCWI_SOLVE_THAR'
version = repstr('$Revision$ $Date$','$','')
q=''
;
; do we want to display stuff? 
display = (ppar.display ge 2)

deepcolor
if ppar.display ge 1 then begin
	!p.multi=0
	!p.background=colordex('white')
	!p.color=colordex('black')
	th=2.0
	si=2.0
endif
;
; set some system parameters -- these may need to be populated
;                               differently later
pix = 0.0150d		; pixel size in mm
ybin = kgeom.ybinsize	; binning in spectral direction
fcam = 305.0d		; focal length of camera in mm
gamma = 4.0d		; mean out-of-plane angle for diffraction.
;
; which image number
imgnum = kgeom.arcimgnum
imglab = 'Img # '+strn(imgnum)
;
; which grating? 
grating = kgeom.gratid
;
; which filter? 
filter = kgeom.filter
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
; any anomolous tilt in the grating?
gratanom = kgeom.gratanom
;
; log info
kcwi_print_info,ppar,pre,version,/info
kcwi_print_info,ppar,pre,'img, grat, filt, nasmsk, cwave', $
	imgnum,grating,filter,nasmask,cwvl, $
	format='(a,i6,2x,a-8,2x,a-8,i4,f12.3)'
;
; for tweaking: array of border fractions to ignore.
if nasmask then $
	edgespace = [ 0.34 ] $
else	edgespace = [ 0.25, 0.18, 0.12, 0.06, 0.00]
;
; we will be initially using a third degree fit to the peak positions:
degree = 3
;
; set the grating specific parameters
rho = kgeom.rho
slant = kgeom.slant
lastdegree = kgeom.lastdegree
;
; which is the reference bar?
refbar = kgeom.refbar
;
; load the reference atlas spectrum.
rdfits1dspec,kgeom.refspec,refwvl,thar, $
	wavezero=refw0, deltawave=refdisp, refpix=refpix
refspec = thar>0  
;
; we want to degrade this spectrum to the instrument resolution... for
; CWI this is, roughly, 1A FWHM
xx = findgen(99)-50.0d
fwhm = resolution/refdisp
gaus = gaussian(xx,[1.0,0.0,fwhm/2.355])
gaus /= total(gaus)
refspec = convolve(refspec,gaus)
;
; Next we refine the central dispersion estimate
specs = double(specs)
specsz = size(specs,/dim)
;
; 1- compute the prelim angle of diffraction
prelim_beta = asin(cwvl/10000.0 * rho/2.0)+slant/!radeg
;
; 1b - add the grating tilt anomoly
prelim_beta = prelim_beta + gratanom/!radeg
;
; 2- compute the preliminary dispersion
prelim_disp = cos(prelim_beta)/rho/fcam*(pix*ybin)*1e4
; the 1e4 is there to make the units Angstrom/binnedpixel
;
; redo this for the MEDREZ grating which is a surface profile grating
if strtrim(strupcase(grating),2) eq 'MEDREZ' then begin
	; preliminary beta
	prelim_alpha = -((-264500.0)-kgeom.gratpos)/2000.0
	prelim_beta = 60. - prelim_alpha
	prelim_disp = ((-cos(prelim_beta/!radeg)) / $
		(rho * fcam ) ) * (pix*ybin)*1e4
endif
;
; need to correct for the out-of-band angle here... not much, but
; there is some... so
;
prelim_disp *= cos(gamma/!radeg)
kcwi_print_info,ppar,pre,'Initial calculated dispersion (A/binned pixel)', $
	prelim_disp,format='(a,f8.3)'
;
; 3- generate an index array with its 0 point at the center of the
;    detector 
x0 = specsz[0]/2
xvals = dindgen(specsz[0])-x0
;
; 4- Pick out the central third of the detector in the spectral 
;    direction.  This is where the dispersion is linear and is the
;    best place to try to cross-correlate the reference bar spectrum
;    with the ThAr spectrum to refine the shift.
;
; the min and max row to use for the adjustment
minrow = (1*specsz[0])/3
maxrow = (2*specsz[0])/3
;
; the corresponding preliminary wavelengths
prelim_wvl = cwvl + xvals*prelim_disp
prelim_minwvl = min( [prelim_wvl[minrow],prelim_wvl[maxrow]] )
prelim_maxwvl = max( [prelim_wvl[minrow],prelim_wvl[maxrow]] )
;
; now we have to interpolate the bar spectrum to the same scale as the
; ThAr spectrum.
;
; subspectrum to interpolate and subindex to interpolate from
prelim_spec = reform(specs[minrow:maxrow,refbar])
prelim_xvals = xvals[minrow:maxrow]
prelim_subwvl = prelim_wvl[minrow:maxrow]
;
; determine the wavelengths to interpolate to and extract the relevant
; ThAr portion
qwvl = where(refwvl gt prelim_minwvl and refwvl lt prelim_maxwvl, nqwvl)
if nqwvl eq 0 then begin
	kcwi_print_info,ppar,pre,'Did not find ThAr data to match to',/error
	kgeom.status=3
	return
endif
;
prelim_refspec = refspec[qwvl]
prelim_refwvl = refwvl[qwvl]
;
; and interpolate
prelim_intspec = interpol(prelim_spec,prelim_subwvl,prelim_refwvl,/spline)
;
; check for scattered light problems
mmm,prelim_intspec,skymod,skysig
if skymod gt 0. and skysig gt 0. and skymod-2.*skysig gt 0. then begin
	prelim_intspec = prelim_intspec - (skymod-2.*skysig)
	kcwi_print_info,ppar,pre,'subtracting scattered light offset of', $
		(skymod-2.*skysig),format='(a,f9.3)'
endif
;
if ppar.display ge 2 then begin
	plot,prelim_subwvl,prelim_spec/max(prelim_spec),charsi=si,charthi=th,thick=th, $
		xthick=th, xtitle='Wave(A)', $
		ythick=th, ytitle='Rel. Flux',title=imglab+', No Offset',/xs
	oplot,prelim_refwvl,prelim_refspec/max(prelim_refspec),color=colordex('red'),thick=th
	legend,['Ref Bar ('+strn(refbar)+')','Atlas'],linesty=[0,0],thick=[3,3],box=0, $
		color=[colordex('black'),colordex('red')],charsi=si,charthi=th
	read,'next: ',q
endif
;
; now we have two spectra we can try to cross-correlate
; (prelim_intspec and prelim_refspec), so let's do that:
kcwi_xspec,prelim_intspec,prelim_refspec,ppar,prelim_offset,prelim_value, $
	/min,/shift,/plot,label='Obj(0) vs Atlas(1)'
;
if ppar.display ge 2 then begin
	plot,prelim_subwvl,prelim_spec/max(prelim_spec),charsi=si,charthi=th,thick=th, $
		xthick=th,xtitle='Wave(A)', $
		ythick=th,ytitle='Rel. Flux',title=imglab+', Offset = ' + $
		strtrim(string(prelim_offset,form='(f9.3)'),2)+' px',/xs
	oplot,prelim_refwvl+prelim_offset*refdisp,prelim_refspec/max(prelim_refspec), $
		color=colordex('red'),thick=th
	legend,['Ref Bar ('+strn(refbar)+')','Atlas'],linesty=[0,0],thick=[th,th],box=0, $
		color=[colordex('black'),colordex('red')],charsi=si,charthi=th
	read,'next: ',q
endif
;
; At this point we have the offsets between bars and the approximate offset from
; the reference bar to the actual spectrum and the approximate
; dispersion. 
;
; let's populate the 0 points array. 
p0 = cwvl + kgeom.baroff*prelim_disp - prelim_offset * refdisp
;
; next we are going to brute-force scan around the preliminary
; dispersion for a better solution. We will wander 5% away from it. 
;
;we will try nn values
;max_ddisp = 0.025d	; fraction (0.05 equiv to 5%)
max_ddisp = 0.05d	; fraction (0.05 equiv to 5%)
nn = (fix((1+max_ddisp)*max_ddisp*abs(prelim_disp)/refdisp*(maxrow-minrow)/2.0))>10<25
delta = (nn/10)<3>2	; may want to adjust this more?
kcwi_print_info,ppar,pre,'N disp. samples: ',nn
;
; which are:
disps = prelim_disp * ( 1.0d + max_ddisp * (dindgen(nn+1)-double(nn)/2.0d)*2.0d/double(nn))
;
; make sure degree makes sense
if degree lt 2 then degree = 2 
if degree gt 6 then degree = 6
kcwi_print_info,ppar,pre,'Using fits of degree',degree,/info
;
;containers for output values
maxima = dblarr(nn+1)
shifts = maxima
maxidx = dindgen(nn+1)
;
; containers for bar-specific values
bardisp = dblarr(120)
barshift = dblarr(120)
;
; data coefficients 
coeff = dblarr(9)
dcoeff = dblarr(9)
;
; coefficients for intermediate transformation
intcoeff = dblarr(9,120)
;
; coefficients for the final transformation
fincoeff = intcoeff
;
; keep track of final sigmas
sigmas = dblarr(120)
means  = dblarr(120)
;
; loop over bars
for b = 0,119 do begin
	;
	; now loop over the dispersions...
	for d = 0, nn do begin
		;
		; populate the coefficients
		coeff[0] = p0[b]
		coeff[1] = disps[d]
		beta = acos(coeff[1]/(pix*ybin)*rho*fcam*1d-4)
		coeff[2] = -(pix*ybin/fcam)^2*sin(beta)/2.0d/rho*1d4
		coeff[3] = -(pix*ybin/fcam)^3*cos(beta)/6.0d/rho*1d4
		coeff[4] = (pix*ybin/fcam)^4*sin(beta)/24.0d/rho*1d4
		coeff[5] = (pix*ybin/fcam)^5*sin(beta)/120.0d/rho*1d4
		coeff[6] = -(pix*ybin/fcam)^6*sin(beta)/720.0d/rho*1d4
		coeff[7] = -(pix*ybin/fcam)^7*sin(beta)/5040.0d/rho*1d4
		coeff[8] = (pix*ybin/fcam)^8*sin(beta)/40320.0d/rho*1d4
		; what degree did we specify? 
		if degree lt 8 then coeff[degree+1:*] = 0.0d
		; for this initial solution we
		; really don't need a higher
		; than 3rd degree...
		coeff[4:*] = 0.0d
		;
		; what are the minimum and maximum wavelengths to consider 
		; for the bar?
		subxvals = xvals[minrow:maxrow]
		subspec = reform(specs[minrow:maxrow,b])
		;
		minwvl = min( [poly(xvals[minrow],coeff), poly(xvals[maxrow],coeff)] )
		maxwvl = max( [poly(xvals[minrow],coeff), poly(xvals[maxrow],coeff)] )
		;
		; where will we need to interpolate to cross correlate? 
		qwvl = where(refwvl gt minwvl and refwvl lt maxwvl, nqwvl)
		if nqwvl eq 0 then begin
			kcwi_print_info,ppar,pre,'Insufficient reference wavelengths',/err
			kgeom.status=4
			return
		endif
		;
		subrefwvl = refwvl[qwvl]
		subrefspec = refspec[qwvl]
		;
		; adjust the spectra
		waves = poly(subxvals,coeff)
		;
		; calculate derivatives
		;for j=0,8 do dcoeff[j] = double(j)*coeff[j]
		;dcoeff = shift(dcoeff,-1)
		;derivs = poly(subrefwvl,dcoeff)
		;
		; interpolate the bar spectrum
		intspec = interpol(subspec,waves,subrefwvl,/spline)
		;intspec /= derivs
		;
		; get a label
		xslab = 'Bar '+strn(b)+', '+strn(d)+'/'+strn(nn)+', Dsp = '+string(disps[d],form='(f6.3)')
		; 
		; cross correlate the interpolated spectrum with the atlas spectrum
		kcwi_xspec,intspec,subrefspec,ppar,soffset,svalue, $
			/mean,/shift,/pad,/central,plot=display,label=xslab
		;
		maxima[d] = double(svalue)/total(subrefspec)/total(intspec)
		shifts[d] = soffset
		;
	endfor  ; d 
	;
	; now find the max of the
	; cross-correlation scan and determine
	; what the corresponding shift and
	; dispersion are
	mx = max(maxima,mi)
	;
	submax = maxima[mi-delta>0:mi+delta<nn]
	submaxidx = maxidx[mi-delta>0:mi+delta<nn]
	;
	; fit parabola to peak
	res = poly_fit(submaxidx,submax,2,yfit=yf)
	pk = -res[1]/2.0/res[2]
	;yf = mpfitpeak(submaxidx,submax,a,nterms=4)
	;
	; centroid
	pkc = cntrd1d(submaxidx,submax)
	;
	; derivative peak finder
	; thresh=0.75 means just major peaks
	pkd = pkfind(maxima,nzeros,thresh=0.75)
	;
	; check for more than one local maximum
	if nzeros gt 1 then begin
		diff = abs(pkd - (n_elements(maxima)-1)/2.)
		;
		; pick most central maximum
		gpkd = where(diff eq min(diff))
		pkd = pkd[gpkd]
	endif else if nzeros eq 1 then $
		pkd = pkd[0] $
	else	pkd = pk	; use parabola fit when nzeros eq 0
	;
	barshift[b] = interpol(shifts,maxidx,pkd,/spline) * refdisp
	bardispp = interpol(disps,maxidx,pk,/spline)
	bardispc = interpol(disps,maxidx,pkc,/spline)
	bardispd = interpol(disps,maxidx,pkd,/spline)
	bardisp[b] = bardispd
	;
	coeff[0] = p0[b]-barshift[b]
	coeff[1] = bardisp[b]
	beta = acos(coeff[1]/(pix*ybin)*rho*fcam*1d-4)
	coeff[2] = -(pix*ybin/fcam)^2*sin(beta)/2.0d/rho*1d4
	coeff[3] = -(pix*ybin/fcam)^3*cos(beta)/6.0d/rho*1d4
	coeff[4] = (pix*ybin/fcam)^4*sin(beta)/24.0d/rho*1d4
	coeff[5] = (pix*ybin/fcam)^5*sin(beta)/120.0d/rho*1d4
	coeff[6] = -(pix*ybin/fcam)^6*sin(beta)/720.0d/rho*1d4
	coeff[7] = -(pix*ybin/fcam)^7*sin(beta)/5040.0d/rho*1d4
	coeff[8] = (pix*ybin/fcam)^8*sin(beta)/40320.0d/rho*1d4
	; what degree did se specify?
	if degree lt 8 then coeff[degree+1:*]=0.0d
	; again... for the prelim solution, we don't need higher than 3rd
	coeff[4:*] = 0.0d
	intcoeff[*,b] = coeff
	scoeff = pascal_shift(coeff,x0,/silent)
	;
	kcwi_print_info,ppar,pre,'Initial Fit: Bar#,Cdisp,Coefs',b,bardisp[b],scoeff[0:3], $
		format='(a,i4,2x,f8.4,f9.2,f8.4,2g13.5)'
	;
	if display then begin
		yrng = get_plotlims(maxima)
		plot,disps,maxima,psym=-4,charsi=si,charthi=th,thick=th, $
			xthick=th,xtitle='Central Dispersion',/xs, $
			ythick=th,ytitle='X-Corr Value',yrange=yrng,/ys, $
			title=imglab+', Bar: '+string(b,"(i3)") + $
			', Slice: '+string(fix(b/5),"(i2)")
		oplot,disps[submaxidx],yf,color=colordex('orange'),thick=th
		oplot,[bardispp,bardispp],!y.crange,color=colordex('green'),thick=th
		oplot,[bardispc,bardispc],!y.crange,color=colordex('blue'),thick=th
		oplot,[bardispd,bardispd],!y.crange,color=colordex('red'),thick=th*2.
		legend,['Parab: '+string(bardispp,form='(f7.4)'), $
			'Cntrd: '+string(bardispc,form='(f7.4)'), $
			'DERIV: '+string(bardispd,form='(f7.4)')], $
			linesty=[0,0,0],thick=[th,th,th*2.],box=0, $
			color=[colordex('green'),colordex('blue'),colordex('red')], $
			charsi=si,charthi=th
		read,'Next? (Q-quit plotting, <cr> - next): ',q
		if strupcase(strmid(q,0,1)) eq 'Q' then display = (1 eq 0)
	endif
endfor	; b
;
; now clean each slice of outlying bars
if ppar.cleancoeffs then $
	kcwi_clean_coeffs,intcoeff,degree,ppar
;
; recover display state
display = (ppar.display ge 2)
;
if nasmask then begin
	; minrow and maxrow (also defined above)
	minrow = (1*specsz[0])/3
	maxrow = (2*specsz[0])/3
endif else begin
	lastrow = 50
	minrow = lastrow
	maxrow = specsz[0]-lastrow-1
endelse				; no nasmask
;
; find wavelength range
mnwvs = fltarr(120)
mxwvs = fltarr(120)
for b=0,119 do begin
	waves = poly(xvals,intcoeff[*,b])
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
	kcwi_print_info,ppar,pre,'using '+strn(n_elements(edgespace))+' iterations to tweak higher order terms'
	;
	pksig = ppar.pksig		; which peaks to find? 
	pkdel = ppar.pkdel*resolution	; peak matching delta required in Angstroms
	pkiso = ppar.pkiso*resolution	; isolation gap between peaks required in Angstroms
	kcwi_print_info,ppar,pre,'Spectral line finding/matching params: PkSig, PkDel, PkIso', $
		pksig,pkdel,pkiso,format='(a,2x,3f7.3)'
	;
	; coefficients we will be tweaking.
	twkcoeff = intcoeff
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
	twk_ref_cent = clnpeaks(twk_reference_wavelengths,twk_reference_spectrum, $
		(resolution*2.)/refdisp,pkiso/refdisp,pksig,count=twk_ref_npks, $
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
			if ppar.verbose ge 2 then $
				print,twk_ref_cent[j]
			oplot,[twk_ref_cent[j],twk_ref_cent[j]],10.^!y.crange,color=colordex('blue'),thick=1.0
		endfor
		oplot,[minwav,minwav],10.^!y.crange,color=colordex('green'),thick=th,linesty=5
		oplot,[maxwav,maxwav],10.^!y.crange,color=colordex('green'),thick=th,linesty=5
		legend,['PeakFit','WavRng'],linesty=[0,5],thick=[1.0,th], $
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
	niter = n_elements(edgespace)
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
				kcwi_print_info,ppar,pre,'not enough good spec peaks!',/error
				kgeom.status=7
				wset,0
				return
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
				kcwi_print_info,ppar,pre,'No good spec points found',/error
				kgeom.status=8
				wset,0
				return
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
			;
			; plot matches, if requested
			if ddisplay and iter eq 0 then begin
				for pp = 0,n_elements(twk_spec_cent)-1 do $
					oplot,[twk_spec_cent[pp],twk_spec_cent[pp]], $
						10.^!y.crange,color=colordex('R')
				for pp = 0,nmatchedpeaks-1 do $
					oplot,[twk_spec_cent[matchedpeaks[pp]],twk_spec_cent[matchedpeaks[pp]]], $
						10.^!y.crange,color=colordex('G')
				legend,['PkSig','PkIso','Wid'],psym=[6,4,2],symsi=[2.5,2.0,1.5], $
					charthi=th,charsi=si,box=0
				legend,['PkDel','Fail'],linesty=[0,0],/bottom,/clear, $
					color=[colordex('G'),colordex('R')],charthi=th,charsi=si
				legend,['NPkSig: '+strn(npksig),'NPkDel: '+strn(nmatchedpeaks)],/right, $
					charthi=th,charsi=si,/bottom,/clear
				;print,'n, del, cwave'
				;forprint,indgen(n_elements(mn)),mn,twk_spec_cent,form='(i4,2x,2f9.3)'
				print,''
				read,'Next? (Q-quit plotting, <cr> - next): ',q
				if strupcase(strmid(q,0,1)) eq 'Q' then ddisplay = (1 eq 0)
			endif
			; print,"matched "+string(nmatchedpeaks)+" peaks."
			if nmatchedpeaks le 2 then begin
				kcwi_print_info,ppar,pre,'Not enough peaks matched in Bar # '+strn(b)+': '+strn(nmatchedpeaks),/error
				kgeom.status=9
				wset,0
				return
			endif
			;
			twk_ref_idx = mi[matchedpeaks] mod twk_ref_npks
			twk_spec_idx = matchedpeaks
			;
			targetw = twk_ref_cent[twk_ref_idx]
			initx = cspline(subwvals,subxvals,twk_spec_cent[twk_spec_idx])
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
			; if it is the last iteration, then do some stuff here. 
			if iter eq niter-1 then begin
				scoeff = pascal_shift(reform(newcoeff),x0,/silent)
				finwaves = poly(initx,twkcoeff[*,b])
				cmpwaves = targetw
				cmpdiff = targetw-finwaves
				mom = moment(cmpdiff)
				sigmas[b] = sqrt(mom[1])
				means[b]  = mom[0]
				if nasmask then $
					xrng = [minwav+(maxwav-minwav)*0.30,minwav+(maxwav-minwav)*0.72] $
				else	xrng = [minwav,maxwav]
				kcwi_print_info,ppar,pre,'Tweaked Fit: Bar#,Npks,Sig,Cdisp,Coefs', $
					b,nmatchedpeaks,sqrt(mom[1]),newcoeff[1],scoeff, $
					format='(a,2i5,2x,f7.3,f8.4,f9.2,f8.4,'+strn((degree-1)>1)+'g13.5)'
				if display then begin
					if abs(twkcoeff[1,refbar]) gt 0.4 then $
						yrng = [-0.4,0.4] $
					else	yrng = [-0.2,0.2]
					if niter gt 1 then wset,0
					plot,finwaves,cmpdiff,psym=4,charsi=si,charthi=th,thick=th, $
						xthick=th,xtitle='Wave(Ang)',xrange=xarng,/xs, $
						ythick=th,ytitle='Resid(Ang)',yrange=yrng,/ys, $
						title=imglab+', Bar: '+string(b,"(i3)") + $
						', Slice: '+string(fix(b/5),"(i2)")
					oplot,!x.crange,[0,0],linestyle=2,color=colordex('blue')
					oplot,!x.crange,[means[b]+sigmas[b],means[b]+sigmas[b]], $
						linesty=1,color=colordex('blue')
					oplot,!x.crange,[means[b]-sigmas[b],means[b]-sigmas[b]], $
						linesty=1,color=colordex('blue')
					legend,["MEAN = "+string(means[b],form='(g10.4)')+' Ang', $
						"SIG = "+string(sigmas[b],form='(f7.3)')+' Ang'], $
						charsi=si,charthi=th
					read,'Next? (Q-quit plotting, <cr> - next): ',q
					if strupcase(strmid(q,0,1)) eq 'Q' then display = (1 eq 0)
				endif
			endif else $		; last iter
				print,string(13B),iter+1,niter,b,nmatchedpeaks, $
					format='($,a1,"Iteration: ",i2," of ",i2," Bar:",i3,"  Valid peaks:",i3)'
		endfor			; b
		;
		; report iteration
		if iter lt niter-1 then $
			print,string(13b),format='($,a1)'
		kcwi_print_info,ppar,pre,"Iteration "+string(iter+1,"(i2)")+" of "+ string(niter,"(i2)")+" done."
		;
		; now clean each slice of outlying bars
		if ppar.cleancoeffs then begin
			;
			; only go interactive on last iteration
			if iter lt niter-1 then $
				clnplot = 0 $
			else	clnplot = 1
			wset,0
			kcwi_clean_coeffs,twkcoeff,degree,ppar,plot=clnplot
		endif
	endfor; iter
endif; tweak
; endtweak
;
; use the tweaked coefficients, if asked to.
if keyword_set(tweak) and niter gt 1 then $
	usecoeff = twkcoeff $
else	usecoeff = intcoeff
;
; shift solution reference to x0 instead of center
for b=0,119 do $
	fincoeff[*,b] = pascal_shift(usecoeff[*,b],x0,/silent)
;
; now let's make some plots!
wset,0
!p.multi=0
if keyword_set(plot_file) then begin
	psfile,plot_file
	kcwi_print_info,ppar,pre,'plotting diagnostics to: '+plot_file+'.ps'
endif
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')

!p.multi=[0,1,2]
;
; get fit rms status
if keyword_set(tweak) then begin
	mom = moment(sigmas)
	fitrmslab = strtrim(string(mom[0],format='(f9.3)'),2) + ' +- ' + $
	      strtrim(string(sqrt(mom[1]),format='(f9.3)'),2)
endif
;
; first plot sigmas and/or initial values
;
; full-ccd tweak
if keyword_set(tweak) and niter gt 1 then begin 
	yrng = get_plotlims(sigmas)
	mom = moment(sigmas)
	kgeom.avewavesig = mom[0]
	kgeom.stdevwavesig = sqrt(mom[1])
	outliers = where(abs(sigmas-mom[0]) gt 3.*sqrt(mom[1]), noutliers)
	plot,sigmas,psym=4, $
		title=imglab+', Tweaked Fit Sigmas: ' + fitrmslab, $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle='Sigma (Ang)',yrange=yrng,/ys
	oplot,!x.crange,[mom[0],mom[0]],linesty=5,thick=th
	oplot,!x.crange,[mom[0]+sqrt(mom[1]),mom[0]+sqrt(mom[1])],linesty=1,thick=th
	oplot,!x.crange,[mom[0]-sqrt(mom[1]),mom[0]-sqrt(mom[1])],linesty=1,thick=th
	kcwi_oplot_slices
	yrng = get_plotlims(twkcoeff[1,*])
	plot,twkcoeff[1,*],psym=4, $
		title=imglab+', Tweaked Central Dispersion', $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle='Ang/pix',yrange=yrng,/ys
	kcwi_oplot_slices
	kcwi_print_info,ppar,pre,'average bar wavelength sigma (Ang)',mom[0],' +- ',sqrt(mom[1]),$
		format='(a,f7.3,a,f7.3)'
	kcwi_print_info,ppar,pre,'min/max bar wavelength sigma (Ang)',minmax(sigmas), $
		format='(a,f7.3,2x,f7.3)'
	if noutliers gt 0 then $
		kcwi_print_info,ppar,pre,'> 3sig outlier bars present, may want to tweak ppar.pkiso: Imgnum, Bars', $
			imgnum,outliers,format='(a,i7,2x,'+strn(noutliers)+'i5)',/warning
;
; N&S tweak with a single iter, just to get fit sigmas
endif else if keyword_set(tweak) and niter le 1 then begin
	yrng = get_plotlims(sigmas)
	mom = moment(sigmas)
	kgeom.avewavesig = mom[0]
	kgeom.stdevwavesig = sqrt(mom[1])
	outliers = where(abs(sigmas-mom[0]) gt 3.*sqrt(mom[1]), noutliers)
	plot,sigmas,psym=4, $
		title=imglab+', Fit Sigmas: ' + fitrmslab, $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle='Sigma (Ang)',yrange=yrng,/ys
	oplot,!x.crange,[mom[0],mom[0]],linesty=5,thick=th
	oplot,!x.crange,[mom[0]+sqrt(mom[1]),mom[0]+sqrt(mom[1])],linesty=1,thick=th
	oplot,!x.crange,[mom[0]-sqrt(mom[1]),mom[0]-sqrt(mom[1])],linesty=1,thick=th
	kcwi_oplot_slices
	yrng = get_plotlims(intcoeff[1,*])
	plot,intcoeff[1,*],psym=4, $
		title=imglab+', Initial Central Dispersion', $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle='Ang/pix',yrange=yrng,/ys
	kcwi_oplot_slices
	kcwi_print_info,ppar,pre,'average bar wavelength sigma (Ang)',mom[0],' +- ',sqrt(mom[1]),$
		format='(a,f7.3,a,f7.3)'
	kcwi_print_info,ppar,pre,'min/max bar wavelength sigma (Ang)',minmax(sigmas), $
		format='(a,f7.3,2x,f7.3)'
	if noutliers gt 0 then $
		kcwi_print_info,ppar,pre,'> 3sig outlier bars present, may want to tweak ppar.pkiso: Imgnum, Bars', $
			imgnum,outliers,format='(a,i7,2x,'+strn(noutliers)+'i5)',/warning
;
; no tweaking at all
endif else begin
	yrng = get_plotlims(intcoeff[0,*])
	plot,intcoeff[0,*],psym=4, $
		title=imglab+', Initial Central Zero point', $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle='Ang',yrange=yrng,/ys
	kcwi_oplot_slices
	yrng = get_plotlims(intcoeff[1,*])
	plot,intcoeff[1,*],psym=4, $
		title=imglab+', Initial Central Dispersion', $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle='Ang/pix',yrange=yrng,/ys
	kcwi_oplot_slices
endelse 
;
; next plot fit coeff's
if not keyword_set(plot_file) then $
	read,'next: ',q
for i=0,degree do begin
	yrng = get_plotlims(fincoeff[i,*])
	if keyword_set(tweak) and niter gt 1 then $
		tlab = imglab + ', Tweaked Coef '+strn(i) $
	else	tlab = imglab + ', Initial Coef '+strn(i)
	plot,fincoeff[i,*],psym=4,title=tlab, $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle='Value',yrange=yrng,/ys
	kcwi_oplot_slices
	if i mod 2 eq 1 and not keyword_set(plot_file) then $
		read,'next: ',q
endfor
;
if not keyword_set(plot_file) then $
	!p.multi=0 $
else	!p.multi  = [0,1,2]
;
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

y0max = max(y0wvs)
y0min = min(y0wvs)
y1max = max(y1wvs)
y1min = min(y1wvs)
trim0 = min(t0wvs)
trim1 = max(t1wvs)
;
; check for negative dispersion
if trim0 gt trim1 then begin
	trim0 = min(t1wvs)
	trim1 = max(t0wvs)
endif

wavgood0 = min([y0max,y1max])
wavgood1 = max([y0min,y1min])
wavall0  = min([y0min,y1min])
wavall1  = max([y0max,y1max])

if nasmask then begin
	delwav = maxwav - minwav
	maxwav = minwav + delwav * 0.72
	minwav = minwav + delwav * 0.30
endif

if display or keyword_set(plot_file) then begin
	for b=0,119 do begin
		cf = intcoeff[*,b]
		waves = poly(xvals,cf)
		ts = where(waves ge wavgood0 and waves le wavgood1)
		ta = where(refwvl ge wavgood0 and refwvl le wavgood1)
		yrng = get_plotlims(specs[ts,b],/minzero)
		fac = max(refspec[ta])/max(specs[ts,b])
		plot,refwvl,refspec/fac,thick=th,charthi=th, $
			title=imglab + ', Bar = '+string(b,"(i03)") + $
			', Slice = '+string(fix(b/5),"(i02)"), $
			xthick=th,xtitle='Wavelength (A)',xrange=[minwav,maxwav],/xs, $
			ythick=th,ytitle='Flux',yrange=yrng,/ys
		oplot,waves,specs[*,b],color=colordex('red'),thick=1.0
		oplot,[wavgood0,wavgood0],!y.crange,color=colordex('green'),linesty=1,thick=th
		oplot,[wavgood1,wavgood1],!y.crange,color=colordex('green'),linesty=1,thick=th
		oplot,[wavall0,wavall0],!y.crange,color=colordex('orange'),linesty=1,thick=th
		oplot,[wavall1,wavall1],!y.crange,color=colordex('orange'),linesty=1,thick=th
		legend,['ThAr Atlas','Initial Arc Fit'],linesty=[0,0],charthi=th, $
			color=[colordex('black'),colordex('red')], $
			thick=[th,th],/clear,clr_color=!p.background
		;
		if keyword_set(tweak) and niter gt 1 then begin
			cf = twkcoeff[*,b]
			waves = poly(xvals,cf)
			plot,refwvl,refspec/fac,thick=th,charthi=th, $
				title=imglab+', Bar = '+string(b,"(i03)") + $
				', Slice = '+string(fix(b/5),"(i02)") + $
				', Sigma = '+string(sigmas[b],"(f5.3)"), $
				xthick=th,xtitle='Wavelength (A)',xrange=[minwav,maxwav],/xs, $
				ythick=th,ytitle='Flux',yrange=yrng,/ys
			oplot,waves,specs[*,b],color=colordex('blue'),thick=1.0
			oplot,[wavgood0,wavgood0],!y.crange,color=colordex('green'),linesty=1,thick=th
			oplot,[wavgood1,wavgood1],!y.crange,color=colordex('green'),linesty=1,thick=th
			oplot,[wavall0,wavall0],!y.crange,color=colordex('orange'),linesty=1,thick=th
			oplot,[wavall1,wavall1],!y.crange,color=colordex('orange'),linesty=1,thick=th
			legend,['ThAr Atlas','Tweaked Arc Fit'],linesty=[0,0],charthi=th, $
				color=[colordex('black'),colordex('blue')], $
				thick=[th,th],/clear,clr_color=!p.background
		endif
		if not keyword_set(plot_file) then begin
			read,'Next? (Q-quit plotting, <cr> - next): ',q
			if strupcase(strmid(q,0,1)) eq 'Q' then $
				display = (1 eq 0)
		endif
	endfor		;b
endif			;display or plot_file

if keyword_set(plot_file) then begin
	psclose
endif
;
; now let's update kgeom struct
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
;
; make sure wavelengths are on fiducial scale and
; zeropoint is integer number of bins from reference wl (3000A)
ndels = long((trim0 - kgeom.wavefid)/kgeom.dwout)
kgeom.wave0out = kgeom.wavefid + float(ndels) * kgeom.dwout
ndels = long((trim1 - kgeom.wavefid)/kgeom.dwout)
kgeom.wave1out = kgeom.wavefid + float(ndels) * kgeom.dwout
;
; log values
kcwi_print_info,ppar,pre,'min,max good wavelengths',wavgood0,wavgood1, $
	format='(a,2f9.2)'
kcwi_print_info,ppar,pre,'min,max inclusive wavelengths',wavall0,wavall1, $
	format='(a,2f9.2)'
kcwi_print_info,ppar,pre,'min,max trim wavelengths',kgeom.wave0out,kgeom.wave1out, $
	format='(a,2f9.2)'
;
; now update all the coeffs and calculate effective dispersion
disp = dblarr(120)
for b=0,119 do begin
	;
	; update coeffs in kgeom struct
	dcoeff = fincoeff[*,b]
	kcwi_apply_coeffs,kgeom,b,dcoeff
	;
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
; finally, let's plot the dispersion at some fixed wavelength
; as a function of bar.
if ppar.display ge 1 then begin
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	if keyword_set(tweak) then begin
		if niter gt 1 then $
			tlab = imglab+', Tweaked' $
		else	tlab = imglab+','
		yrngd = get_plotlims(disp)
		yrngs = get_plotlims(sigmas)
		!p.multi=[0,1,2]
		plot,sigmas,psym=4,charsi=si,charthi=th,thick=th,title=tlab+' Fit Sigmas: '+fitrmslab, $
			xthick=th,xtitle='Bar #',xrange=[-1,120],/xs, $
			ythick=th,ytitle='Sigma (Ang)',yrange=yrngs,/ys
		oplot,!x.crange,[mom[0],mom[0]],linesty=5,thick=th
		oplot,!x.crange,[mom[0]+sqrt(mom[1]),mom[0]+sqrt(mom[1])],linesty=1,thick=th
		oplot,!x.crange,[mom[0]-sqrt(mom[1]),mom[0]-sqrt(mom[1])],linesty=1,thick=th
		kcwi_oplot_slices
	endif else begin
		tlab = imglab+', Initial'
		yrngd = get_plotlims(disp)
		!p.multi=0
	endelse
	plot,disp,psym=4,charsi=si,charthi=th,thick=th, $
		title=tlab+' Dispersion @ '+string(cwvl,"(f8.2)")+' Ang', $
		xthick=th,xtitle='Bar #',xrange=[-1,120],/xs, $
		ythick=th,ytitle='Ang/pix',yrange=yrngd,/ys
	kcwi_oplot_slices
	if ppar.display ge 2 then read,'next: ',q
endif
!p.multi=0
;
; if we get here, the fit is at least workable
kgeom.status=0
return
end		; kcwi_solve_thar

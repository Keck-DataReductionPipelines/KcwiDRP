; $Id: kcwi_fit_center.pro,v 1.7 2015/01/24 01:08:31 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_FIT_CENTER
;
; PURPOSE:
;	Solve the wavelength solutions for the central third of
;	each bar of the arc spectrum using a fourth degree
;	polynomial approximation
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_FIT_CENTER, Specs, Kgeom, Ppar, Centcoeff
;
; INPUTS:
;	Specs	- a array of arc spectra produced by KCWI_EXTRACT_ARCS
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Centcoeff - coefficients of central fit (4th degree)
;
; INPUT KEYWORDS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Matt Matuszewski
;	2014-SEP-18	Initial Revision
;-
;
pro kcwi_fit_center, specs, kgeom, ppar, centcoeff

pre = 'KCWI_FIT_CENTER'
version = repstr('$Revision: 1.7 $ $Date: 2015/01/24 01:08:31 $','$','')
q=''
;
; log info
kcwi_print_info,ppar,pre,version,/info
;
; do we want to display stuff? 
display = (ppar.display ge 2)
ddisplay = (ppar.display ge 3)

if ppar.display ge 1 then begin
	deepcolor
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
; central wavelength?
cwvl = kgeom.cwave
;
; canonical resolution?
resolution = kgeom.resolution
;
; any anomolous tilt in the grating?
gratanom = kgeom.gratanom
;
; set the grating specific parameters
rho = kgeom.rho
slant = kgeom.slant
;
; which is the reference bar?
refbar = kgeom.refbar
;
; we will be using a third degree fit here
degree = 4
kcwi_print_info,ppar,pre,'Using polynomial approximation of degree',degree,/info
;
; load the reference atlas spectrum.
kcwi_read_atlas,kgeom,ppar,refspec,refwvl,refdisp
;
; make sure spectrum is double precision
specs = double(specs)
;
; get dimensions
specsz = size(specs,/dim)
;
; coefficients for central region fit
centcoeff = dblarr(9,120)
;
; Next we refine the central dispersion estimate
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
;
; report results
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
; atlas spectrum.
;
; subspectrum to interpolate and subindex to interpolate from
prelim_spec = reform(specs[minrow:maxrow,refbar])
prelim_xvals = xvals[minrow:maxrow]
prelim_subwvl = prelim_wvl[minrow:maxrow]
;
; determine the wavelengths to interpolate to and extract the relevant
; atlas portion
qwvl = where(refwvl gt prelim_minwvl and refwvl lt prelim_maxwvl, nqwvl)
if nqwvl eq 0 then begin
	kcwi_print_info,ppar,pre,'Did not find atlas data to match to',/error
	kgeom.status=2
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
	kcwi_legend,['Ref Bar ('+strn(refbar)+')','Atlas'],linesty=[0,0],thick=[3,3],box=0, $
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
	kcwi_legend,['Ref Bar ('+strn(refbar)+')','Atlas'],linesty=[0,0],thick=[th,th],box=0, $
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
;containers for output values
maxima = dblarr(nn+1)
shifts = maxima
maxidx = dindgen(nn+1)
dspstat = intarr(nn+1)
;
; containers for bar-specific values
bardisp = dblarr(120)
barshift = dblarr(120)
barstat = intarr(120)
;
; data coefficients 
coeff = dblarr(9)
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
		if nqwvl le 0 then begin
			kcwi_print_info,ppar,pre,'Insufficient reference wavelengths',/err
			kgeom.status=3
			return
		endif
		;
		subrefwvl = refwvl[qwvl]
		subrefspec = refspec[qwvl]
		;
		; adjust the spectra
		waves = poly(subxvals,coeff)
		;
		; interpolate the bar spectrum
		intspec = interpol(subspec,waves,subrefwvl,/spline)
		;
		; get a label
		xslab = 'Bar '+strn(b)+', '+strn(d)+'/'+strn(nn)+', Dsp = '+string(disps[d],form='(f6.3)')
		; 
		; cross correlate the interpolated spectrum with the atlas spectrum
		kcwi_xspec,intspec,subrefspec,ppar,soffset,svalue,status=status, $
			/min,/shift,/pad,/central,plot=ddisplay,label=xslab
		;
		maxima[d] = double(svalue)/total(subrefspec)/total(intspec)
		shifts[d] = soffset
		dspstat[d] = status
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
	; check status of cross-correlations
	if total(dspstat) gt (nn+1)/2 then $
		barstat[b] = 1
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
	centcoeff[*,b] = coeff
	scoeff = pascal_shift(coeff,x0,/silent)
	;
	kcwi_print_info,ppar,pre,'Central Fit: Bar#,Cdisp,Coefs',b,bardisp[b],scoeff[0:3], $
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
		kcwi_legend,['Parab: '+string(bardispp,form='(f7.4)'), $
			'Cntrd: '+string(bardispc,form='(f7.4)'), $
			'DERIV: '+string(bardispd,form='(f7.4)')], $
			linesty=[0,0,0],thick=[th,th,th*2.],box=0, $
			color=[colordex('green'),colordex('blue'),colordex('red')], $
			charsi=si,charthi=th
		if ppar.display ge 3 then $
			read,'Next? (Q - quit plotting, D - diagnostic plots, <cr> - next): ',q $
		else	read,'Next? (Q - quit plotting, <cr> - next): ',q
		if strupcase(strmid(q,0,1)) eq 'Q' then display = (1 eq 0)
		if strupcase(strmid(q,0,1)) eq 'D' then ddisplay = (1 eq 1)
	endif
endfor	; b
;
; now clean each slice of outlying bars
if ppar.cleancoeffs then $
	kcwi_clean_coeffs,centcoeff,degree,ppar
;
; let's check the status of the fits
if total(barstat) gt n_elements(barstat)/2 then begin
	kcwi_print_info,ppar,pre,'Too many bar fit failures', $
		fix(total(barstat)),/error
	kgeom.status = 4
endif else	kgeom.status = 0
;
return
end		; kcwi_fit_center

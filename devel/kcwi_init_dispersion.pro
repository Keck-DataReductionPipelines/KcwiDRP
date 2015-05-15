; $Id$
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_INIT_DISPERSION
;
; PURPOSE:
;	Derive initial estimate of spectral dispersion.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_INIT_DISPERSION,Spec, Kgeom
;
; INPUTS:
;	Spec	- a array of arc spectra produced by KCWI_EXTRACT_ARCS
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Updates Kgeom.rbcoeffs to contain the reference bar wavelength coeffs
;
; PROCEDURE:
;
; EXAMPLE:
;	Define the geometry from a 'cbars' image and use it to extract and 
;	display the spectra from an 'arc' image from the same calibration
;	sequence.
;
;	cbars = mrdfits('image7142_int.fits',0,chdr)
;	kcwi_trace_cbars,cbars,Kgeom,/centroid
;	arc = mrdfits('image7140_int.fits',0,ahdr)
;	kcwi_extract_arcs,arc,kgeom,arcspec,ppar
;	kcwi_init_dispersion,arcspec,kgeom,ppar
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-26	Initial Revision
;	2013-AUG-17	Use window for each plot, plot using /ylog
;	2013-SEP-15	Use ppar to pass parameters
;	2013-DEC-15	Uses kcwi_sort_atlas to indicate good atlas lines
;-
;
pro kcwi_init_dispersion, spec, kgeom, ppar, help=help
;
; startup
pre = 'KCWI_INIT_DISPERSION'
version = repstr('$Revision$ $Date$','$','')
q = ''
;
; check inputs
if n_params(0) lt 3 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', ArcSpec, Kgeom, Ppar'
	return
endif
;
; check spec
ssz = size(spec)
if ssz[0] ne 2 or ssz[2] ne 120 then begin
	print,pre+': Error - Input spec array malformed, run KCWI_EXTRACT_ARCS first.'
	return
endif
;
; Check structs
if kcwi_verify_geom(kgeom,/init) ne 0 then return
if n_params(0) lt 3 then $
	ppar = { kcwi_ppar }
if kcwi_verify_ppar(ppar) ne 0 then return
;
; check Ref Bar number
if kgeom.refbar lt 0 or kgeom.refbar gt 119 then begin
	kcwi_print_info,ppar,pre,'Reference bar number out of range (0-119)', $
		kgeom.refbar,/error
	return
endif
barno = kgeom.refbar
;
; number of bars
nbars = ssz[2]
;
; number of pixels in each spectrum
npspec = ssz[1]
;
; get half-width for spectral lines
if kgeom.halfwidth ge 0 and kgeom.halfwidth lt 10 then $
	hw = kgeom.halfwidth $
else	hw = 3
;
; get resolution sigma
if kgeom.resolution ge 0. and kgeom.resolution lt 10. then $
	res = kgeom.resolution $
else	res = 0.5
;
; get fit order
if kgeom.nasmask eq 1 then $
	fito = 2 $
else	fito = 3
;
; read reference spectrum
while not file_test(kgeom.refspec) do begin
	print,pre+': Error reference spectrum not found: ',kgeom.refspec
	read,'Enter full file spec for reference spectrum: ',q
	kgeom.refspec = strtrim(q,2)
endwhile
rdfits1dspec,kgeom.refspec,refw,reflxo,deltawave=refdw
refhw = fix(1.5 * res / refdw)
;
; smooth to appropriate spectral resolution
reflx = gaussfold(refw,reflxo,res)
;
; set params
pksig = 1.5
pkiso = 0.75
pkgap = 2.0
;
; get line list
while not file_test(kgeom.reflist) do begin
	print,pre+': Error reference line list not found: ',kgeom.reflist
	read,'Enter full file spec for reference line list: ',q
	kgeom.reflist = strtrim(q,2)
endwhile
;readcol,kgeom.reflist,atlas_w,atlas_id,format='d,a',/silent
;
; set up plot
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=2
si=1.75
device,get_screen_size=ss
;
; initial central wavelength
wc0 = kgeom.cwave
;
; start over here
redo:
;
; wavelength range
cfg = kcwi_read_cfg(kgeom.arcfname)
kcwi_print_cfgs,cfg,/header
wave0 = wc0 - kgeom.wavran/2.
wave1 = wc0 + kgeom.wavran/2.
xrng=[0,npspec-1]
rrng=[wave0,wave1]
xspec = lindgen(npspec)
;
; check for nod-and-shuffle
if kgeom.nasmask eq 1 then begin
	;
	; update ranges
	pxchop = fix( npspec / 3.16 )
	xrng = [pxchop,npspec-pxchop]
	wrange = (wave1 - wave0)/3.
	rrng = [wave0+wrange,wave1-wrange]
	;
	; label
	naslab = ' (NAS Mask)'
	;
	; get intensity limit for plot
	tspec = spec[xrng[0]:xrng[1],barno]
	maxspec = max(tspec)
	mmm,tspec,sky,skysig
	intlim = sky-skysig*0.25 > 1.0
endif else begin
	maxspec = max(spec[*,barno])
	naslab = ''
	intlim = 1.0
endelse
;
; extract spectrum range of interest
yvals=reform(spec[xrng[0]:xrng[1],barno])
;
good = where(refw gt rrng[0] and refw lt rrng[1], ngood)
if ngood le 0 then begin
	kcwi_print_info,ppar,pre,'The atlas spectrum is lacking in data in wavelength range',/error
	return
endif
ref_spec = reflx[good]
ref_wave = refw[good]
;
; now find isolated peaks in reference spectrum
ref_cent = clnpeaks(ref_wave,ref_spec,2./refdw,pkgap/refdw,pksig,count=ref_npks)
;
; plot reference spectrum
window,0,xpos=0,ypos=ss[1]-300,xsize=1200,ysize=300,title='Atlas Spectrum'
plot,refw,reflx,thick=th,xthick=th,ythick=th, charsi=si,charthi=th, $
	xran=rrng,xtitle='ATLAS Wave',xstyle=1, ytitle='Int', /ylog, $
	title='ThAr ATLAS' + naslab
oplot,ref_cent,replicate(3.*10.^!y.crange[0],n_elements(ref_cent)),psym=1
;
; plot ref bar spectrum
window,1,xpos=0,ypos=ss[1]-625,xsize=1200,ysize=300, $
	title='Arc Bar '+strn(barno)+' Spectrum'
plot,spec[*,barno]>intlim,thick=th,xthick=th,ythick=th, charsi=si,charthi=th, $
	xran=xrng,xtitle='OBJ Pixel',xstyle=1, ytitle='Int (e-)', /ylog
;
; check range
read,'Enter new central wave or <cr> to continue: ',q
if strlen(q) gt 0 then begin
	wc0 = float(q)
	kgeom.cwave = wc0
	goto,redo
endif
;
; set up input vectors
xob = fltarr(100) - 9.
;
; we need at least 5 points for a good estimate of the dispersion
print,'Click on five or more bright, isolated lines in OBJ spectrum'
print,'Be sure each corresponds to a marked line (+) in ATLAS spectrum'
print,'Click outside plot range to finish:'
minpts = 5
;
; get nice isolated lines
done = (1 eq 0)
lines = 0
print,'OBJ line centroid (pixels): '
while not done and lines lt 100 do begin
	cursor,x,y,/up,/data
	y = alog10(y)
	if x lt !x.crange[0] or x gt !x.crange[1] or $
	   y lt !y.crange[0] or y gt !y.crange[1] then begin
		read,'Finished marking object lines? (Y/n): ',q
		if strupcase(strmid(q,0,1)) eq 'N' then begin
			print,'resuming...'
		endif else begin
			done = (1 eq 1)
		endelse
	endif else begin
		xpix = fix(x)
		if xpix - hw lt 0 or xpix + hw ge npspec then begin
			print,'Avoid edges, resuming...'
		endif else begin
			xx = findgen( 2*hw+1 ) + float(xpix-hw)
			xob[lines] = cntrd1d(xx,spec[xpix-hw:xpix+hw,barno])
			print,xob[lines]
			oplot,[xob[lines],xob[lines]],10.^!y.crange,linesty=1
			xyouts,xob[lines],10.^!y.crange[1],strn(lines+1), $
				charsi=si,charthi=th
			lines += 1
		endelse
	endelse
endwhile
;
; check results
if lines lt minpts then begin
	print,pre+': Error - not enough lines marked, try again.'
	goto, redo
endif
;
; trim xobs vector
xob = xob[0:(lines-1)]
;
; sort xobs vector
xob = xob[sort(xob)]
;
; indicate we are done
legend,['DONE',''],charsi=si,charthi=th,box=0,/right
;
; re-set plot window to reference
wset,0
plot,refw,reflx,thick=th,xthick=th,ythick=th, charsi=si,charthi=th, $
	xran=rrng,xstyle=1,/nodata,/noerase, /ylog
;
; get reference wavelengths for nice isolated lines
print,'Click on same line(s) in the ATLAS spectrum, in any order'
wref = fltarr(lines)
wrcn = fltarr(lines)
rlines = 0
print,'Ln#     Cntrd Wl   ATL Wl   Id:'
while rlines lt lines do begin
	cursor,x,y,/up,/data
	if x lt 0 or y lt 0 then begin
		print,'No match, resuming...'
	endif else begin
		;
		; get the centroid in the ref spectrum
		woff = abs( refw - x )
		w = where(woff eq min(woff))
		wcen = refw[w-refhw:w+refhw]
		fcen = reflx[w-refhw:w+refhw]
		;
		; peak up
		p = where(fcen eq max(fcen))
		w = (w[0]-refhw) + p[0]
		;
		; new window
		wcen = refw[w-refhw:w+refhw]
		fcen = reflx[w-refhw:w+refhw]
		;
		; initial guess
		xcen = cntrd1d(wcen,fcen)
		;
		; fit peak
		est = [max(fcen), xcen, res, 1.]
		yfit = mpfitpeak(wcen,fcen,a,nterms=4,estimate=est, $
			error=sqrt(fcen),chisq=chi2,dof=dof,perror=sig)
		redchi = chi2/float(dof)
		if finite(redchi) eq 1 and redchi lt 200. and $
			redchi gt 0. and sig[1] lt 99. and sig[1] gt 0. and $
			a[2] le res*1.25 then begin
			xcen = a[1]
		endif else print,'Fit failed, using centroid.'
		;
		; now compare to atlas wavelengths
		woff = abs( ref_cent - xcen )
		w = where(woff eq min(woff))
		;
		; make sure we have a real line
		if woff[w] gt 1 then begin
			print,'No match, resuming...'
		;
		; it's real so record it
		endif else begin
			print,rlines+1,xcen,ref_cent[w],$
			format='(i3,2x,2f11.4)'
			wref[rlines] = ref_cent[w]
			wrcn[rlines] = xcen
			oplot,[wref[rlines],wref[rlines]],10.^!y.crange, $
				linesty=1
			xyouts,wref[rlines],10.^!y.crange[1],'X', $
				charsi=si,charthi=th
			rlines += 1
		endelse
	endelse
endwhile
wdelete,0,1
;
; sort
wref = wref[sort(wrcn)]
wrcn = wrcn[sort(wrcn)]
;
; initial fit
coefs = polyfit(xob,wrcn,fito,yfit)
print,coefs
diff = wrcn-yfit
mom=moment(diff)
reslab = ['Wavelength RMS: '+strtrim(string(sqrt(mom[1]),format='(f8.4)'),2), $
	  'Fit Wave0     : '+strtrim(string(coefs[0],format='(f10.3)'),2), $
	  'Fit Dispersion: '+strtrim(string(coefs[1],format='(f9.4)'),2) ]
print,reslab
;
waves = poly(xspec,coefs)
;
; get new set of peaks
bar_cent = clnpeaks(waves,spec[*,barno],2.0/coefs[1],pkgap/coefs[1],pksig, $
		count=bar_npks)
if bar_npks le 0 then begin
	kcwi_print_info,ppar,pre,'No ref bar peaks found',/error
	return
endif
;
; now match them
one_ref = fltarr(ref_npks)+1.0
one_bar = fltarr(bar_npks)+1.0
diff = abs( (bar_cent##one_ref) - (one_bar##ref_cent) )
mn = min(diff,dim=1,mi)
;
pkm = pkiso
matchedpeaks = where(mn lt pkm, nmatchedpeaks)
orig_nmp = nmatchedpeaks
while nmatchedpeaks lt 5 and pkm lt pkgap do begin
	;
	; open up match criterion
	pkm = pkm + 025
	;
	; try again
	matchedpeaks = where(mn lt pkm, nmatchedpeaks)
endwhile
if pkm ne pkiso then begin
	print,'pkiso updated to ',pkm,': ',orig_nmp, $
		' --> ',nmatchedpeaks,' peaks', $
		format='(a,f5.2,a,i2,a,i2,a)'
endif
if nmatchedpeaks le 0 then begin
	kcwi_print_info,ppar,pre,'No peaks matched!',/error
	return
endif
;
ref_idx = mi[matchedpeaks] mod ref_npks
bar_idx = matchedpeaks
;
wrcn = ref_cent[ref_idx]
xob = cspline(waves,xspec,bar_cent[bar_idx])
;
; re-fit coefs
fito += 1
coefs = polyfit(xob,wrcn,fito,yfit)
diff = wrcn - yfit
;
; plot results
yrng = [ min(diff)<(-1), max(diff)> 1 ]
plot,wrcn,diff,thick=th,xthick=th,ythick=th,charsi=si,charthi=th, $
	psym=4,xtitle='Wavelength(A)',ytitle='OBS - FIT(A)',yran=yrng
oplot,!x.crange,[0,0],linesty=1
legend,reslab,charsi=si,charthi=th,box=0
;
; look for baddies
nrej = 0
g = where(abs(diff) lt max(abs(diff)), ng)
;
; mark the bad guy
bad = where(abs(diff) eq max(abs(diff)))
oplot,wrcn[bad],diff[bad],thick=th,psym=1
;
; query status
read,'Reject Bad guy? (Y/n): ',q
if strupcase(strmid(q,0,1)) ne 'N' then $
	do_rej = ( 1 eq 1) $
else	do_rej = ( 1 eq 0)
;
; enter reject cycle
while do_rej do begin
	;
	; get the good guys
	xob = xob[g]
	wrcn = wrcn[g]
	;
	; refit
	coefs = polyfit(xob,wrcn,fito,yfit)
	print,coefs
	;
	; calculate results
	diff = wrcn-yfit
	mom = moment(diff)
reslab = ['Wavelength RMS: '+strtrim(string(sqrt(mom[1]),format='(f8.4)'),2), $
	  'Fit Wave0     : '+strtrim(string(coefs[0],format='(f10.3)'),2), $
	  'Fit Dispersion: '+strtrim(string(coefs[1],format='(f9.4)'),2) ]
	print,reslab
	;
	; plot results
	yrng = [ min(diff)<(-1), max(diff)> 1 ]
	plot,wrcn,diff,thick=th,xthick=th,ythick=th,charsi=si,charthi=th, $
		psym=4,xtitle='Wavelength(A)',ytitle='OBS - FIT(A)',yran=yrng
	oplot,!x.crange,[0,0],linesty=1
	legend,reslab,charsi=si,charthi=th,box=0
	;
	; new baddies?
	g = where(abs(diff) lt max(abs(diff)),ng)
	;
	nrej += 1
	;
	; mark the bad guy
	bad = where(abs(diff) eq max(abs(diff)))
	oplot,wrcn[bad],diff[bad],thick=th,psym=1
	;
	; query status
	read,'Reject Bad guy? (Y/n): ',q
	if strupcase(strmid(q,0,1)) ne 'N' then $
		do_rej = ( 1 eq 1) $
	else	do_rej = ( 1 eq 0)
endwhile
;
; report reject results
print,'N rejected: ',nrej
;
; query status
read,'Fit OK? (Y/n): ',q
if strupcase(strmid(q,0,1)) eq 'N' then begin
	print,pre+': Warning - bad fit, retrying...'
	goto, redo
endif
;
; record final results
for i=0,n_elements(reslab)-1 do $
	kcwi_print_info,ppar,pre,reslab[i]
;
; update Kgeom
kgeom.rbcoeffs[0:fito] = coefs
;
; Kgeom timestamp
kgeom.timestamp = systime(1)
;
return
end

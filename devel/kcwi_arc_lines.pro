;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_ARC_LINES
;
; PURPOSE:
;	Extract a list of pixel positions and corresponding wavelengths
;	for the given arc bar spectrum.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_ARC_LINES,Spec, Kgeom, Barno, Pixpos, WAtlas, WCntrd
;
; INPUTS:
;	Ppar	- KCWI_PPAR pipeline parameters struct
;	Spec	- a array of arc spectra produced by KCWI_EXTRACT_ARCS
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Barno	- which bar spectrum to get coordinates for (0 - 119)
;
; INPUT KEYWORDS:
;	FITORDER- order of polynomial fit to wavelengths
;	PXWINDOW- size of exclusion window around each line in pixels
;	SIGTHRESH-sky sigma factor to cut for starting line list
;
; OUTPUTS:
;	Pixpos	- the pixel position of each line
;	WAtlas	- the atlas line list wavelength that is the closest match
;	WCntrd	- the centroid from the reference spectrum
;
; SIDE EFFECTS:
;	None.
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
;	kcwi_extract_arcs,arc,kgeom,arcspec
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-25	Initial Revision
;	2013-NOV-21	Added ppar to parameter list
;	2013-DEC-15	Re-work to use atlas line list
;-
;
pro kcwi_arc_lines,ppar,spec,kgeom,barno,pixpos,watlas,wcntrd, $
	fitorder=fitorder, pxwindow=pxwindow, sigthresh=sigthresh, $
	help=help
;
; startup
pre = 'KCWI_ARC_LINES'
version = repstr('$Revision: v0.2.10-40-g60c9d44 $ $Date: Fri May 15 10:35:48 2015 -0700 $','$','')
q = ''
;
; initial values
pixpos = [-1.]
watlas = [-1.]
wcntrd = [-1.]
;
; check inputs
if n_params(0) lt 5 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', Ppar, ArcSpec, Kgeom, Barno, Pixpos, WAtlas, WCntrd'
	return
endif
;
; check spec
ssz = size(spec)
if ssz[0] ne 2 or ssz[2] ne 120 then begin
	kcwi_print_info,ppar,pre,'input spec array malformed, run KCWI_EXTRACT_ARCS first.',/error
	return
endif
;
; Check Kgeom
if kcwi_verify_geom(kgeom,/init) ne 0 then return
;
; check Barno
if barno lt 0 or barno gt 119 then begin
	kcwi_print_info,ppar,pre,'bar number out of range (0-119)',barno,/error
	return
endif
;
; keywords
do_peaks = (ppar.display ge 3)
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
; fit order
if keyword_set(fitorder) then $
	fito = fitorder $
else	fito = 3
;
; pixel window for line searches
if keyword_set(pxwindow) then $
	pxwin = pxwindow $
else	pxwin = 50
pxwin = 3
;
pksig = 1.5
pkiso = 0.75
pkgap = 2.5
;
; threshhold for lines
if keyword_set(sigthresh) then $
	sigth = sigthresh $
else	sigth = 10.
;
; get bar offset
baroff = kgeom.baroff[barno]
;
; read reference spectrum
rdfits1dspec,!KCWI_DATA+'thar.fits',refw,reflxo, $
	wavezero=refw0, deltawave=refdw, refpix=refpix
refhw = fix(1.5 * res / refdw)
;
; smooth to appropriate spectral resolution
reflx = gaussfold(refw,reflxo,res)
;
; get initial central wavelength
wc0 = kgeom.cwave
wave0 = wc0 - kgeom.wavran/2.
wave1 = wc0 + kgeom.wavran/2.
;
; get line list
xrng=[0,npspec-1]
rrng=[wave0,wave1]
xspec=findgen(npspec)
if kgeom.nasmask eq 1 then begin
	pxchop = fix( npspec / 3.16 )
	xrng = [pxchop,npspec-pxchop]
	xspec = findgen((xrng[1]-xrng[0])+1) + xrng[0]
	wrange = (wave1 - wave0)/3.
	rrng = [wave0+wrange,wave1-wrange]
endif
yspec=reform(spec[xrng[0]:xrng[1],barno])
;
; get good atlas lines
;
; trim to wavelength range
good = where(refw gt rrng[0] and refw lt rrng[1], ngood)
if ngood le 0 then begin
	kcwi_print_info,ppar,pre,'The atlas specturm is lacking in data in wavelength range',/error
	return
endif
ref_spec = reflx[good]
ref_wave = refw[good]
;
; now find isolated peaks in reference spectrum
ref_pks = isopeaks(ref_spec,pkgap/refdw,pksig,count=ref_npks)
if ref_npks eq 0 then begin
	kcwi_print_info,ppar,pre,'not enough good atlas peaks',/error
	return
endif
;
; centroid the peaks we find
ref_cent = dblarr(ref_npks)
ref_width= dblarr(ref_npks)
delta = fix(2.0/refdw)
for j=0,ref_npks-1 do begin
	subspec = ref_spec[ref_pks[j]-delta>0:ref_pks[j]+delta<ngood-1]
	subwave = ref_wave[ref_pks[j]-delta>0:ref_pks[j]+delta<ngood-1]
	res = mpfitpeak(subwave,subspec,a,nterms=5,/silent)
	ref_cent[j] = a[1]
	ref_width[j]= a[2]
endfor
;
ref_width_moment = moment(ref_width)
goodsigma = where(abs(ref_width-ref_width_moment[0])/sqrt(ref_width_moment[1]) lt 1.0, ngoodsigma)
;
if ngoodsigma le 0 then begin
	kcwi_print_info,ppar,pre,'No good atlas points found',/error
	return
endif
;
ref_npks = ngoodsigma
ref_pks = ref_pks[goodsigma]
ref_cent = ref_cent[goodsigma]
ref_width = ref_width[goodsigma]
;
; start with reference bar coeffs
coeffs = kgeom.rbcoeffs
;
; initial wavelengths for pixels
xvals = xspec
yvals = yspec
wvals = poly(xspec+baroff,coeffs)
;
; get isolated peaks
spec_pks = isopeaks(yvals,pkgap,pksig,count=spec_npks)
;
; cetroid
spec_cent = dblarr(spec_npks)
spec_width = dblarr(spec_npks)
delta = fix(2.0/coeffs[1])
; now loop over peaks and fit them
for pk = 0,spec_npks-1 do begin
	subspec = yvals[spec_pks[pk]-delta>0:spec_pks[pk]+delta<npspec-1]
	subwave = wvals[spec_pks[pk]-delta>0:spec_pks[pk]+delta<npspec-1]
	res = mpfitpeak(subwave,subspec,a,nterms=5,estimate=[yvals[spec_pks[pk]], wvals[spec_pks[pk]],0.6],/silent)
	spec_cent[pk] = a[1]
	spec_width[pk] = a[2]
endfor
;
; keep lines with widths within one sigma of average
spec_moment = moment(spec_width)
goodsigma = where(abs(spec_width-spec_moment[0])/sqrt(spec_moment[1]) lt 1.0, ngoodsigma)
;
; is anyone left?
if ngoodsigma le 0 then begin
	kcwi_print_info,ppar,pre,'No good spec points found',/error
	return
endif
;
spec_npks = ngoodsigma
spec_pks = spec_pks[goodsigma]
spec_cent = spec_cent[goodsigma]
spec_width = spec_width[goodsigma]
;
; at this point we have a catalog of good reference points
; and a list of good points in this bar.  We have to match them.
one_ref = fltarr(ref_npks)+1.0
one_spec = fltarr(spec_npks)+1.0
diff = abs((spec_cent##one_ref) - (one_spec##ref_cent))
;
mn = min(diff,dim=1,mi)
;
; here we match the peaks to one another
pkm = pkiso
matchedpeaks = where(mn lt pkm, nmatchedpeaks)
;
while nmatchedpeaks lt 5 and pkm lt pkgap do begin
	;
	; open up match criterion
	pkm = pkm + 0.25
	;
	; try again
	matchedpeaks = where(mn lt pkm, nmatchedpeaks)
endwhile
;
; repor adjustments
if pkm ne pkiso then begin
	print,'Bar: ',barno,', pkiso updated to ',pkm,'; ', $
		nmatchedpeaks,'peaks',format='(a,i3,a,f5.2,a,i2,a)'
endif
;
if nmatchedpeaks eq 0 then begin
	kcwi_print_info,ppar,pre,'No peaks matched!',/error
	return
endif
ref_idx = mi[matchedpeaks] mod ref_npks
spec_idx = matchedpeaks
;
targetw = ref_cent[ref_idx]
initx = cspline(wvals,xvals,spec_cent[spec_idx])
;
; fit coeffs
fitdegree = 2
newcoeff = poly_fit(initx,targetw,fitdegree)
;
return
end

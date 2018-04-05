;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_EXTRACT_ARCS
;
; PURPOSE:
;	Extract individual arc spectra along continuum bar image bars.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_EXTRACT_ARCS,ArcImg, Kgeom, Spec
;
; INPUTS:
;	ArcImg	- 2-D image of type 'arc'
;	Kgeom	- KCWI_GEOM struct constructed from KCWI_TRACE_CBARS run
;
; INPUT KEYWORDS:
;	NAVG	- number of pixels in x to average for each y pixel
;	CCWINDIW- cross-correlation window in pixels for offset plots
;	VERBOSE - extra output
;	DISPLAY - set to display a plot of each spectrum after shifting
;
; OUTPUTS:
;	Spec	- a array of vectors, one for each bar defined by Kgeom
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;	Use the Kgeom.[kx,ky] coefficients to de-warp the arc image, after
;	which a spectrum is extracted by simple summing for each of the
;	bars in the 'cbars' image using the positions from Kgeom.barx.  The
;	spectra are assumed aligned with dispersion along the y-axis of 
;	the image.  The extraction window in x at each y pixel is determined
;	by the keyword NAVG.  Since the spectra are extracted using whole
;	pixel windows, the reference x position for each spectrum is then
;	centered on the window center.  These are recorded in Kgeom.refx.
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
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-23	Initial Revision
;	2013-JUL-24	Checks if NASMASK in place, computes bar offsets
;	2013-DEC-09	Default nav now 2, takes median of bar sample
;-
;
pro kcwi_extract_arcs,img,kgeom,spec,ppar, $
	navg=navg, ccwindow=ccwindow, $
	help=help
;
; startup
pre = 'KCWI_EXTRACT_ARCS'
q = ''
;
; check inputs
if n_params(0) lt 3 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', ArcImg, Kgeom, Spec'
	return
endif
;
; check Ppar
if kcwi_verify_ppar(ppar,/init) ne 0 then return
;
; Check Kgeom
if kcwi_verify_geom(kgeom) ne 0 then return
;
; keywords
if keyword_set(navg) then $
	nav = (fix(navg/2)-1) > 1 $
else	nav = 8/kgeom.ybinsize
;
if keyword_set(ccwindow) then $
	ccwn = ccwindow $
else	ccwn = kgeom.ccwn
do_plots = (ppar.display ge 2 or ppar.saveplots ge 3)
;
; interactive?
interact = (ppar.display ge 2)
;
; check size of input image
sz = size(img,/dim)
if sz[0] eq 0 then begin
	kcwi_print_info,ppar,pre, $
		'input image must be 2-D, preferrably an arc image.',/error
	return
endif
nx = sz[0]
ny = sz[1]
;
; number of spectra
ns = n_elements(kgeom.barx)
;
; log
kcwi_print_info,ppar,pre,'Extracting arcs from image',kgeom.arcimgnum
;
; transform image
warp = poly_2d(img,kgeom.kx,kgeom.ky,2,cubic=-0.5)
;
; output spectra
spec = fltarr(ny,ns)
;
; bar offsets
boff = fltarr(ns)
;
; loop over x values, get sample, compute spectrum
for i=0,ns-1 do begin
	xpix = fix(kgeom.barx[i])
	kgeom.refx[i] = float(xpix)+0.5
	;
	; get sample
	sub = warp[(xpix-nav):(xpix+nav),*]
	;
	; compute spectrum
	vec = median(sub,dim=1)
	ims_asym,vec,mn,sig,wgt,siglim=[1.5,3.0]
	cont = mn - 3.*sig
	vec -= cont
	spec(*,i) = vec
endfor
;
; get arc header
hdr = headfits(kgeom.arcfname)
sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
;
; geometry origins
sxaddpar,hdr, 'CBARSFL', kgeom.cbarsfname,' Continuum bars image'
sxaddpar,hdr, 'ARCFL',   kgeom.arcfname, ' Arc image'
sxaddpar,hdr, 'CBARSNO', kgeom.cbarsimgnum,' Continuum bars image number'
sxaddpar,hdr, 'ARCNO',   kgeom.arcimgnum, ' Arc image number'
sxaddpar,hdr, 'GEOMFL',  kgeom.geomfile,' Geometry file'
;
; write out arcs
outfile = kcwi_get_imname(ppar,kgeom.arcimgnum,"_arcs",/reduced)
kcwi_print_info,ppar,pre,'Writing',outfile,/info,format='(a,1x,a)'
mwrfits,spec,outfile,hdr,/create,/iscale
;
; setup for display (if requested)
if do_plots then begin
	if !d.window lt 0 then $
		window,0,title=kcwi_drp_version() $
	else	wset,0
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	th=2
	si=1.75
	w=findgen(ny)
	xrng=[-ccwn,ny+ccwn]
	if kgeom.nasmask eq 1 then $
		xrng = [((1200./kgeom.ybinsize)-ccwn), $
			((3000./kgeom.ybinsize)+ccwn)]
endif
;
; get reference bar from kgeom
if kgeom.refbar ge 0 and kgeom.refbar lt 120 then $
	refb = kgeom.refbar $
else	refb = 57	; default
;
; log
kcwi_print_info,ppar,pre,'cross-correlating with reference bar',refb
kcwi_print_info,ppar,pre,'using cc window (px)',ccwn
;
; cross-correlate to reference line
for i=0,ns-1 do begin
	off = 0.
	if i ne refb then $
		off = ccpeak(spec[*,i],spec[*,refb],ccwn,offset=kgeom.ccoff[i/5])
	;
	; record offsets
	boff[i] = off
	;
	; log
	kcwi_print_info,ppar,pre,'Bar#, yoffset',i,off,format='(a,i4,f9.3)',info=2
	;
	; plot if requested
	if do_plots then begin
		plot,w,spec[*,refb],thick=th,xthick=th,ythick=th, $
			charsi=si,charthi=th, $
			xran=xrng,xtitle='Pixel',xstyle=1, $
			ytitle='Int', /nodata, $
			title='Image: '+strn(kgeom.arcimgnum)+ $
			', Bar: '+strn(i)+', Slice: '+strn(fix(i/5))+$
			', Y Offset: '+strtrim(string(off,form='(f9.3)'),2)+' px'
		oplot,w,spec[*,refb],color=colordex('R'),thick=th
		oplot,w+off,spec[*,i],color=colordex('G')
		kcwi_legend,['RefBar: '+strn(refb),'Bar: '+strn(i)], $
			linesty=[0,0], thick=[th,0], $
			color=[colordex('R'),colordex('G')],box=0
		;
		; LEVEL 3 output
		if ppar.saveplots ge 3 then begin
			plotfn = kcwi_get_imname(ppar,kgeom.arcimgnum, $
					'_arc'+string(i,form='(i03)'), $
					/reduced)
			plotfn = repstr(plotfn,'.fits','.png')
			write_png,plotfn,tvrd(/true)
			kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
		endif
		if interact then begin
			read,'Next? (Q-quit plotting, <cr>-next): ',q
			if strupcase(strmid(q,0,1)) eq 'Q' then interact = 0
		endif
	endif
endfor
;
; update Kgeom
kgeom.baroff = boff
;
; Kgeom timestamp
kgeom.progid = pre
kgeom.timestamp = systime(1)
;
return
end

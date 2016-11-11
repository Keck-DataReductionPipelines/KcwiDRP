;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SOLVE_DGEOM
;
; PURPOSE:
;	Solve the direct mode image geometry using and arc and cbars image set
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SOLVE_DGEOM, Kdgeom, Ppar
;
; INPUTS:
;	Kdgeom	- KCWI_DGEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;
; SIDE EFFECTS:
;	Modifies KCWI_DGEOM struct with results of geometry solution.
;	NOTE: sets KDGEOM.STATUS to 0 if fitting succeeded, otherwise sets to
;	1 or greater depending on reason for failure.
;
; PROCEDURE:
;	Find the arc flat images and fit their shape, then extract the
;	arcbars and find the relative offsets
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2016-NOV-09	Initial Revision
;-
;
pro kcwi_solve_dgeom,kdgeom,ppar, help=help
;
; startup
pre = 'KCWI_SOLVE_DGEOM'
q = ''
;
; check inputs
if n_params(0) lt 2 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', ArcSpec, Kdgeom'
	return
endif
;
; Check structs
if kcwi_verify_geom(kdgeom,/init) ne 0 then return
if kcwi_verify_ppar(ppar,/init) ne 0 then return
;
; read in the arc image
arf = kdgeom.arcfname
arc = mrdfits(arf,0,ahdr,/silent)
sz = size(arc,/dim)
nx = sz[0]
ny = sz[1]
;
; read in bars image
cbf = kdgeom.cbarsfname
bar = mrdfits(cbf,0,bhdr,/silent)
sz = size(bar,/dim)
if sz[0] ne nx or sz[1] ne ny then begin
	kcwi_print_info,ppar,pre,'Arc and Bars size mis-match, returning',/error
	kdgeom.status=1
	return
endif
;
; useful params
xbin = kdgeom.xbinsize
ybin = kdgeom.ybinsize
;
; set up data arrays
data = fltarr(3,24,1000) - 1.
sl = -1
last = 0
maximx = 0
dx = 2 / xbin
bspec = fltarr(1000,24)
;
; loop over image columns
for i=4,nx-4 do begin
	;
	; extract column
	col = reform(median(arc[(i-dx):(i+dx),*],dimen=1))
	;
	; find start of slice
	t = where(col gt kdgeom.rdnoise * 10., nt)
	if nt gt kdgeom.minpix then begin
		;
		; are we starting a new slice?
		if last le kdgeom.minpix then begin
			sl += 1
			nxp = 0
			if sl gt 23 then begin
				kcwi_print_info,ppar,pre,'Slice overflow',sl,/warn
				break
			endif
		endif
		;
		; get ranges for bright pixels
		rangepar,t,tstr
		ran = strsplit(tstr,',',/extract,count=n)
		;
		; do we have more than one?
		if n gt 1 then begin
			for j=0,n-1 do begin
				rangepar,ran[j],ys
				;
				; if large, this is the one we want
				if n_elements(ys) gt 5 then begin
					ran = ran[j]
					break
				endif
			endfor
		endif
		;
		; get trace of arc image
		rangepar,ran[0],iy
		cnt = cntrd1d(iy,col[iy])
		err = fltarr(n_elements(iy)) + kdgeom.rdnoise
		if kdgeom.do_gauss then begin
			while n_elements(iy) lt 5 do begin
				iy = [min(iy)-1, iy, max(iy)+1]
				err = [kdgeom.rdnoise, err, kdgeom.rdnoise]
			endwhile
			startp = [max(col[iy]), cnt, 2.]
			yfit = gaussfit(iy, col[iy], pars, nterm=3, $
				estimates=startp, measure_err=err, sigma=perr)
			if n_elements(perr) eq 3 then begin
				fcnt = pars[1]
				fwid = pars[2]
			endif else begin
				perr = [1.e9, 1.e9]
				fcnt = 1.e9
				fwid = 1
			endelse
			ferr = perr[1]
		endif else begin
			startp = [cnt, (n_elements(iy)-5)>5, 2., 2., $
					max(col[iy]), 0.]
			pars = mpfitfun('erfcfit', iy, col[iy], err, startp, $
				yfit=yfit, perror=perr, /quiet)
			if n_elements(perr) eq 6 then begin
				fcnt = pars[0]
				fwid = pars[1]
			endif else begin
				perr = 1.e9
				fcnt = 1.e9
				fwid = 1
			endelse
			ferr = perr[0]
		endelse
		;
		; get data
		if ferr le 0.001 and abs(cnt-fcnt) le 1. then begin
			data[0,sl,nxp] = float(i)
			data[1,sl,nxp] = fcnt
			data[2,sl,nxp] = fwid
			nxp += 1
		endif
	endif
	last = nt
endfor
sl += 1
;
; log number of slices found
kcwi_print_info,ppar,pre,'Found this many slices',sl,/info
;
; now get spatial extent of arc images
;
; loop over the slices
for isl = 0, sl-1 do begin
	xf = reform(data[0,isl,*])
	yf = reform(data[1,isl,*])
	wf = reform(data[2,isl,*])
	good = where(yf gt 0.)
	xf = xf[good]
	yf = yf[good]
	wf = wf[good]
	c = poly_fit(xf,yf,1)
	angle = atan(c[1]) / !DTOR
	kdgeom.angles[isl] = angle
	ims,wf,w,wstd
	kdgeom.wid[isl] = fix(w/2.)>1
	;
	; extract trace from bar image
	xx = lindgen( fix(max(xf)-min(xf)) + 20/xbin ) + $
		      long(min(xf)-14/xbin)
	nxx = n_elements(xx)
	x0 = xx[0]
	x1 = xx[nxx-1]
	y0 = fix(poly(x0,c)+0.5)
	y1 = fix(poly(x1,c)+0.5)
	if y0 lt y1 then begin
		y0 = y0 - w*2.
		y1 = y1 + w*2.
	endif else begin
		yt = y0
		y0 = y1 - w*2.
		y1 = yt + w*2.
	endelse
	kdgeom.x0[isl] = x0
	kdgeom.x1[isl] = x1
	kdgeom.y0[isl] = y0
	kdgeom.y1[isl] = y1
	;
	; extract subimg
	sub = bar[x0:x1,y0:y1]
	subr = rot(sub,angle,cubic=-0.5)
	yc = fix( ( (y1-y0)+1 ) / 2 )
	bs = median(subr[0:nxx-1,yc-w:yc+w],dimen=2)
	bspec[0:nxx-1,isl] = bs
	if nxx gt maximx then $
		maximx = nxx
endfor
;
; now get x offsets w.r.t. ref slice
for isl = 0, sl-1 do $
	kdgeom.xoff[isl] = ccpeak(bspec[*,isl],bspec[*,kdgeom.refslice],20)
;
; we get here, all is good
kdgeom.status = 0
;
; log our change to geom struct
kdgeom.progid = pre
kdgeom.timestamp = systime(1)
;
return
end

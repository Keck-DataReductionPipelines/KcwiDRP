; $Id: kcwi_fit_flat.pro,v 1.16 2015/01/24 01:08:31 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_FIT_FLAT
;
; PURPOSE:
;	This fits the master continuum flat row-by-row to determine the
;	pixel-to-pixel response variations of the detector.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_FIT_FLAT, Img, Hdr, Ppar, Flat
;
; INPUTS:
;	Img	- Master continuum flat image
;	Hdr	- Header for above image
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Flat	- Pixel-to-pixel response flat
;
; KEYWORDS:
;	SPLORD	- Spline fit order (number of pieces, def:31)
;
; PROCEDURE:
;	Uses master continuum flat to determine the pixel-to-pixel
;	response of the detector by fitting a high-order spline to
;	each column.  This fit is divided by the input row to produce
;	a scale factor for each pixel that when multiplied onto the
;	object frame removes the pixel-to-pixel variation.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-29	Initial version
;	2013-SEP-16	Use ppar to pass pipeline params
;	2014-SEP-05	Check for NaN's in splinefit output and deal with CRs
;	2014-NOV-03	Removed CR rejection, now plots selected column
;-
pro kcwi_fit_flat,img,hdr,ppar,flat,splord=splord
	;
	; version
	version = repstr('$Revision: 1.16 $ $Date: 2015/01/24 01:08:31 $','$','')
	;
	; initialize
	pre = 'KCWI_FIT_FLAT'
	q = ''
	;
	; log
	kcwi_print_info,ppar,pre,version
	;
	; plotting?
	do_plot = (ppar.display ge 2)
	;
	; image size
	sz=size(img,/dim)
	;
	; image number
	imgnum = sxpar(hdr,'imgnum')
	;
	; initialize flat
	flat = fltarr(sz) + 1.
	;
	; Spline order
	if keyword_set(splord) then $
		splo = splord $
	else	splo = 31
	splo = long(splo)
	;
	; is the nod-and-shuffle mask in?
	if sxpar(hdr,'NASMASK') eq 1 then begin
		y0 = sxpar(hdr,'NSOBJR0')
		y1 = sxpar(hdr,'NSOBJR1')
		if y0 eq 0 or y1 eq 0 then begin
			y0 = 685
			y1 = 1369
		endif
		y0 = y0 + 27
		y1 = y1 - 20
		splo = long(splo/3)
	endif else begin
		y0 = 0
		y1 = sz[1]-1
	endelse
	;
	; log
	kcwi_print_info,ppar,pre,'Spline order, y0, y1',splo,y0,y1, $
			format='(a,3i5)'
	;
	; set up fitting variables
	;
	; cut-out what is needed
	tofit = img[*,y0:y1]
	ny = (size(tofit,/dim))[1]
	yrng = get_plotlims(tofit)
	;
	; X is simply row number
	x = findgen(ny)
	;
	; where the spline nodes are
	spi = lindgen(splo) * ny/splo
	xs = x[spi]
	;
	; set up plotting if needed
	if do_plot then begin
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		th=2.0
		si=1.5
	endif
	;
	; loop over columns
	for i=0,sz[0]-1 do begin
		;
		; data for this column
		y = reform(tofit[i,*])
		;
		; spline node value
		ys = y[spi]
		;
		; uniform weights
		w = fltarr(ny) + 1.
		;
		; fitting
		yfit = splinefit(x,y,w,xs,ys,sigys)
		;
		; get residuals
		resid = y - yfit
		ims,resid,mn,sg,wgt,siglim=5.
		;
		; re-weight
		w = float(wgt)
		;
		; re-fit
		yfit = splinefit(x,y,w,xs,ys,sigys)
		;
		; check for NaN's: substitute a value of 1.0
		pnan = where(finite(yfit) eq 0, npnan)
		if npnan gt 0 then begin
			yfit[pnan] = 1.0
			y[pnan] = 1.0
		endif
		;
		; derive response
		flat[i,y0:y1] = yfit/y
		;
		; print values
		resid = y - yfit
		mo = moment(resid[where(w gt 0.)])
		print,string(13B),i+1,'/',sz[0],sqrt(mo[1]), $
			format='($,a1,"Col: ",i6,a1,i6," Resid: ",f5.1," e-")'
	endfor
	print,' '
	;
	; update header
	sxaddpar,hdr,'COMMENT','  '+pre+' '+version
	sxaddpar,hdr,'FFITSPO',splo,' spline order for flat fit'
	;
	; plot specific columns
	i = 0
	while do_plot and i ge 0 and i lt sz[0] do begin
		;
		; data for this column
		y = reform(tofit[i,*])
		;
		; spline node value
		ys = y[spi]
		;
		; uniform weights
		w = fltarr(ny) + 1.
		;
		; fitting
		yfit = splinefit(x,y,w,xs,ys,sigys)
		;
		; get residuals
		resid = y - yfit
		ims,resid,mn,sg,wgt,siglim=5.
		;
		; re-weight
		w = float(wgt)
		;
		; re-fit
		yfit = splinefit(x,y,w,xs,ys,sigys)
		;
		; check for NaN's: substitute a value of 1.0
		pnan = where(finite(yfit) eq 0, npnan)
		if npnan gt 0 then begin
			yfit[pnan] = 1.0
			y[pnan] = 1.0
		endif
		;
		; plot
		xrng=get_plotlims(x+y0)
		plot,x+y0,y,title='Image: '+strn(imgnum)+ $
			', Col: '+string(i,format='(i4)'), $
			charsi=si,charthi=th,xthi=th,ythi=th, $
			xran=xrng,/xs,xtitle='Row', $
			yran=yrng,/ys,ytitle='e-/px'
		oplot,x+y0,yfit,color=colordex('green')
		;
		bad = where(w le 0., nbad)
		if nbad gt 0 then $
			oplot,x[bad]+y0,y[bad],psym=7
		; residuals
		resid = y - yfit
		mo = moment(resid[where(w gt 0.)])
		kcwi_legend,['Resid RMS: '+string(sqrt(mo[1]),form='(f5.1)')+$
			' e-'],box=0,charthi=th,charsi=si,/right
		read,'Next column? (Q-quit plotting, <cr> - next): ',q
		if strupcase(strmid(q,0,1)) eq 'Q' then begin
			do_plot = (1 eq 0)
		endif else if strlen(strtrim(q,2)) le 0 then begin
			i += 1L
		endif else begin
			i = fix(q)
		endelse
		;
	endwhile
	;
	return
end

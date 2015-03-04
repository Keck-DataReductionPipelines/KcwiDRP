; $Id: kcwi_plot_arcfits.pro,v 1.14 2015/01/24 01:08:30 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_PLOT_ARCFITS
;
; PURPOSE:
;	plot arc wavelength solution diagnostics
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_PLOT_ARCFITS, Specs, Kgeom, Ppar, CntCoeff, FinCoeff, Sigmas
;
; INPUTS:
;	Specs	- a array of arc spectra produced by KCWI_EXTRACT_ARCS
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Ppar	- KCWI_PPAR pipeline parameter struct
;	CntCoeff - Coefficients from fits to central region
;	FinCoeff - Coefficients from fits to full region
;	Sigmas	- Line center residuals (Ang) of fits
;
; INPUT KEYWORDS:
;	TWEAK	- set to indicate the fits are from iteratively tweaked fits
;	PLOT_FILE - if set to a string, will be used to produce postscript 
;		output of diagnostic plots
;
; SIDE EFFECTS:
;	outputs postscript plot file specified in PLOT_FILE
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Matt Matuszewski, Don Neill
;	2014-SEP-19	Initial Revision
;-
;
pro kcwi_plot_arcfits, specs, kgeom, ppar, cntcoeff, fincoeff, sigmas, $
	fwaves, dwaves, $
	tweak=tweak, plot_file=plot_file

pre = 'KCWI_PLOT_ARCFITS'
version = repstr('$Revision: 1.14 $ $Date: 2015/01/24 01:08:30 $','$','')
q=''
;
; do we want to display stuff? 
display = (ppar.display ge 2)
;
; which image number
imgnum = kgeom.arcimgnum
imglab = 'Img # '+strn(imgnum)+' Fl: '+strtrim(kgeom.filter,2)+' Gr: '+strtrim(kgeom.gratid,2)
;
; is the N+S mask in?
nasmask = kgeom.nasmask
;
; degree defaults to 3 for central region fit
degree = 3
;
; check if we need the last degree for full-ccd tweaked fits
if keyword_set(tweak) and not nasmask then $
	degree = kgeom.lastdegree
;
; load the reference atlas spectrum.
kcwi_read_atlas,kgeom,ppar,refspec,refwvl,refdisp
;
; input spectrum dimensions
specsz = size(specs,/dim)
;
; set up array with zero point in the center
x0 = specsz[0]/2
xxvals = dindgen(specsz[0])
xvals = xxvals-x0
;
; now let's make some plots!
wset,0
!p.multi=0
if keyword_set(plot_file) then begin
	psfile,plot_file
	!p.multi=[0,1,2]
	kcwi_print_info,ppar,pre,'plotting diagnostics to: '+plot_file+'.ps'
endif
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=2.0
si=2.0
;
; get good bars
bargood = where(sigmas ge 0., nbargood)
;
; bad bars
barbad = where(sigmas lt 0., nbarbad)
;
; first plot sigmas
if keyword_set(tweak) then begin 
	;
	; get fit rms status
	mom = moment(sigmas[bargood])
	fitrmslab = strtrim(string(mom[0],format='(f9.3)'),2) + ' +- ' + $
	      strtrim(string(sqrt(mom[1]),format='(f9.3)'),2)
	;
	; set title
	if nasmask then $
		tlab=imglab+', Central Fit <RMS>: ' + fitrmslab $
	else	tlab=imglab+', FullCCD Fit <RMS>: ' + fitrmslab
	;
	; plot range
	yrng = get_plotlims(sigmas[bargood])
	;
	plot,sigmas,psym=4,title=tlab, $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle='RMS (Ang)',yrange=yrng,/ys
	oplot,!x.crange,[mom[0],mom[0]],linesty=5,thick=th
	oplot,!x.crange,[mom[0]+sqrt(mom[1]),mom[0]+sqrt(mom[1])],linesty=1,thick=th
	oplot,!x.crange,[mom[0]-sqrt(mom[1]),mom[0]-sqrt(mom[1])],linesty=1,thick=th
	if nbarbad gt 0 then $
		oplot,barbad,replicate(mom[0],nbarbad),psym=7,thick=th
	kcwi_oplot_slices
	;
	; next
	if not keyword_set(plot_file) then $
		read,'next: ',q
endif
;
; plot each coeff
for i=0,degree do begin
	;
	; set title
	if keyword_set(tweak) and not nasmask then $
		tlab = imglab + ', FullCCD Coef '+strn(i) $
	else	tlab = imglab + ', Central Coef '+strn(i)
	;
	; set y title
	if i eq 0 then begin
		ylab = 'Ang'
	endif else if i eq 1 then begin
		ylab = 'Ang/pix'
	endif else begin
		ylab = 'Ang/pix^'+strn(i)
	endelse
	;
	; plot range
	yrng = get_plotlims(fincoeff[i,*])
	;
	plot,fincoeff[i,*],psym=4,title=tlab, $
		xtitle='Bar #',xrange=[-1,120],/xs, $
		ytitle=ylab,yrange=yrng,/ys
	if nbarbad gt 0 then $
		oplot,barbad,fincoeff[i,barbad],psym=7,thick=th
	kcwi_oplot_slices
	if not keyword_set(plot_file) then $
		read,'next: ',q
endfor
;
if not keyword_set(plot_file) then $
	!p.multi = 0 $
else	!p.multi = [0,1,2]
;
; inclusive, all, and trim wavelengths
wavall0 = kgeom.waveall0
wavall1 = kgeom.waveall1
wavgood0 = kgeom.wavegood0
wavgood1 = kgeom.wavegood1
wavmid = kgeom.wavemid
cwave = kgeom.cwave
minwav = kgeom.wave0out
maxwav = kgeom.wave1out
;
; atlas name
atnam = kgeom.refname
;
for b=0,119 do begin
	if display or keyword_set(plot_file) then begin
		cf = cntcoeff[*,b]
		;
		; check coeffs
		gcf = where(finite(cf) eq 1, ngcf)
		;
		; generate wavelengths
		if ngcf gt 0 then $
			waves = poly(xvals,cf[gcf]) $
		else	waves = replicate(0.,n_elements(xvals))
		;
		; mark off good regions for plot limits
		ta = where(refwvl ge wavgood0 and refwvl le wavgood1)
		ts = where(waves ge wavgood0 and waves le wavgood1,nts)
		;
		; are there good object wavelengths?
		if nts gt 0 then begin
			yrng = get_plotlims(specs[ts,b],/minzero)
			fac = max(refspec[ta])/max(specs[ts,b])
		endif else begin
			yrng = get_plotlims(refspec[ta],/minzero)
			fac = 1.
		endelse
		tlab = imglab + ', Bar = '+string(b,"(i03)") + $
			', Slice = '+string(fix(b/5),"(i02)")
		if keyword_set(tweak) and nasmask and sigmas[b] gt 0 then $
			tlab = tlab + ', RMS = '+string(sigmas[b],"(f5.3)")
		plot,refwvl,refspec/fac,thick=th,charthi=th, title=tlab, $
			xthick=th,xtitle='Wavelength (A)',xrange=[minwav,maxwav],/xs, $
			ythick=th,ytitle='Flux',yrange=yrng,/ys
		oplot,waves,specs[*,b],color=colordex('red'),thick=1.0
		oplot,[wavgood0,wavgood0],!y.crange,color=colordex('green'),linesty=1,thick=th
		oplot,[wavgood1,wavgood1],!y.crange,color=colordex('green'),linesty=1,thick=th
		oplot,[wavall0,wavall0],!y.crange,color=colordex('orange'),linesty=1,thick=th
		oplot,[wavall1,wavall1],!y.crange,color=colordex('orange'),linesty=1,thick=th
		oplot,[wavmid,wavmid],!y.crange,color=colordex('green'),linesty=1,thick=th
		oplot,[cwave,cwave],!y.crange,color=colordex('black'),linesty=1,thick=th
		;
		; plot lines used for RMS calc
		if keyword_set(tweak) and nasmask then begin
			ddwaves = reform(dwaves[b,*])
			ffwaves = reform(fwaves[b,*])
			li = where(ffwaves gt 0, nli)
			if nli gt 0 then begin
				ffwaves = ffwaves[li]
				ddwaves = ddwaves[li]
				for i=0,nli-1 do $
					oplot,[ffwaves[i],ffwaves[i]],!y.crange
			endif
		endif else nli = 0
		kcwi_legend,[atnam+' Atlas','Central Arc Fit'],linesty=[0,0],charthi=th, $
			color=[colordex('black'),colordex('red')], $
			thick=[th,th],/clear,clr_color=!p.background
		;
		if not keyword_set(plot_file) then begin
			read,'Next? (Q-quit plotting, <cr> - next): ',q
			if strupcase(strmid(q,0,1)) eq 'Q' then $
				display = (1 eq 0)
		endif
		;
		; plot residuals
		if keyword_set(tweak) and nasmask then begin
			residrng = get_plotlims(ddwaves)
			resrng = [residrng[0]<(-0.2),residrng[1]>0.2]
			plot,ffwaves,ddwaves,psym=4,charthi=th,thick=th, $
				xthick=th,xtitle='Wave(Ang)',xrange=[minwav,maxwav],/xs, $
				ythick=th,ytitle='Resid(Ang)',yrange=resrng,/ys, $
				title=tlab,/nodata
			oplot,!x.crange,[0,0]
			if nli gt 0 then begin
				oplot,ffwaves,ddwaves,psym=4,thick=th
				oplot,!x.crange,[sigmas[b],sigmas[b]],linesty=2
				oplot,!x.crange,-[sigmas[b],sigmas[b]],linesty=2
			endif
		endif
		;
		if not keyword_set(plot_file) then begin
			read,'Next? (Q-quit plotting, <cr> - next): ',q
			if strupcase(strmid(q,0,1)) eq 'Q' then $
				display = (1 eq 0)
		endif
		;
		; plot full CCD fits
		if keyword_set(tweak) and not nasmask then begin
			if display or keyword_set(plot_file) then begin
				tlab = tlab + ', RMS = '+string(sigmas[b],"(f5.3)")
				cf = fincoeff[*,b]
				waves = poly(xxvals,cf)
				plot,refwvl,refspec/fac,thick=th,charthi=th, title=tlab, $
					xthick=th,xtitle='Wavelength (A)',xrange=[minwav,maxwav],/xs, $
					ythick=th,ytitle='Flux',yrange=yrng,/ys
				oplot,waves,specs[*,b],color=colordex('blue'),thick=1.0
				oplot,[wavgood0,wavgood0],!y.crange,color=colordex('green'),linesty=1,thick=th
				oplot,[wavgood1,wavgood1],!y.crange,color=colordex('green'),linesty=1,thick=th
				oplot,[wavall0,wavall0],!y.crange,color=colordex('orange'),linesty=1,thick=th
				oplot,[wavall1,wavall1],!y.crange,color=colordex('orange'),linesty=1,thick=th
				oplot,[wavmid,wavmid],!y.crange,color=colordex('green'),linesty=1,thick=th
				oplot,[cwave,cwave],!y.crange,color=colordex('black'),linesty=1,thick=th
				;
				; plot lines used for RMS calc
				ddwaves = reform(dwaves[b,*])
				ffwaves = reform(fwaves[b,*])
				li = where(ffwaves gt 0, nli)
				if nli gt 0 then begin
					ffwaves = ffwaves[li]
					ddwaves = ddwaves[li]
					for i=0,nli-1 do $
						oplot,[ffwaves[i],ffwaves[i]],!y.crange
				endif
				kcwi_legend,[atnam+' Atlas','FullCCD Arc Fit'],linesty=[0,0],charthi=th, $
					color=[colordex('black'),colordex('blue')], $
					thick=[th,th],/clear,clr_color=!p.background
				if not keyword_set(plot_file) then begin
					read,'Next? (Q-quit plotting, <cr> - next): ',q
					if strupcase(strmid(q,0,1)) eq 'Q' then $
						display = (1 eq 0)
				endif
				;
				; plot residuals
				erase
				!p.multi=[1,1,2]
				residrng = get_plotlims(ddwaves)
				resrng = [residrng[0]<(-0.2),residrng[1]>0.2]
				plot,ffwaves,ddwaves,psym=4,charthi=th,thick=th, $
					xthick=th,xtitle='Wave(Ang)',xrange=[minwav,maxwav],/xs, $
					ythick=th,ytitle='Resid(Ang)',yrange=resrng,/ys, $
					title=tlab,/nodata
				oplot,!x.crange,[0,0]
				if nli gt 0 then begin
					oplot,ffwaves,ddwaves,psym=4,thick=th
					oplot,!x.crange,[sigmas[b],sigmas[b]],linesty=2
					oplot,!x.crange,-[sigmas[b],sigmas[b]],linesty=2
				endif
				if not keyword_set(plot_file) then begin
					read,'Next? (Q-quit plotting, <cr> - next): ',q
					if strupcase(strmid(q,0,1)) eq 'Q' then $
						display = (1 eq 0)
				endif else !p.multi=[0,1,2]
			endif	; display or keyword_set(plot_file)
		endif	; keyword_set(tweak) and not nasmask
	endif	; display or keyword_set(plot_file)
endfor		;b

if keyword_set(plot_file) then begin
	psclose
endif
!p.multi=0
;
return
end		; kcwi_plot_arcfits

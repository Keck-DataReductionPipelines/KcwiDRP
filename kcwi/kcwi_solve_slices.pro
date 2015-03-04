; $Id: kcwi_solve_slices.pro,v 1.12 2014/10/01 22:42:59 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_SOLVE_SLICES
;
; PURPOSE:
;	Solves the geometric transformation for each slice.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_SOLVE_SLICES,Ppar,Kgeom
;
; INPUTS:
;	Kgeom	- KCWI_GEOM struct from KCWI_TRACE_CBARS and KCWI_EXTRACT_ARCS
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;	HELP	- display usage help and exit
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Updates Kgeom KWX and KWY geomtric transformation coefficients.
;
; PROCEDURE:
;	The Kgeom KWX and KWY geomtric transformation coefficients are fit
;	with POLYWARP based on control points in Kgeom XI,YI,XW,YW for 
;	each slice.
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
;	2013-JUL-31	Initial Revision
;	2013-AUG-12	Added low pixel end padding
;	2014-SEP-11	Now pass ppar
;-
;
pro kcwi_solve_slices,ppar,kgeom, help=help
;
; setup
pre = 'KCWI_SOLVE_SLICES'
version = repstr('$Revision: 1.12 $ $Date: 2014/10/01 22:42:59 $','$','')
q = ''
;
; check inputs
if n_params(0) lt 1 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', Ppar, Kgeom'
	return
endif
;
; Check structs
if kcwi_verify_geom(kgeom,/init) ne 0 then return
if kcwi_verify_ppar(ppar,/init) ne 0 then return
;
; check fit status
if kgeom.status ne 0 then begin
	kcwi_print_info,ppar,pre,'Kgeom fit no good.',/error
	return
endif
;
; diagnostic plots
display = (ppar.display ge 3)
if display then $
	!p.multi=[0,1,2]
;
; degree
if kgeom.nasmask eq 1 then $
	degree = 3 $
else	degree = 3
;
; loop over slices
for i=0,23 do begin
	sli = where(kgeom.slice eq i and kgeom.xi gt 0. and $
		    finite(kgeom.xw) and finite(kgeom.yw), nsli)
	if nsli le 0 then begin
		kcwi_print_info,ppar,pre,'Kgeom slice index error.',/error
		return
	endif
	;
	; get control points
	xi = kgeom.xi[sli]
	yi = kgeom.yi[sli] + kgeom.ypad	; pad to avoid data cutoff
	xw = kgeom.xw[sli]
	yw = kgeom.yw[sli]
	;
	; fit
	polywarp,xi,yi,xw,yw,degree,kwx,kwy,/double,status=status
	;
	; get residuals
	kcwi_geom_resid,xi,yi,xw,yw,degree,kwx,kwy,xrsd,yrsd
	;
	; check status
	if status ne 0 then $
		kcwi_print_info,ppar,pre,'Polywarp non-zero status: ',status, $
			/warning
	;
	; insert into kgeom
	kgeom.kwx[0:degree,0:degree,i] = kwx
	kgeom.kwy[0:degree,0:degree,i] = kwy
	;
	; insert residuals
	xmo = moment(xrsd,/nan)
	ymo = moment(yrsd,/nan)
	kgeom.xrsd[i] = sqrt(xmo[1])
	kgeom.yrsd[i] = sqrt(ymo[1])
	;
	; plot if requested
	if display then begin
		tlab = 'Slice '+strn(i) + ':' + $
			' Xsig = '+string(kgeom.xrsd[i], format='(f7.3)') + $
			' Ysig = '+string(kgeom.yrsd[i], format='(f7.3)')
		;
		; x residuals
		xrng=get_plotlims(xw)
		yrng = [min([min(xrsd),-0.2]),max([max(xrsd),0.2])]
		plot,xw,xrsd,psym=4,title=tlab, $
			xran=xrng,/xs,xtitle='X coord (pix)', $
			yran=yrng,/ys,ytitle='X rsd (pix)'
		oplot,!x.crange,[0,0],linesty=0
		oplot,!x.crange,[xmo[0],xmo[0]],linesty=2
		oplot,!x.crange,[xmo[0]+kgeom.xrsd[i],xmo[0]+kgeom.xrsd[i]],linesty=2
		oplot,!x.crange,[xmo[0]-kgeom.xrsd[i],xmo[0]-kgeom.xrsd[i]],linesty=2
		;
		; y residuals
		yw = yw*kgeom.dwout + kgeom.wave0out
		xrng=get_plotlims(yw)
		yrng = [min([min(yrsd),-0.2]),max([max(yrsd),0.2])]
		plot,yw,yrsd,psym=4, $
			xran=xrng,/xs,xtitle='Y coord (Ang)', $
			yran=yrng,/ys,ytitle='Y rsd (pix)'
		oplot,!x.crange,[0,0],linesty=0
		oplot,!x.crange,[ymo[0],ymo[0]],linesty=2
		oplot,!x.crange,[ymo[0]+kgeom.yrsd[i],ymo[0]+kgeom.yrsd[i]],linesty=2
		oplot,!x.crange,[ymo[0]-kgeom.yrsd[i],ymo[0]-kgeom.yrsd[i]],linesty=2
		read,'Next? (Q - quit plotting, <cr> - next): ',q
		if strupcase(strmid(q,0,1)) eq 'Q' then display = (1 eq 0)
	endif
endfor
;
; Kgeom timestamp
kgeom.timestamp = systime(1)
;
if ppar.display ge 3 then $
	!p.multi=0
;
return
end

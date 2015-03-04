; $Id: kcwi_clean_coeffs.pro,v 1.11 2015/01/24 01:08:31 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_CLEAN_COEFFS
;
; PURPOSE:
;	Cleans errant bar coefficients within a slice.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Images (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_CLEAN_COEFFS, Coeffs, Forder, Ppar
;
; INPUTS:
;	Coeffs	- 9 x 120 array of spectral fit coefficients for each bar
;	Forder	- order of spectral fit (3 - 8)
;	Ppar	- KCWI_PPAR pipeline paramter struct
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Replaces bad coefficients with estimate of appropriate replacement coeff
;
; PROCEDURE:
;	Examines coeffs for the bars within a slice and compares ensemble
;	sigma to sigma after removing one bar at a time.  If sigma drops below
;	THRESH * ensemble sigma, then replace the coefficient.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill
;	2014-MAR-18	Initial Revision
;	2014-MAR-20	Enforce linear trend for Coef 0
;
;-
pro kcwi_clean_coeffs,coeffs,forder,ppar,plot=plot
;
; startup
pre = 'KCWI_CLEAN_COEFFS'
version = repstr('$Revision: 1.11 $ $Date: 2015/01/24 01:08:31 $','$','')
q = ''
;
; check pipeline params
if kcwi_verify_ppar(ppar) ne 0 then $
	ppar = { kcwi_ppar }
;
; log
kcwi_print_info,ppar,pre,version
;
; check inputs
csz = size(coeffs,/dim)
if csz[0] lt forder+1 or csz[1] ne 120 then begin
	kcwi_print_info,ppar,pre, $
		'Coeffs dimension error, must be >= '+strn(forder+1)+ $
		' x 120, returning',/error
	return
endif
;
if forder le 0 then begin
	kcwi_print_info,ppar,pre,'Fit order must be greater than 0', $
		forder,/error,format='(a,2x,i6)'
	return
endif
;
if forder gt 8 or forder lt 1 then $
	kcwi_print_info,ppar,pre,'Fit order warning, should be in range 1 - 8',$
		forder,/warning,format='(a,2x,i6)'
;
; number of coefficients
ncof = (forder+1) < 9
;
; must drive sigma down to 1/3 of ensemble value
thresh = 0.333
;
; number of bars per slice
nbars = 5
;
; operate on copy of coeffs so we don't 
; influence statistics as we go along
fcoeffs = coeffs
modified = ( 1 eq 0 )	; did we do anything?
;
; display?
display = (ppar.display ge 2 and keyword_set(plot))
;
; loop over slices
for sl = 0,23 do begin
	;
	; which bars are in this slice?
	bars = indgen(nbars) + sl * nbars
	;
	; get ensemble stats
	statall = fltarr(2,ncof)
	for i=0,ncof-1 do begin
		mo = moment(coeffs[i,bars],/nan)
		statall[0,i] = mo[0]
		statall[1,i] = sqrt(mo[1])
		if i eq 0 then begin
			ares = linfit(float(bars),coeffs[i,bars],sigma=asigma)
			statall[1,i] = asigma[0]
		endif
	endfor
	;
	; single bar removed stats
	statrm = fltarr(2,ncof,nbars)
	;
	; for each coeff
	for i=0,ncof-1 do begin
		;
		; now remove one bar at a time
		for j=0,nbars-1 do begin
			brs = bars
			remove,j,brs
			;
			; get stats
			mo = moment(coeffs[i,brs])
			statrm[0,i,j] = mo[0]
			statrm[1,i,j] = sqrt(mo[1])
			;
			; for wavelength zeropoint, should be linear
			if i eq 0 then begin
				rres = linfit(float(brs),coeffs[i,brs],sigma=rsigma)
				statrm[0,i,j] = poly(float(bars[j]),rres)
				statrm[1,i,j] = rsigma[0]
			endif
			;
			; check stats against thresh
			if not finite(coeffs[i,bars[j]]) or statrm[1,i,j] lt statall[1,i]*thresh then begin
				;
				; log the operation
				if i eq 0 then $
					fmt = '(a,3i4,2x,2f13.5)' $
				else	fmt = '(a,3i4,2x,2g13.5)'
				kcwi_print_info,ppar,pre, $
					'Slice, Bar, Coef, old, new',$
					sl,bars[j],i,coeffs[i,bars[j]], $
					statrm[0,i,j],form=fmt,info=2
				;
				; do the replacement
				fcoeffs[i,bars[j]] = statrm[0,i,j]
				modified = ( 1 eq 1 )
				;
				; display if requested
				if display then begin
					;
					; plot linear fit, if coeff 0
					if i eq 0 then begin
						xrng = [min(bars)-1, max(bars)+1]
						ya = reform(coeffs[i,bars]) - poly(bars,rres)
						yr = reform(fcoeffs[i,bars]) - poly(bars,rres)
						yrng = get_plotlims(ya,replicate(asigma[0],nbars)/nbars)
						plot,bars,ya,psym=4,symsi=2.0, $
							title='Slice '+strn(sl)+', Coef '+strn(i), $
							xtitle='Bar #', xran=xrng,/xsty, $
							ytitle='Linear Residual',charsi=1.5,yran=yrng
						oplot,bars,yr,psym=5, symsi=2.0
						oplot,!x.crange,[0.,0.],linesty=0
						oplot,!x.crange,[asigma[0],asigma[0]]/nbars,linesty=2
						oplot,!x.crange,-[asigma[0],asigma[0]]/nbars,linesty=2
						oplot,!x.crange,[rsigma[0],rsigma[0]]/nbars,linesty=1
						oplot,!x.crange,-[rsigma[0],rsigma[0]]/nbars,linesty=1
					endif else begin
						yrng = get_plotlims(coeffs[i,bars],replicate(statall[1,i],nbars))
						xrng = [min(bars)-1, max(bars)+1]
						plot,bars,coeffs[i,bars],psym=4,symsi=2.0, $
							title='Slice '+strn(sl)+', Coef '+strn(i), $
							xtitle='Bar #', xran=xrng,/xsty, $
							ytitle='Value',charsi=1.5,yran=yrng
						oplot,bars,fcoeffs[i,bars],psym=5, symsi=2.0
						oplot,!x.crange,[statall[0,i],statall[0,i]],linesty=5
						oplot,!x.crange,[statall[0,i]+statall[1,i],statall[0,i]+statall[1,i]], $
							linesty=2
						oplot,!x.crange,[statall[0,i]-statall[1,i],statall[0,i]-statall[1,i]], $
							linesty=2
						oplot,!x.crange,[statrm[0,i,j],statrm[0,i,j]],linesty=0
						oplot,!x.crange,[statrm[0,i,j]+statrm[1,i,j],statrm[0,i,j]+statrm[1,i,j]], $
							linesty=1
						oplot,!x.crange,[statrm[0,i,j]-statrm[1,i,j],statrm[0,i,j]-statrm[1,i,j]], $
							linesty=1
					endelse
					kcwi_legend,['Original','Fixed'],psym=[4,5],symsi=[2.,2.],charsi=1.5, $
						box=0,spac=2.5
					;
					; query user
					read,'Next? (Q-quit plotting, <cr>-next): ',q
					if strupcase(strmid(strtrim(q,2),0,1)) eq 'Q' then $
						display = 0
				endif
			endif	; stats below thresh
		endfor	; remove one bar at a time
	endfor	; loop over coeffs
endfor	; loop over slices
;
; update coeffs
if modified then begin
	coeffs = fcoeffs
endif else begin
	kcwi_print_info,ppar,pre,'No cleaning required.'
endelse
;
return
end		; kcwi_clean coeffs

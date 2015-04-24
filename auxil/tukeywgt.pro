function tukeywgt, n, p
; tukey weight function for a given sample size (n)
;	and a given fractional taper (p)
if p gt 1. or p lt 0. then begin
	print,'TUKEYWGT: Error - p out of range: ( 0. < p < 1. ): ',p
	return,-1
endif
x = findgen(n)/float(n)
w = fltarr(n) + 1.
if p gt 0. then begin
	;
	; lower taper fraction
	p0 = where(x lt p/2., np0)
	if np0 gt 0 then $
		w[p0] = 0.5 * ( 1. - cos(2.*!dpi*x[p0]/p) ) $	; cosine bell
	else	print,'TUKEYWGT: Warning - no points in lower taper'
	;
	; upper taper fraction
	p1 = where(x gt (1.-p/2.), np1)
	if np1 gt 0 then $
		w[p1] = 0.5 * ( 1. - cos(2.*!dpi*(1.-x[p1])/p) ) $ ; cosine bell
	else	print,'TUKEYWGT: Warning - no points in upper taper'
endif
;
return,w
end

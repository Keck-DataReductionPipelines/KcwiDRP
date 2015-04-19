; $Id: pkfind.pro,v 1.2 2013/12/20 00:53:39 neill Exp $
function pkfind, y, npeaks, thresh=thresh
	;
	; THRESH is a fractional thresshold in units of the highest peak
	;
	; index array
	x = findgen(n_elements(y))
	;
	; initial return value
	pks = -1
	;
	; first and second derivatives
	dy = deriv(x,y)
	ddy = deriv(x,dy)
	;
	; calculate zero crossings
	; where sequential first derivatives
	; change sign
	root = dy*shift(dy,1)
	;
	; clean ends
	root[0]=1
	root[n_elements(root)-1]=1
	;
	; look for zero crossings with negative 
	; second derivatives (local maxima)
	peak = where(root lt 0 and ddy lt 0., npeaks)
	;
	; if thresh keyword set make sure we are
	; above thresh times max peak (or max y)
	if keyword_set(thresh) then begin
		thr = thresh > 0. < 1.
		if npeaks gt 0 then $
			mxy = max(y[peak]) $
		else	mxy = max(y)
		peak = where(root lt 0 and ddy lt 0. and $
			y ge mxy*thr, npeaks)
	endif
	;
	; now interpolate x position of pricise zero
	if npeaks gt 0 then begin
		pks = fltarr(npeaks)
		for i=0,npeaks-1 do begin
			ii = [peak[i],peak[i]-1]
			res = interpol(x[ii],dy[ii],0.)
			pks[i] = res[0]
		endfor
	endif
	;
	return,pks
end

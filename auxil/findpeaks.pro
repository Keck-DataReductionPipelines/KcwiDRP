; sth is pixel slope
function findpeaks,x,y,wid,sth,ath,pkg,verbose=verbose,count=npks
;
; smooth derivative
d = smooth(deriv(y),wid)
nx = n_elements(x)
;
; set up windowing
if n_elements(pkg) ne 1 then pkg = wid
hgrp = fix(pkg/2)
;
pks = [-1.0]	; x values of peaks
sgs = [-1.0]	; Gaussian widths
npks = 0l
;
; loop over spectrum
; limits to avoid edges, given pkg
for i=pkg,nx-(pkg+1) do begin
    ; find zero-crossings
    if sign(d[i]) gt sign(d[i+1]) then begin
	; pass slope threshhold?
	if (d[i]-d[i+1]) gt sth*y[i] then begin
	    ; pass amplitude threshhold?
	    if y[i] gt ath or y[i+1] gt ath then begin
		; get subvectors around peak in window
		xx = x[(i-hgrp):(i+hgrp)]
		yy = y[(i-hgrp):(i+hgrp)]
		; gaussian fit
		!quiet = 1	; supress curvefit error messages
		res = gaussfit(xx,yy,a,nterms=3)
		!quiet = 0	; turn error messages back on
		; check results
		if finite(a[1]) then begin
		    ; check offset of fit from initial peak
		    r=abs(x-a[1])
		    t = where(r eq min(r))
		    ; don't use if peak wandered off
		    if abs(i-t[0]) gt pkg then begin
			if keyword_set(verbose) then $
				print,i,t[0],x[i],a[1],x[t[0]]
		    ; we're good!
		    endif else begin
		    	pks = [pks,a[1]]
			sgs = [sgs,a[2]]
			npks += 1
		    endelse
	    	endif	; a[1] finite?
	    endif	; pass amplitude threshhold?
	endif		; pass slope threshhold?
    endif		; at zero-crossing?
endfor			; loop over spectrum
;
; clean 3-sigma outlying Gaussian widths
if npks gt 0 then begin
	pks = pks[1:*]
	sgs = sgs[1:*]
	mo = moment(sgs)
	ul = mo[0] + 3.*sqrt(mo[1])
	ll = mo[0] - 3.*sqrt(mo[1])
	good = where(sgs le ul and sgs ge ll, npks)
	pks = pks[good]
endif
return,pks
end

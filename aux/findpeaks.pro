function findpeaks,x,y,wid,sth,ath,pkg,verbose=verbose,count=npks
;
; smooth derivative
d = smooth(deriv(y),wid)
nx = n_elements(x)
if n_elements(pkg) ne 1 then pkg = 1
;
pks = [-1.0]
npks = 0l
;
; loop over spectrum
for i=fix(wid),nx-fix(wid) do begin
    ; find zero-crossings
    if sign(d[i]) gt sign(d[i+1]) then begin
	; pass slope threshhold?
	if (d[i]-d[i+1]) gt sth*y[i] then begin
	    ; pass amplitude threshhold?
	    if y[i] gt ath or y[i+1] gt ath then begin
		; get subvectors around peak
		xx = x[(i-pkg):(i+pkg)]
		yy = y[(i-pkg):(i+pkg)]
		; gaussian fit
		res = gaussfit(xx,yy,a,nterms=3)
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
			npks += 1
		    endelse
		endif
	    endif
	endif
    endif
endfor
;
if npks gt 0 then pks = pks[1:*]
return,pks
end

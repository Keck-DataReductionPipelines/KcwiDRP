; sth is pixel slope
function findvalleys,y,wid,sth,verbose=verbose,count=nval
;
; smooth derivative
d = smooth(deriv(y),wid)
ny = n_elements(y)
;
vals= [-1]	; x values of peaks
nval = 0l
;
; loop over spectrum
; limits to avoid edges, given wid
for i=wid,ny-(wid+1) do begin
    ; find zero-crossings
    if sign(d[i]) lt sign(d[i+1]) then begin
	; pass slope threshhold?
	if (d[i+1]-d[i]) gt sth*y[i] then begin
	    vals = [vals,i]
	    nval += 1
	endif		; pass slope threshhold?
    endif		; at zero-crossing?
endfor			; loop over spectrum
;
if nval gt 0 then begin
	vals = vals[1:*]
endif
return,vals
end

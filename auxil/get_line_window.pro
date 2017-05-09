function get_line_window, x, y, c, count=count, verbose=verbose
;+
; get a window that includes the fwhm of the line
;
; INPUTS:
;	x	- x vector (usually wavelength)
;	y	- y vector (fluxes)
;	c	- line center (scalar)
;
; RETURNS:
;	Indexes of vector elements containing the line from the peak
;	down to the half maximum on both sides of the line.
;
; KEYWORDS:
;	count	- returns the number of elements in the index
;	verbose	- set to get error messages
;
; HISTORY:
;	17-MAY-09	- Initial version by neill@caltech.edu
;-
; vocal?
vocal = keyword_set(verbose)
;
; total size
nx = n_elements(x)
;
; find closest pixel to input peak of line
li = (where(x ge c))[0]
if li lt 2 or li gt nx-3 then begin
	count = 0
	if vocal then print,'too close to edge'
	return,-1
endif
;
; initial window of 5 pixels
li = [li-2, li-1, li, li+1, li+2]
count = 5
;
; find line peak
pky = (where(y[li] eq max(y[li])))[0]
mx = y[li[pky]]
;
; check low side
if li[0]-1 lt 0 then begin
	count = 0
	if vocal then print,'ran into low edge'
	return,-1
endif
while y[li[0]-1] gt mx do begin
	mx = y[li[0]-1]
	li = [li[0]-1, li]
	count += 1
	if li[0]-1 lt 0 then begin
		count = 0
		if vocal then print,'ran into low edge'
		return,-1
	endif
endwhile
;
; check high side
if li[count-1]+1 gt nx-1 then begin
	count = 0
	if vocal then print,'ran into high edge'
	return,-1
endif
while y[li[count-1]+1] gt mx do begin
	mx = y[li[count-1]+1]
	li = [li, li[count-1]+1]
	count += 1
	if li[count-1]+1 gt nx-1 then begin
		count = 0
		if vocal then print,'ran into high edge'
		return,-1
	endif
endwhile
;
hmx = mx * 0.5
;
; expand until we get half max
;
; low index side
prev = mx
while y[li[0]] gt hmx do begin
	;
	; missed the max
	if y[li[0]] gt mx then begin
		count = 0
		if vocal then print,'low: missed max'
		return,-1
	endif
	;
	; at the edge
	if li[0] le 0 then begin
		count = 0
		if vocal then print,'low: at edge'
		return,-1
	endif
	;
	; not monotonically decreasing
	if y[li[0]] gt prev then begin
		count = 0
		if vocal then print,'low: wiggly'
		return,-1
	endif
	;
	; expand
	prev = y[li[0]]
	li = [li[0]-1, li]
	count += 1
endwhile
;
; high index side
prev = mx
while y[li[(n_elements(li)-1)]] gt hmx do begin
	;
	; missed the max
	if y[li[(n_elements(li)-1)]] gt mx then begin
		count = 0
		if vocal then print,'high: missed max'
		return,-1
	endif
	;
	; at the edge
	if li[(n_elements(li)-1)] ge (nx-2) then begin
		count = 0
		if vocal then print,'high: at edge'
		return,-1
	endif
	;
	; not monotonically decreasing
	if y[li[(n_elements(li)-1)]] gt prev then begin
		count = 0
		if vocal then print,'high: wiggly'
		return, -1
	endif
	;
	; expand
	prev = y[li[(n_elements(li)-1)]]
	li = [li, li[(n_elements(li)-1)]+1]
	count += 1
endwhile
;
; done
return,li
end

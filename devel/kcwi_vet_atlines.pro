pro kcwi_vet_atlines, atlfn, atsig, fwid
;+
; vet atlas lines for goodness
;-
; read in atlas line list
readcol,atlfn, w, f, form='f,f'
nl = n_elements(w)
use = intarr(nl)
ids = strarr(nl)
;
; which atlas?
if strpos(atlfn, 'FeAr') ge 0 then $
	atlas = 'fear' $
else	atlas = 'thar'
;
; read in atlas spectrum
atspecfn = !KCWI_DATA + atlas + '.fits'
rdfits1dspec,atspecfn, refwave,atspec
;
; read in line list
readcol,!KCWI_DATA + atlas + '_list.txt', llw, llid, $
	format='f,a', comment='#', /silent
;
; smoothed version
xx = findgen(99)-49.0d
gaus = gaussian(xx, [1., 0.0, atsig])
gaus /= total(gaus)
atspec_smooth = convolve(atspec, gaus)
;
; set up plot
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th = 5
si = 1.75
!p.font = 0
;
; loop over lines
print,'examining ',nl,' lines',format='(a,i,a)'
for i=0,nl-1 do begin
	;
	; nearest list line
	r = abs(llw - w[i])
	c = where(r eq min(r))
	;
	; plot limits
	w0 = w[i] - fwid
	w1 = w[i] + fwid
	plot,refwave,atspec,xran=[w0,w1],/xs,xtitle='Wave(A)', $
		ytitle='Flux', title=atlfn+': Line '+strn(i)
	oplot,refwave,atspec_smooth, linesty=2
	oplot,[w[i], w[i]], !y.crange
	oplot,[llw[c], llw[c]], !y.crange, color=colordex('green')
	q=''
	print,''
	print,': ',i+1,'/',nl,w[i],llw[c], llid[c], $
		format='(a, i3,a,i3, 2f12.3, 2x, a)'
	read,'wave,id: ', q
	if strlen(q) gt 0 then begin
		wstr = gettok(q,' ')
		new_wave = float(wstr)
		if new_wave gt 0 then begin
			w[i] = new_wave
			ids[i] = q
			use[i] = 1
		endif
	endif
endfor	; loop over lines
;
; get surviving lines
good = where(use ge 1, ngood)
if ngood gt 0 then begin
	w=w[good]
	f=f[good]
	;
	outf = repstr(atlfn,'.txt','_new.txt')
	openw,ol,outf,/get_lun
	for i=0,ngood-1 do $
		printf,ol,w[i],f[i],ids[i], format='(f12.3, f12.3, 2x, a)'
	free_lun,ol
	print,'printed ',ngood,' surviving lines',format='(a, i, a)'
endif else print,'no surviving lines'
;
return
end

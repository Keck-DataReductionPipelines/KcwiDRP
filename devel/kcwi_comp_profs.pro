;+
; Compare profile images
;-
pro kcwi_comp_profs
;
pre = 'KCWI_COMP_PROFS'
version = repstr('$Revision: v0.2.10-40-g60c9d44 $ $Date: Fri May 15 10:35:48 2015 -0700 $','$','')
;
flist=file_search('*_prof.fits',count=nf)
profs = fltarr(nf,100,24)
mxx = 0
for i=0,nf-1 do begin
	prof = mrdfits(flist[i],0,hdr)
	sz=size(prof,/dimen)
	profs[i,0:(sz[0]-1),0:(sz[1]-1)] = prof[*,*]
	if sz[0] gt mxx then mxx = sz[0]
endfor
;
; now compare them
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.5
;
splo = 5
lo = 15
hi = 65
;
yrng = [0.6,1.1+float(nf)*0.0666]
;
; slice
s = 0
q = ''
while s ge 0 and s lt 24 do begin
	lsty = [0]
	lth = [th]
	fy = reform(profs[0,lo:hi,s])
	ny = n_elements(fy)
	fx = findgen(ny)
	spi = lindgen(splo) * ny/splo
	xs = fx[spi]
	ys = fy[spi]
	w = fltarr(ny) + 1.
	yfit = splinefit(fx,fy,w,xs,ys,sigys)
	mo = moment(yfit-fy)
	rms = 'RMS = '+string(sqrt(mo[1])*100.,form='(f4.1)')+' %'
	fx0=fx
	yfit0=yfit
	plot,profs[0,*,s],thick=th,charsi=si,title='Slice '+strn(s), $
		xran=[0,mxx],/xs,xtitle='Spatial px', $
		yran=yrng,/ys,ytitle='Norm. Response'
	for i=1,nf-1 do begin
		oplot,profs[i,*,s],thick=th,linesty=(i mod 7)
		fy = reform(profs[i,lo:hi,s])
		ny = n_elements(fy)
		fx = findgen(ny)
		spi = lindgen(splo) * ny/splo
		xs = fx[spi]
		ys = fy[spi]
		w = fltarr(ny) + 1.
		yfit = splinefit(fx,fy,w,xs,ys,sigys)
		mo = moment(yfit-fy)
		rms = [rms,'RMS = '+string(sqrt(mo[1])*100.,form='(f4.1)')+' %']
		lsty=[lsty,(i mod 7)]
		lth = [lth,th]
	endfor
	oplot,fx0+lo,yfit0,color=colordex('R'),thick=th
	kcwi_legend,flist,linesty=lsty,box=0,charsi=si,thick=lth, $
		/clear,clr_color=!p.background,spac=2.0
	kcwi_legend,rms,box=0,charsi=si,/right, $
		/clear,clr_color=!p.background,spac=2.0
	if !d.name ne 'PS' then begin
		read,'Slice? (0-23): ',q
		if strlen(q) le 0 then begin
			s += 1
		endif else begin
			if strpos(strupcase(q),'Q') ge 0 then $
				s = -1 $
			else	s = fix(q)
		endelse
	endif	else	s += 1
endwhile
;
return
end

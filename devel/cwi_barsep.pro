;$Id$
pro cwi_barsep,barsep,wavmid,grat,jd
;+
;	cwi_barsep - analyze average bar separation in ref slice
;-
geoms = file_search('/Users/neill/cwi/data/1*/redux/*_geom.save',count=ngeom)
;
; set up vectors
barsep = fltarr(ngeom) - 1.
wavmid = fltarr(ngeom)
jd = dblarr(ngeom)
grat = strarr(ngeom)
;
; loop over geoms
for ig = 0, ngeom-1 do begin
	geo = geoms[ig]
	imgf = repstr(geo,'geom.save','icube.fits')
	if file_test(imgf) then begin
		hdr = headfits(imgf)
		;
		; get values
		barsep[ig] = sxpar(hdr,'barsep')
		wavmid[ig] = sxpar(hdr,'wavmid')
		grat[ig] = sxpar(hdr,'gratid')
		date = sxpar(hdr,'date')
		jd[ig] = kcwi_parse_dates(date)
	endif
endfor
;
; clean results
g = where(barsep gt 0., ng)
barsep = barsep[g]
wavmid = wavmid[g]
grat = grat[g]
jd = jd[g]
;
; plot it up
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=2
si=1.5
yrng = get_plotlims(barsep)
xrng = get_plotlims(wavmid)
;
plot,wavmid,barsep,psym=4,charsi=si,charthi=th,/nodata, $
	xthick=th,xran=xrng,/xs,xtitle='MID WAVE (A)', $
	ythick=th,yran=yrng,/ys,ytitle='<REF BAR SEP> (px)', $
	title='CWI 2013,2014 Data: '+strn(ng)+' Points'
;
; red
r=where(strpos(grat,'RED') ge 0, nr)
if nr gt 0 then begin
	vsym,24,/fill
	oplot,wavmid[r],barsep[r],psym=8,color=colordex('red'),symsi=si
endif
; blue
b = where(strpos(grat,'BLUE') ge 0, nb)
if nb gt 0 then begin
	vsym,24,/fill
	oplot,wavmid[b],barsep[b],psym=8,color=colordex('blue'),symsi=si
endif
; yellow
y = where(strpos(grat,'YELL') ge 0, ny)
if ny gt 0 then begin
	vsym,24,/fill
	oplot,wavmid[y],barsep[y],psym=8,color=colordex('orange'),symsi=si
endif
; medrez
m = where(strpos(grat,'MED') ge 0, nm)
if nm gt 0 then begin
	vsym,5,/fill,/star
	oplot,wavmid[m],barsep[m],psym=8,color=colordex('purple'),symsi=si*2.
endif
xyouts,4150,13.95,'MEDREZ',charsi=si,charthi=th,color=colordex('purple')
xyouts,4950,13.95,'BLUE',charsi=si,charthi=th,color=colordex('blue')
xyouts,6270,13.95,'YELLOW',charsi=si,charthi=th,color=colordex('orange')
xyouts,6950,13.95,'RED',charsi=si,charthi=th,color=colordex('red')
;
return
end


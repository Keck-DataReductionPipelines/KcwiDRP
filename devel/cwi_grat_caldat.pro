pro cwi_grat_caldat
;+
;	cwi_grat_caldat - get data for calibrating the CWI gratings
;-
geoms = file_search('/Users/neill/cwi/data/1*/redux/*_geom.save',count=ngeom)
;
; set up vectors
gratpos = fltarr(ngeom)
campos = fltarr(ngeom)
wavmid = fltarr(ngeom) - 1.0
jd = dblarr(ngeom)
grat = strarr(ngeom)
imgno = lonarr(ngeom)
;
; loop over geoms
for ig = 0, ngeom-1 do begin
	geo = geoms[ig]
	imgf = repstr(geo,'geom.save','icube.fits')
	if file_test(imgf) then begin
		hdr = headfits(imgf)
		;
		; get values
		gratpos[ig] = sxpar(hdr,'gratpos')
		campos[ig] = sxpar(hdr,'campos')
		wavmid[ig] = sxpar(hdr,'wavmid')
		grat[ig] = sxpar(hdr,'gratid')
		date = sxpar(hdr,'date')
		jd[ig] = kcwi_parse_dates(date)
		imgno[ig] = sxpar(hdr,'imgnum')
	endif
endfor
;
; clean results
g = where(wavmid gt 0., ng)
gratpos = gratpos[g]
campos = campos[g]
wavmid = wavmid[g]
grat = grat[g]
jd = jd[g]
imgno = imgno[g]
;
; print results
forprint,wavmid,gratpos,campos,grat,jd,imgno,textout='cwi_grat_caldat.txt', $
	format='(f10.3,2i10,2x,a-6,2x,f15.3,i9)',/nocomment
;
return
end


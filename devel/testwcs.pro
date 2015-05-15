pro testwcs,kcfg,ppar
;+
; testwcs - testing wcs
;-
	pre='TESTWCS'
	icub = kcwi_read_image(kcfg,ppar,'_icube',hdr,stat=stat)
	if stat ne 0 then return
	;
	; get sky coords
	ra = sxpar(hdr,'RA',count=nra)
	dec = sxpar(hdr,'DEC',count=ndec)
	crota = sxpar(hdr,'ROTPA',count=npa)
	if nra ne 1 or ndec ne 1 then begin
		ra = sxpar(hdr,'TARGRA',count=nra)
		dec = sxpar(hdr,'TARGDEC',count=ndec)
	endif
	if nra ne 1 or ndec ne 1 or npa ne 1 then begin
		kcwi_print_info,ppar,pre,'no coords',/error
		return
	endif
	;
	; get image number format
	i_fmt = '(i0'+strn(ppar.fdigits)+')'
	;
	; get filename root
	root = ppar.froot
	;
	; convert rotation angle to radians
	;crota = 0.044
	crota = crota / !RADEG 
	;
	; calculate CD matrix
	cdelt2 = 0.00075437d0	; degrees per slice (column)
	cdelt1 = -0.00016192d0	; degrees per spatial pixel (row)
	;
	CD11 = cdelt1*cos(crota)			; RA degrees per column
	CD12 = abs(cdelt2)*sign(cdelt1)*sin(crota)	; RA degress per row
	CD21 = -abs(cdelt1)*sign(cdelt2)*sin(crota)	; DEC degress per column
	CD22 = cdelt2*cos(crota)			; DEC degrees per row
	;
	; collapse image on H-alpha region
	sz = size(icub,/dim)
	w0 = sxpar(hdr,'CRVAL3')
	x0 = sxpar(hdr,'CRPIX3')
	dw = sxpar(hdr,'CDELT3')
	wave = w0 + ( findgen(sz[2]) - x0 ) * dw
	wha = 6573.0
	t=where(wave ge 6573.,nt)
	t=t[0]
	t0 = (t-20)>0
	t1 = (t+20)<(sz[2]-1)
	print,t0,t1
	img = total(icub[*,*,t0:t1],3)/( (t1-t0) + 1. )
	;img = rotate(img,1)
	;
	; update header
	sxaddpar,hdr,'WCSDIM',2
	sxaddpar,hdr,'EPOCH',2000.
	sxaddpar,hdr,'EQUINOX',2000.
	sxaddpar,hdr,'CTYPE1','RA---TAN'
	sxaddpar,hdr,'CTYPE2','DEC--TAN'
	sxaddpar,hdr,'CRVAL1',ra
	sxaddpar,hdr,'CRVAL2',dec
	sxaddpar,hdr,'CRPIX2',11.5
	sxaddpar,hdr,'CRPIX1',(sz[0]/2. + 15.0)
	;sxaddpar,hdr,'PC1_1',cd11
	;sxaddpar,hdr,'PC1_2',cd12
	;sxaddpar,hdr,'PC2_1',cd21
	;sxaddpar,hdr,'PC2_2',cd22
	;sxaddpar,hdr,'CDELT1',cdelt1
	;sxaddpar,hdr,'CDELT2',cdelt2
	sxaddpar,hdr,'CD1_1',cd11
	sxaddpar,hdr,'CD1_2',cd12
	sxaddpar,hdr,'CD2_1',cd21
	sxaddpar,hdr,'CD2_2',cd22
	;sxaddpar,hdr,'CDELT1',0.00069444d0
	;sxaddpar,hdr,'CDELT2',0.00015152d0
	sxdelpar,hdr,'NAXIS3'
	sxdelpar,hdr,'CTYPE3'
	sxdelpar,hdr,'CUNIT3'
	sxdelpar,hdr,'CNAME3'
	sxdelpar,hdr,'CRPIX3'
	sxdelpar,hdr,'CRVAL3'
	sxdelpar,hdr,'CDELT3'
	;
	; write out image file
	ofil = root + string(kcfg.imgnum,form=i_fmt)+'_wcs.fits'
	kcwi_write_image,img,hdr,ofil,ppar
	return
end

pro kcwi_resamp_err, imno
;+
; kcwi_resamp_err - calculate resampling error plot for given image number
;-
pre = 'KCWI_RESAMP_ERR'
;
; read ppar
ppar = kcwi_read_ppar()
;
; read proc
kproc = kcwi_read_proc(ppar,pfn,imgnum,count=nproc)
;
; check image number
t = where(imgnum eq imno, nim)
if nim ne 1 then begin
	kcwi_print_info,ppar,pre, $
	'Wrong number of entries in proc file for image number', $
	imno, nim,/error
	return
endif
kproc = kproc[t[0]]
;
; get output image number and filename
out_imno = imno + 10L^(ppar.fdigits-1)
out_fname = ppar.froot + string(out_imno,format='(i0'+strn(ppar.fdigits)+')') +$
	'_intk.fits'
;
; find intk.fits file
fspec = ppar.froot + string(imno,format='(i0'+strn(ppar.fdigits)+')')
flist = file_search(fspec+'_intk.fit*', count=nf)
;
; did we find the file?
if nf ne 1 then begin
	kcwi_print_info,ppar,pre, $
		'Wrong number of _intk.fits files found for image number', $
		imno,nf,/error
	return
endif
;
; read in config
kcfg = kcwi_read_cfgs('./',filespec=flist[0])
;
; read in the slice map
smf = repstr(kproc.geomcbar,'_int.fits','_slicemap.fits')
slice = mrdfits(smf, 0)
;
; find the middle of slice 11
sl = where(slice eq 11)
slinds = array_indices(slice,sl)
row = fix(avg(slinds[0,*]))
;
; read image in
img = mrdfits(flist[0], 0, hdr)
;
; update header
sxaddpar,hdr,'FRAMENO',out_imno
sxaddpar,hdr,'OFNAME',ppar.froot + $
	string(out_imno,format='(i0'+strn(ppar.fdigits)+')') + '.fits'
;
; generate fake counts
outim = img - img
outim[row:row,*] = 6000.
;
; write out fake data
mwrfits,outim,out_fname,hdr,/create
;
; get config summary
kcwi_print_cfgs,kcfg,imsum,/silent
for k=0,2 do junk = gettok(imsum, ' ')
sta = strsplit(imsum,/extract)
;
; add entry to proc file
openw,pl,kproc.prfname,/append,/get_lun
printf,pl,out_imno,imsum,format='(i6,1x,a)'
printf,pl,'geomcbar='+kproc.geomcbar
printf,pl,'geomarc='+kproc.geomarc
free_lun,pl
;
; run kcwi_stage6cube
kcwi_stage6cube
;
; read in cube
cube_fname = ppar.froot + string(out_imno,format='(i0'+strn(ppar.fdigits)+')')+$
	'_icube.fits'
cube = mrdfits(cube_fname, 0, chdr)
csize = size(cube,/dim)
;
; get wavelengths
cdel = float(sxpar(chdr,'CD3_3',/silent))
crpix = float(sxpar(chdr,'CRPIX3',/silent)-1)
crval = float(sxpar(chdr,'CRVAL3',/silent))
wl = crval + ((findgen(csize[2]) - crpix) * cdel)
wl0 = sxpar(chdr,'WAVGOOD0')
wl1 = sxpar(chdr,'WAVGOOD1')
;
; sum cube
csum = total(cube,2)
csum = total(csum,1)
;
; percent error
perr = ((csum/6000.) - 1.0) * 100.
;
; get stats
g = where(wl gt wl0 and wl lt wl1)
ylims = get_plotlims(perr[g])
;
; get title
tlab = 'Im: ' +strn(imno)+ ', '+sta[0]+', '+sta[6]+', '+sta[7]+', '+sta[9]
;
; plot ratio
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
si = 1.75
th = 3
plot,wl,perr,title=tlab,charsi=si,charthi=th, $
	xthick=th,xtitle='WAVELENGTH(A)',xran=[wl0,wl1],/xs, $
	ythick=th,ytitle='% ERROR',yran=ylims,/ys
oplot,!x.crange,[0,0],linesty=2

return
end

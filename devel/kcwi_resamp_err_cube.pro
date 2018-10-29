pro kcwi_resamp_err_cube, imno
;+
; kcwi_resamp_err_cube - make a cube of resampling error data
;-
pre = 'KCWI_RESAMP_ERR_CUBE'
;
; read ppar
ppar = kcwi_read_ppar()
;
; read proc
kproc = kcwi_read_proc(ppar,pfn,imgnum, count=nproc)
;
; check image number
t = where(imgnum eq imno, nim)
if nim ne 1 then begin
	kcwi_print_info,ppar,pre, $
	'Wrong number of entries in proc file for image number', $
	imno, nim, /error
	return
endif
kproc = kproc[t[0]]
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
; read image in
img = mrdfits(flist[0], 0, hdr)
;
; read in config
kcfg = kcwi_read_cfgs('./',filespec=flist[0])
;
; read in the slice map
smf = repstr(kproc.geomcbar,'_int.fits','_slicemap.fits')
slice_map = mrdfits(smf, 0)
;
; find the cube file
flist = file_search(fspec+'_icube.fit*', count=nf)
;
; did we find the file
if nf ne 1 then begin
	kcwi_print_info,ppar,pre, $
		'Wrong number of _icube.fits files found for image number', $
		imno,nf,/error
	return
endif
;
; read cube in
cube = mrdfits(flist[0], 0, chdr)
cube = cube - cube
csize = size(cube,/dim)
;
; loop over slices
for si = 0, 23 do begin
	for oi = 0,csize[1]-1 do begin
		off = oi - csize[1]/2
		kcwi_resamp_err, ppar, kproc, kcfg, img, hdr, slice_map, $
			si, off, wl, perr
		cube[si,oi,*] = perr
	endfor
endfor
;
; get good wavelength range
wl = sxpar(chdr, 'CRVAL3') + findgen(csize[2]) * sxpar(chdr, 'CD3_3')
wg0 = sxpar(chdr, 'WAVGOOD0')
wg1 = sxpar(chdr, 'WAVGOOD1')
gw = where(wl ge wg0 and wl le wg1)
vec = cube[*, 10:(csize[1]-10), min(gw):max(gw)]
mo = moment(vec)
print,'Min,Max,Avg,Std: ',min(vec),max(vec),mo[0],sqrt(mo[1])
sxaddpar,chdr,'SAMERMIN',min(vec),' resampling error min'
sxaddpar,chdr,'SAMERMAX',max(vec),' resampling error max'
sxaddpar,chdr,'SAMERAVG',mo[0],' resampling error avg'
sxaddpar,chdr,'SAMERSTD',sqrt(mo[1]),' resampling error StDev'
;
; output file
ofile = 'resamp_err_'+string(imno,format='(i0'+strn(ppar.fdigits)+')') + $
	'_icube.fits'
mwrfits,cube,ofile,chdr,/create
return
end

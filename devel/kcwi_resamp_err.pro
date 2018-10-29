pro kcwi_resamp_err, ppar, kproc, kcfg, img, hdr, slice_map, slice, $
	offset, wl, perr, ps=ps, noclean=noclean
;+
; kcwi_resamp_err - calculate resampling error plot for given image number
;-
pre = 'KCWI_RESAMP_ERR'
;
; get slice
if n_params() lt 2 then $
	slice = 11 $
else	slice = fix(slice)
;
; get offset
if n_params() lt 3 then $
	off = 0 $
else	off = fix(offset)
;
; get image number
imno = sxpar(hdr,'FRAMENO')
;
; get output image number and filename
out_imno = imno + 10L^(ppar.fdigits-1)
out_fname = ppar.froot + string(out_imno,format='(i0'+strn(ppar.fdigits)+')') +$
	'_intk.fits'
;
; find the middle of slice
sl = where(slice_map eq slice)
slinds = array_indices(slice_map,sl)
row = fix(avg(slinds[0,*])) + off
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
rpf = 'resamp_err.proc'
openw,pl,rpf,/append,/get_lun
printf,pl,out_imno,imsum,format='(i6,1x,a)'
printf,pl,'geomcbar='+kproc.geomcbar
printf,pl,'geomarc='+kproc.geomarc
free_lun,pl
;
; run kcwi_stage6cube
kcwi_stage6cube,rpf
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
; get note
tlab = 'Im: ' +strn(imno)+ ', '+sta[0]+', '+sta[6]+', '+sta[7]+', '+sta[9] + $
	', Sl: '+strn(slice)+', Off: '+strn(off)
;print,tlab
;
; postscript output requested?
if keyword_set(ps) then begin
	psf = 'resamp_err_'+strn(imno)+'_'+string(slice,form='(i02)')
	if off lt 0 then $
		psf = psf + '_m'+strn(abs(off)) $
	else	if off gt 0 then $
		psf = psf + '_p'+strn(abs(off))
	psfile,psf
	!p.font = 0
endif
;
; plot percent error
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
si = 1.75
th = 3
plot,wl,perr,title=tlab,charsi=si,charthi=th, $
	xthick=th,xtitle='WAVELENGTH(A)',xran=[wl0,wl1],/xs, $
	ythick=th,ytitle='% ERROR',yran=ylims,/ys
oplot,!x.crange,[0,0],linesty=2
;
if keyword_set(ps) then psclose
;
if not keyword_set(noclean) then begin
	file_delete,out_fname
	file_delete,rpf
	file_delete,cube_fname
	file_delete,'kcwi_stage6cube.log'
endif

return
end

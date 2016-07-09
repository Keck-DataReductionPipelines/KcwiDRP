pro kcwi_cube_fwhm,cfil,parr,sarr,warr,farr,ccddw=ccddw,cubedw=cubedw
;+
;	kcwi_cube_fwhm - measure statistics of line widths in arc cube
;-
pre = 'KCWI_CUBE_FWHM'
cube = mrdfits(cfil,0,chdr)
csz = size(cube,/dimen)
if n_elements(csz) ne 3 then begin
	print,pre+': Error - must be a 3-dim data cube, returning'
	return
end
;
; get wavelengths
w0 = sxpar(chdr,'crval3')
dw = sxpar(chdr,'cd3_3')
wl = w0 + findgen(csz[2]) * dw
;
; open data file
tmp = cfil
rute = gettok(tmp,'.')
ofil = rute+'_fwhm.tab'
filestamp,ofil,/arch
openw,ol,ofil,/get_lun
;
; set up vectors
parr = [0]
sarr = [0]
warr = [0.]
farr = [0.]
;
; get pixel range
p0 = fix(csz[0]/9.)
p1 = csz[0] - p0
;
; now loop over slices
for s = 0,csz[1]-1 do begin
    for p = p0, p1 do begin
	    flx = reform(cube[p,s,*])
	    pks = isopeaks(flx,20.,7,count=np)
	    if np gt 0 then begin
		    for l = 0,np-1 do begin
			    if pks[l] gt 20 and pks[l] lt (csz[2]-21) then begin
				    i0=pks[l]-20
				    i1=pks[l]+20
				    x = wl[i0:i1]
				    y = flx[i0:i1]
				    fwhm = fwhm1d(x,y,xl,xu)
				    printf,ol,p,s,wl[pks[l]],fwhm, $
					    format='(2i9,2x,2f9.3)'
				    parr = [parr,p]
				    sarr = [sarr,s]
				    warr = [warr,wl[pks[l]]]
				    farr = [farr,fwhm]
			    endif
		    endfor
	    endif
    endfor
endfor
;
; close data table
free_lun,ol
;
; trim vectors
parr = parr[1:*]
sarr = sarr[1:*]
warr = warr[1:*]
farr = farr[1:*]
;
; get stats
ims,farr,meen,std,siglim=2.2
med = median(farr)
hwhm = (std*2.355)*0.5
print,'Median FWHM (Ang)    : ',med,form='(a,f9.3)'
print,'        STD (Ang)    : ',std,form='(a,f9.3)'
print,'       FWHM (Ang)    : ',std*2.355,form='(a,f9.3)'
;
; plot
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.7
;
plothist,farr,bin=0.001,title=cfil,charsi=si,charthi=th,thick=th, $
	xran=[med-std*5.,med+std*5.], xtitle='FWHM (Ang)',/xs, $
	ytitle='N'
oplot,[meen,meen],!y.crange
oplot,[med,med],!y.crange,linesty=2
oplot,[med+hwhm,med+hwhm],!y.crange,linesty=3
oplot,[med-hwhm,med-hwhm],!y.crange,linesty=3
;
; check pixel values
if keyword_set(ccddw) then begin
	print,'Median FWHM (ccd px) : ',med/ccddw,form='(a,f9.3)'
	print,'        STD (ccd px) : ',std/ccddw,form='(a,f9.3)'
	print,'       FWHM (ccd px) : ',std*2.355/ccddw,form='(a,f9.3)'
endif
if keyword_set(cubedw) then begin
	print,'Median FWHM (cube px): ',med/cubedw,form='(a,f9.3)'
	print,'        STD (cube px): ',std/cubedw,form='(a,f9.3)'
	print,'       FWHM (cube px): ',std*2.355/cubedw,form='(a,f9.3)'
endif
;
return
end	; kcwi_cube_fwhm

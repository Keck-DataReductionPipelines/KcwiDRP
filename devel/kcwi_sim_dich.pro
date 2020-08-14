;+
; KCWI_SIM_DICH
; 
; Simulate KCRM dichroic edge
;-
pro get_dich_data, dich_wave, dich_25, dich_30, dich_35
	ddir = '/Users/neill/kcrm/dichedge/'
	readcol,ddir+'Theory25.csv',dich_wave,tp,tpp,tsp,dich_25, $
		delim=',', comment='#', form='f,f,f,f,f',/silent
	readcol,ddir+'Theory30.csv',dich_wave,tp,tpp,tsp,dich_30, $
		delim=',', comment='#', form='f,f,f,f,f',/silent
	readcol,ddir+'Theory35.csv',dich_wave,tp,tpp,tsp,dich_35, $
		delim=',', comment='#', form='f,f,f,f,f',/silent
	dich_wave += 20.
	dich_wave *= 10.
return
end

pro KCWI_SIM_DICH, imfile

pre = "KCWI_SIM_DICH"

; get ppar
ppar = kcwi_read_ppar()
if kcwi_verify_ppar(ppar) ne 0 then begin
	ppar = {kcwi_ppar}
endif
ppar.initialized = 1
ppar.verbose = 2
;
; AOI coeffs
lo_aoi_zero = 24.4
hi_aoi_zero = 30.7
aoi_slope = 0.425
;
; get dichroic reflectances
get_dich_data, diw, di25, di30, di35
;
; get cube header
cubfile = 'redux/' + repstr(imfile, '.fits', '_icube.fits')
cubhdr = headfits(cubfile)
;
; get maps
geomfl = sxpar(cubhdr, 'geomfl')
wmap = mrdfits(repstr(geomfl, '_geom', '_wavemap'), 0, whdr, /silent)
smap = mrdfits(repstr(geomfl, '_geom', '_slicemap'), 0, shdr, /silent)
pmap = mrdfits(repstr(geomfl, '_geom', '_posmap'), 0, phdr, /silent)
;
; read raw image
rawimg = mrdfits(imfile, 0, rawhdr, /fscale, /silent)
;
; read int image
intfile = 'redux/' + repstr(imfile, '.fits', '_int.fits')
intimg = mrdfits(intfile, 0, ihdr, /silent)
;
; get image size
sz = size(intimg, /dim)
;
; Y indices
yind = indgen(sz[1])
;
; program control
old_slice = -1
mid_slice = max(pmap) / 2.
;
; print header
print,'     Xpx Slice    Xpos         AOI'
;
; loop over x pixels
for xi = 0, sz[0]-1 do begin
	; check pvec for status
	pvec = reform(pmap[xi, *])
	; are we in a slice?
	if max(pvec) gt -50. then begin
		svec = reform(smap[xi, *])
		wvec = reform(wmap[xi, *])
		ivec = reform(intimg[xi, *])
		good = where(pvec gt -50., ngood)
		if ngood gt 5 then begin
			;
			; get good data
			pvec = pvec[good]
			wvec = wvec[good]
			svec = svec[good]
			ivec = ivec[good]
			yvec = yind[good]
			;
			; get dich data on this wavelength scale
			rsdi25 = interpol(di25, diw, wvec, /nan, /spline) / 100.
			rsdi30 = interpol(di30, diw, wvec, /nan, /spline) / 100.
			rsdi35 = interpol(di35, diw, wvec, /nan, /spline) / 100.
			;
			; get AOI
			slice = min(svec)
			if slice lt 12 then $
				aoi = lo_aoi_zero + float(slice) * aoi_slope $
			else 	aoi = hi_aoi_zero + float(slice-12) * aoi_slope
			if old_slice ne slice and max(pvec) gt mid_slice then begin
				print,xi,slice,max(pvec),aoi
				old_slice = slice
				plot,wvec,rsdi25, linesty=2, /ys, $
					title='slice # '+strn(slice), $
					xtitle='WAV(A)', ytitle='Reflect.'
				oplot,wvec,rsdi30
				oplot,wvec,rsdi35, linesty=5
				oplot,[5600., 5600.], !y.crange
				q=''
				;read,'next: ',q
			endif
			;
			; loop over wavelengths
			for iw = 0, n_elements(wvec)-1 do begin
				divec = [rsdi25[iw], rsdi30[iw], rsdi35[iw]]
				rfac = interpol(divec, [25., 30., 35.], aoi) < 1. > 0.
				intimg[xi, yvec[iw]] *= rfac
			endfor	; iw = 0, n_elements(wvec)-1
		endif ; ngood gt 5
	endif ; max(pvec) gt -50.
endfor	; xi = 0, sz[0]-1
;
; disect image
;
; first map CCD from raw bias
kcwi_map_ccd, rawhdr, asec, bsec, dsec, tsec, direc, namps=namps
;
; now process each amp region
for ia = 0, namps-1 do begin
	;
	; get oscan
	;
	; overscan x range - buffer avoids edge effects
	osx0	= bsec[ia,0,0] + 20
	osx1	= bsec[ia,0,1] - 20
	;
	; range in x to subtract overscan from
	dsx0	= dsec[ia,0,0]
	dsx1	= dsec[ia,0,1]
	;
	; row range (y)
	osy0	= bsec[ia,1,0]
	osy1	= bsec[ia,1,1]
	;
	; collapse each row
	osvec = median(rawimg[osx0:osx1,osy0:osy1],dim=1)
	nx = n_elements(osvec)
	xx = findgen(nx) + osy0
	;
	; check order of fit
	if namps lt 4 then $
		order = 7 $
	else	order = 2
	;
	; fit overscan vector
	;
	; don't let first read rows skew the fit (they can be high)
	; are we reading out so that larger y-valued rows are read out first?
	if direc[ia,1] lt 0 then begin
		res = polyfit(xx[0:nx-50],osvec[0:nx-50],order)
	;
	; reading out so that smaller y-valued rows are read out first
	endif else begin
		res = polyfit(xx[49:*],osvec[49:*],order)
	endelse
	osfit = poly(xx,res)

	gain = sxpar(rawhdr, 'GAIN'+strn(ia+1))
	;
	; dich sim ranges
	xd0 = tsec[ia, 0, 0]
	xd1 = tsec[ia, 0, 1]
	yd0 = tsec[ia, 1, 0]
	yd1 = tsec[ia, 1, 1]
	;
	; bias ranges
	xb0 = dsec[ia, 0, 0]
	xb1 = dsec[ia, 0, 1]
	yb0 = dsec[ia, 1, 0]
	yb1 = dsec[ia, 1, 1]
	;
	; transfer data
	rawimg[xb0:xb1, yb0:yb1] = fix(intimg[xd0:xd1, yd0:yd1] / gain)
	;
	; add in overscan
	for iy = osy0, osy1 do begin
		;
		; get oscan fit value at row iy
		ip = where(xx eq iy, nip)
		if nip eq 1 then begin
			osval = osfit[ip[0]]
		endif else osval = 0.
		rawimg[dsx0:dsx1, iy] += osval
	endfor
endfor
;
; check directory
if not file_test('dich', /directory) then file_mkdir,'dich'
;
; write out image
sxaddpar, rawhdr, 'HISTORY', '  '+pre+' '+systime(0)
;outfile = 'redux/' + repstr(imfile, '.fits', '_intDich.fits')
outfile = 'dich/' + imfile
mwrfits, rawimg, outfile, rawhdr, /create,iscale=[1,32768]

kcwi_print_info,ppar,pre,"Generated simulated image: "+outfile,/info

end

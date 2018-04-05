;+
; KCWI_SIM_GEOM
; 
; Simulate KCRM geometry files
;-

pro KCWI_SIM_GEOM, new_cwave, rl=rl, rm1=rm1, rm2=rm2

pre = "KCWI_SIM_GEOM"

; get ppar
ppar = kcwi_read_ppar()
if kcwi_verify_ppar(ppar) ne 0 then begin
	ppar = {kcwi_ppar}
endif

; get geom
if keyword_set(rl) then begin
	kgeom = mrdfits('store/BL_Large_4500_2x2_geom.fits',1,ghdr)
endif
if keyword_set(rm1) then begin
	kgeom = mrdfits('store/BM_Large_4000_2x2_geom.fits',1,ghdr)
endif
if keyword_set(rm2) then begin
	kgeom = mrdfits('store/BM_Large_4900_2x2_geom.fits',1,ghdr)
endif
if kcwi_verify_geom(kgeom,/init) ne 0 then begin
	kcwi_print_info,ppar,pre,'Bad geometry input',/error
	return
endif

; read in quantities from the geometry.
; trying to avoid using these inside of the code proper
nx = kgeom.nx
ny = kgeom.ny
ypad = kgeom.ypad
trimy0 = kgeom.trimy0
trimy1 = kgeom.trimy1
kwx = kgeom.kwx
kwy = kgeom.kwy
xi = kgeom.xi
yi = kgeom.yi
xw = kgeom.xw
yw = kgeom.yw
slice = kgeom.slice
x0out = kgeom.x0out
dwout = kgeom.dwout
wave0out = kgeom.wave0out
xbin = kgeom.xbinsize
xpad = 17/xbin
;
; select grating
if keyword_set(rl) then begin
	outgrat = 'RL'
	disprat = kgeom.rho / 0.514
endif else if keyword_set(rm1) then begin
	outgrat = 'RM1'
	disprat = kgeom.rho / 1.220
endif else if keyword_set(rm2) then begin
	outgrat = 'RM2'
	disprat = kgeom.rho / 0.921
endif
waveoff = new_cwave - (kgeom.wavemid * disprat)
kcwi_print_info,ppar,pre,'Disprat, WaveOff',disprat,waveoff, $
	format='(a,2f9.3)'
;
; apply
yw *= disprat
wave0out = wave0out*disprat + waveoff


x = dindgen(nx)
y = dindgen(ny); ypad
onex = x-x+1.00000d
oney = y-y+1.000000d
  
xx = x#oney
yy = onex#y
  
wavemap = xx-xx-10
slicemap=wavemap+100
posmap=wavemap-90.0
tmp_posmap=posmap
tmp_wavemap = wavemap - wavemap
; loop over slices
for s=0, 23 do begin
     qs = where(slice eq s and xi gt 0 and finite(xw) and finite(yw), nqs)
     if nqs eq 0 then begin
	     kcwi_print_info,ppar,pre, $
	     	"Sorry, no points in this slice. Confused. Exiting.",/error
	     return
     endif
     x0 = xi[qs]
     y0 = yi[qs]+ypad
     x1 = xw[qs]
     y1 = yw[qs]
     x0min = min(x0)
     x0temp = x0-x0min+x0out
     ; generate a mapping
     ;polywarp,x1,y1,x0temp,y0,3,kx,ky,/double
     deg2d = [2,4]
     mod_polywarp,x1,y1,x0temp,y0,deg2d,kx,ky,/double
     ; reset the "in" values
     xmm = minmax(x0)
     qin = where(xx gt xmm[0]-xpad and xx lt xmm[1]+xpad and $
     		 yy ge trimy0 and yy le trimy1, nqxin)
     if nqxin eq 0 then begin
	     kcwi_print_info,ppar,pre, $
		"Sorry, no suitable points found. Confused. Exiting.",/error
	     return
     endif
     xin = xx[qin]-x0min+x0out
     yin = yy[qin]+ypad
     kcwi_poly_map,xin,yin,kx,ky,xout,yout,deg2d=deg2d

     ; set the pixel values to the wavelengths.
     tmp_wavemap[xin-x0out+x0min,yin-ypad] = yout*dwout+wave0out
     tmp_posmap[xin-x0out+x0min,yin-ypad] = xout ;-x0min+x0out
     qz = where(tmp_posmap ge -2/xbin and tmp_posmap le 140.0/xbin)
     slicemap[qz]=s
     posmap[qz]=tmp_posmap[qz]
     wavemap[qz] = tmp_wavemap[qz]
     tmp_posmap[*]=-100
endfor
;
; get atlas
kcwi_read_atlas,kgeom,ppar,refspec,refwvl,refdisp
;
; sim image
sim = wavemap
;
; loop over image
for il = 0,nx-1 do begin
	wvs = reform(wavemap[il,*])
	gw = where(wvs gt 0, ngw)
	if ngw gt 0 then begin
		linterp,refwvl,refspec,wvs[gw],sspec
		;sss = gaussfold(refwvl,refspec,kgeom.atsig)
		wvs[gw] = sspec
		sim[il,*] = wvs
	endif
endfor

;
; get header
hdr = headfits(kgeom.arcfname)
sxaddpar,hdr, 'BCWAVE',new_cwave, ' Blue central wavelength (Ang,sim)'
sxaddpar,hdr, 'BGRATNAM', outgrat, ' Blue Grating name (sim)'

;
outno = long(new_cwave)
outarcf = kcwi_get_imname(ppar,outno,"_int",/reduced)
outcbaf = kcwi_get_imname(ppar,outno-1,"_int",/reduced)
sxaddpar,hdr,'FRAMENO',outno
sxaddpar,hdr,'OFNAME','kb180000_'+string(outno,form='(i05)')+'.fits'
; write the arc file
kcwi_print_info,ppar,pre,"Writing",outarcf,/info,format='(a,1x,a)'
mwrfits, float(sim), outarcf, hdr,/create
;
; read in cbars image
cbars = mrdfits(kgeom.cbarsfname,0,chdr)
sxaddpar,chdr, 'BCWAVE',new_cwave, ' Blue central wavelength (Ang,sim)'
sxaddpar,chdr, 'BGRATNAM', outgrat, ' Blue Grating name (sim)'
sxaddpar,chdr,'FRAMENO',outno-1
sxaddpar,chdr,'OFNAME','kb180000_'+string(outno-1,form='(i05)')+'.fits'
; write the cbars file
kcwi_print_info,ppar,pre,"Writing",outcbaf,/info,format='(a,1x,a)'
mwrfits, cbars, outcbaf, chdr,/create
kcwi_print_info,ppar,pre,"Generated simulated images.",/info

end

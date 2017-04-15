;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_APPLY_DGEOM
;
; PURPOSE:
;	Apply the final geometric transformation to an input image.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_APPLY_DGEOM, Img, Hdr, Kdgeom, Ppar, Dimg, Dhdr
;
; INPUTS:
;	Img	- Stage1 processed object image to apply geometry to
;	Hdr	- the corresponding header of the input image
;	Kdgeom	- KCWI_DGEOM struct after KCWI_SOLVE_DGEOM has been run
;
; INPUT KEYWORDS:
;	VERBOSE - extra output
;
; OUTPUTS:
;	Dimg	- 2-D direct mode image
;	Dhdr	- A fits header with WCS info from Kdgeom
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;	Apply the geomtric transformation in Kdgeom to Img and copy Hdr to
;	Dhdr and update WCS keywords.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2016-NOV-09	Initial Revision
;-
pro kcwi_apply_dgeom,img,hdr,kdgeom,ppar,dimg,dhdr
;
; startup
pre = 'KCWI_APPLY_DGEOM'
q = ''
;
; Check structs
if kcwi_verify_geom(kdgeom,/init) ne 0 then return
if kcwi_verify_ppar(ppar) ne 0 then begin
	ppar = {kcwi_ppar}
endif
;
; get image original size
sz = size(img,/dim)
;
; output image
onx = 180 / kdgeom.xbinsize
ony = 300 / kdgeom.ybinsize
dimg = fltarr(onx,ony)
maximx = 0
;
; image number
imgnum = sxpar(hdr,'FRAMENO')
object = sxpar(hdr,'OBJECT')
imgtyp = sxpar(hdr,'CALTYPE')
;
; log
kcwi_print_info,ppar,pre,'Slicing and dicing image '+strn(imgnum)+': '+object+'...'
;
; loop over slices
for i=0,23 do begin
	x0 = kdgeom.x0[i]
	x1 = kdgeom.x1[i]
	y0 = kdgeom.y0[i]
	y1 = kdgeom.y1[i]
	w = kdgeom.wid[i]
	nxx = (x1-x0) + 1
	if nxx gt maximx then maximx = nxx
	sub = img[x0:x1,y0:y1]
	subr = rot(sub,kdgeom.angles[i],cubic=-0.5)
	si = size(subr)
	x = lindgen(si[1],si[2])
	y = x/si[1]
	x = x MOD si[1]
	res = interpolate(subr,x-kdgeom.xoff[i],y)
	yc = si[2]/2
	if i gt 0 then begin
		yp0 = yp1
		yp1 = yp0 + w*2
	endif else begin
		yp0 = 0
		yp1 = w*2
	endelse
	dimg[0:nxx-1,yp0:yp1] += res[0:nxx-1,yc-w:yc+w]
	if ppar.verbose eq 1 then $
		print,strn(i)+' ',format='($,a)'
endfor
maximy = yp1
;
dimg = transpose(dimg[0:maximx, 0:maximy])
if ppar.verbose eq 1 then begin
	print,'Done.',format='($,a)'
	print,''
endif
;
; update header
dhdr = hdr
;
; image dimensions
sxaddpar,dhdr,'NAXIS',2
sxaddpar,dhdr,'NAXIS1',maximy
sxaddpar,dhdr,'NAXIS2',maximx
;
; pixel scales
sxaddpar,dhdr,'PXSCL', kdgeom.pxscl*kdgeom.xbinsize,' Pixel scale along slice'
sxaddpar,dhdr,'SLSCL', kdgeom.slscl,' Pixel scale purpendicular to slices'
;
; geometry origins
sxaddpar,dhdr, 'CBARSFL', kdgeom.cbarsfname,' Continuum bars image'
sxaddpar,dhdr, 'ARCFL',   kdgeom.arcfname, ' Arc image'
sxaddpar,dhdr, 'CBARSNO', kdgeom.cbarsimgnum,' Continuum bars image number'
sxaddpar,dhdr, 'ARCNO',   kdgeom.arcimgnum, ' Arc image number'
sxaddpar,dhdr, 'GEOMFL',  kdgeom.dgeomfile,' Geometry file'
;
; get sky coords
rastr = sxpar(hdr,'RA',count=nra)
decstr = sxpar(hdr,'DEC',count=ndec)
if nra ne 1 or ndec ne 1 then begin
	rastr = sxpar(hdr,'TARGRA',count=nra)
	decstr = sxpar(hdr,'TARGDEC',count=ndec)
endif
if nra eq 1 and ndec eq 1 then begin
	radec_parse,rastr,decstr,':',ra,dec
endif else begin
	ra = -99.d0
	dec = -99.d0
endelse
;
; Position Angle ( = SKYPA) in degrees
; Plus an offset between rotator and IFU (may be zero)
skypa = sxpar(hdr,'ROTPOSN',count=npa) + sxpar(hdr,'ROTREFAN')
crota = -(skypa + kdgeom.rotoff) / !RADEG
sxaddpar,dhdr,'IFUPA',skypa,' IFU position angle (degrees)'
sxaddpar,dhdr,'IFUROFF',kdgeom.rotoff,' IFU-SKYPA offset (degrees)'
;
; pixel scales
cdelt1 = -kdgeom.pxscl			; RA degrees per px (column)
cdelt2 = kdgeom.pxscl*kdgeom.xbinsize	; Dec degrees per slice (row)
;
; did we get good coords?
if nra ne 1 or ndec ne 1 or npa ne 1 then begin
	;
	; no good coords
	; a warning for objects
	if strcmp(imgtyp,'object') eq 1 then begin
		kcwi_print_info,ppar,pre,'no coords for image',imgnum,imgtyp, $
			format='(a,2x,a,2x,a)',/warning
	; otherwise just info
	endif else begin
		kcwi_print_info,ppar,pre,'no coords for image',imgnum,imgtyp, $
			format='(a,2x,a,2x,a)'
	endelse
	;
	; zero coords
	ra = 0.
	dec = 0.
	;
	; nominal CD matrix (no rotation)
	CD11 = cdelt1*cos(0.)
	CD12 = abs(cdelt2)*sign(cdelt1)*sin(0.)
	CD21 = -abs(cdelt1)*sign(cdelt2)*sin(0.)
	CD22 = cdelt2*cos(0.)
endif else begin
	;
	; calculate CD matrix
	CD11 = cdelt1*cos(crota)			; RA degrees per column
	CD12 = abs(cdelt2)*sign(cdelt1)*sin(crota)	; RA degress per row
	CD21 = -abs(cdelt1)*sign(cdelt2)*sin(crota)	; DEC degress per column
	CD22 = cdelt2*cos(crota)			; DEC degrees per row
endelse
;
; get reference pixels
if ppar.crpix1 le 0. then $
	crpix1 = maximy/2. $	; spatial slice direction
else	crpix1 = ppar.crpix1
if ppar.crpix2 le 0. then $
	crpix2 = maximx/2. $	; spatial slit direction
else	crpix2 = ppar.crpix2
;
; WCS keywords
sxaddpar,dhdr,'WCSDIM',2,' number of dimensions in WCS'
sxaddpar,dhdr,'WCSNAME','KCWI'
sxaddpar,dhdr,'EQUINOX',2000.
sxaddpar,dhdr,'RADESYS','FK5'
sxaddpar,dhdr,'CTYPE1','RA---TAN'
sxaddpar,dhdr,'CTYPE2','DEC--TAN'
sxaddpar,dhdr,'CUNIT1','deg',' RA units'
sxaddpar,dhdr,'CUNIT2','deg',' DEC units'
sxaddpar,dhdr,'CNAME1','KCWI RA',' RA name'
sxaddpar,dhdr,'CNAME2','KCWI DEC',' DEC name'
sxaddpar,dhdr,'CRVAL1',ra,' RA zeropoint'
sxaddpar,dhdr,'CRVAL2',dec,' DEC zeropoint'
sxaddpar,dhdr,'CRPIX1',crpix1,' RA reference pixel'
sxaddpar,dhdr,'CRPIX2',crpix2,' DEC reference pixel'
sxaddpar,dhdr,'CD1_1',cd11,' RA degrees per column pixel'
sxaddpar,dhdr,'CD2_1',cd21,' DEC degrees per column pixel'
sxaddpar,dhdr,'CD1_2',cd12,' RA degrees per row pixel'
sxaddpar,dhdr,'CD2_2',cd22,' DEC degrees per row pixel'
sxaddpar,dhdr,'LONPOLE',180.0,' Native longitude of Celestial pole'
sxaddpar,dhdr,'LATPOLE',0.0,' Celestial latitude of native pole'
sxaddpar,dhdr,'HISTORY','  '+kdgeom.progid+' '+systime(0,kdgeom.timestamp)
sxaddpar,dhdr,'HISTORY','  '+pre+' '+systime(0)
;
return
end

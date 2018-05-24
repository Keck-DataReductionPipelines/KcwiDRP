;
; Copyright (c) 2017, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_FLATTEN_CUBE
;
; PURPOSE:
;	Take a 3-D cube and output a 2-D version
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_FLATTEN_CUBE, Cfile [, trim=trim, /reverse]
;
; INPUTS:
;	Cfile	- Image cube file name
;
; OUTPUTS:
;
; KEYWORDS:
;	HELP	- print calling sequence
;	ISCALE	- set to scale output into integers
;	TRIM	- set to number of pixels to trim off slice edges
;	REVERSE	- set to reverse the flattening: create a 3d cube from 2d flattened cube
;
; SIDE EFFECTS:
;	Writes a 2-D image with name based on input.  If keyword REVERSE set, then
;	writes out a 3-D image with name based on original 3-D image.
;
; PROCEDURE:
;	NOTE - when reversing the 2d image back into a 3d cube, the original 3d cube
;	file needs to be used as input.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2017-APR-27	Initial version
;	2017-OCT-27	Added TRIM keyword
;	2017-OCT-30	Added REVERSE keyword
;-
pro kcwi_flatten_cube,cfile,help=help,iscale=iscale,trim=trim,reverse=reverse
	;
	; setup
	pre = 'KCWI_FLATTEN_CUBE'
	;
	; help request
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', <cube_filename> [,/iscale, /reverse, trim=n_pixels'
		return
	endif
	;
	; check inputs
	if n_params(0) lt 1 then begin
		cfile = ''
		read,'Input cube image file name: ',cfile
	endif
	;
	; check trim keyword
	if keyword_set(trim) then $
		trm = trim $
	else	trm = 0
	;
	; check if it exists
	if file_test(cfile) then begin
		;
		; read in cube
		cub = mrdfits(cfile,0,hdr,/fscale,/silent)
		;
		; is it 3-D?
		sz = size(cub,/dimen)
		if n_elements(sz) eq 3 then begin
			;
			; constructing a 3d cube from a 2d flattened cube
			if keyword_set(reverse) then begin
				;
				; get 2d file name
				tfil = repstr(cfile,'.fits','_2d.fits')
				;
				; check for 2d file
				if file_test(tfil) then begin
					;
					; read in 2d file
					img = mrdfits(tfil,0,thdr,/fscale,/silent)
					;
					; unpack slices
					for i=0,sz[0]-1 do begin
						ix0 = 0 + trm
						ix1 = sz[1] - (trm + 1)
						ox0 = i*sz[1] + trm
						ox1 = (i+1)*sz[1] - (trm + 1)
						cub[i,ix0:ix1,*] = img[ox0:ox1,*]
					endfor
					;
					; get 3d file name
					ofil = repstr(tfil,'_2d.fits','_3d.fits')
					mwrfits,cub,ofil,hdr,/create
					
				endif else begin
					print,pre+': no 2d version found - ', tfil
				endelse
			;
			; flattening a 3d cube to 2d
			endif else begin
				;
				; output image
				oim = fltarr(sz[0]*sz[1],sz[2])
				;
				; pack slices
				for i=0,sz[0]-1 do begin
					ix0 = 0 + trm
					ix1 = sz[1] - (trm + 1)
					ox0 = i*sz[1] + trm
					ox1 = (i+1)*sz[1] - (trm + 1)
					oim[ox0:ox1,*] = cub[i,ix0:ix1,*]
				endfor
				;
				; get output name
				ofil = repstr(cfile,'.fits','_2d.fits')
				;
				; get wavelength values
				w0 = sxpar(hdr,'CRVAL3')
				dw = sxpar(hdr,'CD3_3')
				crpixw = sxpar(hdr,'CRPIX3')
				;
				; set spatial scale
				s0 = 0.
				ds = 24.0 / (sz[0]*sz[1])
				crpixs = 1.0
				;
				; update header
				sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
				fxaddpar,hdr,'INCUBEF',cfile,' Inpute cube filename',before='HISTORY'
				;
				; remove old WCS
				sxdelpar,hdr,'NAXIS3'
				sxdelpar,hdr,'CTYPE1'
				sxdelpar,hdr,'CTYPE2'
				sxdelpar,hdr,'CTYPE3'
				sxdelpar,hdr,'CUNIT1'
				sxdelpar,hdr,'CUNIT2'
				sxdelpar,hdr,'CUNIT3'
				sxdelpar,hdr,'CNAME1'
				sxdelpar,hdr,'CNAME2'
				sxdelpar,hdr,'CNAME3'
				sxdelpar,hdr,'CRVAL1'
				sxdelpar,hdr,'CRVAL2'
				sxdelpar,hdr,'CRVAL3'
				sxdelpar,hdr,'CRPIX1'
				sxdelpar,hdr,'CRPIX2'
				sxdelpar,hdr,'CRPIX3'
				sxdelpar,hdr,'CD1_1'
				sxdelpar,hdr,'CD1_2'
				sxdelpar,hdr,'CD2_1'
				sxdelpar,hdr,'CD2_2'
				sxdelpar,hdr,'CD3_3'
				;
				; set wavelength axis WCS values
				sxaddpar,hdr,'WCSDIM',2
				sxaddpar,hdr,'CTYPE1','SPATIAL',' SLICE'
				sxaddpar,hdr,'CUNIT1','slu',' SLICE units'
				sxaddpar,hdr,'CNAME1','KCWI SLICE',' SLICE name'
				sxaddpar,hdr,'CRVAL1',s0,' SLICE zeropoint'
				sxaddpar,hdr,'CRPIX1',crpixs,' SLICE reference pixel'
				sxaddpar,hdr,'CDELT1',ds,' SLICE per pixel'
				sxaddpar,hdr,'CTYPE2','AWAV',' Air Wavelengths'
				sxaddpar,hdr,'CUNIT2','Angstrom',' Wavelength units'
				sxaddpar,hdr,'CNAME2','KCWI 2D Wavelength',' Wavelength name'
				sxaddpar,hdr,'CRVAL2',w0,' Wavelength zeropoint'
				sxaddpar,hdr,'CRPIX2',crpixw,' Wavelength reference pixel'
				sxaddpar,hdr,'CDELT2',dw,' Wavelength Angstroms per pixel'
				;
				; write it out
				mwrfits,oim,ofil,hdr,/create,iscale=iscale
			endelse
		endif else begin
			print,pre+': not a 3-D image cube - ', cfile
		endelse
	;
	; report non-existence
	endif else begin
		print,pre+': file not found - ',cfile
	endelse
	;
	return
end

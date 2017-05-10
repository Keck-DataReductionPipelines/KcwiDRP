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
;	KCWI_FLATTEN_CUBE, Cfile
;
; Returns:
;	2-d or 3-d image, or -1 if image file not found
;
; INPUTS:
;	cfile	- Image cube file name
;
; OUTPUTS:
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Writes a 2-D image with name based on input.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2017-APR-27	Initial version
;-
pro kcwi_flatten_cube,cfile
	;
	; setup
	pre = 'KCWI_FLATTEN_CUBE'
	;
	; check inputs
	if n_params(0) lt 1 then begin
		cfile = ''
		read,'Input cube image file name: ',cfile
	endif
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
			; output image
			oim = fltarr(sz[0]*sz[1],sz[2])
			;
			; pack slices
			for i=0,sz[0]-1 do $
				oim[i*sz[1]:(i+1)*sz[1]-1,*] = cub[i,*,*]
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
			sxaddpar,hdr,'INCUBEF',cfile,' Inpute cube filename'
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
			mwrfits,oim,ofil,hdr,/create
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

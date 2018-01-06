;
; Copyright (c) 2017, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_STAGE7DAR
;
; PURPOSE:
;	This procedure applies a differential atmospheric refraction 
;	correction to the input cube.  The input cube should be slice
;	profile corrected and relative response corrected.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_STAGE7DAR, Procfname, Pparfname
;
; OPTIONAL INPUTS:
;	Procfname - input proc filename generated by KCWI_PREP
;			defaults to './redux/kcwi.proc'
;	Pparfname - input ppar filename generated by KCWI_PREP
;			defaults to './redux/kcwi.ppar'
;
; KEYWORDS:
;	VERBOSE	- set to verbosity level to override value in ppar file
;	DISPLAY - set to display level to override value in ppar file
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Outputs processed files in output directory specified by the
;	KCWI_PPAR struct read in from Pparfname.
;
; PROCEDURE:
;	Reads Pparfname to derive input/output directories and reads the
;	corresponding '*.proc' file in output directory to derive the list
;	of input files.  Uses header values of wavelengths and airmass to
;	calculate the differential asmospheric refraction in arcseconds.
;	Then it projects this offset according to the parallactic angle
;	and the position angle of the cube.  It loops through each
;	wavelength slice and applies the appropriate fractional translation
;	offset to account for the differential refraction.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2017-AUG-03	Initial version
;-
pro kcwi_stage7dar,procfname,ppfname,help=help,verbose=verbose,display=display
	;
	; setup
	pre = 'KCWI_STAGE7DAR'
	startime=systime(1)
	q = ''	; for queries
	;
	; padding for each type of grating
	gpad = [2.0, 3.0, 4.0]	; arcsecs for BH, BM, BL
	;
	; help request
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', Proc_filespec, Ppar_filespec'
		print,pre+': Info - default filespecs usually work (i.e., leave them off)'
		return
	endif
	;
	; get ppar struct
	ppar = kcwi_read_ppar(ppfname)
	;
	; verify ppar
	if kcwi_verify_ppar(ppar,/init) ne 0 then begin
		print,pre+': Error - pipeline parameter file not initialized: ',ppfname
		return
	endif
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir,/nocreate) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; check keyword overrides
	if n_elements(verbose) eq 1 then $
		ppar.verbose = verbose
	if n_elements(display) eq 1 then $
		ppar.display = display
	;
	; log file
	lgfil = reddir + 'kcwi_stage7dar.log'
	filestamp,lgfil,/arch
	openw,ll,lgfil,/get_lun
	ppar.loglun = ll
	printf,ll,'Log file for run of '+pre+' on '+systime(0)
	printf,ll,'DRP Ver: '+kcwi_drp_version()
	printf,ll,'Raw dir: '+rawdir
	printf,ll,'Reduced dir: '+reddir
	printf,ll,'Calib dir: '+cdir
	printf,ll,'Data dir: '+ddir
	printf,ll,'Filespec: '+ppar.filespec
	printf,ll,'Ppar file: '+ppfname
	if ppar.clobber then $
		printf,ll,'Clobbering existing images'
	printf,ll,'Verbosity level   : ',ppar.verbose
	printf,ll,'Plot display level: ',ppar.display
	;
	; read proc file
	kpars = kcwi_read_proc(ppar,procfname,imgnum,count=nproc)
	;
	; gather configuration data on each observation in reddir
	kcwi_print_info,ppar,pre,'Number of input images',nproc
	;
	; loop over images
	for i=0,nproc-1 do begin
		;
		; image to process
		;
		; first check for sky subtracted cube
		obfil = kcwi_get_imname(kpars[i],imgnum[i],'_icubek',/reduced)
		;
		; if not check for relative response corrected cube
		if not file_test(obfil) then $
			obfil = kcwi_get_imname(kpars[i],imgnum[i],'_icuber',/reduced)
		;
		; if not check for stage 6cube output
		if not file_test(obfil) then $
			obfil = kcwi_get_imname(kpars[i],imgnum[i],'_icube',/reduced)
		;
		; check if input file exists
		if file_test(obfil) then begin
			;
			; read configuration
			kcfg = kcwi_read_cfg(obfil)
			;
			; final output file
			ofil = kcwi_get_imname(kpars[i],imgnum[i],'_icubed',/reduced)
			;
			; trim image type
			kcfg.imgtype = strtrim(kcfg.imgtype,2)
			;
			; DAR-correct only dispersed object files
			if strpos(kcfg.imgtype,'object') lt 0 or $
			   strpos(kcfg.obstype,'direct') ge 0 then begin
				kcwi_print_info,ppar,pre, $
					'Only DAR-correct dispersed object data, skipping', $
					obfil, format='(a,a)'
			endif else begin
				;
				; check of output file exists already
				if kpars[i].clobber eq 1 or not file_test(ofil) then begin
					;
					; print image summary
					kcwi_print_cfgs,kcfg,imsum,/silent
					if strlen(imsum) gt 0 then begin
						for k=0,1 do junk = gettok(imsum,' ')
						imsum = string(i+1,'/',nproc,format='(i3,a1,i3)')+' '+imsum
					endif
					print,""
					print,imsum
					printf,ll,""
					printf,ll,imsum
					flush,ll
					;
					; report input file
					kcwi_print_info,ppar,pre,'input cube',obfil,format='(a,a)'
					;
					; read in image
					img = mrdfits(obfil,0,hdr,/fscale,/silent)
					;
					; get dimensions
					sz = size(img,/dimension)
					;
					; grating
					grating = strtrim(sxpar(hdr,'GRATNAM'),2)
					;
					; get wavelengths
					w0 = sxpar(hdr,'crval3')
					dw = sxpar(hdr,'cd3_3')
					w1 = w0 + (sz[2]-1L) * dw
					wl = w0 + findgen(sz[2]) * dw
					wgoo0 = sxpar(hdr,'wavgood0')
					wgoo1 = sxpar(hdr,'wavgood1')
					wref  = sxpar(hdr,'wavmid')
					kcwi_print_info,ppar,pre,'Ref WL and good WL range (ang)', $
						wref,wgoo0,wgoo1,format='(a,3f9.1)'
					;
					; spatial scales in arcsec/item
					yscl = sxpar(hdr,'pxscl')*3600.d0	; along slice
					xscl = sxpar(hdr,'slscl')*3600.d0	; purp to slice
					;
					; padding
					if strpos(grating,'H') ge 0 then $
						pad_as = gpad[0] $
					else if strpos(grating,'M') ge 0 then $
						pad_as = gpad[1] $
					else	pad_as = gpad[2]
					pad_x = fix(pad_as / xscl)
					pad_y = fix(pad_as / yscl)
					kcwi_print_info,ppar,pre,'Cube padding x, y (pix)', $
						pad_x, pad_y, format='(a,2i6)'
					img_out = fltarr(sz[0]+2*pad_x, sz[1]+2*pad_y,sz[2])
					img_out[pad_x:pad_x+sz[0]-1, pad_y:pad_y+sz[1]-1,*] = img
					;
					; update WCS
					crpix1 = sxpar(hdr,'crpix1')
					crpix2 = sxpar(hdr,'crpix2')
					sxaddpar,hdr,'crpix1',crpix1+float(pad_x)
					sxaddpar,hdr,'crpix2',crpix2+float(pad_y)
					;
					; airmass
					air = sxpar(hdr,'airmass')
					kcwi_print_info,ppar,pre,'Airmass',air, $
						format='(a,f9.3)'
					;
					; ifu orientation
					ifupa = sxpar(hdr,'ifupa')
					;
					; parallactic angle
					parang = sxpar(hdr,'parang')
					;
					; projection angle in radians
					projang_deg = ifupa - parang
					projang = projang_deg * !dpi / 180.d0
					;
					; report angles
					kcwi_print_info,ppar,pre,'DAR Angles: ifu_pa, parang, projang (deg)', $
						ifupa,parang,projang_deg,format='(a,3f9.2)'
					;
					; dispersion over good wl range in arcsec
					dmax_as = atm_disper(wgoo1,wgoo0,air)
					;
					; projected onto ifu
					xdmax_as = dmax_as * sin(projang)
					ydmax_as = dmax_as * cos(projang)
					kcwi_print_info,ppar,pre,'DAR over GOOD WL range: total, x, y (asec)', $
						dmax_as,xdmax_as,ydmax_as, format='(a,3f9.2)'
					;
					; now report in pixels
					xdmax_px = xdmax_as / xscl
					ydmax_px = ydmax_as / yscl
					dmax_px = sqrt(xdmax_px^2 + ydmax_px^2)
					kcwi_print_info,ppar,pre,'DAR over GOOD WL range: total, x, y (pix)', $
						xdmax_px,ydmax_px, format='(a,3f9.2)'
					;
					; read variance cube
					vfil = repstr(obfil,'_icube','_vcube')
					if file_test(vfil) then begin
						var = mrdfits(vfil,0,varhdr,/fscale,/silent)
						;
						; update WCS
						crpix1 = sxpar(varhdr,'crpix1')
						crpix2 = sxpar(varhdr,'crpix2')
						sxaddpar,varhdr,'crpix1',crpix1+float(pad_x)
						sxaddpar,varhdr,'crpix2',crpix2+float(pad_y)
					endif else begin
						var = fltarr(sz)
						var[0] = 1.	; give var value range
						varhdr = hdr
						kcwi_print_info,ppar,pre,'variance image not found for: '+obfil,/warning
					endelse
					var_out = fltarr(sz[0]+2*pad_x, sz[1]+2*pad_y,sz[2])
					var_out[pad_x:pad_x+sz[0]-1, pad_y:pad_y+sz[1]-1,*] = var
					;
					; read mask cube
					mfil = repstr(obfil,'_icube','_mcube')
					if file_test(mfil) then begin
						msk = mrdfits(mfil,0,mskhdr,/silent)
						;
						; update WCS
						crpix1 = sxpar(mskhdr,'crpix1')
						crpix2 = sxpar(mskhdr,'crpix2')
						sxaddpar,mskhdr,'crpix1',crpix1+float(pad_x)
						sxaddpar,mskhdr,'crpix2',crpix2+float(pad_y)
					endif else begin
						msk = bytarr(sz)
						msk[0] = 1b	; give mask value range
						mskhdr = hdr
						kcwi_print_info,ppar,pre,'mask image not found for: '+obfil,/warning
					endelse
					msk_out = fltarr(sz[0]+2*pad_x, sz[1]+2*pad_y,sz[2]) + 128.
					msk_out[pad_x:pad_x+sz[0]-1, pad_y:pad_y+sz[1]-1,*] = float(msk)
					;
					; do correction
					for j = 0,sz[2]-1 do begin
						dcor = atm_disper(wref,wl[j],air)
						xsh = dcor * sin(projang) / xscl
						ysh = dcor * cos(projang) / yscl
						img_out[*,*,j] = fshift(img_out[*,*,j],xsh,ysh)
						var_out[*,*,j] = fshift(var_out[*,*,j],xsh,ysh)
						msk_out[*,*,j] = fshift(msk_out[*,*,j],xsh,ysh)
					endfor
					msk_out = byte(msk_out)
					;
					; update header
					sxaddpar,mskhdr,'HISTORY','  '+pre+' '+systime(0)
					sxaddpar,mskhdr,'DARCOR','T',' DAR corrected?'
					sxaddpar,mskhdr,'DARANG',projang_deg,' DAR projection angle (deg)'
					sxaddpar,mskhdr,'DARPADX',pad_x,' DAR X padding (pix)'
					sxaddpar,mskhdr,'DARPADY',pad_y,' DAR Y padding (pix)'
					sxaddpar,mskhdr,'DAREFWL',wref,' DAR reference wl (Ang)'
					;
					; write out dar corrected mask image
					ofil = kcwi_get_imname(kpars[i],imgnum[i],'_mcubed',/nodir)
					kcwi_write_image,msk_out,mskhdr,ofil,kpars[i]
					;
					; update header
					sxaddpar,varhdr,'HISTORY','  '+pre+' '+systime(0)
					sxaddpar,varhdr,'DARCOR','T',' DAR corrected?'
					sxaddpar,varhdr,'DARANG',projang_deg,' DAR projection angle (deg)'
					sxaddpar,varhdr,'DARPADX',pad_x,' DAR X padding (pix)'
					sxaddpar,varhdr,'DARPADY',pad_y,' DAR Y padding (pix)'
					sxaddpar,varhdr,'DAREFWL',wref,' DAR reference wl (Ang)'
					;
					; write out dar corrected variance image
					ofil = kcwi_get_imname(kpars[i],imgnum[i],'_vcubed',/nodir)
					kcwi_write_image,var_out,varhdr,ofil,kpars[i]
					;
					; update header
					sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
					sxaddpar,hdr,'DARCOR','T',' DAR corrected?'
					sxaddpar,hdr,'DARANG',projang_deg,' DAR projection angle (deg)'
					sxaddpar,hdr,'DARPADX',pad_x,' DAR X padding (pix)'
					sxaddpar,hdr,'DARPADY',pad_y,' DAR Y padding (pix)'
					sxaddpar,hdr,'DAREFWL',wref,' DAR reference wl (Ang)'
					;
					; write out dar corrected intensity image
					ofil = kcwi_get_imname(kpars[i],imgnum[i],'_icubed',/nodir)
					kcwi_write_image,img_out,hdr,ofil,kpars[i]
					;
					; check for nod-and-shuffle sky image
					sfil = repstr(obfil,'_icube','_scube')
					if file_test(sfil) then begin
						sky = mrdfits(sfil,0,skyhdr,/fscale,/silent)
						sky_out = fltarr(sz[0]+2*pad_x, sz[1]+2*pad_y,sz[2])
						sky_out[pad_x:pad_x+sz[0]-1, pad_y:pad_y+sz[1]-1,*] = sky
						;
						; do correction
						for j = 0,sz[2]-1 do begin
							dcor = atm_disper(w1,wl[j],air)
							xsh = dcor * sin(projang) / xscl
							ysh = dcor * cos(projang) / yscl
							sky_out[*,*,j] = fshift(sky_out[*,*,j],xsh,ysh)
						endfor
						;
						; update header
						sxaddpar,skyhdr,'HISTORY','  '+pre+' '+systime(0)
						sxaddpar,skyhdr,'DARCOR','T',' DAR corrected?'
						sxaddpar,skyhdr,'DARANG',projang_deg,' DAR projection angle (deg)'
						sxaddpar,skyhdr,'DARPADX',pad_x,' DAR X padding (pix)'
						sxaddpar,skyhdr,'DARPADY',pad_y,' DAR Y padding (pix)'
						sxaddpar,skyhdr,'DAREFWL',wref,' DAR reference wl (Ang)'
						;
						; update WCS
						crpix1 = sxpar(skyhdr,'crpix1')
						crpix2 = sxpar(skyhdr,'crpix2')
						sxaddpar,skyhdr,'crpix1',crpix1+float(pad_x)
						sxaddpar,skyhdr,'crpix2',crpix2+float(pad_y)
						;
						; write out dar corrected sky panel image
						ofil = kcwi_get_imname(kpars[i],imgnum[i],'_scubed',/nodir)
						kcwi_write_image,sky_out,skyhdr,ofil,kpars[i]
					endif
					;
					; check for nod-and-shuffle obj image
					nfil = repstr(obfil,'_icube','_ocube')
					if file_test(nfil) then begin
						obj = mrdfits(nfil,0,objhdr,/fscale,/silent)
						obj_out = fltarr(sz[0]+2*pad_x, sz[1]+2*pad_y,sz[2])
						obj_out[pad_x:pad_x+sz[0]-1, pad_y:pad_y+sz[1]-1,*] = obj
						;
						; do correction
						for j = 0,sz[2]-1 do begin
							dcor = atm_disper(w1,wl[j],air)
							xsh = dcor * sin(projang) / xscl
							ysh = dcor * cos(projang) / yscl
							obj_out[*,*,j] = fshift(obj_out[*,*,j],xsh,ysh)
						endfor
						;
						; update header
						sxaddpar,objhdr,'HISTORY','  '+pre+' '+systime(0)
						sxaddpar,objhdr,'DARCOR','T',' DAR corrected?'
						sxaddpar,objhdr,'DARANG',projang_deg,' DAR projection angle (deg)'
						sxaddpar,objhdr,'DARPADX',pad_x,' DAR X padding (pix)'
						sxaddpar,objhdr,'DARPADY',pad_y,' DAR Y padding (pix)'
						sxaddpar,objhdr,'DAREFWL',wref,' DAR reference wl (Ang)'
						;
						; update WCS
						crpix1 = sxpar(objhdr,'crpix1')
						crpix2 = sxpar(objhdr,'crpix2')
						sxaddpar,objhdr,'crpix1',crpix1+float(pad_x)
						sxaddpar,objhdr,'crpix2',crpix2+float(pad_y)
						;
						; write out dar corrected obj panel image
						ofil = kcwi_get_imname(kpars[i],imgnum[i],'_ocubed',/nodir)
						kcwi_write_image,obj_out,objhdr,ofil,kpars[i]
					endif
				;
				; end check if output file exists already
				endif else begin
					kcwi_print_info,ppar,pre,'file not processed: '+obfil+' type: '+kcfg.imgtype,/warning
					if kpars[i].clobber eq 0 and file_test(ofil) then $
						kcwi_print_info,ppar,pre,'processed file exists already',/warning
				endelse
			;
			; end check if file is dispersed object image
			endelse
		;
		; end check if input file exists
		endif else $
			kcwi_print_info,ppar,pre,'input file not found: '+obfil,/warning
	endfor	; loop over images
	;
	; report
	eltime = systime(1) - startime
	print,''
	printf,ll,''
	kcwi_print_info,ppar,pre,'run time in seconds',eltime
	kcwi_print_info,ppar,pre,'finished on '+systime(0)
	;
	; close log file
	free_lun,ll
	;
	return
end	; kcwi_stage7dar

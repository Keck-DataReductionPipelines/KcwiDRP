;
; Copyright (c) 2019, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_STD_FWHM
;
; PURPOSE:
;	Tests a standard star reduced with it's own inverse sensitivity.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_TEST_STD, Imno
;
; INPUTS:
;	Imno	- Image number of calibrated standard star observation
;
; OPTIONAL INPUTS:
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	None
;
; KEYWORDS:
;	PS	- set to output postscript file
;	VERBOSE	- set for more output
;	DISPLAY	- set for more plots
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-APR-22	Initial Revision
;-
pro kcwi_std_fwhm,imno,ppar,ps=ps,verbose=verbose,display=display,fwhm=fwhm, $
	allbins=allbins
	;
	; setup
	pre = 'KCWI_STD_FWHM'
	version = kcwi_drp_version()
	q=''
	fwhm = -1.
	;
	; check input
	if n_params(0) lt 1 then begin
		print,pre+': Info - Usage: '+pre+', Imno <, Ppar>'
		return
	endif
	;
	; get params
	if n_elements(ppar) le 0 then begin
		pfile = 'kcwi.ppar'
		if not file_test(pfile) then begin
			pfile = 'redux/kcwi.ppar'
			if not file_test(pfile) then begin
				print,'Parameter file not found: ',pfile
				return
			endif
		endif
		ppar = kcwi_read_ppar(pfile)
	endif
	;
	; verify ppar
	if kcwi_verify_ppar(ppar,/init) ne 0 then begin
		print,pre + $
		    ': ERROR - pipeline parameter file not initialized: '
		return
	endif
	;
	; get invsens file
	invf = kcwi_get_imname(ppar,imno,'_invsens',/reduced)
	invh = headfits(invf)
	;
	; get input file
	ifil = kcwi_get_imname(ppar,imno,'_icubed',/reduced)
	if file_test(ifil) then begin
		kcfg = kcwi_read_cfg(ifil)
	endif else begin
		print,'Input file not found: ',ifil
		return
	endelse
	cfg = kcwi_cfg_string(kcfg,/delim,/long)
	kcfg.bgratnam = strtrim(kcfg.bgratnam,2)
	kcfg.ifunam = strtrim(kcfg.ifunam,2)
	;
	; get image number string
	imstr = string(imno,format='(i0'+strn(ppar.fdigits)+')')
	;
	; check keyword overrides
	if keyword_set(verbose) then $
		ppar.verbose = verbose
	if keyword_set(display) then $
		ppar.display = display
	;
	; log
	kcwi_print_info,ppar,pre,version
	;
	; is this a standard star object observation?
	if strmatch(strtrim(kcfg.imgtype,2),'object') eq 0 then begin
		kcwi_print_info,ppar,pre,'not a std obs',/warning
	endif
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir,/read) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; read in image (already extinction corrected)
	icub = kcwi_read_image(imno,ppar,'_icubed',hdr,/calib, $
								status=stat)
	if stat ne 0 then begin
		kcwi_print_info,ppar,pre,'could not read input file',/error
		return
	endif
	;
	; get size
	sz = size(icub,/dim)
	;
	; get DAR padding in y
	pad_y = sxpar(hdr,'DARPADY')
	;
	; good spatial range
	gx0 = pad_y > 1
	gx1 = sz[1] - (pad_y > 2)
	;
	; display status
	doplots = (ppar.display ge 2 or keyword_set(ps))
	;
	; get standard image params
	mxsl = sxpar(invh,'INVSLMX')
	cx = sxpar(invh,'INVSLY')
	z0 = sxpar(invh,'INVSZ0')
	z1 = sxpar(invh,'INVSZ1')
	y0 = sxpar(invh,'INVSY0')
	y1 = sxpar(invh,'INVSY1')
	pxscl = sxpar(hdr,'PXSCL')*3600.0
	xx = findgen(gx1-gx0+1)+gx0
	zave = (z0 + z1) / 2
	;
	; log results
	kcwi_print_info,ppar,pre,'Std slices; max, spatial cntrd', $
		mxsl,cx,format='(a,i4,f9.2)'
	;
	; get vector and fit
	if keyword_set(allbins) then $
		vec = reform(total(icub[mxsl,gx0:gx1,z0:z1],3))/float(z1-z0) $
	else	vec = reform(total(icub[mxsl,gx0:gx1,zave-2:zave+2],3))/5.0
	res = gaussfit(xx,vec,a)
	fwhm = a[2]*2.354*pxscl
	;
	; plot?
	if doplots then begin
		if keyword_set(ps) then begin
			font_store = !p.font
			psname = 'fwhm_'+strn(imno)
			psfile,psname
		endif
		;
		; plot
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		si = 1.75
		th = 5
		;
		plot,xx,vec,title='Imno: '+strn(imno),charsi=si, $
			xthick=th,ythick=th,charthi=th, $
			xtitle='Xpx',/xs, $
			ytitle='<e->',/ys
		oplot,[cx,cx],!y.crange,linesty=2
		oplot,xx,res,color=colordex('G')
		oplot,[a[1],a[1]],!y.crange,linesty=2,color=colordex('G')
		xyouts,a[1]*0.4,a[0]*0.8,string('FWHM=',fwhm,'arcsec', $
			form='(a,f5.2,1x,a)'),charsi=si,charthi=th
	endif
	kcwi_print_info,ppar,pre,'FHWM',a[2]*2.354,'px',form='(a,f6.2,1x,a)'
	kcwi_print_info,ppar,pre,'FHWM',a[2]*2.354*pxscl,'arcsec',form='(a,f6.2,1x,a)'
	;
	; check if we are making hardcopy
	if keyword_set(ps) then begin
		!p.font=font_store
		psclose
		kcwi_print_info,ppar,pre,'Plotting to ',psname + '.ps'
	endif
	;
	return
end

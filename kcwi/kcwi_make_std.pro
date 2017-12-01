;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MAKE_STD
;
; PURPOSE:
;	This procedure creates a standard star inverse sensitivity
;	spectrum (in units of Flam/e-) from the input data cube.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_MAKE_STD, Kcfg,  Ppar, Invsen
;
; INPUTS:
;	Kcfg	- KCWI_CFG struct for the input data cube, preferrably
;			from a sky or dome-flat observation
;	Ppar	- KCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Invsen	- a vector giving the inverse sensitivity in Flam/e-
;
; SIDE EFFECTS:
;	Outputs a fits image of the standard star inverse sensitivity with 
;	same image number root as the input file, but with '_std'
;	appended. For example, if 'image1234.fits' is pointed to by the
;	input KCWI_CFG struct, then the output std image would have the
;	filename 'image1234_std.fits'.
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Find the standard star in the slices, sky subtract and then add up
;	the flux.  Read in standard star flux and divide to get effective
;	inverse sensitivity (Flam/e-).
;
; EXAMPLE:
;
; TODO:
;	fit low-order polynomial to invsen function
;	mask known atmospheric lines/bands
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-APR-22	Initial Revision
;	2014-SEP-23	Added extinction correction
;-
pro kcwi_make_std,kcfg,ppar,invsen
	;
	; setup
	pre = 'KCWI_MAKE_STD'
	q=''
	;
	; check inputs
	if kcwi_verify_cfg(kcfg,/init) ne 0 then return
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
	;
	; is this a standard star object observation?
	if strmatch(strtrim(kcfg.imgtype,2),'object') eq 0 then begin
		kcwi_print_info,ppar,pre,'not a std obs',/warning
	endif
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; get output image (in reduced directory)
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_invsens',/reduced)
	if file_test(ofil) then begin
		if ppar.clobber ne 1 then begin
			kcwi_print_info,ppar,pre, $
				'output file already exists',ofil,/error
			return
		endif else $
			kcwi_print_info,ppar,pre, $
				'output file will be overwritten',ofil,/warning
	endif
	;
	; read in image
	icub = kcwi_read_image(kcfg.imgnum,ppar,'_icubed',hdr,/calib, $
								status=stat)
	if stat ne 0 then begin
		kcwi_print_info,ppar,pre,'could not read input file',/error
		return
	endif
	;
	; check standard
	sname = strcompress(strlowcase(strtrim(kcfg.targname,2)),/remove)
	;
	; is standard file available?
	spath = !KCWI_DATA + '/stds/'+sname+'.fits'
	if not file_test(spath) then begin
		kcwi_print_info,ppar,pre, $
			'standard star data file not found for: '+sname,/error
		return
	endif
	kcwi_print_info,ppar,pre, $
		'generating effective inverse sensitivity curve from '+sname
	;
	; get size
	sz = size(icub,/dim)
	;
	; default pixel ranges
	z = findgen(sz[2])
	z0 = 175
	z1 = sz[2] - 175
	;
	; get exposure time
	expt = sxpar(hdr,'XPOSURE')
	if expt eq 0. then begin
		kcwi_print_info,ppar,pre, $
			'no exposure time found, setting to 1s',/warn
		expt = 1.
	endif else $
		kcwi_print_info,ppar,pre,'Using exposure time of',expt,/info
	;
	; get wavelength scale
	w0 = sxpar(hdr,'CRVAL3')
	dw = sxpar(hdr,'CD3_3')
	crpixw = sxpar(hdr,'CRPIX3')
	;
	; get all good wavelength range
	wgoo0 = sxpar(hdr,'WAVGOOD0') > 3650.0
	wgoo1 = sxpar(hdr,'WAVGOOD1')
	;
	; get all inclusive wavelength range
	wall0 = sxpar(hdr,'WAVALL0')
	wall1 = sxpar(hdr,'WAVALL1')
	;
	; get DAR padding in y
	pad_y = sxpar(hdr,'DARPADY')
	;
	; get sky subtraction status
	skycor = sxpar(hdr,'SKYCOR')
	;
	; get telescope and atm. correction
	tel = strtrim(sxpar(hdr,'telescop',count=ntel),2)
	if ntel le 0 then tel = 'Keck II'
	area = -1.0
	if strpos(tel,'Keck') ge 0 then begin
		area = 760000.d0	; Keck effective area in cm^2
	endif else if strpos(tel,'5m') ge 0 then begin
		area = 194165.d0	; Hale 5m area in cm^2
	endif
	tlab = tel
	;
	; compute good y pixel ranges
	if w0 gt 0. and dw gt 0. and wgoo0 gt 0. and wgoo1 gt 0. then begin
		z0 = fix( (wgoo0 - w0) / dw ) + 10
		z1 = fix( (wgoo1 - w0) / dw ) - 10
	endif
	gz = where(z ge z0 and z le z1)
	;
	; wavelength scale
	w = w0 + z*dw
	;
	; good spatial range
	gy0 = pad_y > 1
	gy1 = sz[1] - (pad_y > 2)
	;
	; log results
	kcwi_print_info,ppar,pre,'Invsen. Pars: Y0, Y1, Z0, Z1, Wav0, Wav1', $
		gy0,gy1,z0,z1,w[z0],w[z1],format='(a,4i6,2f9.3)'
	;
	; display status
	interact = (ppar.display ge 2)
	plotsky = (ppar.display ge 3)
	;
	; find standard in slices
	tot = total(icub[*,gy0:gy1,z0:z1],3)	; sum over wavelength
	yy = findgen(gy1-gy0)+gy0		; spatial coordinates
	mxsl = -1			; max slice
	mxsg = 0.			; sigma of max slice
	;
	; for each slice
	for i=0,sz[0]-1 do begin
		mo = moment(tot[i,*])
		;
		; find the slice with the maximum sigma
		if sqrt(mo[1]) gt mxsg then begin
			mxsg = sqrt(mo[1])
			mxsl = i
		endif
	endfor
	;
	; relevant slices
	sl0 = (mxsl-3)>0
	sl1 = (mxsl+3)<(sz[0]-1)
	;
	; get y position of std
	cy = (pkfind(tot[mxsl,*],npeaks,thresh=0.99))[0] + gy0
	;
	; log results
	kcwi_print_info,ppar,pre,'Std slices; max, sl0, sl1, spatial cntrd', $
		mxsl,sl0,sl1,cy,format='(a,3i4,f9.2)'
	;
	; copy of input cube
	scub = icub
	;
	; do sky subtraction, if needed
	if not skycor then begin
		;
		; sky window width in pixels
		skywin = ppar.psfwid/kcfg.xbinsize
		;
		; plotting prep
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		;
		; for each relevant slice
		for i=sl0,sl1 do begin
			skyspec = fltarr(sz[2])
			;
			; for each wavelength
			for j = 0,sz[2]-1 do begin
				;
				; get y-spanning vector from input cube
				skyv = reform(icub[i,gy0:gy1,j])
				;
				; avoid standard
				gsky = where(yy le (cy-skywin) or $
					     yy ge (cy+skywin))
				;
				; median of what's left is sky value
				sky = median(skyv[gsky])
				;
				; store the sky value at this wavelength,slice
				skyspec[j] = sky
				;
				; perform subtraction
				scub[i,*,j] = icub[i,*,j] - sky
			endfor
			if plotsky then begin
				yrng = get_plotlims(skyspec[gz])
				plot,w,skyspec,title='Slice '+strn(i)+ $
				    ' SKY, Img #: '+strn(kcfg.imgnum), $
				    xtitle='Wave (A)',xran=[wall0,wall1],/xs,$
				    ytitle='Sky e-',yran=yrng,/ys
				oplot,[wgoo0,wgoo0],!y.crange, $
				    color=colordex('green'),thick=3
				oplot,[wgoo1,wgoo1],!y.crange, $
				    color=colordex('green'),thick=3
				read,'Next? (Q-quit plotting, <cr> - next): ',q
				if strupcase(strmid(strtrim(q,2),0,1)) eq 'Q' then $
					plotsky = 0
			endif
		endfor
	endif	; do sky subtraction (not skycor)
	;
	; apply extinction correction
	ucub = scub	; uncorrected cube
	kcwi_correct_extin, scub, hdr, ppar
	;
	; get slice spectra y limits
	sy0 = (cy-skywin) > 0
	sy1 = (cy+skywin) < (sz[1]-1)
	;
	; sum over y range
	slspec = total(scub[*,sy0:sy1,*],2)
	ulspec = total(ucub[*,sy0:sy1,*],2)
	;
	; sum over slices
	obsspec = total(slspec[sl0:sl1,*],1)
	ubsspec = total(ulspec[sl0:sl1,*],1)
	;
	; convert to e-/second
	obsspec = obsspec / expt
	ubsspec = ubsspec / expt
	;
	; read in standard
	sdat = mrdfits(spath,1,shdr)
	swl = sdat.wavelength
	sflx = sdat.flux	; Units of Flam
	sfw = sdat.fwhm
	;
	; get region of interest
	sroi = where(swl ge w[0] and swl le w[n_elements(w)-1L], nsroi)
	if nsroi le 0 then begin
		kcwi_print_info,ppar,pre, $
			'no standard wavelengths in common',/error
		return
	endif
	;
	; expand range after checking for edges
	if sroi[0] gt 0 then begin
		sroi = [ sroi[0]-1, sroi ]
		nsroi += 1
	endif
	if sroi[nsroi-1] le n_elements(swl)-1L then begin
		sroi = [ sroi, sroi[nsroi-1]+1 ]
		nsroi += 1
	endif
	;
	; very sparsely sampled w.r.t. object
	if nsroi le 1 then begin
		kcwi_print_info,ppar,pre, $
			'not enough standard points',/error
		return
	endif
	kcwi_print_info,ppar,pre,'Number of standard points',nsroi
	swl = swl[sroi]
	sflx = sflx[sroi]
	sfw = sfw[sroi]
	fwhm = max(sfw)
	kcwi_print_info,ppar,pre,'reference spectrum FWHM used',fwhm, $
					'Angstroms', format='(a,f5.1,1x,a)'
	;
	; resample standard onto our wavelength grid
	rsflx = interpol(sflx,swl,w,/nan,/spline)
	;
	; get effective inverse sensitivity
	invsen = rsflx / obsspec
	;
	; convert to photons/s/cm^2/(wl bin = dw)
	rspho = 5.03411250d+07 * rsflx * w * dw
	;
	; get effective area
	earea = ubsspec / rspho
	;
	; Balmer lines
	blines = [4861., 4341., 4102., 3970., 3889., 3835.]
	;
	; default values (for BM)
	bwid = 0.017	; fractional width to mask
	ford = 9	; fit order
	sigf = 3.0	; rejection sigma
	if strpos(kcfg.bgratnam,'BL') ge 0 then begin
		bwid = 0.008
		ford = 7
		sigf = 4.0
	endif
	if strpos(kcfg.bgratnam,'BH') ge 0 then begin
		bwid = 0.008
		ford = 9
		sigf = 3.0
	endif
	;
	; fit inverse sensitivity and effective area
	t=where(w ge wgoo0 and w le wgoo1, nt)
	if nt gt 0 then begin
		if interact then begin
			;
			; get good wavelength range
			gd = where(obsspec gt 0.)
			plot,w[gd],obsspec[gd],title=sname+' Img #: '+strn(kcfg.imgnum), $
				xtitle='Wave (A)', $
				ytitle='Observed flux',/ylog
			oplot,[wgoo0,wgoo0],10^!y.crange,color=colordex('green'), $
				thick=3
			oplot,[wgoo1,wgoo1],10^!y.crange,color=colordex('green'), $
				thick=3
			wlma = -1.
			wlmb = -1.
			print,'Mark first wavelength limit'
			cursor,wlma,yy,/data,/down
			oplot,[wlma,wlma],10^!y.crange,linesty=2,color=colordex('orange')
			print,'Mark next wavelength limit'
			cursor,wlmb,yy,/data,/down
			oplot,[wlmb,wlmb],10^!y.crange,linesty=2,color=colordex('orange')
			wlm0 = min([wlma,wlmb])
			wlm1 = max([wlma,wlmb])
			kcwi_print_info,ppar,pre,'Wavelength range for Invsens fit',wlm0,wlm1, $
				format='(a,2f9.2)'
			read,'Next: ',q
			;
			; final wavelength range
			t = where(w ge wlm0 and w le wlm1, nt)
		endif
		;
		; set up fitting vectors, flux, waves, measure errors
		sf = invsen[t]
		wf = w[t]
		me = sf * 1.e-3
		use = intarr(nt) + 1
		;
		; loop over Balmer lines
		for ib = 0, n_elements(blines)-1 do begin
			roi = where(wf gt blines[ib] - blines[ib]*bwid and $
				    wf lt blines[ib] + blines[ib]*bwid, nroi)
			if nroi gt 0 then begin
				use[roi] = 0
				kcwi_print_info,ppar,pre, $
					'Masking Balmer line',blines[ib],bwid, $
					format='(a,f6.1,f8.4)'
			endif
		endfor
		;
		; ignore bad points by setting large errors
		mf = sf-sf
		b = where(use le 0, nb)
		if nb gt 0 then begin
			me[b] = sf[b] * 1.e9
			mf[b] = sf[b]
		endif
		;
		; initial polynomial fit of inverse sensitivity
		wf0 = min(wf)
		res = poly_fit(wf-wf0,sf,ford,/double,measure_error=me, $
			yfit=yfit,yband=yband,status=fitstat)
		finvsen = poly(w-wf0,res)
		;
		; create a standard flux window
		device,Window_State=wins
		if wins[1] ne 1 then $
			window,1,title='Standard Flux'
		;
		; plot fits
		done = (1 eq 0)
		while not done do begin
			;
			; good points
			g = where(use ge 1, ng)
			if ng le 0 then begin
				g = lindgen(nt)
				ng = nt
			endif
			;
			; new bad points, marked by large errors
			me = sf * 1.e-3
			mf = sf-sf
			b = where(use le 0, nb)
			if nb gt 0 then begin
				me[b] = sf[b] * 1.e9
				mf[b] = sf[b]
			endif
			;
			; calculate calibrated spectrum
			calspec = obsspec * finvsen
			;
			; now plot standard flux and calibrated spectrum
			wset,1
			yrng=get_plotlims(calspec[t])
			if yrng[0] lt 0 then yrng[0] = 0.
			plot,w,calspec,title=sname+' Img #: '+strn(kcfg.imgnum), $
				xran=[wall0,wall1],/xs,xtickformat='(a1)', $
				ytitle='!3Flam (erg s!U-1!N cm!U-2!N A!U-1!N',yran=yrng,/ys, $
				pos=[0.07,0.30,0.98,0.95]
			oplot,w,rsflx,thick=3,color=colordex('red')
			oplot,[wlm0,wlm0],!y.crange,color=colordex('orange'),linesty=2
			oplot,[wlm1,wlm1],!y.crange,color=colordex('orange'),linesty=2
			for ib=0,n_elements(blines)-1 do $
				oplot,[blines[ib],blines[ib]],!y.crange,color=colordex('blue'), $
					linesty=2
			;
			; calculate residuals
			rsd = calspec - rsflx
			frsd = 100.d0 * (rsd/rsflx)
			mo = moment(rsd[t[g]],/nan)
			fmo = moment(frsd[t[g]],/nan)
			;
			; annotate residuals
			kcwi_legend,['<Resid> = '+strtrim(string(mo[0],format='(g13.3)'),2) + $
				' +- '+strtrim(string(sqrt(mo[1]),format='(g13.3)'),2)+' Flam',$
				'<Resid> = '+strtrim(string(fmo[0],format='(f8.2)'),2) + $
				' +- '+strtrim(string(sqrt(fmo[1]),format='(f8.2)'),2)+' %'], $
				/clear,clr_color=!p.background,/bottom
			;
			; plot residuals
			yrng = get_plotlims(rsd[t[g]])
			plot,w,rsd,xtitle='Wave (A)',xran=[wall0,wall1], /xs, $
				ytitle='!3Obs-Cal',yran=yrng,/ys,pos=[0.07,0.05,0.98,0.30], $
				/noerase
			oplot,!x.crange,[0,0]
			oplot,[wlm0,wlm0],!y.crange,color=colordex('orange'),linesty=2
			oplot,[wlm1,wlm1],!y.crange,color=colordex('orange'),linesty=2
			if nb gt 0 then $
				oplot,w[t[b]],rsd[t[b]],psym=7
			for ib=0,n_elements(blines)-1 do $
				oplot,[blines[ib],blines[ib]],!y.crange,color=colordex('blue'), $
					linesty=2
			;
			; plot inverse sensitivity
			wset,0
			yrng = get_plotlims(sf,/log)
			plot,w,invsen,title=sname+' Img #: '+strn(kcfg.imgnum), $
				xtitle='Wave (A)',xran=[wall0,wall1],/xs, $
				ytitle='Effective Inv. Sens. (erg/cm^2/A/e-)', $
				yran=yrng,/ylog,/ys,xmargin=[11,3]
			oplot,w,finvsen,color=colordex('red')
			if nb gt 0 then $
				oplot,wf[b],mf[b],psym=7
			oplot,[wgoo0,wgoo0],10.^!y.crange,color=colordex('green'),thick=3
			oplot,[wgoo1,wgoo1],10.^!y.crange,color=colordex('green'),thick=3
			oplot,[wlm0,wlm0],10.^!y.crange,color=colordex('orange'),linesty=2
			oplot,[wlm1,wlm1],10.^!y.crange,color=colordex('orange'),linesty=2
			for ib=0,n_elements(blines)-1 do $
				oplot,[blines[ib],blines[ib]],10.^!y.crange,color=colordex('blue'), $
					linesty=2
			read,'r - restore pts, d - delete pts, + - increase fit order, - - decrease fit order, f - re-fit, q - quit fitting: ',q
			;
			; all done
			if strupcase(strtrim(q,2)) eq 'Q' then begin
				done = (1 eq 1)
			endif else begin
				;
				; re-fit
				if strupcase(strtrim(q,2)) eq 'F' then begin
					res = poly_fit(wf-wf0,sf,ford,/double,measure_error=me, $
							status=fitstat)
					finvsen = poly(w-wf0,res)
				endif else if strtrim(q,2) eq '+' then begin
					ford += 1
					print,'Fitting order: ',ford
				endif else if strtrim(q,2) eq '-' then begin
					ford -= 1
					print,'Fitting order: ',ford
				;
				; mark a region for resoration or deletion
				endif else begin
					wlma = -1.
					wlmb = -1.
					print,'Mark first wavelength limit'
					cursor,wlma,yy,/data,/down
					oplot,[wlma,wlma],10.^!y.crange,linesty=2
					print,'Mark next wavelength limit'
					cursor,wlmb,yy,/data,/down
					oplot,[wlmb,wlmb],10.^!y.crange,linesty=2
					wl0 = min([wlma,wlmb])
					wl1 = max([wlma,wlmb])
					roi = where(wf ge wl0 and wf le wl1, nroi)
					;
					if nroi gt 0 then begin
						;
						; restore points
						if strupcase(strtrim(q,2)) eq 'R' then begin
							use[roi] = 1
						;
						; delete points
						endif else if strupcase(strtrim(q,2)) eq 'D' then begin
							use[roi] = 0
						endif else print,'I do not understand ',q
					endif
				endelse	; marking a region
			endelse	; not quitting
		endwhile	; still working on fits
		;
		; now fit effective area
		res = poly_fit(wf-wf0,earea[t],ford,/double,measure_error=me, $
				status=fitstat)
		fearea = poly(w-wf0,res)
	endif else begin
		kcwi_print_info,ppar,pre,'no good wavelengths to fit',/error
		return
	endelse
	;
	; plot inverse sensitivity
	if interact then begin
		;
		; plot effective area (cm^2)
		goo = where(w gt wgoo0 and w lt wgoo1, ngoo)
		if ngoo gt 5 then begin
			maxea = max(fearea[goo])
			mo = moment(fearea[goo])
			yrng = get_plotlims(fearea[goo])
		endif else begin
			maxea = max(fearea)
			mo = moment(fearea)
			yrng = get_plotlims(fearea)
		endelse
		if yrng[0] lt 0. then yrng[0] = 0.0
		if area gt 0 then begin
			plot,w,earea, $
				title=sname+' Img #: '+strn(kcfg.imgnum)+' '+ $
				strtrim(kcfg.bgratnam,2)+' '+tlab, $
				xtitle='Wave (A)',xran=[wall0,wall1],/xs, $
				ytitle='Effective Area (cm^2/A)',ys=9, $
				yran=yrng,xmargin=[11,8]
			oplot,[wgoo0,wgoo0],!y.crange,color=colordex('green'), $
				thick=3
			oplot,[wgoo1,wgoo1],!y.crange,color=colordex('green'), $
				thick=3
			oplot,!x.crange,[maxea,maxea],linesty=2
			oplot,!x.crange,[mo[0],mo[0]],linesty=3
			axis,yaxis=1,yrange=100.*(!y.crange/area),ys=1, $
				ytitle='Efficiency (%)'
		endif else begin
			plot,w,earea, $
				title=sname+' Img #: '+strn(kcfg.imgnum)+' '+ $
				strtrim(kcfg.bgratnam,2)+' '+tlab, $
				xtitle='Wave (A)',xran=[wall0,wall1],/xs, $
				ytitle='Effective Area (cm^2/A)',yran=yrng,/ys
			oplot,[wgoo0,wgoo0],!y.crange,color=colordex('green'), $
				thick=3
			oplot,[wgoo1,wgoo1],!y.crange,color=colordex('green'), $
				thick=3
			oplot,!x.crange,[maxea,maxea],linesty=2
			oplot,!x.crange,[mo[0],mo[0]],linesty=3
		endelse
		;
		; overplot fit
		if ngoo gt 5 then $
			oplot,w[goo],fearea[goo],thick=5,color=colordex('blue')
		read,'Next: ',q
	endif
	;
	; write out effective inverse sensitivity
	;
	; update invsens header
	sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
	sxaddpar,hdr,'INVSENS','T',' effective inv. sens. spectrum?'
	sxaddpar,hdr,'INVSW0',w[z0],' low wavelength for eff inv. sens.', $
		format='F9.2'
	sxaddpar,hdr,'INVSW1',w[z1],' high wavelength for eff inv. sens.', $
		format='F9.2'
	sxaddpar,hdr,'INVSZ0',z0,' low wave pixel for eff inv. sens.'
	sxaddpar,hdr,'INVSZ1',z1,' high wave pixel for eff inv. sens.'
	sxaddpar,hdr,'INVSY0',gy0,' low spatial pixel for eff inv. sens.'
	sxaddpar,hdr,'INVSY1',gy1,' high spatial pixel for eff inv. sens.'
	sxaddpar,hdr,'INVSLMX',mxsl,' brightest std star slice'
	sxaddpar,hdr,'INVSL0',sl0,' lowest std star slice summed'
	sxaddpar,hdr,'INVSL1',sl1,' highest std star slice summed'
	sxaddpar,hdr,'INVSLY',cy,' spatial pixel position of std within slice'
	sxaddpar,hdr,'BUNIT','erg/cm^2/A/e-',' brightness units'
	sxaddpar,hdr,'EXPTIME',1.,' effective exposure time (seconds)'
	sxaddpar,hdr,'XPOSURE',1.,' effective exposure time (seconds)'
	;
	; remove old WCS
	sxdelpar,hdr,'RADESYS'
	sxdelpar,hdr,'EQUINOX'
	sxdelpar,hdr,'LONPOLE'
	sxdelpar,hdr,'LATPOLE'
	sxdelpar,hdr,'NAXIS2'
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
	sxaddpar,hdr,'WCSDIM',1
	sxaddpar,hdr,'CTYPE1','AWAV',' Air Wavelengths'
	sxaddpar,hdr,'CUNIT1','Angstrom',' Wavelength units'
	sxaddpar,hdr,'CNAME1','KCWI INVSENS Wavelength',' Wavelength name'
	sxaddpar,hdr,'CRVAL1',w0,' Wavelength zeropoint'
	sxaddpar,hdr,'CRPIX1',crpixw,' Wavelength reference pixel'
	sxaddpar,hdr,'CDELT1',dw,' Wavelength Angstroms per pixel'
	;
	; write out inverse sensitivity file
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_invsens',/nodir)
	kcwi_write_image,[[invsen],[finvsen],[obsspec]],hdr,ofil,ppar
	;
	; update effective area header
	sxaddpar,hdr,'INVSENS','F',' effective inv. sens. spectrum?'
	sxaddpar,hdr,'EFFAREA','T',' effective area spectrum?'
	sxaddpar,hdr,'BUNIT','cm^2/A',' brightness units'
	sxaddpar,hdr,'CNAME1','KCWI EA Wavelength',' Wavelength name'
	;
	; write out effective area file
	ofil = kcwi_get_imname(ppar,kcfg.imgnum,'_ea',/nodir)
	kcwi_write_image,[[earea],[fearea]],hdr,ofil,ppar
	;
	return
end

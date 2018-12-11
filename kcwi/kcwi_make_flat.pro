;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MAKE_FLAT
;
; PURPOSE:
;	This procedure creates a median-stacked cflat from a list of flat
;	image numbers given in KCWI_PPAR struct, then creates a flat image
;	that will take out the pixel-to-pixel variations.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_MAKE_FLAT, Ppar, Gfile
;
; INPUTS:
;	Ppar	- KCWI_PPAR struct for flat group to combine
;	Gfile	- FITS file containing KCWI_GEOM struct from STAGE3GEOM
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Uses list of flat frames to construct a master flat frame that
;	is median stacked.  Then fits each column to take out low
;	frequency structure and leave the pixel-to-pixel structure.
;
; EXAMPLE:
;
;	ppar = KCWI_PPAR_READ('mflat_0.ppar')
;	KCWI_MAKE_FLAT,ppar
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-29	Initial version
;	2013-SEP-10	Preliminary calculation of variance and mask images
;	2013-SEP-16	use ppar to pass pipeline params
;	2017-NOV-01	change from median to mean stack because of intensity decay
;	2017-NOV-13	adapt for bspline fitting of continuum for illum corr.
;-
pro kcwi_make_flat,ppar,gfile
	;
	; initialize
	pre = 'KCWI_MAKE_FLAT'
	;
	; check inputs
	if kcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	kcwi_print_info,ppar,pre,systime(0)
	;
	; are there flats listed?
	if strlen(ppar.cflats) le 0 then begin
		kcwi_print_info,ppar,pre,'no flats entered',/error
		return
	endif
	;
	; is geometry solved?
	if not file_test(gfile) then begin
		kcwi_print_info,ppar,pre,'no geometry file',/error
		return
	endif
	;
	; read in geometry file
	kgeom = mrdfits(gfile,1,ghdr,/silent)
	if kgeom.status ne 0 then begin
		kcwi_print_info,ppar,pre,'bad geometry solution',/error
		return
	endif
	;
	; read in wavemap image
	wmf = repstr(gfile,'_geom', '_wavemap')
	if file_test(wmf) then begin
		wavemap = mrdfits(wmf,0,wmfh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no wavemap file',/error
		return
	endelse
	;
	; read in slice image
	slf = repstr(gfile,'_geom','_slicemap')
	if file_test(slf) then begin
		slice = mrdfits(slf,0,slfh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no slicemap file',/error
		return
	endelse
	;
	; read in position image
	pof = repstr(gfile,'_geom','_posmap')
	if file_test(pof) then begin
		pos = mrdfits(pof,0,pofh,/fscale,/silent)
	endif else begin
		kcwi_print_info,ppar,pre,'no posmap file',/error
		return
	endelse
	;
	; directories
	if kcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		kcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; get image list for this group
	rangepar,ppar.cflats,fnums
	nf = n_elements(fnums)
	;
	; get image number format
	i_fmt = '(i0'+strn(ppar.fdigits)+')'
	;
	; get filename root
	root = ppar.froot
	;
	; log number of images
	kcwi_print_info,ppar,pre,'combining this many images',nf
	;
	; start with dark-subtracted flats
	tail = '_intd.fits'
	;
	; check first image
	ffil = reddir + root + string(fnums[0],form=i_fmt)+tail
	if not file_test(ffil) then begin
		;
		; try stage 1 output
		tail = '_int.fits'
		ffil = reddir + root + string(fnums[0],form=i_fmt)+tail
	endif
	if not file_test(ffil) then begin
		kcwi_print_info,ppar,pre,'Image file not found',ffil, $
			format='(a,a)',/error
		return
	endif
	;
	; read first image
	im = mrdfits(ffil,0,hdr,/fscale,/silent)
	;
	; id label
	idlab = 'Sl: ' + strtrim(kgeom.ifunam,2) + $
		', Fl: ' + strtrim(kgeom.filter,2) + $
		', Gr: ' + strtrim(kgeom.gratid,2) + $
		', Im: ' + strn(sxpar(hdr,'FRAMENO'))
	;
	; get type of flat
	internal = (strpos(sxpar(hdr,'CALTYPE'),'cflat') ge 0)
	;
	; get size
	sz = size(im,/dim)
	nx = sz[0]
	ny = sz[1]
	;
	; single image
	mflat = im
	mfvar = im
	;
	; how many images?
	if nf eq 2 then begin
		;
		; read second image
		ffil = reddir + root + string(fnums[1],form=i_fmt)+tail
		if file_test(ffil) then begin
			im = mrdfits(ffil,0,fhdr,/fscale,/silent)
		endif else begin
			kcwi_print_info,ppar,pre,'Image file not found', $
				ffil,/error
			return
		endelse
		;
		; make sure the sizes match
		sz = size(im,/dim)
		if sz[0] ne nx or sz[1] ne ny then begin
			kcwi_print_info,ppar,pre,'Size mismatch',ffil,/error
			return
		endif
		;
		; simple average
		mflat = ( mflat + im ) / 2.0
	endif else if nf ge 3 then begin
		;
		; set up stack
		stack = fltarr(nf,nx,ny)
		stack[0,*,*] = im
		djs_iterstat,im,mean=ref_mn,sigrej=3.0
		;
		; loop over remaining images
		for i=1,nf-1 do begin
	    		;
	    		; read next image
			ffil = reddir + root + string(fnums[i],form=i_fmt)+tail
			if file_test(ffil) then begin
				im = mrdfits(ffil,0,fhdr,/fscale,/silent)
			endif else begin
				kcwi_print_info,ppar,pre,'File not found', $
					ffil,/error
				return
			endelse
			;
			; make sure sizes match
			sz = size(im,/dim)
			if sz[0] ne nx or sz[1] ne ny then begin
				kcwi_print_info,ppar,pre,'Size mismatch', $
					ffil,/error
				return
			endif
			;
			; get new image mean
			djs_iterstat,im,mean=new_mn,sigrej=3.0
			;
			; insert into stack
			stack[i,*,*] = im * (ref_mn/new_mn)
		endfor	; loop over remaining images
		;
		; create master flat and variance from sigma rejected mean
		mflat = fltarr(nx,ny)
		mfvar = fltarr(nx,ny)
		for yi=0,ny-1 do for xi=0,nx-1 do begin
			djs_iterstat,stack[*,xi,yi],mean=mn,sigrej=3., $
				sigma=sg
			mflat[xi,yi] = mn
			mfvar[xi,yi] = sg^2
		endfor
	endif
	;
	; readnoise
	nba = sxpar(hdr,'NVIDINP')
	avrn = 0.
	;
	; did we have a master bias?
	if sxpar(hdr,'BIASRN1') then $	; yes, average the bias rn
		for ia = 0,nba-1 do $
			avrn = avrn + sxpar(hdr,'BIASRN'+strn(ia+1)) $
	else	for ia = 0,nba-1 do $	; no, average the oscan rn
			avrn = avrn + sxpar(hdr,'OSCNRN'+strn(ia+1))
	avrn = avrn / float(nba)
	avrn = avrn / sqrt(nf)
	;
	; binning
	xbin = kgeom.xbinsize
	ybin = kgeom.ybinsize
	;
	; setup queries
	ask=''
	;
	; set up plots
	do_plots = (ppar.display ge 1 or ppar.saveplots ge 2)
	if do_plots then begin
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		th = 3
		si = 1.75
	endif
	;
	; check for amp configuration that requires gainfloat correction
	; NB: this is currently disabled.  Overscan subtraction may be
	; doing this work.
	if nba eq 5 then begin
		;
		; get some keyword values
		gain1 = sxpar(hdr,'GAIN1')
		gain2 = sxpar(hdr,'GAIN2')
		oscor = sxpar(hdr,'OSCANSUB')
		;
		; subtract residual bias
		if not oscor then begin
			leftbias = mflat[2000/xbin+1:2048/xbin-1,0:4112/ybin-1]
			ritebias = mflat[2048/xbin+1:2096/xbin-1,0:4112/ybin-1]
			plothist,leftbias,xlh,ylh,xrange=[-5,5]
			resl = mpfitpeak(xlh,ylh,al)
			oplot,[al[1],al[1]],!y.crange,color=colordex('B')
			print,al[1]
			if ppar.display ge 2 then read,'next: ',ask
			plothist,ritebias,xrh,yrh
			resr = mpfitpeak(xrh,yrh,ar)
			oplot,[ar[1],ar[1]],!y.crange,color=colordex('R')
			print,ar[1]
			if ppar.display ge 2 then read,'next: ',ask
			diff = mflat - mflat
			diff[0:nx/2-1,*] = mflat[0:nx/2-1,*] - al[1]
			diff[nx/2:*  ,*] = mflat[nx/2:*  ,*] - ar[1]
		endif else diff = mflat
		;
		; pick up a trace to the left and to the right of the
		; physical amplifier seam
		buffer = 12
		left = diff[2046/xbin-buffer:2046/xbin-1,0:4112/ybin-1]
		rite = diff[2050/xbin:2050/xbin+buffer-1,0:4112/ybin-1]
		left = total(left,1)/(buffer*1.)
		rite = total(rite,1)/(buffer*1.)
		plot,left,thick=0.5,color=colordex('B')
		oplot,rite,thick=0.5,color=colordex('R')
		if ppar.display ge 2 then read,'next: ',ask
		grat = rite/left
		plothist,grat,xh,yh,bin=0.01
		res = mpfitpeak(xh,yh,ah)
		oplot,xh,res,color=colordex('O')
		meanrat = ah[1]
		kcwi_print_info,ppar,pre,'right/left amplifier ratio',meanrat, $
			format='(a,f9.3)'
		if ppar.display ge 2 then read,'next: ',ask
		plot,smooth(grat,15),thick=0.1
		oplot,!x.crange,[meanrat,meanrat],linestyle=2, $
			color=colordex('R')
		print,gain1,gain2,gain2*meanrat, (gain2-gain1)*1000
		mflat[nx/2:*,*] = mflat[nx/2:*,*] * meanrat
		sxaddpar,hdr,'GNTWK',meanrat,' / GAIN2 tweak factor'
		if ppar.display ge 2 then read,'next: ',ask
	endif
	;
	; parameters for fitting
	;
	; vignetted slice position range
	fitl = 4/xbin
	fitr = 24/xbin
	;
	; un-vignetted slice position range
	flatl = 34/xbin
	flatr = 72/xbin
	;
	; flat fitting slice position range
	ffleft = 10/xbin
	ffright = 70/xbin
	;
	buffer = 5.0/float(xbin)
	;
	; reference slice
	refslice = 9
	ffslice = refslice
	ffslice2 = refslice
	sm = 25
	allidx = findgen(140/xbin)
	str = string(fnums[0],"(i05)")
	;
	; correct vignetting if we are using internal flats
	if internal then begin
		;
		; get good region for fitting
		q=where(wavemap gt 0)
		waves=minmax(wavemap[q])
		dw=(waves[1]-waves[0])/30.0
		wavemin=(waves[0]+waves[1])/2.0-dw
		wavemax=(waves[0]+waves[1])/2.0+dw
		;
		; get reference slice data
		q = where(slice eq refslice and wavemap ge wavemin and $
			  wavemap le wavemax)
		;
		; account for spectral gradient
		xflat=pos[q] 
		yflat=mflat[q]
		wflat=wavemap[q]
		;
		; fit wavelength slope
		qflat=where(xflat ge flatl and xflat le flatr)
		xflat=xflat[qflat]
		yflat=yflat[qflat]
		wflat=wflat[qflat]
		sw=sort(wflat)
		xwflat=xflat[sw]
		ywflat=yflat[sw]
		wwflat=wflat[sw]
		wavlinfit=polyfit(wwflat,ywflat,2)
		yflat=yflat/poly(wflat,wavlinfit)
		pl_wlf = poly(wwflat,wavlinfit)
		s=sort(xflat)
		xflat=xflat[s]
		yflat=yflat[s]
		wflat=wflat[s]
		resflat=linfit(xflat,yflat)
		;
		; select the points we will fit for the vignetting 
		xfit=pos[q]  
		yfit=mflat[q]
		wflat=wavemap[q]
		yfit=yfit/poly(wflat,wavlinfit)
		;
		; fit vignetted region
		qfit=where(xfit ge fitl and xfit le fitr)
		xfit=xfit[qfit]
		yfit=yfit[qfit]
		s=sort(xfit)
		xfit=xfit[s]
		yfit=yfit[s]
		resfit=linfit(xfit,yfit)
		;
		; plot results
		if do_plots then begin
			;
			; plot wavelength slope fit
			yrng = get_plotlims([ywflat,pl_wlf])
			plot,wwflat,ywflat,title='Wavelength Slope for '+idlab,charsi=si, $
				charthi=th,xthick=th,ythick=th, $
				xtitle='Wavelength (A)', $
				ytitle='Flux (e-)',yran=yrng,/ys
			oplot,wwflat,pl_wlf,color=colordex('orange')
			if ppar.saveplots ge 3 then begin
				plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_wslope.png')
				write_png,plotfn,tvrd(/true)
				kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
			endif
			if ppar.display ge 2 then read,'next: ',ask
			;
			; plot vignetting correction fits
			plot,pos[q],mflat[q]/poly(wavemap[q],wavlinfit), $
				title='Vignetting for '+idlab, $
				charthi=th,charsi=si,psym=3,/nodata, $
				xtitle='Slice Pos (px)',xthick=th,/xs, $
				ytitle='Ratio',ythick=th
			oplot,allidx,resfit[0]+resfit[1]*allidx, $
				color=colordex('purple')
			oplot,allidx,resflat[0]+resflat[1]*allidx, $
				color=colordex('red')
			oplot,pos[q],mflat[q]/poly(wavemap[q],wavlinfit),psym=3
			oplot,[fitl,fitl],!y.crange,color=colordex('blue')
			oplot,[fitr,fitr],!y.crange,color=colordex('blue')
			oplot,[flatl,flatl],!y.crange,color=colordex('green')
			oplot,[flatr,flatr],!y.crange,color=colordex('green')
			if ppar.saveplots ge 2 then begin
				plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_vcor.png')
				write_png,plotfn,tvrd(/true)
				kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
			endif
			if ppar.display ge 2 then read,'next: ',ask
		endif
		; compute the intersection
		xinter=-(resflat[0]-resfit[0])/(resflat[1]-resfit[1])
		; figure out where the correction applies
		qinter=where(pos ge 0 and pos lt xinter-buffer)
		; apply the corection!
		newflat=mflat
		newflat[qinter] = (resflat[0]+resflat[1]*pos[qinter]) / $
				(resfit[0]+resfit[1]*pos[qinter])*mflat[qinter]
		; now deal with the the intermediate (buffer) region
		qspline=where(pos ge xinter-buffer and pos le xinter+buffer)
		posmin=min(pos[qspline])
		posmax=max(pos[qspline])
		valuemin = (resflat[0]+resflat[1]*posmin) / $
			   (resfit[0]+resfit[1]*posmin)
		valuemax=1
		slopeleft = resflat[1]/(resfit[1]*posmin+resfit[0]) - $
			   (resflat[1]*posmin+resflat[0])*resfit[1] / $
			  ((resfit[1]*posmin+resfit[0]) * $
			   (resfit[1]*posmin+resfit[0]))
		sloperight = resflat[1]/(resfit[1]*posmax+resfit[0]) - $
			    (resflat[1]*posmax+resflat[0])*resfit[1] / $
			   ((resfit[1]*posmax+resfit[0]) * $
			    (resfit[1] * posmax+resfit[0]))
		spline_p,[posmin,posmax],[valuemin,valuemax],xr,yr, $
			 interval=0.1,tan0=[-slopeleft,0],tan1=[-sloperight,0]
		yvals=interpol(yr,xr,pos[qspline])*mflat[qspline]
		newflat[qspline]=yvals
	;
	; non-internal flats don't need correction for vignetting
	endif else $
		newflat = mflat
	;
	; now fit master flat
	qref=where(slice eq refslice and pos ge ffleft and pos le ffright)
	xfr=wavemap[qref]
	yfr=newflat[qref]
	s=sort(xfr)
	xfr=xfr[s]
	yfr=yfr[s]
	;; correction for BM where we see a ledge.
        ;; added 171128
        if strtrim(kgeom.gratid) eq 'BM' then begin
           ledge_wave=kcwi_bm_ledge_position(kgeom.cwave)
	   kcwi_print_info,ppar,pre,'BM ledge calculated wavelength for ref slice (A)',ledge_wave, $
	   	format='(a,f10.2)'
           if ( ledge_wave ge kgeom.wavegood0 and $
	   	ledge_wave le kgeom.wavegood1 ) then begin
              qledge=where(xfr ge ledge_wave-25 and $
	      		   xfr le ledge_wave+25,nqledge)
              xledge=xfr[qledge]
              yledge=yfr[qledge]
              s=sort(xledge)
              xledge=xledge[s]
              yledge=yledge[s]
              smyledge=smooth(yledge,150)
              fpoints=findgen(101)/100.*50+ledge_wave-25
              ledgefit=bspline_iterfit(xledge,smyledge,fullbkpt=fpoints, $
	      				yfit=ylfit2,upper=1,lower=1)
              ylfit=bspline_valu(fpoints,ledgefit)
              deriv=-(shift(ylfit,1)-shift(ylfit,-1))/2.0
              deriv=deriv[2:-3]
              xvals=fpoints[2:-3]
              pkfit=mpfitpeak(xvals,deriv,apk,nterms=6)
              ;; how far?
              xlow=apk[1]-3-5
              xhi=apk[1]-3
              zlow=apk[1]+3
              zhi=apk[1]+3+5
              qlow=where(fpoints ge xlow and fpoints le xhi)
              lowfit=linfit(fpoints[qlow],ylfit[qlow])
              qhi=where(fpoints ge zlow and fpoints le zhi)
              hifit=linfit(fpoints[qhi],ylfit[qhi])
              ratio=(hifit[0]+hifit[1]*apk[1])/(lowfit[0]+lowfit[1]*apk[1])
              print,ratio
              qcorrect=where(xledge ge apk[1])
              yledge[qcorrect]/=ratio
              qcorr=where(xfr ge apk[1])
              yfr[qcorr]/=ratio
	      if do_plots then begin
	      	yrng = get_plotlims([smyledge,ylfit])
              	plot,xledge,smyledge,title='BM Grating Ledge for '+idlab,psym=3, $
			charsi=si,charth=th, $
			xtitle='Wavelength (A)',xthick=th,/xs, $
			ytitle='Flux (e-)',ythick=th,yran=yrng,/ys
              	oplot,fpoints,ylfit,color=colordex('orange')
              	oplot,[xlow,xlow],!y.crange,color=colordex('blue'),linestyle=2
              	oplot,[xhi,xhi],!y.crange,color=colordex('blue'),linestyle=2
              	oplot,[zlow,zlow],!y.crange,color=colordex('red'),linestyle=2
              	oplot,[zhi,zhi],!y.crange,color=colordex('red'),linestyle=2
                oplot,fpoints,lowfit[0]+lowfit[1]*fpoints, $
			color=colordex('purple'),linestyle=1,thick=th
                oplot,fpoints,hifit[0]+hifit[1]*fpoints, $
			color=colordex('green'),linestyle=1,thick=th
                oplot,xledge,yledge,psym=3
		if ppar.saveplots ge 3 then begin
			plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_grledge.png')
			write_png,plotfn,tvrd(/true)
			kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
		endif
		if ppar.display ge 2 then read,'next: ',ask
	      endif
           endif
        endif	; if handling ledge in BM data
        ;; 
	invvar=1/(1+abs(yfr))
	n=100.0
	bkpt = min(wavemap[qref])+findgen(n+1) * $
	      (max(wavemap[qref])-min(wavemap[qref]))/n
	sftr = bspline_iterfit(xfr,yfr,fullbkpt=bkpt,yfit=yfitr)
	; Generate a blue slice spectrum bspline fit 
	blueslice=12
	blueleft=60/xbin
	blueright=80/xbin
	qblue=where(slice eq blueslice and pos ge blueleft and pos le blueright)
	xfb=wavemap[qblue]
	yfb=newflat[qblue]
	s=sort(xfb)
	xfb=xfb[s]
	yfb=yfb[s]
	invvar=1/(1+abs(yfb))
	n=100.0
	bkpt = min(wavemap[qblue]) + findgen(n+1) * (max(wavemap[qblue]) - $
	       min(wavemap[qblue])) / n
	sftb = bspline_iterfit(xfb,yfb,fullbkpt=bkpt,yfit=yfitb)
	; 

	; Generate a red slice spectrum bspline fit
	redslice=23
	redleft=60/xbin
	redright=80/xbin
	qred=where(slice eq redslice and pos ge redleft and pos le redright)
	xfd=wavemap[qred]
	yfd=newflat[qred]
	s=sort(xfd)
	xfd=xfd[s]
	yfd=yfd[s]
	invvar=1/(1+abs(yfd))
	n=100.0
	bkpt = min(wavemap[qred]) + findgen(n+1) * (max(wavemap[qred]) - $
	       min(wavemap[qred])) / n
	sftd = bspline_iterfit(xfd,yfd,fullbkpt=bkpt,yfit=yfitd)
	
	; waves.
	minwave=min(xfb)
	maxwave=max(xfd)
	nwaves=1000.0
	waves=minwave+(maxwave-minwave)*findgen(nwaves+1)/nwaves

	if do_plots then begin
		yrng = get_plotlims([yfitr,yfitb,yfitd])
		plot,xfr,yfitr,title='Blue/Red Fits for '+idlab,charsi=si,charthi=th, $
			xtitle='Wavelength (A)',xthick=th, $
			xrange=[kgeom.waveall0,kgeom.waveall1],/xs, $
			ytitle='Flux (e-)',ythick=th,yrange=yrng,/ys
		oplot,xfb,yfitb,color=colordex('blue')
		oplot,xfd,yfitd,color=colordex('red')
		oplot,[kgeom.wavegood0,kgeom.wavegood0],!y.crange, $
			color=colordex('green')
		oplot,[kgeom.wavegood1,kgeom.wavegood1],!y.crange, $
			color=colordex('green')
		oplot,!x.crange,[0.,0.]
		if ppar.saveplots ge 3 then begin
			plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_brfits.png')
			write_png,plotfn,tvrd(/true)
			kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
		endif
		if ppar.display ge 2 then read,'next: ',ask
	endif

	wavebuffer=0.1
	minrwave=min(xfr)
	maxrwave=max(xfr)
	wavebuffer2=0.05

	qbluefit = where(waves lt minrwave+(maxrwave-minrwave)*wavebuffer and $
		         waves gt minrwave+(maxrwave-minrwave)*wavebuffer2, $
			 nqb)
	qredfit = where(waves ge minrwave+(maxrwave-minrwave)*(1-wavebuffer) $
		and waves lt minrwave+(maxrwave-minrwave)*(1-wavebuffer2), $
		nqr)

	if nqb gt 0 then begin
		bluefit = bspline_valu(waves[qbluefit],sftb)
		refbluefit = bspline_valu(waves[qbluefit],sftr)
		bluelinfit = linfit(waves[qbluefit],refbluefit/bluefit)
	endif
	if nqr gt 0 then begin
		redfit = bspline_valu(waves[qredfit],sftd)
		refredfit = bspline_valu(waves[qredfit],sftr)
		redlinfit = linfit(waves[qredfit],refredfit/redfit)
	endif

	if do_plots then begin
		if nqb gt 1 then begin
			;
			; plot the blue fits
			yrng = get_plotlims([refbluefit,bluefit])
			yrng[0] = 0.
			plot,waves[qbluefit],refbluefit,title='Blue Fits for '+idlab, $
				charsi=si,charthi=th, $
				xtitle='Wavelength (A)',xthick=th,/xs, $
				ytitle='Flux (e-)',ythick=th,yran=yrng,/ys
			oplot,waves[qbluefit],bluefit,color=colordex('blue')
			if ppar.saveplots ge 3 then begin
				plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_bfits.png')
				write_png,plotfn,tvrd(/true)
				kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
			endif
			if ppar.display ge 2 then read,'next: ',ask
			;
			; plot blue ratios
			yrng = get_plotlims([refbluefit/bluefit, $
				bluelinfit[0]+bluelinfit[1]*waves[qbluefit]])
			plot,waves[qbluefit],refbluefit/bluefit, $
				title='Blue Ratio for '+idlab,charsi=si,charthi=th, $
				xtitle='Wavelength (A)',xthick=th,/xs, $
				ytitle='Ratio',ythick=th,yran=yrng,/ys
			oplot,waves[qbluefit], $
		      		bluelinfit[0]+bluelinfit[1]*waves[qbluefit], $
		      		color=colordex('blue')
			if ppar.saveplots ge 3 then begin
				plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_bratio.png')
				write_png,plotfn,tvrd(/true)
				kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
			endif
			if ppar.display ge 2 then read,'next: ',ask
		endif

		if nqr gt 1 then begin
			;
			; plot the red fits
			yrng = get_plotlims([refredfit,redfit])
			yrng[0] = 0.
			plot,waves[qredfit],refredfit,title='Red Fits for '+idlab, $
				charsi=si,charthi=th, $
				xtitle='Wavelength (A)',xthick=th,/xs, $
				ytitle='Flux (e-)',ythick=th,yran=yrng,/ys
			oplot,waves[qredfit],redfit,color=colordex('red')
			if ppar.saveplots ge 3 then begin
				plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_rfits.png')
				write_png,plotfn,tvrd(/true)
				kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
			endif
			if ppar.display ge 2 then read,'next: ',ask
			;
			; plot the red ratios
			yrng = get_plotlims([refredfit/redfit, $
				redlinfit[0]+redlinfit[1]*waves[qredfit]])
			plot,waves[qredfit],refredfit/redfit, $
				title='Red Ratio for '+idlab,charsi=si,charth=th, $
				xtitle='Wavelength (A)',xthick=th,/xs, $
				ytitle='Ratio',ythick=th,yran=yrng,/ys
			oplot,waves[qredfit], $
		      		redlinfit[0]+redlinfit[1]*waves[qredfit], $
		      		color=colordex('red')
			if ppar.saveplots ge 3 then begin
				plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_rratio.png')
				write_png,plotfn,tvrd(/true)
				kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
			endif
			if ppar.display ge 2 then read,'next: ',ask
		endif
	endif

	;; at this point we are going to try to merge the points
	qselred=where(xfd ge maxrwave, nqsr)
	qselblue=where(xfb le minrwave, nqsb)
	
	if nqsr gt 0 then $
		redfluxes=yfd[qselred]*(redlinfit[0]+redlinfit[1]*xfd[qselred])
	if nqsb gt 0 then $
		bluefluxes=yfb[qselblue]*(bluelinfit[0]+bluelinfit[1]*xfb[qselblue])
	allx = xfr
	ally = yfr
	if nqsb gt 0 then begin
		allx = [xfb[qselblue],allx]
		ally = [bluefluxes,ally]
	endif
	if nqsr gt 0 then begin
		allx = [allx, xfd[qselred]]
		ally = [ally, redfluxes]
	endif
	
	s=sort(allx)
	allx=allx[s]
	ally=ally[s]
	
	
	invvar=1/(1+abs(yfb))
	n=100.0
	bkpt=min(allx)+findgen(n+1)*(max(allx)-min(allx))/n
	sftall=bspline_iterfit(allx,ally,fullbkpt=bkpt,yfit=yfitall)
	fdecomp,ppar.masterflat,disk,dir,flrute,ext
	
	if do_plots then begin
		yrng = get_plotlims([yfr,ally,yfitall,yfitr])
		plot,xfr,yfr,title='Master Illumination for '+idlab,psym=3, $
			charsi=si,charthi=th, /nodata, $
			xtitle='Wavelength (A)',xthick=th, $
			xrange=[kgeom.waveall0,kgeom.waveall1],/xs, $
			ytitle='Flux (e-)',ythick=th,yran=yrng,/ys
		oplot,allx,ally,psym=3,color=colordex('purple')
		oplot,allx,yfitall,color=colordex('red'),thick=2
		oplot,xfr,yfr,psym=3
		oplot,xfr,yfitr,color=colordex('green'),thick=2
		oplot,[kgeom.wavegood0,kgeom.wavegood0],!y.crange, $
			color=colordex('orange')
		oplot,[kgeom.wavegood1,kgeom.wavegood1],!y.crange, $
			color=colordex('orange')
		if ppar.saveplots ge 2 then begin
			plotfn = reddir + repstr(ppar.masterflat, $
						'_mflat.fits','_mffit.png')
			write_png,plotfn,tvrd(/true)
			kcwi_print_info,ppar,pre,'saved plot to',plotfn,format='(a,a)'
		endif
		if ppar.display ge 2 then read,'next: ',ask
	endif

	; OK. Now we have extended to the full range... so... we are going to
	; make a ratio flat!
	
	comflat=newflat-newflat
	qz= where(wavemap ge 0 );and slice ge 0 and slice le 23)
	
	comvals=bspline_valu(wavemap[qz],sftall)
	
	comflat[qz]=comvals
	ratio=newflat-newflat
	qzer = where(newflat ne 0)
	ratio[qzer]=comflat[qzer]/newflat[qzer]
	;
	; set up mask image
	mask = byte(ratio-ratio)
	;
	; trim negative points
	qq=where(ratio lt 0, nfix)
	if nfix gt 0 then begin
		ratio[qq] = 0.0
		mask[qq] = 8b
	endif
	;
	; trim high points near edges of slice
	qq=where(ratio ge 3 and (pos le 4/xbin or pos ge 136/xbin), nfix)
	if nfix gt 0 then begin
		ratio[qq] = 0.0
		mask[qq] = 16b
	endif
	;
	; don't correct low signal points
	qq = where(newflat lt 30., nfix)
	if nfix gt 0 then begin
		ratio[qq] = 1.0
		mask[qq] = 32b
	endif
	;
	; update master flat header
	sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
	sxaddpar,hdr,'NMEAN',nf, $
		' number of images used for stack'
	sxaddpar,hdr,'FLATLST',ppar.cflats, $
		' range list of image numbers for stack'
	sxaddpar,hdr,'AVRDN',avrn,' Stack RN accounting for root n'
	fxaddpar,hdr,'POSMAPF',pof,' Posmap file',after='AVRDN'
	fxaddpar,hdr,'SLIMAPF',slf,' Slicemap file',after='AVRDN'
	fxaddpar,hdr,'WAVMAPF',wmf,' Wavemap file',after='AVRDN'
	varhdr = hdr
	imghdr = hdr
	mskhdr = hdr
	;
	; write out master image
	sxaddpar,imghdr,'BUNIT','electrons',' brightness units'
	sxaddpar,imghdr,'HISTORY','  Master flat stack image'
	isf = repstr(ppar.masterflat,'_mflat','_mfimg')
	kcwi_write_image,mflat,imghdr,isf,ppar
	;
	; write out master flat
	sxaddpar,hdr,'MASTFLAT','T',' master flat image?'
	sxdelpar,hdr,'BUNIT'
	kcwi_write_image,ratio,hdr,ppar.masterflat,ppar
	;
	; write out master variance
	sxaddpar,varhdr,'BUNIT','electrons^2',' brightness units (variance)'
	sxaddpar,varhdr,'HISTORY','  Master cflat stack variance'
	vaf = repstr(ppar.masterflat,'_mflat','_mfvar')
	kcwi_write_image,mfvar,varhdr,vaf,ppar
	;
	; write out master mask
	sxdelpar,mskhdr,'BUNIT'
	sxaddpar,mskhdr,'BSCALE',1.
	sxaddpar,mskhdr,'BZERO',0
	sxaddpar,mskhdr,'MASKIMG','T',' mask image?'
	msf = repstr(ppar.masterflat,'_mflat','_mfmsk')
	kcwi_write_image,mask,mskhdr,msf,ppar
	;
	return
end

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
	; get type of flat
	internal = (strpos(sxpar(hdr,'CALTYPE'),'cflat') ge 0)
	;
	; get size
	sz = size(im,/dim)
	nx = sz[0]
	ny = sz[1]
	;
	; set up stacks
	stack = fltarr(nf,nx,ny)
	nstack = 1
	stack[0,*,*] = im
	mflat = im		; single image
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
			; insert into stack
			stack[i,*,*] = im
		endfor	; loop over remaining images
		;
		; create master flat from median (mean) stack
		mflat = fltarr(nx,ny)
		for yi=0,ny-1 do for xi=0,nx-1 do $
			mflat[xi,yi] = mean(stack[*,xi,yi])
	endif
	;
	; parameters for fitting
	xbin = kgeom.xbinsize
	fitl = 4/xbin
	fitr = 24/xbin
	flatl = 34/xbin
	flatr = 72/xbin
	ffleft = 10/xbin
	ffright = 70/xbin
	buffer = 5.0/float(xbin)
	refslice = kgeom.refbar/5
	if xbin le 1 then refslice = 9
	ffslice = refslice
	ffslice2 = refslice
	sm = 25
	allidx = findgen(140/xbin)
	str = string(fnums[0],"(i05)")
	ask=''
	;
	; set up plots
	do_plots = (ppar.display ge 1)
	if do_plots then begin
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
	endif
	;
	; correct vignetting if we are using internal flats
	if internal then begin
		q=where(wavemap gt 0)
		waves=minmax(wavemap[q])
		dw=(waves[1]-waves[0])/30.0
		wavemin=(waves[0]+waves[1])/2.0-dw
		wavemax=(waves[0]+waves[1])/2.0+dw
		print,wavemin,wavemax
		q = where(slice eq refslice and wavemap ge wavemin and $
			  wavemap le wavemax)
		; select the points we will fit for the vignetting 
		xfit=pos[q]  
		yfit=mflat[q]
		qfit=where(xfit ge fitl and xfit le fitr)
		xfit=xfit[qfit]
		yfit=yfit[qfit]
		s=sort(xfit)
		xfit=xfit[s]
		yfit=yfit[s]
		resfit=linfit(xfit,yfit)
		; select the template region
		xflat=pos[q] 
		yflat=mflat[q]
		qflat=where(xflat ge flatl and xflat le flatr)
		xflat=xflat[qflat]
		yflat=yflat[qflat]
		s=sort(xflat)
		xflat=xflat[s]
		yflat=yflat[s]
		resflat=linfit(xflat,yflat)
		if do_plots then begin
			plot,pos[q],mflat[q],psym=3,/xs,/nodata
			oplot,allidx,resfit[0]+resfit[1]*allidx, $
				color=colordex('purple')
			oplot,allidx,resflat[0]+resflat[1]*allidx, $
				color=colordex('red')
			oplot,pos[q],mflat[q],psym=3
			oplot,[fitl,fitl],!y.crange,color=colordex('blue')
			oplot,[fitr,fitr],!y.crange,color=colordex('blue')
			oplot,[flatl,flatl],!y.crange,color=colordex('green')
			oplot,[flatr,flatr],!y.crange,color=colordex('green')
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
		plot,xfr,yfitr,xrange=[kgeom.waveall0,kgeom.waveall1]
		oplot,xfb,yfitb,color=colordex('blue')
		oplot,xfd,yfitd,color=colordex('red')
		oplot,[kgeom.wavegood0,kgeom.wavegood0],!y.crange, $
			color=colordex('green')
		oplot,[kgeom.wavegood1,kgeom.wavegood1],!y.crange, $
			color=colordex('green')
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
			plot,waves[qbluefit],refbluefit
			oplot,waves[qbluefit],bluefit,color=colordex('blue')
			if ppar.display ge 2 then read,'next: ',ask

			plot,waves[qbluefit],refbluefit/bluefit,/ys
			oplot,waves[qbluefit], $
		      		bluelinfit[0]+bluelinfit[1]*waves[qbluefit], $
		      		color=colordex('blue')
			if ppar.display ge 2 then read,'next: ',ask
		endif

		if nqr gt 1 then begin
			plot,waves[qredfit],refredfit
			oplot,waves[qredfit],redfit,color=colordex('red')
			if ppar.display ge 2 then read,'next: ',ask

			plot,waves[qredfit],refredfit/redfit,/ys
			oplot,waves[qredfit], $
		      		redlinfit[0]+redlinfit[1]*waves[qredfit], $
		      		color=colordex('red')
			if ppar.display ge 2 then read,'next: ',ask
		endif
	endif

	;; at this point we are going to try to merge the points
	qselred=where(xfd ge maxrwave)
	qselblue=where(xfb le minrwave)
	
	redfluxes=yfd[qselred]*(redlinfit[0]+redlinfit[1]*xfd[qselred])
	bluefluxes=yfb[qselblue]*(bluelinfit[0]+bluelinfit[1]*xfb[qselblue])
	
	allx=[xfb[qselblue],xfr,xfd[qselred]]
	ally=[bluefluxes,yfr,redfluxes]
	
	s=sort(allx)
	allx=allx[s]
	ally=ally[s]
	
	
	invvar=1/(1+abs(yfb))
	n=100.0
	bkpt=min(allx)+findgen(n+1)*(max(allx)-min(allx))/n
	sftall=bspline_iterfit(allx,ally,fullbkpt=bkpt,yfit=yfitall)
	
	if do_plots then begin
		plot,xfr,yfr,psym=3,xrange=[kgeom.waveall0,kgeom.waveall1], $
			/xs,/nodata
		oplot,allx,ally,psym=3,color=colordex('purple')
		oplot,allx,yfitall,color=colordex('red'),thick=2
		oplot,xfr,yfr,psym=3
		oplot,xfr,yfitr,color=colordex('green'),thick=2
		oplot,[kgeom.wavegood0,kgeom.wavegood0],!y.crange, $
			color=colordex('orange')
		oplot,[kgeom.wavegood1,kgeom.wavegood1],!y.crange, $
			color=colordex('orange')
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
	
	qq=where(ratio lt 0)
	ratio[qq]=0.0
	qq=where(ratio ge 2)
	ratio[qq]=2
	
	rff = repstr(ppar.masterflat,'_mflat','_ratio_f')
	kcwi_write_image,ratio,hdr,rff,ppar

	; select the pixels in the reference slice and correct positions
	qff0=where(slice eq ffslice and pos ge ffleft and pos le ffright)
	qff=where(slice eq ffslice2 and pos ge ffleft and pos le ffright)
	; plot a little more than the KCWI blue range
	if do_plots then begin
		plot,wavemap[qff0],smooth(newflat[qff0],sm),psym=3, $
			/xs,xrange=[kgeom.waveall0,kgeom.waveall1]
	
		oplot,wavemap[qff],smooth(newflat[qff],sm),psym=3, $
			color=colordex('orange')
		oplot,[kgeom.wavegood0,kgeom.wavegood0],!y.crange, $
			color=colordex('green')
		oplot,[kgeom.wavegood1,kgeom.wavegood1],!y.crange, $
			color=colordex('green')
	endif
	
	; generate the bspline fit.
	xf=wavemap[qff0]
	yf=newflat[qff0]
	s=sort(xf)
	xf=xf[s]
	yf=yf[s]
	res=1/(1+abs(yf))
	n=100.0
	bkpt = min(wavemap[qff0])+findgen(n+1)*(max(wavemap[qff0]) - $
	       min(wavemap[qff0]))/n
	sft = bspline_iterfit(xf,yf,fullbkpt=bkpt,yfit=yfit)

	xf1=wavemap[qff]
	yf1=newflat[qff]
	s=sort(xf1)
	xf1=xf1[s]
	yf1=yf1[s]
	bkpt = min(wavemap[qff])+findgen(n+1)*(max(wavemap[qff]) - $
	       min(wavemap[qff]))/n
	sft0=bspline_iterfit(xf1,yf1,fullbkpt=bkpt,yfit=yfit1)
	yfita=bspline_valu(xf,sft0)
	
	; so the spline works pretty good, but we need to stitch it together
	; for the full wavelenth range... 
	
	;print,ec
	if do_plots then begin
		oplot,wavemap[qff0],yfit,color=colordex('blue'),psym=3
		if ppar.display ge 2 then read,'next: ',ask

		plot,wavemap[qff0],yfit-newflat[qff0],psym=3,/xs
		if ppar.display ge 2 then read,'next: ',ask

		plot,wavemap[qff0],smooth(newflat[qff0],sm),psym=3, $
			/xs,xrange=[kgeom.waveall0,kgeom.waveall1]
		oplot,wavemap[qff0],yfit,color=colordex('blue'),psym=3
		oplot,wavemap[qff],smooth(newflat[qff],sm),psym=3, $
			color=colordex('green')
		oplot,wavemap[qff0],yfita,color=colordex('red'),psym=3
		oplot,[kgeom.wavegood0,kgeom.wavegood0],!y.crange, $
			color=colordex('orange')
		oplot,[kgeom.wavegood1,kgeom.wavegood1],!y.crange, $
			color=colordex('orange')
		if ppar.display ge 2 then read,'next: ',ask
	endif
	
	qz = where(pos ge 0)
	
	xp=wavemap[qz]
	newvals=bspline_valu(xp,sft)

	if do_plots then begin
		plot,xp,newvals,psym=3,title='newvals'
		oplot,[kgeom.wavegood0,kgeom.wavegood0],!y.crange, $
			color=colordex('green')
		oplot,[kgeom.wavegood1,kgeom.wavegood1],!y.crange, $
			color=colordex('green')
		if ppar.display ge 2 then read,'next: ',ask
	endif
	
	comflat=newflat-newflat
	comflat[qz]=newvals
	cff = repstr(ppar.masterflat,'_mflat','_comflat')
	kcwi_write_image,comflat,hdr,cff,ppar
	
	qnz = where(newflat ne 0)
	ratio=newflat-newflat
	ratio[qnz]=comflat[qnz]/newflat[qnz]

	;
	; update master flat header
	sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
	sxaddpar,hdr,'NMEAN',nf, $
		' number of images used for stack'
	sxaddpar,hdr,'MASTFLAT','T',' master flat image?'
	sxaddpar,hdr,'FLATLST',ppar.cflats, $
		' range list of image numbers for stack'
	;
	; write out image file
	kcwi_write_image,ratio,hdr,ppar.masterflat,ppar
	;
	return
end

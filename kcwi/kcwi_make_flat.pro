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
	; correct vignetting if we are using internal flats
	if internal then begin
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		xbin = kgeom.xbinsize
		fitl = 4/xbin
		fitr = 24/xbin
		flatl = 34/xbin
		flatr = 72/xbin
		buffer = 5.0/float(xbin)
		refslice = kgeom.refbar/5
		allidx = findgen(140/xbin)
		str = string(fnums[0],"(i05)")
		q=where(wavemap gt 0)
		waves=minmax(wavemap[q])
		dw=(waves[1]-waves[0])/30.0
		wavemin=(waves[0]+waves[1])/2.0-dw
		wavemax=(waves[0]+waves[1])/2.0+dw
		print,wavemin,wavemax
		q=where(slice eq refslice and wavemap ge wavemin and wavemap le wavemax)
		plot,pos[q],mflat[q],psym=3,/xs
		ask=''
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
		oplot,allidx,resfit[0]+resfit[1]*allidx,color=colordex('purple')
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
		oplot,allidx,resflat[0]+resflat[1]*allidx,color=colordex('red')
		; compute the intersection
		xinter=-(resflat[0]-resfit[0])/(resflat[1]-resfit[1])
		; figure out where the correction applies
		qinter=where(pos ge 0 and pos lt xinter-buffer)
		; apply the corection!
		newflat=mflat
		newflat[qinter]= (resflat[0]+resflat[1]*pos[qinter])/(resfit[0]+resfit[1]*pos[qinter])*mflat[qinter]
		; now deal with the the intermediate (buffer) region
		qspline=where(pos ge xinter-buffer and pos le xinter+buffer)
		posmin=min(pos[qspline])
		posmax=max(pos[qspline])
		valuemin=(resflat[0]+resflat[1]*posmin)/(resfit[0]+resfit[1]*posmin)
		valuemax=1
		slopeleft=resflat[1]/(resfit[1]*posmin+resfit[0])-(resflat[1]*posmin+resflat[0])*resfit[1]/((resfit[1]*posmin+resfit[0])*(resfit[1]*posmin+resfit[0]))
		sloperight=resflat[1]/(resfit[1]*posmax+resfit[0])-(resflat[1]*posmax+resflat[0])*resfit[1]/((resfit[1]*posmax+resfit[0])*(resfit[1]*posmax+resfit[0]))
		;slopemid=resflat[1]/(resfit[1]*xinter+resfit[0])-(resflat[1]*xinter+resflat[0])*resfit[1]/((resfit[1]*xinter+resfit[0])*(resfit[1]*xinter+resfit[0]))
		;print,slopeleft,slopemid,sloperight
		spline_p,[posmin,posmax],[valuemin,valuemax],xr,yr,interval=0.1,tan0=[-slopeleft,0],tan1=[-sloperight,0]
		read,'next: ',ask
		plot,xr,yr,/ys               ;,psym=3
		yvals=interpol(yr,xr,pos[qspline])*mflat[qspline]
		newflat[qspline]=yvals
   
		print,xinter
	endif
	;
	; now fit master flat
	kcwi_ratio_flat,mflat,hdr,ppar,flato
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
	kcwi_write_image,flato,hdr,ppar.masterflat,ppar
	;
	return
end

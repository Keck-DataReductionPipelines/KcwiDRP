; $Id: kcwi_quick.pro,v 1.9 2015/02/25 07:04:55 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_QUICK
;
; PURPOSE:
;	This does a quick-look reduction on a single observation.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_QUICK, RawDir, ReducedDir, CalibDir, DataDir
;
; OPTIONAL INPUTS:
;	RawDir		- input raw directory (string) defaults to current dir
;	ReducedDir	- reduced data directory (string) defaults to './redux/'
;	CalibDir	- calib source directory (string) defaults to !KCWI_DATA + 'calib/'
;	DataDir		- KCWI data directory (string) defaults to !KCWI_DATA
;
; KEYWORDS:
;	IMGTARG		- target image number for processing
; Params
;	FROOT		- root of image filenames (def: 'image')
;	FDIGITS		- number of digits in image numbers (def: 4)
;	MINGROUPBIAS	- minimum number of bias images per group (def: 5)
;	MINGROUPDARK	- minimum number of dark images per group (def: 3)
;	MINOSCANPIX	- minimum number of overscan pixels for subtraction (def: 100)
; Wavelength fitting params (only relevant for full-ccd images)
;	PKSIG		- significance of peaks to find (def:1.5)
;	PKDEL		- matching thresshold in Ang (def: 0.75)
;	PKISO		- isolation of peaks in Ang (def: 2.0)
; Switches
;	CWI		- set for CWI data: skip first bias image, use CWI associations (def: NO)
;	NOCRREJECT	- set to skip cosmic ray rejection
;	NONASSUB	- set to skip nod-and-shuffle subtraction
;	NOCLEANCOEFFS	- set to skip cleaning wavelength sol'n coeffs
;	SAVEINTIMS	- set to save intermediate images (def: NO)
;	INCLUDETEST	- set to include test images in reduction (def: NO)
;	CLOBBER		- set to clobber existing images (def: no clobber)
;	VERBOSE		- set gt 0 to get extra screen output (def: 0)
;	DISPLAY		- set gt 0 to display diagnostic result plots
;	DS9		- set to display resulting image with ds9
;	SAVEPLOTS	- set to create hardcopies of displayed plots
;	HELP		- set to only print a list of params and keywords
;				(overrides all other keywords)
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	outputs pipeline parameter files *.ppar in ODIR with associations
;	for running KCWI_STAGE{N}.  Master calibration image pipeline parameter 
;	files are also generated in ODIR.  Link files are also generated 
;	for each correction stage (bias,dark,flat,geom) listing the default 
;	master calibration image(s) for each image to be processed.
;
; PROCEDURE:
;	Analyzes the FITS headers of the images in InputDir to determine the 
;	bias groups.  These are then associated with each calibration and
;	object image based on instrument configuration and temporal proximity.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-SEP-30	Initial version
;-
pro kcwi_quick,rawdir,reduceddir,calibdir,datadir, $
	imgtarg=imgtarg, $
	froot=froot, $
	fdigits=fdigits, $
	mingroupbias=mingroupbias, $
	mingroupdark=mingroupdark, $
	minoscanpix=minoscanpix, $
	pksig=pksig, pkdel=pkdel, pkiso=pkiso, $
	cwi=cwi, $
	nocrreject=nocrreject, $
	nonassub=nonassub, $
	nocleancoeffs=nocleancoeffs, $
	saveintims=saveintims, $
	includetest=includetest, $
	clobber=clobber, $
	verbose=verbose, $
	display=display, $
	ds9=ds9, $
	saveplots=saveplots, $
	help=help
	;
	; setup
	pre = 'KCWI_QUICK'
	version = repstr('$Revision: 1.9 $ $Date: 2015/02/25 07:04:55 $','$','')
	startime=systime(1)
	q = ''	; for queries
	;
	; requested help?
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', RawDir, ReducedDir, CalibDir, DataDir'
		print,pre+': Select input imgnum   : IMGTARG=<img_num> (defaults to most recent object image)'
		print,pre+': Info - Param  Keywords: FROOT=<img_file_root>, FDIGITS=N, MINGROUPBIAS=N, MINOSCANPIX=N'
		print,pre+': Info - Wl Fit Keywords: PKSIG=<sigma_significance>, PKDEL=<match_delta>, PKISO=<peak_isolation>'
		print,pre+': Info - Switch Keywords: /CWI, /NOCRREJECT, /NONASSUB, /NOCLEANCOEFFS, /DS9'
		print,pre+': Info - Switch Keywords: /SAVEINTIMS, /INCLUDETEST, /CLOBBER, VERBOSE=, DISPLAY=, /SAVEPLOTS, /HELP'
		return
	endif
	;
	; instantiate and init a ppar structure for the pipeline parameters
	A = {kcwi_ppar}
	ppar = struct_init(A)
	;
	; set initialized and version
	ppar.initialized = 1
	ppar.progid = pre+': '+version
	;
	; set from keyword values
	ppar.verbose = verbose
	ppar.display = display
	;
	; check directory inputs
	if n_elements(rawdir) le 0 then $
		indir = ppar.rawdir $
	else	indir = rawdir
	if n_elements(reduceddir) le 0 then $
		odir = ppar.reddir $
	else	odir = reduceddir
	if n_elements(calibdir) le 0 then $
		caldir = !KCWI_DATA+'calib/' $
	else	caldir = calibdir
	if n_elements(datadir) le 0 then $
		ddir = !KCWI_DATA $
	else	ddir = datadir
	;
	; set caldir to reduced data dir for now
	caldir = odir
	;
	; expand paths
	odir   = kcwi_expand_dir(odir)
	indir  = kcwi_expand_dir(indir)
	caldir = kcwi_expand_dir(caldir)
	ddir   = kcwi_expand_dir(ddir)
	;
	; check if odir exists
	if not file_test(odir,/directory) then begin
		print,pre+': Warning - output dir does not exist: ',odir
		read,'Create? (Y/n): ',q
		q = strupcase(strtrim(q,2))
		if strmid(q,0,1) ne 'N' then begin
			file_mkdir,odir,/noexpand
			if verbose ge 1 then $
				print,pre+': Info - created directory: ',odir
		endif else begin
			print,pre+': Error - no output dir, returning'
			return
		endelse
	endif
	;
	; check if odir accessible
	if not file_test(odir,/directory,/executable,/write) then begin
		print,pre+': Error - output dir not accessible, returning'
		return
	endif
	;
	; check if indir accessible
	if not file_test(indir,/directory,/executable,/read) then begin
		print,pre+': Error - cannot access input dir: ',indir,', returning'
		return
	endif
	;
	; check if ddir accessible
	if not file_test(ddir,/directory,/executable,/read) then begin
		print,pre+': Error - cannot access data dir: ',ddir,', returning'
		return
	endif
	;
	; check if caldir accessible
	if not file_test(caldir,/directory,/executable,/read) then begin
		print,pre+': Error - cannot access calib dir: ',caldir,', returning'
		return
	endif
	;
	; record directories
	ppar.rawdir = indir
	ppar.reddir = odir
	ppar.caldir = caldir
	ppar.datdir = ddir
	;
	; check image name prefix or root
	if keyword_set(froot) then $
		ppar.froot = froot
	;
	; now check number of digits in image number
	;
	; specified with keyword
	if keyword_set(fdigits) then $
		ppar.fdigits = fdigits $
	;
	; derive from file names in INDIR
	else begin
		flist = file_search(indir + ppar.froot+'*.fit*', count=nf)
		if nf le 0 then begin
			kcwi_print_info,ppar,pre,'no fits files found in '+indir,/error
			return
		endif
		fdig = 0
		for i=0,nf-1 do begin
			ndig = kcwi_get_digits(flist[i])
			if ndig gt fdig then fdig = ndig
		endfor
		ppar.fdigits = fdig
	endelse
	kcwi_print_info,ppar,pre,'number of digits in image numbers', ppar.fdigits
	;
	; create file spec
	fspec = ppar.froot + strjoin(replicate('?',ppar.fdigits)) + '.fit*'
	ppar.filespec = fspec
	;
	; gather configuration data on each observation in indir
	kcfg = kcwi_read_cfgs(indir,filespec=fspec)
	nf = n_elements(kcfg)
	;
	; check for requested object image
	if keyword_set(imgtarg) then begin
		it = where(kcfg.imgnum eq imgtarg, nit)
		if nit le 0 then begin
			kcwi_print_info,ppar,pre,'Requested image number not found',imgtarg,/error
			return
		endif
		if strpos(kcfg[it].imgtype,'object') lt 0 and $
		   strpos(kcfg[it].imgtype,'dflat') lt 0 then $
			kcwi_print_info,ppar,pre,'Requested image not an object/dflat image',imgtarg,kcfg[it].imgtype,/warn
	;
	; or just get most recent object image
	endif else begin
		ob = where(strpos(kcfg.imgtype,'object') ge 0 or $
			   strpos(kcfg.imgtype,'dflat') ge 0, nob)
		if nob le 0 then begin
			kcwi_print_info,ppar,pre,'No object/dflat images taken yet',/error
			return
		endif
		it = where(kcfg[ob].juliandate eq max(kcfg[ob].juliandate))
		it = [ob[it]]
	endelse
	;
	; check other params
	if keyword_set(mingroupbias) then $
		ppar.mingroupbias = mingroupbias
	if keyword_set(mingroupdark) then $
		ppar.mingroupdark = mingroupdark
	if keyword_set(minoscanpix) then $
		ppar.minoscanpix = minoscanpix
	if keyword_set(pksig) then $
		ppar.pksig = pksig
	if keyword_set(pkdel) then $
		ppar.pkdel = pkdel
	if keyword_set(pkiso) then $
		ppar.pkiso = pkiso
	if keyword_set(cwi) then $
		ppar.biasskip1 = 1 $
	else	ppar.biasskip1 = 0
	if keyword_set(nocrreject) then $
		ppar.crzap = 0 $
	else	ppar.crzap = 1
	if keyword_set(nonassub) then $
		ppar.nassub = 0 $
	else	ppar.nassub = 1
	if keyword_set(nocleancoeffs) then $
		ppar.cleancoeffs = 0 $
	else	ppar.cleancoeffs = 1
	if keyword_set(saveintims) then $
		ppar.saveintims = 1 $
	else	ppar.saveintims = 0
	if keyword_set(includetest) then $
		ppar.includetest = 1 $
	else	ppar.includetest = 0
	if keyword_set(clobber) then $
		ppar.clobber = 1 $
	else	ppar.clobber = 0
	if keyword_set(saveplots) then $
		ppar.saveplots = 1 $
	else	ppar.saveplots = 0
	;
	; log file
	lgfil = odir + 'kcwi_quick_'+strn(kcfg[it].imgnum)+'.log'
	filestamp,lgfil,/arch
	openw,ll,lgfil,/get_lun
	ppar.loglun = ll
	printf,ll,'Log file for run of '+pre+' on '+systime(0)
	printf,ll,'Version: '+version
	printf,ll,'DRP Ver: '+kcwi_drp_version()
	printf,ll,'Target Img: '+strn(kcfg[it].imgnum)
	printf,ll,'Raw dir: '+indir
	printf,ll,'Reduced dir: '+odir
	printf,ll,'Calib dir: '+caldir
	printf,ll,'Data dir: '+ddir
	printf,ll,'Filespec: '+fspec
	printf,ll,'Fileroot: '+ppar.froot
	printf,ll,'Filedigits: '+strn(ppar.fdigits)
	printf,ll,'Min Grp Bias: ',ppar.mingroupbias
	printf,ll,'Min Grp Dark: ',ppar.mingroupdark
	printf,ll,'Wl Fit PkSig: ',ppar.pksig
	printf,ll,'Wl Fit PkDel: ',ppar.pkdel
	printf,ll,'Wl Fit PkIso: ',ppar.pkiso
	if keyword_set(cwi) then $
		printf,ll,'CWI data    : skipping first bias in each group, CWI associations'
	if keyword_set(nocrreject) then $
		printf,ll,'No cosmic ray rejection performed'
	if keyword_set(nonassub) then $
		printf,ll,'No nod-and-shuffle sky subtraction performed'
	if keyword_set(nocleancoeffs) then $
		printf,ll,'No wavelength coefficient cleaning performed'
	if keyword_set(saveintims) then $
		printf,ll,'Saving intermediate images'
	if keyword_set(includetest) then $
		printf,ll,'Including test images in processing'
	if keyword_set(clobber) then $
		printf,ll,'Clobbering existing files'
	printf,ll,'Verbosity level   : ',verbose
	printf,ll,'Plot display level: ',display
	if keyword_set(saveplots) then $
		printf,ll,'Saving plots'
	kcwi_print_info,ppar,pre,'Number of input images',nf
	;
	; trim imgtype tag
	kcfg.imgtype = strtrim(kcfg.imgtype,2)
	;
	; trim object name
	kcfg.object = strtrim(kcfg.object,2)
	;
	; exclude biases and test images from process list
	proc = where(strmatch(kcfg.imgtype,'bias') ne 1 and $
		     strmatch(kcfg.imgtype,'test') ne 1 and $
		     strmatch(kcfg.imgtype,'image') ne 1,nproc)
	;
	; if includetest set just exclude biases
	if ppar.includetest eq 1 then $
		proc = where(strmatch(kcfg.imgtype,'bias') ne 1,nproc)
	;
	; is there anything left to process?
	if nproc le 0 then begin
		kcwi_print_info,ppar,pre,'no object/cal images to process', $
			/error
		free_lun,ll
		return
	endif
	kcwi_print_info,ppar,pre,'Using only local calibrations'
	calcfg = kcfg
	ncal = nf
	kcwi_print_info,ppar,pre,'Number of images in calibration pool',ncal,$
		format='(a,i5)'
	;
	; find slice profile images
	profs = where(strcmp(calcfg.imgtype,'object') eq 1 and $
		( calcfg.skyobs eq 1 or (calcfg.nasmask eq 1 and calcfg.shuffmod eq 1) ), nprofs)
	if nprofs gt 0 then begin
		ppar.nprofs = nprofs
		rangepar,calcfg[profs].imgnum,rlst
		ppar.profs = rlst
	endif else kcwi_print_info,ppar,pre,'no slice profile observations found',/warning
	;
	; find relative response images
	rrs = where(strcmp(calcfg.imgtype,'dflat') eq 1 or strcmp(calcfg.imgtype,'tflat') eq 1, nrrs)
	if nrrs gt 0 then begin
		ppar.nrrs = nrrs
		rangepar,calcfg[rrs].imgnum,rlst
		ppar.rrs = rlst
	endif else kcwi_print_info,ppar,pre,'no relative response images found',/warning
	;
	; find sky observation images
	skys = where(kcfg.skyobs eq 1, nskys)
	if nskys gt 0 then begin
		ppar.skyexists = 1
		ppar.nskys = nskys
		rangepar,kcfg[skys].imgnum,rlst
		ppar.skys = rlst
	endif else kcwi_print_info,ppar,pre,'no sky observations found',/warning
	;
	; get image numbers to be processed
	imnums = kcfg[proc].imgnum
	rangepar,imnums,imlist
	ppar.imnum = imlist
	ppar.npims = nproc
	;
	; report
	kcwi_print_info,ppar,pre,'processing '+strtrim(strn(nproc),2)+' images'
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP BIASES
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_biases,calcfg,ppar,bcfg
	;
	; do we have any bias groups?
	if ppar.nbgrps le 0 then $
		kcwi_print_info,ppar,pre,'no bias groups found',/warning
	kcwi_print_info,ppar,pre,'number of bias groups = '+ $
		strtrim(string(ppar.nbgrps),2)
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP DARKS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_darks,calcfg,ppar,dcfg
	;
	; do we have any dark groups?
	if ppar.ndgrps le 0 then $
		kcwi_print_info,ppar,pre,'no dark groups found',/warning
	kcwi_print_info,ppar,pre,'number of dark groups = '+ $
		strtrim(string(ppar.ndgrps),2)
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP FLATS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_flats,calcfg,ppar,fcfg
	;
	; do we have any flat groups?
	if ppar.nfgrps le 0 then $
		kcwi_print_info,ppar,pre,'no flat groups found',/warning
	kcwi_print_info,ppar,pre,'number of flat groups = '+ $
		strtrim(string(ppar.nfgrps),2)
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP CBARS AND ARC FILES (GEOM)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_geom,calcfg,ppar,ccfg,acfg,ngeom
	ppar.ncbars = ngeom
	ppar.narcs = ngeom
	kcwi_print_info,ppar,pre,'number of cbars, arc images',ngeom
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP SLICE PROFILE OBSERVATIONS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if nprofs gt 0 then $
		pcfg = calcfg[profs]
	kcwi_print_info,ppar,pre,'number of slice profile images',nprofs
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP SKY OBSERVATIONS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if nskys gt 0 then $
		scfg = kcfg[skys]
	kcwi_print_info,ppar,pre,'number of sky images',nskys
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP RELATIVE RESPONSE OBSERVATIONS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if nrrs gt 0 then $
		rcfg = calcfg[rrs]
	kcwi_print_info,ppar,pre,'number of relative response images',nrrs
	;
	; write out master KCWI_PPAR into file
	ppar.ppfname = 'kcwi.ppar'
	kcwi_write_ppar,ppar,/archive
	;
	; now set input dir to output for subsequent stages
	ppar.reddir = odir
	;
	; set up configuration matching: here is our list of 
	; default tags to match in the KCWI_CFG struct
	mtags = ['XBINSIZE','YBINSIZE','AMPMODE','GRATID','GRATPOS','FILTER', $
		 'FM4POS','CAMPOS','FOCPOS','NASMASK']
	;
	; set up links
	nlinks = 9	; bias,dark.flat,cbar,arc,prof,sky,rrsp,std
	ibias = 0
	idark = 1
	iflat = 2
	icbar = 3
	iarc =  4
	iprof = 5
	isky  = 6
	irrsp = 7
	istd  = 8
	;
	; open master link files and ppar files
	linkfname = odir+'kcwi.link'
	openw,kl,linkfname,/get_lun
	printf,kl,'#     Img    Mbias    Mdark    Mflat    Cbars      Arc     Prof      Sky     Rrsp      Std  Imgpars'
	;
	; loop over images
	for i=0,nproc-1 do begin
		;
		; pointer to image to process
		p = proc[i]
		;
		; link numbers
		links = lonarr(nlinks)-1
		;
		; get image summary
		kcwi_print_cfgs,kcfg[p],imsum,/silent
		;
		; format for output
		if strlen(imsum) gt 0 then begin
		    for k=0,1 do junk = gettok(imsum,' ')
		    imsumo = string(i+1,'/',nproc,format='(i3,a1,i3)')+' '+imsum
		endif
		print,""
		printf,ll,""
		printf,ll,imsumo
		flush,ll
		if verbose ge 1 then $
			print,imsumo
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH MASTER BIAS IMAGE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		if ppar.nbgrps gt 0 then begin
			mcfg = kcwi_associate(bcfg,kcfg[p],ppar,count=b)
			if b eq 1 then begin
				mbfile = mcfg.groupfile
				blink = mcfg.groupnum
				;
				; log
				kcwi_print_info,ppar,pre,'master bias file = '+$
					mbfile
			;
			; handle the ambiguous case or when no bias frames were taken
			endif else begin
				kcwi_print_info,ppar,pre,$
				     'cannot unambiguously associate with any master bias: '+ $
				     kcfg[p].obsfname,/warning
				mbfile = '-'
				blink = -1
			endelse
			;
			; set bias link
			links[ibias] = blink
		endif	; ppar.nbgrps gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH MASTER DARK IMAGE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		if ppar.ndgrps gt 0 then begin
			mcfg = kcwi_associate(dcfg,kcfg[p],ppar,count=d)
			if d eq 1 then begin
				mdfile = mcfg.groupfile
				dlink = mcfg.groupnum
				;
				; log
				kcwi_print_info,ppar,pre,'master dark file = '+$
					mdfile
			;
			; handle the ambiguous case or when no dark frames were taken
			endif else begin
				kcwi_print_info,ppar,pre, $
				    'cannot unambiguously associate with any master dark: '+ $
				    kcfg[p].obsfname,/warning
				mdfile = '-'
				dlink = -1
			endelse
			;
			; set dark link
			links[idark] = dlink
		endif	; only object frames and ndgrps gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH MASTER FLAT IMAGE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; no sense flat fielding the dark frames
		if strmatch(kcfg[p].imgtype,'dark') ne 1 and ppar.nfgrps gt 0 then begin
			mcfg = kcwi_match_cfg(fcfg,kcfg[p],ppar,mtags,imgtype='cflat',/time,count=f,/silent)
			if f eq 1 then begin
				mffile = mcfg.groupfile
				flink = mcfg.groupnum
				;
				; log
				kcwi_print_info,ppar,pre,'master flat file = ' + mffile
			;
			; handle ambiguous match or the case when no flat frames were taken
			endif else begin
				kcwi_print_info,ppar,pre, $
					'cannot unambiguously associate with any master flat: '+ $
					kcfg[p].obsfname,/warning
				mffile = '-'
				flink = -1
			endelse
			;
			; set flat link
			links[iflat] = flink
		endif	; only object and cflat frames and nfgrps gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH CBARS AND ARC IMAGES (GEOM)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; no sense creating a dark data cube
		if strmatch(kcfg[p].imgtype,'dark') ne 1 and ppar.ncbars gt 0 and ppar.narcs gt 0 then begin
			mcfg = kcwi_match_cfg(ccfg,kcfg[p],ppar,mtags,imgtype='cbars',/time,count=c,/silent)
			if c eq 1 then begin
				;
				; record cbars filename
				cbfile = mcfg.obsfname
				clink  = mcfg.imgnum
				;
				; now find matched arc
				m = where(ccfg.imgnum eq clink)
				mcf2 = acfg[m]
				;
				; record arc filename
				arfile = mcf2.obsfname
				alink  = mcf2.imgnum
				;
				; log
				kcwi_print_info,ppar,pre,'cbars file = '+cbfile
				kcwi_print_info,ppar,pre,'arc   file = '+arfile
				;
				; handle the ambiguous case or when no cbars image can be found
			endif else begin
				kcwi_print_info,ppar,pre, $
				    'cannot unambiguously find geom images (arc, cbars) for object image: '+ $
				    kcfg[p].obsfname,/warning
				clink = -1
				alink = -1
			endelse
			;
			; set cbars and arc links
			links[icbar] = clink
			links[iarc]  = alink
		endif	; only object and cflat frames and ncbars gt 0 and narcs gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH SLICE PROFILE OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; no point profile correcting dark frames
		; also require geometry solution
		if strmatch(kcfg[p].imgtype,'dark') ne 1 and ppar.nprofs gt 0 and $
			links[icbar] ge 0 and links[iarc] ge 0 then begin
			mcfg = kcwi_match_cfg(pcfg,kcfg[p],ppar,mtags,count=s,/time)
			if s eq 1 then begin
				;
				; record slice profile observation filename
				pfile = mcfg.obsfname
				plink = mcfg.imgnum
				;
				; log
				kcwi_print_info,ppar,pre,'slice profile file = '+pfile
				;
				; handle the ambiguous case or when no slice profile image can be found
			endif else begin
				kcwi_print_info,ppar,pre, $
				    'cannot unambiguously find slice profile image for object image: '+ $
				    kcfg[p].obsfname,/warning
				plink = -1
			endelse
			;
			; set prof link
			links[iprof] = plink
		endif	; only object frames and nprofs gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH SKY OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; only object frames can have sky observations
		if strmatch(kcfg[p].imgtype,'object') eq 1 and $
		   kcfg[p].skyobs eq 0 and ppar.nskys gt 0 then begin
			mcfg = kcwi_match_cfg(scfg,kcfg[p],ppar,mtags,/object,/time,count=s,/silent)
			if s eq 1 then begin
				;
				; record sky filename
				skfile = mcfg.obsfname
				slink  = mcfg.imgnum
				;
				; log
				kcwi_print_info,ppar,pre,'sky file = '+skfile
			endif else begin
				kcwi_print_info,ppar,pre,'No sky obs with same object name', $
					kcfg[p].object,format='(a,2x,a)',/warning
				slink = -1
			endelse
			;
			; set sky link
			links[isky] = slink
		endif	; only object frames and nskys gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH RELATIVE RESPONSE OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; no sense response correcting dark frames
		; also require geometry solution
		if strmatch(kcfg[p].imgtype,'dark') ne 1 and ppar.nrrs gt 0 and $
			links[icbar] ge 0 and links[iarc] ge 0 then begin
			mcfg = kcwi_match_cfg(rcfg,kcfg[p],ppar,mtags,count=r,/time)
			if r eq 1 then begin
				;
				; record dome flat filename
				rrfile = mcfg.obsfname
				rlink  = mcfg.imgnum
				;
				; log
				kcwi_print_info,ppar,pre,'relative response file = '+rrfile
			endif else begin
				rlink = -1
			endelse
			;
			; set rrsp link
			links[irrsp] = rlink
		endif	; only object frames and nskys gt 0
		;
		; write out links
		printf,kl,kcfg[p].imgnum,links,imsum,format='(10i9,2x,a)'
	endfor	; loop over images
	;
	; close link file
	free_lun,kl
	;
	; get all possible images involved
	kcwi_read_links,ppar,linkfname,inums,bias=bnums,dark=dnums,flat=fnums, $
		cbar=cnums,arc=anums,prof=pnums,rrsp=rnums
	;
	; target image pointer
	ti = (where(inums eq kcfg[it].imgnum, nti))[0]
	if nti le 0 then begin
		kcwi_print_info,ppar,pre,'Target image not found in link file',/error
		return
	endif
	;
	; stage 1 image and bias lists
	s1img  = inums[ti]
	s1bias = bnums[ti]
	;
	; stage 1 darks
	if dnums[ti] ge 0 then begin
		dpar = kcwi_read_ppar(odir+'mdark_'+strn(dnums[ti])+'.ppar')
		rangepar,dpar.darks,darks
		td = lonarr(n_elements(darks))
		for i=0,n_elements(darks)-1 do begin
			t = where(inums eq darks[i])
			td[i] = t[0]
		endfor
		s1img = [s1img,inums[td]]
		s1bias= [s1bias,bnums[td]]
	endif
	;
	; stage 1 flats
	if fnums[ti] ge 0 then begin
		fpar = kcwi_read_ppar(odir+'mflat_'+strn(fnums[ti])+'.ppar')
		rangepar,fpar.cflats,flats
		tf = lonarr(n_elements(flats))
		for i=0,n_elements(flats)-1 do begin
			t = where(inums eq flats[i])
			tf[i] = t[0]
		endfor
		s1img = [s1img,inums[tf]]
		s1bias= [s1bias,bnums[tf]]
	endif
	;
	; stage 1 cbars
	if cnums[ti] ge 0 then begin
		s1img = [s1img,cnums[ti]]
		s1bias= [s1bias,bnums[ti]]
	endif
	;
	; stage 1 arcs
	if anums[ti] ge 0 then begin
		s1img = [s1img,anums[ti]]
		s1bias= [s1bias,bnums[ti]]
	endif
	;
	; stage 1 profile
	if pnums[ti] ge 0 then begin
		s1img = [s1img,pnums[ti]]
		s1bias= [s1bias,bnums[ti]]
	endif
	;
	; stage 1 relative response
	if rnums[ti] ge 0 then begin
		s1img = [s1img,rnums[ti]]
		s1bias= [s1bias,bnums[ti]]
	endif
	;
	; get sorted uniq list
	s = sort(s1img)
	s1img = s1img[s]
	s1bias= s1bias[s]
	u = uniq(s1img)
	s1img	= s1img[u]
	s1bias	= s1bias[u]
	s2dark	= (s1img-s1img) + dnums[ti]
	s3flat	= (s1img-s1img) + fnums[ti]
	s4cbars	= (s1img-s1img) + cnums[ti]
	s4arcs	= (s1img-s1img) + anums[ti]
	s5profs	= (s1img-s1img) + pnums[ti]
	s6rrs	= (s1img-s1img) + rnums[ti]
	;
	; stage status
	stagetail = strarr(8)
	;
	; do stage 1
	kcwi_stage1,proc_imgnums=s1img,proc_biasnums=s1bias
	stagetail[1] = 'int.fits'
	;
	; do stage 2 dark
	if dnums[ti] ge 0 then begin
		kcwi_stage2dark,proc_imgnums=s1img,proc_darknums=s2dark
		stagetail[2] = 'intd.fits'
	endif
	;
	; do stage 3 flat
	if fnums[ti] ge 0 then begin
		kcwi_stage3flat,proc_imgnums=s1img,proc_flatnums=s3flat
		stagetail[3] = 'intf.fits'
	endif
	;
	; do stage 4 geom
	if cnums[ti] ge 0 and anums[ti] ge 0 then begin
		kcwi_stage4geom,proc_imgnums=s1img,proc_cbarnums=s4cbars,proc_arcnums=s4arcs
		stagetail[4] = 'icube.fits'
	endif
	;
	; do stage 5 profile
	if pnums[ti] ge 0 then begin
		kcwi_stage5prof,proc_imgnums=s1img,proc_profnums=s5profs
		stagetail[5] = 'icubep.fits'
	endif
	;
	; do stage 6 relative response (requires profile correction)
	if pnums[ti] ge 0 and rnums[ti] ge 0 then begin
		kcwi_stage6rr,proc_imgnums=s1img,proc_rrnums=s6rrs
		stagetail[6] = 'icuber.fits'
	endif
	;
	; what is the most advanced stage?
	lstage = max(where(strlen(stagetail) gt 0))
	;
	; get image root
	rute = odir+ppar.froot+string(inums[ti],format='(i0'+strn(ppar.fdigits)+')') + '_'
	;
	; if we got past stage 4 do what is needed for data cubes
	if lstage ge 4 then begin
		;
		; get kgeom struct
		restore,odir+ppar.froot+string(cnums[ti],format='(i0'+strn(ppar.fdigits)+')') + '_geom.save'
		;
		; zoom ratio for data cubes
		zrat = kgeom.slscl/(kgeom.pxscl*kgeom.xbinsize)
		;
		; pick correct tail
		tail = stagetail[lstage]
		;
		; find most processed 2D image
		l2dstage = max(where(strlen(stagetail[0:3]) gt 0)) > 1
		tail2d = stagetail[l2dstage]
		;
		; set up ds9 command
		ds9cmd = 'ds9 '+rute+tail2d+' -minmax auto -log '+ $
			rute+tail+' -zoom 4 '+strn(4.*zrat)+' -minmax auto -log -single -frame last &'
	endif else begin
		;
		; pick correct 2D image tail
		tail = stagetail[lstage]
		;
		; set up ds9 command
		ds9cmd = 'ds9 '+rute+tail+' -minmax auto -log &'
	endelse
	;
	; display with ds9 if requested
	if keyword_set(ds9) then begin
		kcwi_print_info,ppar,pre,'Displaying... ',ds9cmd, $
			format='(a,a)'
		spawn,ds9cmd
	endif
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
end	; kcwi_quick

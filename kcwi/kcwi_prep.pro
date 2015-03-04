; $Id: kcwi_prep.pro,v 1.64 2015/02/25 19:16:47 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_PREP
;
; PURPOSE:
;	This procedure generates the associations and ppar file needed to 
;	run subsequent stages of the pipeline.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_PREP, RawDir, ReducedDir, CalibDir, DataDir
;
; OPTIONAL INPUTS:
;	RawDir		- input raw directory (string) defaults to current dir
;	ReducedDir	- reduced data directory (string) defaults to './redux/'
;	CalibDir	- calib source directory (string) defaults to !KCWI_DATA + 'calib/'
;	DataDir		- KCWI data directory (string) defaults to !KCWI_DATA
;
; KEYWORDS:
; Params
;	FROOT		- root of image filenames (def: 'image')
;	FDIGITS		- number of digits in image numbers (def: 4)
;	MINGROUPBIAS	- minimum number of bias images per group (def: 5)
;	MINGROUPDARK	- minimum number of dark images per group (def: 3)
;	MINOSCANPIX	- minimum number of overscan pixels for subtraction (def: 70)
; Wavelength fitting params (only relevant for full-ccd images)
;	PKSIG		- significance of peaks to find (def:1.5)
;	PKDEL		- matching thresshold in Ang (def: 0.75)
;	PKISO		- isolation of peaks in Ang (def: 2.0)
; Switches
;	CWI		- set for CWI data: skip first bias image, 
;				use CWI associations (def: NO)
;	NOCRREJECT	- set to skip cosmic ray rejection
;	NONASSUB	- set to skip nod-and-shuffle subtraction
;	NOCLEANCOEFFS	- set to skip cleaning wavelength sol'n coeffs
;	SAVEINTIMS	- set to save intermediate images (def: NO)
;	INCLUDETEST	- set to include test images in reduction (def: NO)
;	DOMEPRIORITY	- set to use dome flats over twilight flats (def: NO)
;	CLOBBER		- set to clobber existing images (def: no clobber)
;	VERBOSE		- set gt 0 to get extra screen output (def: 0)
;	DISPLAY		- set gt 0 to display diagnostic result plots
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
;	Prepare to perform KCWI reductions on the images in 'night1' 
;	directory and put results in 'night1/redux':
;
;	KCWI_PREP,'night1','night1/redux'
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-15	Initial version
;	2013-SEP-09	Added check for sky observations
;	2013-SEP-13	Use KCWI_PPAR struct for subroutine parameters
;	2013-NOV-01	Made cbars/arc associations more robust
;	2013-NOV-06	Implemented relative response correction
;	2014-MAR-25	Added nocleancoeffs keyword
;	2014-APR-01	Added kcwi_group_geom and processing of all calib imgs
;	2014-APR-09	Improved association logic based on previous assoc.
;	2014-MAY-28	Removed FILESPEC keyword and now uses FROOT and FDIGITS
;			to generate file spec for input images
;	2014-JUN-03	checks file digits automatically if FDIGITS not set
;-
pro kcwi_prep,rawdir,reduceddir,calibdir,datadir, $
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
	domepriority=domepriority, $
	clobber=clobber, $
	verbose=verbose, $
	display=display, $
	saveplots=saveplots, $
	help=help
	;
	; setup
	pre = 'KCWI_PREP'
	version = repstr('$Revision: 1.64 $ $Date: 2015/02/25 19:16:47 $','$','')
	startime=systime(1)
	q = ''	; for queries
	;
	; requested help?
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', RawDir, ReducedDir, CalibDir, DataDir'
		print,pre+': Info - Param  Keywords: FROOT=<img_file_root>, FDIGITS=N, MINGROUPBIAS=N, MINOSCANPIX=N'
		print,pre+': Info - Wl Fit Keywords: PKSIG=<sigma_significance>, PKDEL=<match_delta>, PKISO=<peak_isolation>'
		print,pre+': Info - Switch Keywords: /CWI, /NOCRREJECT, /NONASSUB, /NOCLEANCOEFFS, /DOMEPRIORITY'
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
	if n_elements(verbose) eq 0 then verbose = 1
	ppar.verbose = verbose
	if n_elements(display) eq 0 then display = 1
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
		print,pre+': Warning - reduced data dir does not exist: ',odir
		read,'Create? (Y/n): ',q
		q = strupcase(strtrim(q,2))
		if strmid(q,0,1) ne 'N' then begin
			file_mkdir,odir,/noexpand
			if verbose ge 1 then $
				print,pre+': Info - created directory: ',odir
		endif else begin
			print,pre+': Error - no reduced data dir, returning'
			return
		endelse
	endif
	;
	; check if odir accessible
	if not file_test(odir,/directory,/executable,/write) then begin
		print,pre+': Error - reduced data dir not accessible, returning'
		return
	endif
	;
	; check if indir accessible
	if not file_test(indir,/directory,/executable,/read) then begin
		print,pre+': Error - cannot access raw data dir: ',indir,', returning'
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
	cd,cur=cwd
	ppar.curdir = cwd + '/'
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
	lgfil = odir + 'kcwi_prep.log'
	filestamp,lgfil,/arch
	openw,ll,lgfil,/get_lun
	ppar.loglun = ll
	printf,ll,'Log file for run of '+pre+' on '+systime(0)
	printf,ll,'Version: '+version
	printf,ll,'DRP Ver: '+kcwi_drp_version()
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
	if keyword_set(domepriority) then $
		printf,ll,'Dome flats have priority for relative response' $
	else	printf,ll,'Twilight flats have priority for relative response'
	if keyword_set(clobber) then $
		printf,ll,'Clobbering existing images'
	printf,ll,'Verbosity level   : ',verbose
	printf,ll,'Plot display level: ',display
	if keyword_set(saveplots) then $
		printf,ll,'Saving plots'
	;
	; gather configuration data on each observation in raw dir
	kcfg = kcwi_read_cfgs(indir,filespec=fspec)
	nf = n_elements(kcfg)
	kcwi_print_info,ppar,pre,'Number of raw input images',nf
	;
	; write out a complete listing
	kcwi_print_cfgs,kcfg,/silent,/header,outfile=odir+'kcwi.imlog'
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
	;
	; gather configuration data on each observation in caldir
	;calcfg = kcwi_read_cfgs(caldir,filespec=fspec,count=ncal,/silent)
	;
	; combine them with local calibrations which take precedence
	;if ncal gt 0 then begin
	;	kcwi_print_info,ppar,pre,'Including library calibrations from',caldir
	;	calcfg = [calcfg,kcfg]
	;
	; if nothing found, then we assume that we will use local images
	;endif else begin
		kcwi_print_info,ppar,pre,'Using only local calibrations'
		calcfg = kcfg
		ncal = nf
	;endelse
	kcwi_print_info,ppar,pre,'Number of images in calibration pool',ncal,$
		format='(a,i5)'
	;
	; find slice profile images
	profs = where(strcmp(calcfg.imgtype,'object') eq 1 and $
		( calcfg.skyobs eq 1 or (calcfg.nasmask eq 1 and calcfg.shuffmod eq 1) ), nprofs)
	; twilight flats may be better for this
	;profs = where(strcmp(calcfg.imgtype,'tflat') eq 1, nprofs)
	if nprofs gt 0 then begin
		ppar.profexists = 1
		ppar.nprofs = nprofs
		rangepar,calcfg[profs].imgnum,rlst
		ppar.profs = rlst
	endif else kcwi_print_info,ppar,pre,'no slice profile observations found',/warning
	;
	; find relative response images
	trrs = where(strcmp(calcfg.imgtype,'tflat') eq 1, ntrrs)
	drrs = where(strcmp(calcfg.imgtype,'dflat') eq 1, ndrrs)
	if ntrrs gt 0 or ndrrs gt 0 then begin
		ppar.rrexists = 1
		ppar.nrrs = ntrrs + ndrrs
		if ntrrs gt 0 then begin
			rrs = trrs
			if ndrrs gt 0 then $
				rrs = [rrs, drrs]
		endif else 	rrs = drrs
		nrrs = n_elements(rrs)
		rangepar,calcfg[rrs].imgnum,rlst
		ppar.rrs = rlst
	endif else begin
		nrrs = 0
		kcwi_print_info,ppar,pre,'no relative response images found',/warning
	endelse
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
	; find standard star observation images
	stds = kcwi_find_stds(kcfg,ppar,nstds)
	if nstds le 0 then $
		kcwi_print_info,ppar,pre,'no standard star images found',/warning
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
	if ppar.ncbars le 0 then $
		kcwi_print_info,ppar,pre,'no geom groups found',/warning
	kcwi_print_info,ppar,pre,'number of geom groups = '+ $
		strtrim(string(ppar.ncbars),2)
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
	if ntrrs gt 0 then $
		rtcfg = calcfg[trrs]
	if ndrrs gt 0 then $
		rdcfg = calcfg[drrs]
	kcwi_print_info,ppar,pre,'number of relative response images',nrrs
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP STANDARD STAR OBSERVATIONS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if nstds gt 0 then $
		stdcfg = kcfg[stds]
	kcwi_print_info,ppar,pre,'number of standard star images',nstds
	;
	; write out master KCWI_PPAR into file
	ppar.ppfname = 'kcwi.ppar'
	kcwi_write_ppar,ppar,/archive
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
	filestamp,odir+'kcwi.link',/arch
	openw,kl,odir+'kcwi.link',/get_lun
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
			mcfg = kcwi_match_cfg(fcfg,kcfg[p],ppar,mtags,imgtype='cflat',/time,count=f,/silent,cwi=cwi)
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
			mcfg = kcwi_match_cfg(ccfg,kcfg[p],ppar,mtags,imgtype='cbars',/time,count=c,/silent,cwi=cwi)
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
			mcfg = kcwi_match_cfg(pcfg,kcfg[p],ppar,mtags,count=s,/time,cwi=cwi)
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
			mcfg = kcwi_match_cfg(scfg,kcfg[p],ppar,mtags,/object,/time,count=s,/silent,cwi=cwi)
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
			;
			; twilight flats
			if ntrrs gt 0 then $
				mtcfg = kcwi_match_cfg(rtcfg,kcfg[p],ppar,mtags,count=rt,/time) $
			else	rt = 0
			;
			; dome flats
			if ndrrs gt 0 then $
				mdcfg = kcwi_match_cfg(rdcfg,kcfg[p],ppar,mtags,count=rd,/time) $
			else	rd = 0
			;
			; do we have a choice?
			if rt eq 1 and rd eq 1 then begin
				if keyword_set(domepriority) then begin
					r = rd
					mcfg = mdcfg
				endif else begin
					r = rt
					mcfg = mtcfg
				endelse
			;
			; nope, one or no choices
			endif else begin
				;
				; only a dome flat
				if rd eq 1 then begin
					r = rd
					mcfg = mdcfg
				;
				; must be a twilight flat or nothing
				endif else begin
					r = rt
					if rt eq 1 then mcfg = mtcfg
				endelse
			endelse
			;
			; did we find any?
			if r eq 1 then begin
				;
				; record twilight or dome flat filename
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
		endif	; only object frames and nrrs gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH STANDARD STAR OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; correct only object frames
		; also require relative response correction
		if nstds gt 0 and strmatch(kcfg[p].imgtype,'object') eq 1 and links[irrsp] ge 0 then begin
			mcfg = kcwi_match_cfg(stdcfg,kcfg[p],ppar,mtags,count=std,/time)
			if std eq 1 then begin
				;
				; record stadard observation filename
				stdfile = mcfg.obsfname
				stdlink = mcfg.imgnum
				;
				; log
				kcwi_print_info,ppar,pre,'standard star observation file = '+stdfile
			endif else begin
				stdlink = -1
			endelse
			;
			; set rrsp link
			links[istd] = stdlink
		endif	; only object frames
		;
		; write out links
		printf,kl,kcfg[p].imgnum,links,imsum,format='(10i9,2x,a)'
	endfor	; loop over images
	;
	; report
	eltime = systime(1) - startime
	print,''
	printf,ll,''
	kcwi_print_info,ppar,pre,'run time in seconds',eltime
	kcwi_print_info,ppar,pre,'finished on '+systime(0)
	;
	; close log and link files
	free_lun,ll,kl
	;
	return
end	; kcwi_prep

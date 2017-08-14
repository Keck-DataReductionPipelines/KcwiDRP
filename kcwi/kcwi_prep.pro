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
;	KCWI_PREP, RawDir, ReducedDir, DataDir
;
; OPTIONAL INPUTS:
;	RawDir		- input raw directory (string) defaults to current dir
;	ReducedDir	- reduced data directory (string) defaults to './redux/'
;	DataDir		- KCWI data directory (string) defaults to !KCWI_DATA
;
; KEYWORDS:
; Params
;	FROOT		- root of image filenames (def: 'kcwi')
;	FDIGITS		- number of digits in image numbers (def: 5)
;	MINGROUPBIAS	- minimum number of bias images per group (def: 5)
;	MINGROUPDARK	- minimum number of dark images per group (def: 3)
;	MINOSCANPIX	- minimum number of overscan pixels for subtraction (def: 70)
;	ALTCALDIR	- alternate source directory for calibrations (string)
; Wavelength fitting params (only relevant for full-ccd images)
;	TAPERFRAC	- taper fraction for cross-correlation (0.2)
;	PKDEL		- matching thresh in frac. of resolution (def: 0.75)
; Switches
;	NOCRREJECT	- set to skip cosmic ray rejection
;	NONASSUB	- set to skip nod-and-shuffle subtraction
;	CLEANCOEFFS	- set to override default clean of wave sol'n coeffs
;	WAVEITER	- set to use iterative wavelength range expansion method
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
;	Outputs a set of files in ODIR: 
;		1) a master pipeline parameter file, kcwi.ppar, with paramters
;			for running the pipeline.
;		2) a master proc file, kcwi.proc, with associations for running 
;			subsequent stages of the pipeline (KCWI_STAGE{N}).
;		3) pipeline paramter (*.ppar) files for generating master 
;			calibration images (mbias, mdark, mflat) for each 
;			master image set found and validated.  
;		4) a general image log file, kcwi.imlog, that records the 
;			configuration of each input raw image.  
;		5) a log file, kcwi_prep.log, that logs all steps executed in 
;			this stage and records execution time.
; NOTE:
;	The kcwi.proc file can be edited to override the automated associations
;	prior to running the next stage in the pipeline.
;
; PROCEDURE:
;	Analyzes the FITS headers of the images in InputDir to determine the 
;	processing groups.  These are then associated with calibration and
;	object images based on instrument configuration and temporal proximity.
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
;	2014-MAR-25	Added cleancoeffs keyword
;	2014-APR-01	Added kcwi_group_geom and processing of all calib imgs
;	2014-APR-09	Improved association logic based on previous assoc.
;	2014-MAY-28	Removed FILESPEC keyword and now uses FROOT and FDIGITS
;			to generate file spec for input images
;	2014-JUN-03	checks file digits automatically if FDIGITS not set
;	2016-APR-04	changes specific to KCWI lab data
;	2017-MAY-04	Added waveiter keyword
;	2017-JUN-28	Added ALTCALDIR keyword
;-
pro kcwi_prep,rawdir,reduceddir,datadir, $
	froot=froot, $
	fdigits=fdigits, $
	mingroupbias=mingroupbias, $
	mingroupdark=mingroupdark, $
	minoscanpix=minoscanpix, $
	altcaldir=altcaldir, $
	taperfrac=taperfrac, pkdel=pkdel, $
	nocrreject=nocrreject, $
	nonassub=nonassub, $
	cleancoeffs=cleancoeffs, $
	waveiter=waveiter, $
	saveintims=saveintims, $
	includetest=includetest, $
	domepriority=domepriority, $
	clobber=clobber, $
	verbose=verbose, $
	display=display, $
	saveplots=saveplots, $
	batch=batch, $
	help=help
	;
	; setup
	pre = 'KCWI_PREP'
	startime=systime(1)
	q = ''	; for queries
	;
	; requested help?
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', RawDir, ReducedDir, CalibDir, DataDir'
		print,pre+': Info - Param  Keywords: FROOT=<img_file_root>, FDIGITS=N, MINGROUPBIAS=N, MINOSCANPIX=N, ALTCALDIR=<full_dir_spec>'
		print,pre+': Info - Wl Fit Keywords: TAPERFRAC=<taper_fraction>, PKDEL=<match_delta>'
		print,pre+': Info - Switch Keywords: /NOCRREJECT, /NONASSUB, /CLEANCOEFFS, /WAVEITER, /DOMEPRIORITY'
		print,pre+': Info - Switch Keywords: /SAVEINTIMS, /INCLUDETEST, /CLOBBER, VERBOSE=, DISPLAY=, /SAVEPLOTS, /BATCH, /HELP'
		return
	endif
	;
	; instantiate and init a ppar structure for the pipeline parameters
	A = {kcwi_ppar}
	ppar = struct_init(A)
	;
	; set initialized and version
	ppar.initialized = 1
	ppar.progid = pre
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
	if keyword_set(altcaldir) then $
		adir = kcwi_expand_dir(altcaldir) $
	else	adir = ''
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
	; check if adir accessible
	if keyword_set(altcaldir) then $
		if not file_test(adir,/directory,/executable,/read) then begin
			print,pre+': Error - cannot access alt cal dir: ', $
				adir,', returning'
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
	ppar.altcaldir = adir
	cd,cur=cwd
	ppar.curdir = cwd + '/'
	;
	; check image name prefix or root
	if keyword_set(froot) then $
		ppar.froot = froot $
	else begin
		ppar.froot = ''
		; get image name prefix from headers
		flist = file_search(indir + '*.fit*', count = nf)
		; loop over fits files
		for i=0,nf-1 do begin
			hdr = headfits(flist[i])
			; make sure it's a science image
			test = sxpar(hdr,'NAMPSXY',count=nk)
			if nk ge 1 then begin
				froot = strtrim(sxpar(hdr,'OUTFILE',count=nfr),2)
				if nfr ge 1 then $
					ppar.froot = froot
			endif
		endfor
	endelse
	kcwi_print_info,ppar,pre,'Input image file prefix', ppar.froot
	;
	; do we have any files?
	flist = file_search(indir + ppar.froot+'*.fit*', count=nf)
	if nf le 0 then begin
		kcwi_print_info,ppar,pre,'no fits files found in '+indir,/error
		return
	endif
	;
	; now check number of digits in image number
	;
	; specified with keyword
	if keyword_set(fdigits) then $
		ppar.fdigits = fdigits $
	;
	; derive from file names in INDIR
	else begin
		fdig = 0
		; loop over fits files
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
	if keyword_set(taperfrac) then $
		ppar.taperfrac = taperfrac
	if keyword_set(pkdel) then $
		ppar.pkdel = pkdel
	if keyword_set(cleancoeffs) then $
		ppar.cleancoeffs = cleancoeffs
	if keyword_set(waveiter) then $
		ppar.waveiter = waveiter
	if keyword_set(nocrreject) then $
		ppar.crzap = 0 $
	else	ppar.crzap = 1
	if keyword_set(nonassub) then $
		ppar.nassub = 0 $
	else	ppar.nassub = 1
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
	printf,ll,'DRP Ver: '+kcwi_drp_version()
	printf,ll,'Raw dir: '+indir
	printf,ll,'Reduced dir: '+odir
	printf,ll,'Calib dir: '+caldir
	if keyword_set(altcaldir) then $
		printf,ll,'AltCal dir: '+adir
	printf,ll,'Data dir: '+ddir
	printf,ll,'Filespec: '+fspec
	printf,ll,'Fileroot: '+ppar.froot
	printf,ll,'Filedigits: '+strn(ppar.fdigits)
	printf,ll,'Min Grp Bias: ',ppar.mingroupbias
	printf,ll,'Min Grp Dark: ',ppar.mingroupdark
	printf,ll,'Wl TaperFrac: ',ppar.taperfrac
	printf,ll,'Wl Fit PkDel: ',ppar.pkdel
	if keyword_set(nocrreject) then $
		printf,ll,'No cosmic ray rejection performed'
	if keyword_set(nonassub) then $
		printf,ll,'No nod-and-shuffle sky subtraction performed'
	if ppar.cleancoeffs eq 1 then $
		printf,ll,'Wavelength coefficient cleaning performed' $
	else	printf,ll,'No Wavelength coefficient cleaning performed'
	if ppar.waveiter eq 1 then $
		printf,ll,'Use iterative method for fitting waves' $
	else	printf,ll,'Use automatic method for fitting waves'
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
	if keyword_set(batch) then $
		printf,ll,'Batch mode'
	;
	; gather configuration data on each observation in raw dir
	kcfg = kcwi_read_cfgs(indir,filespec=fspec, redo_sort=jderr)
	nf = n_elements(kcfg)
	kcwi_print_info,ppar,pre,'Number of raw input images',nf
	if jderr then $
		kcwi_print_info,ppar,pre,'Image numbers out of time sequence', $
					/warning
	;
	; check with user for info about sky and twilight flat observations
	if not keyword_set(batch) then begin
		print,'For image number ranges use:'
		print,"(<cr>) for no images in the night,"
		print,"image number range list (e.g. '12-15,26,27')."
		;
		; SKY observations
		skyrng=''
		read,'Enter sky observations image number range: ',skyrng
		skyrng = strcompress(skyrng,/remove_all)
		if strlen(skyrng) gt 0 then begin
			kcwi_print_info,ppar,pre, $
				'Input sky image numbers', $
				skyrng,format='(a,a)'
			rangepar,skyrng,skyno
			nsky = n_elements(skyno)
			for i=0,nsky-1 do begin
				t = where(kcfg.imgnum eq skyno[i], nt)
				if nt eq 1 then begin
					kcfg[t].skyobs = 1
					kcfg[t].obstype = 'cal'
				endif else if nt eq 0 then begin
					kcwi_print_info,ppar,pre, $
					'Sky image number not found: '+ $
					strn(skyno[i]),/warn
				endif else begin
					kcwi_print_info,ppar,pre, $
					'Ambiguous sky image number: '+ $
					strn(skyno[i]),/warn
				endelse
			endfor
		endif	else	$	; strlen(skyrng) gt 0
			kcwi_print_info,ppar,pre, $
				'No sky image numbers input'
		;
		; twilight flat observations
		twirng=''
		read,'Enter twilight flat observations image number range: ', $
			twirng
		twirng = strcompress(twirng,/remove_all)
		if strlen(twirng) gt 0 then begin
			kcwi_print_info,ppar,pre, $
				'Input twilight flat image numbers', $
				twirng,format='(a,a)'
			rangepar,twirng,twino
			ntwi = n_elements(twino)
			for i=0,ntwi-1 do begin
				t = where(kcfg.imgnum eq twino[i], nt)
				if nt eq 1 then begin
					kcfg[t].imgtype = 'tflat'
					kcfg[t].obstype = 'cal'
				endif else if nt eq 0 then begin
					kcwi_print_info,ppar,pre, $
					'Twilight image number not found: '+ $
					strn(twino[i]),/warn
				endif else begin
					kcwi_print_info,ppar,pre, $
					'Ambiguous twilight image number: '+ $
					strn(twino[i]),/warn
				endelse
			endfor
		endif	else	$	; strlen(twirng) gt 0
			kcwi_print_info,ppar,pre, $
				'No twilight flat image numbers input'
	endif	; not batch
	;
	; write out a complete listing
	kcwi_print_cfgs,kcfg,/silent,/header,outfile=odir+'kcwi.imlog'
	;
	; trim imgtype tag
	kcfg.imgtype = strtrim(kcfg.imgtype,2)
	;
	; trim object name
	kcfg.object = strtrim(kcfg.object,2)
	kcfg.targname = strtrim(kcfg.targname,2)
	;
	; exclude biases and test images from process list
	proc = where(strmatch(kcfg.imgtype,'bias') ne 1 and $
		     strmatch(kcfg.imgtype,'test') ne 1 and $
		     strmatch(kcfg.imgtype,'unknown') ne 1 and $
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
	cals = where(strpos(kcfg.obstype,'cal') ge 0 or $
		     strpos(kcfg.obstype,'zero') ge 0, ncal)
	if ncal gt 0 then begin
		calcfg = kcfg[cals]
	endif else begin
		calcfg = kcfg
		ncal = nf
	endelse
	kcwi_print_info,ppar,pre,'Number of local images in calibration pool', $
			ncal,format='(a,i5)'
	;
	; find slice profile images
	profs = where(strcmp(kcfg.imgtype,'object') eq 1 and $
		( kcfg.skyobs eq 1 or (kcfg.nasmask eq 1 and $
					 kcfg.shuffmod eq 1) ), nprofs)
	; twilight flats may be better for this
	;profs = where(strcmp(calcfg.imgtype,'tflat') eq 1, nprofs)
	if nprofs le 0 then $
		kcwi_print_info,ppar,pre, $
			'no slice profile observations found',/warning
	;
	; find relative response images
	trrs = where(strcmp(calcfg.imgtype,'tflat') eq 1, ntrrs)
	drrs = where(strcmp(calcfg.imgtype,'dflat') eq 1, ndrrs)
	if ntrrs gt 0 or ndrrs gt 0 then begin
		nrrs = ntrrs + ndrrs
		if ntrrs gt 0 then begin
			rrs = trrs
			if ndrrs gt 0 then $
				rrs = [rrs, drrs]
		endif else 	rrs = drrs
		nrrs = n_elements(rrs)
	endif else begin
		nrrs = 0
		kcwi_print_info,ppar,pre, $
			'no relative response images found',/warning
	endelse
	;
	; find sky observation images
	skys = where(kcfg.skyobs eq 1, nskys)
	if nskys le 0 then $
		kcwi_print_info,ppar,pre,'no sky observations found',/warning
	;
	; find standard star observation images
	stds = kcwi_find_stds(kcfg,ppar,nstds)
	if nstds le 0 then $
		kcwi_print_info,ppar,pre, $
			'no standard star images found',/warning
	;
	; report
	kcwi_print_info,ppar,pre,'processing '+strtrim(strn(nproc),2)+' images'
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP BIASES
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_biases,calcfg,ppar,bcfg
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP DARKS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_darks,calcfg,ppar,dcfg
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP FLATS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_flats,calcfg,ppar,fcfg
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP DIRECT ARCBARS AND ARC FILES (DGEOM)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_dgeom,calcfg,ppar,dccfg,dacfg
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP CBARS AND ARC FILES (GEOM)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	kcwi_group_geom,calcfg,ppar,ccfg,acfg
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP SLICE PROFILE OBSERVATIONS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if nprofs gt 0 then $
		pcfg = kcfg[profs]
	kcwi_print_info,ppar,pre,'Number of slice profile images',nprofs
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP SKY OBSERVATIONS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if nskys gt 0 then $
		scfg = kcfg[skys]
	kcwi_print_info,ppar,pre,'Number of sky images',nskys
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP RELATIVE RESPONSE OBSERVATIONS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if ntrrs gt 0 then $
		rtcfg = calcfg[trrs]
	if ndrrs gt 0 then $
		rdcfg = calcfg[drrs]
	kcwi_print_info,ppar,pre,'Number of relative response images',nrrs
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; GROUP STANDARD STAR OBSERVATIONS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if nstds gt 0 then $
		stdcfg = kcfg[stds]
	kcwi_print_info,ppar,pre,'Number of standard star images',nstds
	;
	; write out master KCWI_PPAR into file
	ppar.ppfname = 'kcwi.ppar'
	kcwi_write_ppar,ppar,/archive
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; MATCH EACH OBSERVATION TO CORRESPONDING CAL OBJECT
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; set up configuration matching: here is our list of 
	; default tags to match in the KCWI_CFG struct
	mtags = ['XBINSIZE','YBINSIZE','GRATID','GRANGLE','FILTNUM', $
		 'CAMANG','IFUNUM']
	; tags for direct images
	dtags = ['XBINSIZE','YBINSIZE','GRATID','FILTNUM','CAMANG','IFUNUM']
	;
	; uncalibrated objects
	unbias = ['']
	undark = ['']
	unflat = ['']
	ungeom = ['']
	unprof = ['']
	unrr   = ['']
	unstd  = ['']
	;
	; proc filename
	procfile = odir+'kcwi.proc'
	;
	; log what we are doing
	kcwi_print_info,ppar,pre,'Writing automatic cal associations to: ' + $
		procfile
	kcwi_print_info,ppar,pre,'Edit this file to customize cal associations'
	;
	; open master proc file
	filestamp,procfile,/arch
	openw,kp,procfile,/get_lun
	printf,kp,'# '+pre+'  '+systime(0)
	printf,kp,'# R   = CCD Readout Speed : 0 - slow, 1 - fast'
	printf,kp,'# G   = Gain Multiplier   : 10, 5, 2, 1'
	printf,kp,'# SSM = Sky, Shuffle, Mask: 0 - no, 1 - yes'
	printf,kp,'#  Img Bin AMPS R  G SSM IFU GRAT FILT    Cwave JDobs         Expt Type          Imno   RA          Dec             PA      Air  Object'
	;
	; loop over images
	for i=0,nproc-1 do begin
		;
		; pointer to image to process
		p = proc[i]
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
		printf,kp,imsum,format='(a)'
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH MASTER BIAS IMAGE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		if ppar.nbgrps gt 0 then begin
			tlist = ['xbinsize','ybinsize','ampmode', $
				 'ccdmode','gainmul']
			;
			; we don't really need these to be bias-subtracted
			sile = (strmatch(kcfg[p].imgtype,'cbars') or $
				strmatch(kcfg[p].imgtype,'arc'))
			mcfg = kcwi_match_cfg(bcfg,kcfg[p],ppar,tlist, $
				count=b,silent=sile)
			;
			; multiple matches, take the closest one in sequence
			if b gt 1 then begin
				zdel = abs(mcfg.groupnum - kcfg[p].imgnum)
				zind = (where(zdel eq min(zdel)))[0]
				mbfile = mcfg[zind].groupfile
				;
				; log
				kcwi_print_info,ppar,pre,'master bias file = '+$
					mbfile
				mbfile = odir + mbfile
			;
			; only one match
			endif else if b eq 1 then begin
				mbfile = mcfg.groupfile
				;
				; log
				kcwi_print_info,ppar,pre,'master bias file = '+$
					mbfile
				mbfile = odir + mbfile
			;
			; handle the no match case or when no bias frames were taken
			endif else begin
				kcwi_print_info,ppar,pre,$
				     'cannot associate with any local master bias: '+ $
				     kcfg[p].obsfname,/warning
				mbfile = ''
				;
				; did we specify an alternative?
				if keyword_set(altcaldir) then begin
					mbfile = kcwi_alt_cals(kcfg[p],adir,ppar,/bias)
					;
					; log if matched
					if mbfile ne '' then $
						kcwi_print_info,ppar,pre, $
							'master bias file = '+mbfile
				endif
			endelse
			;
			; print proc
			if mbfile ne '' then begin
				printf,kp,'masterbias='+mbfile
			;
			; if not matched, log as uncalibrated
			endif else begin
				cstr = (kcwi_cfg_string(kcfg[p],/delim,/bias))[0]
				unbias = [ unbias, cstr ]
			endelse
		endif	; ppar.nbgrps gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH MASTER DARK IMAGES
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		if ( strmatch(kcfg[p].imgtype,'dark') eq 1 or $
		     strmatch(kcfg[p].imgtype,'object') eq 1 ) and ppar.ndgrps gt 0 then begin
			tlist = ['xbinsize','ybinsize','ampmode','ccdmode']
			mcfg = kcwi_match_cfg(dcfg,kcfg[p],ppar,tlist,count=d)
			if d ge 1 then begin
				;
				; refine based on exposure time
				tdel = abs(mcfg.exptime - kcfg[p].exptime)
				tind = where(tdel eq min(tdel), ntind)
				;
				; same exposure time, choose closest in sequence
				if ntind gt 1 then begin
					zcfg = mcfg[tind]
					zdel = abs(zcfg.groupnum - kcfg[p].imgnum)
					zind = (where(zdel eq min(zdel)))[0]
					mcfg = zcfg[zind]
				endif else $
					mcfg = mcfg[tind]
				mdfile = mcfg.groupfile
				;
				; log
				kcwi_print_info,ppar,pre,'master dark file = '+$
					mdfile
				mdfile = odir + mdfile
			endif else begin
				kcwi_print_info,ppar,pre, $
					'cannot associate with any local master dark: '+ $
					kcfg[p].obsfname,/warning
				mdfile = ''
				;
				; did we specify an alternative?
				if keyword_set(altcaldir) then begin
					mdfile = kcwi_alt_cals(kcfg[p],adir,ppar,/dark)
					;
					; log if matched
					if mdfile ne '' then $
						kcwi_print_info,ppar,pre, $
							'master dark file = '+mdfile
				endif
			endelse
			;
			; print proc
			if mdfile ne '' then begin
				printf,kp,'masterdark='+mdfile
			;
			; if not matched, log as uncalibrated
			endif else begin
				cstr = (kcwi_cfg_string(kcfg[p],/delim,/bias))[0]
				undark = [ undark, cstr ]
			endelse
		endif	; only object frames and ndgrps gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH MASTER FLAT IMAGE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; no sense flat fielding the dark frames
		if strmatch(kcfg[p].imgtype,'dark') ne 1 and strpos(kcfg[p].obstype,'direct') lt 0 and $
			ppar.nfgrps gt 0 then begin
			;
			; we don't really need these to be flat-fielded
			sile = (strmatch(kcfg[p].imgtype,'cbars') or $
				strmatch(kcfg[p].imgtype,'arcbars') or $
				strmatch(kcfg[p].imgtype,'arc'))
			mcfg = kcwi_match_cfg(fcfg,kcfg[p],ppar,mtags,imgtype='cflat',/time, $
				count=f,silent=sile)
			if f eq 1 then begin
				mffile = mcfg.groupfile
				;
				; log
				kcwi_print_info,ppar,pre,'master flat file = ' + mffile
				mffile = odir + mffile
			;
			; handle ambiguous match or the case when no flat frames were taken
			endif else begin
				kcwi_print_info,ppar,pre, $
					'cannot associate with any local master flat: '+ $
					kcfg[p].obsfname,/warning
				mffile = ''
				;
				; did we specify an alternative?
				if keyword_set(altcaldir) then begin
					mffile = kcwi_alt_cals(kcfg[p],adir,ppar,/flat)
					;
					; log if matched
					if mffile ne '' then $
						kcwi_print_info,ppar,pre, $
							'master flat file = '+mffile
				endif
			endelse
			;
			; print proc
			if mffile ne '' then begin
				printf,kp,'masterflat='+mffile
			;
			; if not matched, log as uncalibrated
			endif else begin
				cstr = (kcwi_cfg_string(kcfg[p],/delim,/long))[0]
				unflat = [ unflat, cstr ]
			endelse
		endif	; only object and cflat frames and nfgrps gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH CBARS AND ARC IMAGES (GEOM)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; init
		cbfile = ''
		arfile = ''
		mgfile = ''
		;
		; no sense creating a dark data cube or matching direct image
		if strmatch(kcfg[p].imgtype,'dark') ne 1 and strpos(kcfg[p].obstype,'direct') lt 0 and $
			ppar.nggrps gt 0 then begin
			mcfg = kcwi_match_cfg(ccfg,kcfg[p],ppar,mtags,imgtype='cbars',/time,count=c,/silent)
			if c eq 1 then begin
				;
				; record cbars filename
				cbfile = strmid(mcfg.obsfname,0,strpos(mcfg.obsfname,'.fit'))+'_int.fits'
				;
				; now find matched arc
				m = where(ccfg.imgnum eq mcfg.imgnum)
				mcf2 = acfg[m]
				;
				; record arc filename
				arfile = strmid(mcf2.obsfname,0,strpos(mcf2.obsfname,'.fit'))+'_int.fits'
				;
				; log
				kcwi_print_info,ppar,pre,'cbars file = '+cbfile
				kcwi_print_info,ppar,pre,'arc   file = '+arfile
				;
				; handle the ambiguous case or when no cbars image can be found
			endif else begin
				kcwi_print_info,ppar,pre, $
				    'cannot find local geom images (arc, cbars) for object image: '+ $
				    kcfg[p].obsfname,/warning
				cbfile = ''
				arfile = ''
				mgfile = ''
				;
				; did we specify an alternative?
				if keyword_set(altcaldir) then begin
					mgfile = kcwi_alt_cals(kcfg[p],adir,ppar,/geom)
					;
					; log if matched
					if mgfile ne '' then begin
						kcwi_print_info,ppar,pre, $
							'master geom file = '+mgfile
						printf,kp,'geom='+mgfile
					endif
				endif
				;
				; if not matched, log as uncalibrated
				if mgfile eq '' then begin
					cstr = (kcwi_cfg_string(kcfg[p],/long,/delim))[0]
					ungeom = [ ungeom, cstr ]
				endif
			endelse
			;
			; print proc
			if cbfile ne '' and arfile ne '' then begin
				printf,kp,'geomcbar='+odir+cbfile
				printf,kp,'geomarc='+odir+arfile
			endif
		endif	; only object and cflat frames and ncbars gt 0 and narcs gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH ARCBARS AND ARC IMAGES (DGEOM)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; no sense creating a dark direct image
		if strpos(kcfg[p].obstype,'direct') ge 0 and strmatch(kcfg[p].imgtype,'dark') ne 1 and $
			ppar.ndggrps gt 0 then begin
			mcfg = kcwi_match_cfg(dccfg,kcfg[p],ppar,dtags,imgtype='arcbars',/time,count=c,/silent)
			if c eq 1 then begin
				;
				; record arcbars filename
				cbfile = strmid(mcfg.obsfname,0,strpos(mcfg.obsfname,'.fit'))+'_int.fits'
				;
				; now find matched arc
				m = where(dccfg.imgnum eq mcfg.imgnum)
				mcf2 = dacfg[m]
				;
				; record arc filename
				arfile = strmid(mcf2.obsfname,0,strpos(mcf2.obsfname,'.fit'))+'_int.fits'
				;
				; log
				kcwi_print_info,ppar,pre,'arcbars file = '+cbfile
				kcwi_print_info,ppar,pre,'arc     file = '+arfile
				;
				; handle the ambiguous case or when no cbars image can be found
			endif else begin
				kcwi_print_info,ppar,pre, $
				    'cannot unambiguously find direct geom images (arc, arcbars) for object image: '+ $
				    kcfg[p].obsfname,/warning
				cbfile = ''
				arfile = ''
				mgfile = ''
				;
				; did we specify an alternative?
				if keyword_set(altcaldir) then begin
					mgfile = kcwi_alt_cals(kcfg[p],adir,ppar,/dgeom)
					;
					; log if matched
					if mgfile ne '' then begin
						kcwi_print_info,ppar,pre, $
							'master dgeom file = '+mgfile
						printf,kp,'geom='+mgfile
					endif
				endif
				;
				; if not matched, log as uncalibrated
				if mgfile eq '' then begin
					cstr = (kcwi_cfg_string(kcfg[p],/long,/delim))[0]
					ungeom = [ ungeom, cstr ]
				endif
			endelse
			;
			; print proc
			if cbfile ne '' and arfile ne '' then begin
				printf,kp,'geomcbar='+odir+cbfile
				printf,kp,'geomarc='+odir+arfile
			endif
		endif	; only object and cflat frames and ncbars gt 0 and narcs gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH SLICE PROFILE OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; no point profile correcting dark frames
		; also require geometry solution
		if strmatch(kcfg[p].imgtype,'dark') ne 1 and $
		   strpos(kcfg[p].obstype,'direct') lt 0 and $
		   nprofs gt 0 and cbfile ne '' and arfile ne '' then begin
			mcfg = kcwi_match_cfg(pcfg,kcfg[p],ppar,mtags, $
						count=s,/time)
			if s eq 1 then begin
				;
				; record slice profile observation filename
				pfile = strmid(mcfg.obsfname,0,strpos(mcfg.obsfname,'.fit'))+'_prof.fits'
				;
				; log
				kcwi_print_info,ppar,pre, $
					'slice profile file = '+pfile
				pfile = odir + pfile
				;
				; handle the ambiguous case or 
				; when no slice profile image can be found
			endif else begin
				kcwi_print_info,ppar,pre, $
				    'cannot associate local slice profile image for object image: '+ $
				    kcfg[p].obsfname,/warning
				pfile = ''
				;
				; did we specify an alternative?
				if keyword_set(altcaldir) then begin
					pfile = kcwi_alt_cals(kcfg[p],adir,ppar,/prof)
					;
					; log if matched
					if pfile ne '' then $
						kcwi_print_info,ppar,pre, $
							'slice profile file = '+pfile
				endif
				;
				; still not matched?  Use twilight flats or domes
				;
				; first check for twilight flats
				if pfile eq '' and ntrrs gt 0 then begin
					mcfg = kcwi_match_cfg(rtcfg,kcfg[p],ppar,mtags, $
								count=s,/time)
					if s eq 1 then begin
						pfile = strmid(mcfg.obsfname,0, $
								strpos(mcfg.obsfname,'.fit'))+'_prof.fits'
						;
						; log
						kcwi_print_info,ppar,pre, $
							'twilight flat slice profile file = '+pfile
						pfile = odir + pfile
					endif
				endif
				;
				; next check for dome flats
				if pfile eq '' and ndrrs gt 0 then begin
					mcfg = kcwi_match_cfg(rdcfg,kcfg[p],ppar,mtags, $
								count=s,/time)
					if s eq 1 then begin
						pfile = strmid(mcfg.obsfname,0, $
								strpos(mcfg.obsfname,'.fit'))+'_prof.fits'
						;
						; log
						kcwi_print_info,ppar,pre, $
							'dome flat slice profile file = '+pfile
						pfile = odir + pfile
					endif
				endif
				;
				; if not matched, log as uncalibrated
				if pfile eq '' then begin
					cstr = (kcwi_cfg_string(kcfg[p],/long,/delim))[0]
					unprof = [ unprof, cstr ]
				endif
			endelse
			;
			; print proc
			if pfile ne '' then $
				printf,kp,'masterprof='+pfile
		endif	; only object frames and nprofs gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH SKY OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; only object frames can have sky observations
		if strmatch(kcfg[p].imgtype,'object') eq 1 and $
		   kcfg[p].skyobs eq 0 and nskys gt 0 then begin
			mcfg = kcwi_match_cfg(scfg,kcfg[p],ppar,mtags, $
					/object,/time,count=s,/silent)
			if s eq 1 then begin
				;
				; record sky filename
				skfile = mcfg.obsfname
				;
				; log
				kcwi_print_info,ppar,pre,'sky file = '+skfile
			endif else begin
				kcwi_print_info,ppar,pre, $
					'No sky obs with same object name', $
					kcfg[p].targname,format='(a,2x,a)', $
					/warning
				skfile = ''
			endelse
			;
			; print proc
			if skfile ne '' then $
				printf,kp,'mastersky='+odir+skfile
		endif	; only object frames and nskys gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH RELATIVE RESPONSE OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; no sense response correcting dark frames
		; also require geometry solution
		if strmatch(kcfg[p].imgtype,'dark') ne 1 and $
		   strpos(kcfg[p].obstype,'direct') lt 0 and $
		   nrrs gt 0 and cbfile ne '' and arfile ne '' then begin
			;
			; twilight flats
			if ntrrs gt 0 then $
				mtcfg = kcwi_match_cfg(rtcfg,kcfg[p],ppar, $
							mtags,count=rt,/time) $
			else	rt = 0
			;
			; dome flats
			if ndrrs gt 0 then $
				mdcfg = kcwi_match_cfg(rdcfg,kcfg[p],ppar, $
							mtags,count=rd,/time) $
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
				rrfile = strmid(mcfg.obsfname,0,strpos(mcfg.obsfname,'.fit'))+'_rr.fits'
				;
				; log
				kcwi_print_info,ppar,pre, $
					'relative response file = '+rrfile
				rrfile = odir + rrfile
			endif else begin
				kcwi_print_info,ppar,pre, $
				    'cannot associate local relative response image for object image: '+ $
				    kcfg[p].obsfname,/warning
				rrfile = ''
				;
				; did we specify an alternative?
				if keyword_set(altcaldir) then begin
					rrfile = kcwi_alt_cals(kcfg[p],adir,ppar,/rr)
					;
					; log if matched
					if rrfile ne '' then $
						kcwi_print_info,ppar,pre, $
							'relative response file = '+rrfile
				endif
			endelse
			;
			; print proc
			if rrfile ne '' then begin
				printf,kp,'masterrr='+rrfile
			;
			; if not matched, log as uncalibrated
			endif else begin
				cstr = (kcwi_cfg_string(kcfg[p],/long,/delim))[0]
				unrr = [ unrr, cstr ]
			endelse
		endif else rrfile = ''	; only object frames and nrrs gt 0
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH DIRECT RELATIVE RESPONSE OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; Use arc image for direct relative response
		if strpos(kcfg[p].obstype,'direct') ge 0 and $
			cbfile ne '' and arfile ne '' then begin
			;
			; get master rr filename
			drrfile = repstr(arfile,'_int','_drr')
			;
			; log
			kcwi_print_info,ppar,pre, $
				'direct relative response file = '+drrfile
			;
			; if we are direct, but there is no arc file
			; just leave the file as set above ('')
			printf,kp,'masterrr='+odir+drrfile
		endif
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ASSOCIATE WITH STANDARD STAR OBSERVATIONS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;
		; correct only object frames
		if strmatch(kcfg[p].imgtype,'object') eq 1 and $
		   strpos(kcfg[p].obstype,'direct') lt 0 and $
		   nstds gt 0 then begin
			mcfg = kcwi_match_cfg(stdcfg,kcfg[p],ppar,mtags, $
						count=std,/time)
			if std eq 1 then begin
				;
				; record stadard observation filename
				stdfile = strmid(mcfg.obsfname,0,strpos(mcfg.obsfname,'.fit'))+'_invsens.fits'
				;
				; log
				kcwi_print_info,ppar,pre,'standard star observation file = '+stdfile
				stdfile = odir + stdfile
			endif else begin
				kcwi_print_info,ppar,pre, $
				    'cannot associate local standard star obs for object image: '+ $
				    kcfg[p].obsfname,/warning
				stdfile = ''
				;
				; did we specify an alternative?
				if keyword_set(altcaldir) then begin
					stdfile = kcwi_alt_cals(kcfg[p],adir,ppar,/std)
					;
					; log if matched
					if stdfile ne '' then $
						kcwi_print_info,ppar,pre, $
							'standard star observation file = '+stdfile
				endif
			endelse
			;
			; print proc
			if stdfile ne '' then begin
				printf,kp,'masterstd='+stdfile
			;
			; if not matched, log as uncalibrated
			endif else begin
				cstr = (kcwi_cfg_string(kcfg[p],/long,/delim))[0]
				unstd = [ unstd, cstr ]
			endelse
		endif	; only object frames
	endfor	; loop over images
	;
	; check for un calibrated observations
	print,''
	printf,ll,""
	kcwi_print_info,ppar,pre,'CONFIGURATION CALIBRATION REPORT'
	if ppar.nbgrps gt 0 then begin
		if n_elements(unbias) gt 1 then begin
			unbias = unbias[1:(n_elements(unbias)-1)]
			unbias = unbias[sort(unbias)]
			unbias = unbias[uniq(unbias)]
			nunbias = n_elements(unbias)
			kcwi_print_info,ppar,pre, $
				'Number of missing bias configurations',nunbias, $
				format='(a,i5)',/warn
			for i = 0,nunbias-1 do $
				kcwi_print_info,ppar,pre,'Missing bias configuration',unbias[i], $
								format='(a,a)'
		endif else $
			kcwi_print_info,ppar,pre,'All bias configurations calibrated'
	endif else kcwi_print_info,ppar,pre,'No bias groups found',/warn
	if ppar.ndgrps gt 0 then begin
		if n_elements(undark) gt 1 then begin
			undark = undark[1:(n_elements(undark)-1)]
			undark = undark[sort(undark)]
			undark = undark[uniq(undark)]
			nundark = n_elements(undark)
			kcwi_print_info,ppar,pre, $
				'Number of missing dark configurations',nundark, $
				format='(a,i5)'
			for i = 0,nundark-1 do $
				kcwi_print_info,ppar,pre,'Missing dark configuration',undark[i], $
								format='(a,a)'
		endif else $
			kcwi_print_info,ppar,pre,'All dark configurations calibrated'
	endif else kcwi_print_info,ppar,pre,'No dark groups found',/warn
	if ppar.nfgrps gt 0 then begin
		if n_elements(unflat) gt 1 then begin
			unflat = unflat[1:(n_elements(unflat)-1)]
			unflat = unflat[sort(unflat)]
			unflat = unflat[uniq(unflat)]
			nunflat = n_elements(unflat)
			kcwi_print_info,ppar,pre, $
				'Number of missing flat configurations',nunflat, $
				format='(a,i5)',/warn
			for i = 0,nunflat-1 do $
				kcwi_print_info,ppar,pre,'Missing flat configuration',unflat[i], $
								format='(a,a)'
		endif else $
			kcwi_print_info,ppar,pre,'All flat configurations calibrated'
	endif else kcwi_print_info,ppar,pre,'No flat groups found',/warn
	if ppar.nggrps gt 0 or ppar.ndggrps gt 0 then begin
		if n_elements(ungeom) gt 1 then begin
			ungeom = ungeom[1:(n_elements(ungeom)-1)]
			ungeom = ungeom[sort(ungeom)]
			ungeom = ungeom[uniq(ungeom)]
			nungeom = n_elements(ungeom)
			kcwi_print_info,ppar,pre, $
				'Number of missing geometry configurations',nungeom, $
				format='(a,i5)',/warn
			for i = 0,nungeom-1 do $
				kcwi_print_info,ppar,pre,'Missing geometry configuration',ungeom[i], $
								format='(a,a)'
		endif else $
			kcwi_print_info,ppar,pre,'All geometry configurations calibrated'
	endif else kcwi_print_info,ppar,pre,'No geom groups found',/warn
	if nprofs gt 0 then begin
		if n_elements(unprof) gt 1 then begin
			unprof = unprof[1:(n_elements(unprof)-1)]
			unprof = unprof[sort(unprof)]
			unprof = unprof[uniq(unprof)]
			nunprof = n_elements(unprof)
			kcwi_print_info,ppar,pre, $
				'Number of missing profile configurations',nunprof, $
				format='(a,i5)',/warn
			for i = 0,nunprof-1 do $
				kcwi_print_info,ppar,pre,'Missing profile configuration',unprof[i], $
								format='(a,a)'
		endif else $
			kcwi_print_info,ppar,pre,'All profile configurations calibrated'
	endif else kcwi_print_info,ppar,pre,'No profile images found',/warn
	if nrrs gt 0 then begin
		if n_elements(unrr) gt 1 then begin
			unrr = unrr[1:(n_elements(unrr)-1)]
			unrr = unrr[sort(unrr)]
			unrr = unrr[uniq(unrr)]
			nunrr = n_elements(unrr)
			kcwi_print_info,ppar,pre, $
				'Number of missing relative response configurations',nunrr, $
				format='(a,i5)',/warn
			for i = 0,nunrr-1 do $
				kcwi_print_info,ppar,pre,'Missing relative response configuration',unrr[i], $
								format='(a,a)'
		endif else $
			kcwi_print_info,ppar,pre,'All relative response configurations calibrated'
	endif else kcwi_print_info,ppar,pre,'No relative response images found',/warn
	if nstds gt 0 then begin
		if n_elements(unstd) gt 1 then begin
			unstd = unstd[1:(n_elements(unstd)-1)]
			unstd = unstd[sort(unstd)]
			unstd = unstd[uniq(unstd)]
			nunstd = n_elements(unstd)
			kcwi_print_info,ppar,pre, $
				'Number of missing standard star configurations',nunstd, $
				format='(a,i5)',/warn
			for i = 0,nunstd-1 do $
				kcwi_print_info,ppar,pre,'Missing standard star configuration',unstd[i], $
								format='(a,a)'
		endif else $
			kcwi_print_info,ppar,pre,'All standard star configurations calibrated'
	endif else kcwi_print_info,ppar,pre,'No standard star images found',/warn
	;
	; report
	eltime = systime(1) - startime
	print,''
	printf,ll,''
	kcwi_print_info,ppar,pre,'run time in seconds',eltime
	kcwi_print_info,ppar,pre,'finished on '+systime(0)
	;
	; close log and proc files
	free_lun,ll,kp
	;
	return
end	; kcwi_prep

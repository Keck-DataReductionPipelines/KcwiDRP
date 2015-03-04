;------------------------------------------------------------------------------
; $Id: kcwi_la_cosmic.pro,v 1.17 2014/10/30 18:39:27 neill Exp $
;
;+
; NAME:
;   kcwi_la_cosmic
;
; PURPOSE:
;   Remove cosmic rays from imaging data.  Images must be debiased for
;     gain estimation to work properly.
;
; CALLING SEQUENCE:
;   kcwi_la_cosmic, img, mask, [sigclip=, gain=, readn=, $
;               skyval=,objlim=, niter=,sigfrac=,statsec=, $
;               zeroindexed=,masksuffix=, isbig=,blocksize=]
;
; INPUTS:
;   img:	float 2-d array
;   ppar:	KCWI_PPAR pipeline parameter struct
;
; KEYWORDS
;   sigclip     Level of cr clipping
;   gain        Gain of the CCD (< 0 to estimate) (default=-1.0)
;   readn       Readnoise of CCD (default=0.0)
;   skyval      Avg. of sky level already subtracted off the images (array)
;   objlim      Detection threshold for objects (not crs)
;   niter       How many passes to take
;   sigfrac     Sigfrac: see PASP paper
;   statsec     image region for statistics
;                string which can be of the form "[23:45,100:300]" or
;                                                 [*,100:400], etc.
;   zeroindexed Is the image region zeroindexed?
;   masksuffix  Suffix to automatically determine mask output name
;   outsuff     Suffix to automatically determine output image name
;   blocksize   Size of working blocks.  Keep in integer multiples of 512
;   isbig       Tell the routine to chop up the images in to more
;                manageable sections of size blocksize x blocksize
;   version	output containing version of kcwi_la_cosmic.pro
;   ntcosmicray output containing the total number of cosmic rays found
; OUTPUTS:
;	mask	byte 2-d array with value of 1 for CR pixels, 0 otherwise
;
; OPTIONAL OUTPUTS:
;
; PROCEDURES CALLED:
;   DJS_ITERSTAT
;   DJS_MEDIAN   
;   reckon_statsec
;   lacos_replace
;   astro-library 
;
; COMMENTS:
;  This routine is based on Pieter Van Dokkum's "LACOSMIC" Cosmic
;  ray rejection routine for IRAF.
;
;  If you find that after ~4 iterations that the routine is still
;  finding up cosmic rays chances are that you are digging into the
;  noise and or objects.  Try setting the sigclip number a bit higher
;
; DEFAULTS:
;   sigclip     4.5
;   gain        -1.0 (e-/DN) (forces routine to estimate for gain)
;   readn       0.0
;   skyval      0.0 
;   objlim      4.0
;   niter       4
;   sigfrac     0.5
;   statsec     "[*,*]" (use the entire image to estimate the gain)
;   zeroindexed 0 (no)
;   isbig       0
;   blocksize   1024
;
; NOTES:
;    (1) This routine will only work on 2-d images 
;    (2) Haven't checked how useful it is on spectroscopic images.
;    (3) Speed is clearly an issue.  It takes about 21 seconds per iteration
;        on a fairly zippy (650 PIII Linux 2.4 kernel) laptop on a 1k x 1k image.
;        Using the isbig=1 the speed should scale as the number of
;        pixels. So the scaling for speed is,
; 
;                t = 21 s * (n/1024)^2 * (cpu/650 MHz)^(-1)
;
;        So that a 2k x 2k image will take ~80s per interation.


; EXAMPLES:
;   1. Remove the cosmic rays from a debiased flat-fielded HST STIS/Clear image
;   hst1.fits and created a replaced image called hst1-cleaned.fits
;   with a mask file called mask.fits. Set the read noise to be 4.46 e-
;   and the gain to be = 1.0 (e-/DN)
;
;      IDL> la_cosmic, ['hst1.fits'], masklist=['mask.fits'], $
;                      outsuffix="-cleaned", readn=4.46, gain=1.0
;
;   2. Remove the cosmic rays from all images starting with the name
;   hst and create masks "hst*-mask.fits" and output images
;   "hst*-out.fits". Set sigclip = 4. Let la_cosmic determine the gain
;   from a region of [50:100,*] (indexed exactly as in IRAF, ie. unity
;   indexed).
;
;      IDL> la_cosmic, 'hst*.fits', outsuffix="-out",
;                masksuffix="-mask",statsec="[50:100,*]",zeroindexed=0, 
;                gain = -1, sigclip = 4.0
;
; BUGS:
;
;  1. If the image has not been debiased, then the gain estimation
;     will go horribly wrong.  Could add a "biassec" parameter to
;     allow the bias to be estimated.
;  2. Speed scaling of the routine works well until the point that
;     the images are too large to store entirely in memory.  Could write
;     a little section which chops up the image in to manageable
;     chunks and pastes those chuncks together...
;
; REVISION HISTORY:
;   20-May-2001  Written by Joshua Bloom, Caltech (jsb@astro.caltech.edu)
;   16-MAY-2013  Shamelessly kludged to work directly on images by Don Neill
;		(neill@srl.caltech.edu)
;   09-JUL-2013  Added timing output
;   17-SEP-2013  Now use ppar to pass loglun, verbose and use kcwi_print_info
;-
;------------------------------------------------------------------------------
pro kcwi_la_cosmic, img, ppar, mask, sigclip=sigclip, $
               gain=gain, readn=readn, skyval=skyval,objlim=objlim, $
               niter=niter,sigfrac=sigfrac, $
               statsec=statsec,zeroindexed=zeroindexed,$
               isbig=isbig, blocksize=blocksize, $
	       version=version, $
	       ntcosmicray=ntcosmicray

;; set some sensible defaults
pre = 'KCWI_LA_COSMIC'
version = repstr('$Revision: 1.17 $ $Date: 2014/10/30 18:39:27 $','$','')
if not keyword_set(gain)	then gain	= -1.0
if not keyword_set(readn)	then readn	= 0.0
if not keyword_set(skyval)	then skyval	= 0.0
if not keyword_set(sigclip)	then sigclip	= 4.5
if not keyword_set(sigfrac)	then sigfrac	= 0.5
if not keyword_set(objlim)	then objlim	= 3.0
if not keyword_set(niter)	then niter	= 4
if not keyword_set(isbig) and $
   not keyword_set(blocksize)	then isbig	= 0
if keyword_set(blocksize)	then isbig	= 1
if not keyword_set(blocksize)	then blocksize	= 512l
if not keyword_set(statsec)	then statsec	= "[*,*]"
if not keyword_set(zeroindexed)	then zeroindexed= 1

;; ppar
if kcwi_verify_ppar(ppar) ne 0 then $
	ppar = { kcwi_ppar }
;; timing
tstart = systime(1)
;; make sure gain/readn are double precision
gain	= double(gain)
readn	= double(readn) > 0d0

if ppar.verbose ge 1 then begin
	print, "----------------------------------------------------------------"
	print, " L.A. Cosmic: Laplacian cosmic ray removal by Pieter van Dokkum"
	print, " IDL version by Josh Bloom, KCWI version by Don Neill"
endif
kcwi_print_info,ppar,pre,version
kcwi_print_info,ppar,pre,'initial gain, readnoise: ' + $
		strtrim(string(gain),2)+', '+strtrim(string(readn),2)
kcwi_print_info,ppar,pre,'Obj thresh, sigma clip : ' + $
		strtrim(string(objlim),2)+', '+strtrim(string(sigclip),2)
;; make the kernel as an array of 3x3
lakernel= [[0.0, -1.0, 0.0],[-1.0,4.0,-1.0],[0.0,-1.0,0.0]]
gkernel	= [[1,1,1],[1,1,1],[1,1,1]]
;; array setups
xsize	= long(n_elements(img[*,0]))
ysize	= long(n_elements(img[0,*]))
mask	= bytarr(xsize,ysize)

usegain = gain
sstop = 0
iter = 1
ntcosmicray = 0L

xblock = blocksize ; 512l
yblock = blocksize ; 512l

if (isbig) then begin
	;; we may have to do some chopping here.  Chop the array into
	;; blocks
	if ((yblock GT ysize) and (xsize GT xblock)) then begin
		;; there's really no need to chop up here.
		isbig = 0
		potx = 1
		poty = 1
		regx = intarr(1,2)
		regy = intarr(1,2)
		regx[0,0] = 0l
		regx[0,1] = xsize-1l
		regy[0,0] = 0l
		regy[0,1] = ysize-1l
	endif else begin
		potx = long(xsize) / long(xblock)
		poty = long(ysize) / long(yblock)
		;; create an array of indices which mark off the regions
		;; of smaller blocks
		regx = intarr(potx,2)
		regy = intarr(poty,2)
		regx[0,0] = 0
		regx[0,1] = xblock - 1
		regy[0,0] = 0
		regy[0,1] = yblock - 1

		for k=1,potx-2 do begin
			regx[k,0] = regx[k-1,1] + 1
			regx[k,1] = regx[k,0] + xblock - 1
		endfor
		;; the last block may be bigger than the others
		regx[potx-1,0] = regx[potx-2,1] + 1
		regx[potx-1,1] = xsize - 1
		for k=1,poty-2 do begin
			regy[k,0] = regy[k-1,1] + 1
			regy[k,1] = regy[k,0] + xblock - 1
		endfor
		regy[poty-1,0] = regy[poty-2,1] + 1
		regy[poty-1,1] = ysize - 1
	endelse
endif else begin
	;; there's really no need to chop up here.
	isbig = 0
	potx = 1
	poty = 1
	regx = intarr(1,2)
	regy = intarr(1,2)
	regx[0,0] = 0l
	regx[0,1] = xsize-1l
	regy[0,0] = 0l
	regy[0,1] = ysize-1l
endelse

while(not sstop) do begin
	if ppar.verbose ge 1 then begin
		print, "-------------------Iteration" + $
			strtrim(string(iter),2) + "-----------------------------------"
	endif
	;; add back in the background if the user so desires
	;img = img + skyval[i]
	if (skyval NE 0.0) then img = temporary(img) + skyval
	;; has an automatic determination of the gain been requested?
	if (gain le 0.0) then begin
		if (ppar.verbose ge 1 and (iter eq 1)) then $
			print, "Trying to determine gain automatically: "
		if (ppar.verbose ge 1 and (iter gt 1)) then $
			print, "Improving gain estimate: "
		;; figure out what statsection to use from the statsec
		;; string
		arr = reckon_statsec(statsec,[xsize,ysize])
		if (zeroindexed eq 0) then begin
			;; user gave a statsec region like IRAF.  Unity indexed.
			arr = arr - 1
		endif
		arr[1] = (arr[0] > arr[1]) < (xsize - 1)
		arr[3] = (arr[2] > arr[3]) < (ysize - 1)
		djs_iterstat, img[arr[0]:arr[1],arr[2]:arr[3]], $
			sigrej=5.0, maxiter=10.0, mean=estmean,$
			median=estmedian, sigma=estsigma
		skylev = estmedian
		sigima = abs(img[arr[0]:arr[1],arr[2]:arr[3]] - $
			djs_median(img[arr[0]:arr[1],arr[2]:arr[3]],$
			width=7,boundary='reflect'))
		djs_iterstat, sigima, $
			sigrej=5.0, maxiter=10.0,mean=estmean,$
			median=estmedian, sigma=estsigma
		sig = estmedian * 1.48
		usegain = skylev/sig^2
		kcwi_print_info,ppar,pre,"Approximate sky level = " + $
				strtrim(string(skylev),2) + ' ADU'
		kcwi_print_info,ppar,pre,"Sigma of sky = " + strtrim(string(sig),2)
		kcwi_print_info,ppar,pre,"Estimated gain = " + strtrim(string(usegain),2)
		if (usegain le 0) then begin
			kcwi_print_info,ppar,pre,'Gain was found to be less than zero',/error
			kcwi_print_info,ppar,pre,'is it possible you forgot to give a "skyval"?',/error
			return
		endif
	endif	;; automatic gain determination requested
	if (ppar.verbose ge 1) then $
		print, 'Convolving image with Laplacian kernel'
	;; we've got to chop this image up
	nchop = 1
	ncosmicray = 0
	for xchop = 0, potx - 1 do for ychop = 0, poty - 1 do begin
		imgwork = img
		maskwork = mask
		if ppar.verbose ge 1 and (potx*poty gt 1) then $
			print, 'Working on block #' + strtrim(string(nchop),2) + $
			' out of ' +  strtrim(string(potx*poty),2)
              
		;; rebin the image and convolve with the kernel then rebin
		tmpxsize = regx[xchop,1] - regx[xchop,0] + 1
		tmpysize = regy[ychop,1] - regy[ychop,0] + 1
              
		im2 = REBIN(imgwork,2*tmpxsize, 2*tmpysize,/SAMPLE)
		im2 = REBIN( (CONVOL(im2, lakernel,1,/CENTER,/EDGE_TRUNCATE) > 0.0), tmpxsize, tmpysize)
		;; compute the 5x5 median to remove all the cosmic rays
		med5 = djs_median(imgwork,width=5,boundary='reflect')
		;; NOTE: the boundary is not handled the same as in IRAF
		;; This affects the outer 2 pixel boundary...could change
		;; with some kludges...

		;; SAME but slower: med5 = lacos_replace(med5,0.0001,'INDEF', 0.0)
		bad = med5 LE 0.0
		med5 = 0.0001 * bad + temporary(med5) * (1.0 - bad)
		bad = [0]

		;; create a noise model based on this median image knowing the
		;; gain, and readnoise of the image 
		;; note that this step supposes that no sky background
		;; subtration has been done
		if gain ne usegain then $
			kcwi_print_info,ppar,pre, $
				'Creating noise model using gain = ',usegain, $
				', readnoise = ',readn, form='(a,f8.3,a,f8.3)'
		noise  = sqrt(med5*usegain + readn^2)/usegain
		med5 = [0]
		sigmap =  im2/noise/2.d 
		;; free up some memory
		im2 = [0]
		sigmap =  -1.0 * (djs_median(sigmap,width=5,boundary='reflect') - $
			temporary(sigmap))
        
		;; do a replacement setting all the high values to 1
		firstsel = sigmap
		firstsel = firstsel * (firstsel GT sigclip)
		firstsel = firstsel * (firstsel LT 0.1) + (firstsel GE 0.1)
		med3 = djs_median(imgwork,width=3,boundary='reflect')
		med7 = djs_median(med3,width=7,boundary='reflect')

		med3 = (temporary(med3) - med7)/noise
		noise = [0]
		med7 = [0]
		med3 = temporary(med3) > 0.01

		starreject = firstsel*sigmap/med3
		med3 = [0]
		firstsel = temporary(firstsel) * (starreject GT objlim)
		starreject = [0]

		;; grow CRs by one pixel and check in original sigma map
		gfirstsel = convol(firstsel,gkernel,/center,/edge_truncate)
		firstsel = [0]
		gfirstsel = sigmap * ((gfirstsel GT 0.5) + $
			gfirstsel*(gfirstsel LE 0.5))
		gfirstsel = (temporary(gfirstsel) GT sigclip)
		sigcliplow = sigfrac * sigclip
        
		finalsel = convol(gfirstsel,gkernel,/center,/edge_truncate)
		finalsel = sigmap * ((finalsel GT 0.5) + $
			finalsel*(finalsel LE 0.5))
		sigmap = [0]
		finalsel = (temporary(finalsel) GT sigcliplow)
        
		;; how many cosmic rays were found here
		gfirstsel = (1.0 - maskwork)*finalsel
		ttt = where(gfirstsel ge 0.5,npix)
		ttt = [0]
		gfirstsel = [0]
		maskwork = (temporary(maskwork) + finalsel) < 1
		finalsel = [0]

		ncosmicray = ncosmicray + npix
		inputmask = (1.0 - 10000.0*maskwork)*imgwork
		inputmask = lacos_replace(inputmask, !VALUES.F_NAN,'INDEF',-9999)
		med5 = djs_median(inputmask,width=5,boundary='reflect')*maskwork
		inputmask = [0]
		output = (1.0 - maskwork)*imgwork + med5
		med5 = [0]
		img[regx[xchop,0]:regx[xchop,1],regy[ychop,0]:regy[ychop,1]] = $
			output[0:(tmpxsize-1),0:(tmpysize-1)]
		mask[regx[xchop,0]:regx[xchop,1],regy[ychop,0]:regy[ychop,1]] = $
			maskwork[0:(tmpxsize-1),0:(tmpysize-1)]
		nchop = nchop + 1
	endfor

	kcwi_print_info,ppar,pre,'found ' + strtrim(string(ncosmicray)) + ' cosmic rays in iteration ' + strtrim(string(iter)),info=2
	if (ncosmicray eq 0) then sstop = 1
	iter = iter + 1
	if (iter gt niter) then sstop = 1
	if (skyval > 0) then img = temporary(img) - skyval
	ntcosmicray = ntcosmicray + ncosmicray
	;; groovy ---------------------------------------
endwhile
;; timing
runtime = systime(1) - tstart
kcwi_print_info,ppar,pre,'found total of ' + strtrim(string(ntcosmicray)) + $
	' cosmic rays in '+string(runtime,form='(f6.1)')+' seconds'
return
end

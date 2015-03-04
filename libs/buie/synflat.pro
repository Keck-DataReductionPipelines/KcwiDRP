;+
; NAME:
;   synflat
; PURPOSE:
;   Create a synthetic flat frame with optional overscan
; DESCRIPTION:
;   Using a base flat field array, a signal level in photons, and a gain,
;   a flat image array is created with the flat aberrations and the
;   photon (Poisson) statistics.
;   We add to this a base dark image and a bias frame that is created by 
;   synbias,
;   which adds  a bias level (overscan level) and simulated read noise 
;   - an optional overscan area is created that contains bias and
;   read noise only. The sigma is specified for the read noise, which
;   is normally distributed wih a mean of 0. The final output is an
;   integer array.
; CATEGORY:
;   CCD data processing
; CALLING SEQUENCE:
;   synflat,flatbase,slvl,darkbase,etime,gain,biasbase,biaslevel,rdnoise,seed,$
;      flatframe
; INPUTS:
;   flatbase = 2-D float input array. This is conceptually a constant and
;              consists of dimensionless values between 0 and 1.0 that multiply
;              the base signal (sky or screen) level of the flat.
;   slvl     = flat signal level in counts.
;   darkbase = 2-D integer float input array. This is conceptually a constant
;              and is multiplied by the exposure time to generate the
;              ideal dark image (units are counts per second)
;   etime    = exposure time in seconds
;   gain       hypothetical gain (e-/DN) for statistics
;   biasbase = 2-D input array.  This is conceptually a constant, normally
;              floating point, with each pixel in units of D/N (counts).
;              It represents the bias structure introduced by the electronics.
;   biaslevel  bias (overscan) level, in units of D/N (counts)
;   rdnoise    value in electrons/pixel. Sigma for bias is rdnoise/gain.
;   seed       for random number generator- both input and output.

; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;   OVERSCAN - integer, number of columns for the explicit overscan area.
;              If less than or equal to 0, there is no overscan area generated
;              and the dimensions of the output array matches biasbase.
;   FITSFN -   Fits file for the flat frame. The exposure time is included
;              in the header. The default is to generate no file.
;   CRSNUM -   number of cosmic ray strikes per ccd area of image. The units
;              are strikes per megapixel per image. Cosmic rays will not be 
;              placed in the overscan area, if one is defined. The count
;              of CRS for an image is chosen as a Poisson deviate with the
;              given mean.
;   CRSMIN  -  minimum D/N for cosmic ray strikes that will be added to the 
;              output.
;   CRSMAX  -  maximum D/N for cosmic ray strikes that will be added to the 
;              output. Intensity of each CRS is drawn uniformly over the
;              range [CRSMIN,CRSMAX].
;   STARRATE - mean number of stars added per image. Stars are gaussian
;              PSF's as computed by synstar. Stars will not be placed in the
;              overscan area, if one is defined. The number of stars
;              for an image is chosen as a Poisson deviate with the
;              given mean.
;   STARMIN -  minimum flux-  as passed to synstar- for stars that will be 
;              added to the output.
;   STARMAX -  maximum flux-  as passed to synstar- for stars that will be 
;              added to the output. Flux of each star is drawn uniformly 
;              over the range [STARMIN,STARMAX].
; KEYWORD OUTPUT PARAMETERS:
;   CRSOUT  -  Output, number of cosmic rays added to the image.
;   STARSOUT - Output, number of stars added to the image.
;
; OUTPUTS:
;   flatframe - 2-D int array that is the synthetic flat image. Its dimensions
;               are n x (m + o) where  n, m are the dimensions of  flatbase and
;               o is the number of overscan columns created. The overscan 
;               area of the flat is flatframe[n:n+o-1, *]. Pixels are in units
;               of D/N counts. Pixel values are  calculated in floating point 
;               and rounded to nearest integer. They are truncated at 32767.
;
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; Only handles single images- the darkbase, flatbase  and biasbase must be 
; identically dimensioned.
; PROCEDURE:
; MODIFICATION HISTORY:
;   2006 June 28, Written by Peter L. Collins, Lowell Observatory  
;   2006/07/10    PLC, reorder arguments for consistency
;   2006/07/12    PLC, CRS keywords- other documentation and argument cleanups.
;   2006/07/12    PLC, add keywords CRSOUT and STARSOUT.
;   2006/07/14, MWB, final cleanup and addition to library.
;-

pro synflat,flatbase,slvl,darkbase,etime,gain,biasbase,biaslevel,rdnoise,seed, $
            flatframe, OVERSCAN=oscsize, FITSFN=fn, $
            CRSNUM=crsnum,CRSMIN=crsmin,CRSMAX=crsmax, $
            STARRATE=starrate,STARMIN=starmin,STARMAX=starmax,CRSOUT=crsout, $
            STARSOUT=starsout

   self='SYNFLAT: '
   if badpar(flatbase,[4,5],2,caller=self+'(flatbase) ') then return
   if badpar(slvl,[1,2,3,4,5],0,caller=self+'(slvl) ') then return
   if badpar(darkbase,[2,3,4,5],2,caller=self+'(darkbase) ') then return
   if badpar(etime,[1,2,3,4,5],0,caller=self+'(etime) ') then return
   if badpar(gain,[4,5],0,caller=self+'(gain) ') then return
   if badpar(biasbase,[2,3,4,5],2,caller=self+'(biasbase) ') then return
   if badpar(biaslevel,[2,3,4,5],0,caller=self+'(biaslevel) ') then return
   if badpar(rdnoise,[2,3,4,5],0,caller=self+'(rdnoise) ') then return
   if badpar(seed,[0,1,2,3,4,5],[0,1],caller=self+'(seed) ') then return
   if badpar(oscsize,[0,2,3],0,caller=self+'(OSCSIZE) ', default=0) then return
   if badpar(fn,[0,7],0,caller=self+'(FITSFN) ', default='') then return
   if badpar(crsnum,[0,2,3,4,5],0,caller=self+'(CRSNUM) ',  $
             default=8.0) then return
   if badpar(crsmin,[0,2,3,4,5],0,caller=self+'(CRSMIN) ',  $
             default=1000.0) then return
   if badpar(crsmax,[0,2,3,4,5],0,caller=self+'(CRSMAX) ',  $
             default=2000.0) then return
   if badpar(starrate,[0,2,3,4,5],0,caller=self+'(STARRATE) ',  $
             default=8.0) then return
   if badpar(starmin,[0,2,3,4,5],0,caller=self+'(STARMIN) ',  $
             default=500.0) then return
   if badpar(starmax,[0,2,3,4,5],0,caller=self+'(STARMAX) ',  $
             default=50000.0) then return
   if badpar(starsout,[0,2,3,4,5],0,caller=self+'(STARSOUT) ',  $
             default=0.0) then return
   if badpar(crsout,[0,2,3,4,5],0,caller=self+'(CRSOUT) ',  $
             default=0.0) then return

   sz = size(biasbase)
   nx = sz[1]
   ny  =sz[2]

   dsz = size(darkbase)
   if sz[1] ne nx or sz[2] ne ny then begin
      printf,self,' ERROR! dark and bias base images have differing dimensions'
      return
   endif

   fsz = size(flatbase)
   if fsz[1] ne nx or fsz[2] ne ny then begin
      printf,self,' ERROR! flat and bias base images have differing dimensions'
      return
   endif

   ; start with a biased (null) image and tack on overscan
   synbias,biasbase,biaslevel,gain,rdnoise,seed,flatframe,OVERSCAN=oscsize, $
           CRSNUM=crsnum,CRSMAX=crsmax,CRSMIN=crsmin,CRSOUT=crsout

   ; start with synthetic stars, note that this first step creates the
   ; synthetic image of the sky and we're working in units of photons for now.
   if starrate gt 0.0 then begin
      starsout = randomn(seed,POISSON=starrate)
      if starsout ne 0 then begin 
         x = randomu(seed,starsout)*(nx-1) < (nx-1)
         y = randomu(seed,starsout)*(ny-1) < (ny-1)
         flux = randomu(seed,starsout)*(starmax - starmin)  + starmin
         flattemp =  synstar(nx, ny,x, y, flux, 1.5)
      endif
   endif else begin
      flattemp = fltarr(nx,ny)
   endelse

   ;  add the dark signal to the temporary flat. Each dark
   ;  pixel is a Poisson deviant with mean equal to the dark base rate
   ;  multiplied by the exposure time to yield
   flattemp += darkbase*gain*etime

   ; now add the signal that the sky (flat field) would generate
   flattemp += flatbase*slvl*gain

   ; all the photons are in place now.  Take the perfect image and push
   ;  it through the poisson noise generator and convert back to counts
   flattemp = poidev(flattemp,SEED=seed)/gain

   ; add the flat data to the already generated bias
   flatframe[0:nx-1,*] = fix( (flattemp<32767.0) + 0.5 )

   if fn ne '' then begin
      mkhdr, hdr,flatframe
      sxaddpar,hdr,'RDNOISE',rdnoise,' Read-noise in image in electrons'
      sxaddpar,hdr,'BIASVAL',biaslevel,' bias level in counts'
      sxaddpar,hdr,'GAIN',gain,' Gain in e-/ADU'
      sxaddpar,hdr,'EXPTIME',etime
      sxaddpar,hdr,'FILPOS',0
      writefits,fn,flatframe,hdr
   endif

end

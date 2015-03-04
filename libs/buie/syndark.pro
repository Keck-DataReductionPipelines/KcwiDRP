;+
; NAME:
;   syndark
; PURPOSE:
;   Create a synthetic CCD dark frame with optional overscan
; DESCRIPTION:
;   Adds a base dark image to a bias frame that is created by synbias,
;   which adds  a bias level (overscan level) and simulated read noise to an
;   arbitrary base image- an optional overscan area contains bias and
;   read noise only. The sigma is specified for the read noise, which
;   is normally distributed wih a mean of 0. The output is an
;   integer array.
; CATEGORY:
;   CCD data processing
; CALLING SEQUENCE:
;   syndark,darkbase,etime,gain,biasbase,biaslevel,rdnoise,seed,darkframe
; INPUTS:
;   darkbase = 2-D input array.  This is conceptually a constant, normally
;              floating point, with each pixel in units of counts/sec.
;   etime    = exposure time in seconds, which is a multiplier for darkbase
;   gain       hypothetical gain (e-/DN) for statistics.
;   biasbase = 2-D input array.  This is conceptually a constant, normally
;              floating point, with each pixel in units of D/N (counts).
;              It represent the bias structure introduced by the electronics.
;   biaslevel  integer or float, bias (overscan) level, in units of D/N (counts)
;   rdnoise    integer or float, in electrons/pixel. Sigma for bias is 
;              rdnoise/gain.
;   seed       for random number generator- both input and output.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;   OVERSCAN - integer, number of columns for the explicit overscan area.
;              if less than or equal to 0, there is no overscan area generated
;              and the dimensions of the output array match biasbase.
;
;   FITSFN -   Fits file for the output dark frame. Exposure time is included
;              in the header. The default is to generate no file.
;   CRSNUM -   Average number of cosmic ray strikes per ccd area of image.
;   CRSMAX  -  maximum D/N for cosmic ray strikes that will be added to the 
;              output. Intensity of each CRS is drawn uniformly over the
;              range [CRSMIN,CRSMAX].
;   CRSMIN  -  minimum D/N for cosmic ray strikes that will be added to the 
;              output.
; KEYWORD OUTPUT PARAMETERS:
;   CRSOUT  -  Output, number of cosmic rays added to the image.
;
; OUTPUTS:
;   darkframe - 2-D int array that is the synthetic dark image. It's dimensioned
;               n x (m + o) where  n, m are the dimensions of  darkbase and
;               o is the number of overscan columns created. The overscan 
;               area of the dark is darkframe[n:n+o-1, *]. Pixels are 
;               in units of D/N counts.
;               Pixel values are  calculated in floating point and rounded
;               and rounded to nearest integer. They are truncated at 32767.
; COMMON BLOCKS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; Only handles single images- the darkbase and biasbase must be identically
; dimensioned.
; PROCEDURE:
; MODIFICATION HISTORY:
;   2006 June 28, Written by Peter L. Collins, Lowell Observatory  
;   2006/07/10    PLC, reorder arguments for consistency
;   2006/7/12    PLC, CRS keywords- other documentation and argument cleanups.
;   2006/07/13    PLC, add keyword CRSOUT.
;   2006/07/14, MWB, final cleanup and addition to library.
;-

pro syndark,darkbase,etime,gain,biasbase,biaslevel,rdnoise,seed,darkframe, $
            OVERSCAN=oscsize,FITSFN=fn, $
            CRSNUM=crsnum,CRSMIN=crsmin,CRSMAX=crsmax,CRSOUT=crsout

   self='SYNDARK: '
   if badpar(darkbase,[2,3,4,5],2,caller=self+'(darkbase) ') then return
   if badpar(etime,[1,2,3,4,5],0,caller=self+'(etime) ') then return
   if badpar(gain,[4,5],0,caller=self+'(gain) ') then return
   if badpar(biasbase,[2,3,4,5],2,caller=self+'(biasbase) ') then return
   if badpar(biaslevel,[2,3,4,5],0,caller=self+'(biaslevel) ') then return
   if badpar(rdnoise,[2,3,4,5],0,caller=self+'(rdnoise) ') then return
   if badpar(seed,[0,1,2,3,4,5],[0,1],caller=self+'(seed) ') then return
   if badpar(oscsize,[0,2,3],0,caller=self+'(oscsize) ', default=0) then return
   if badpar(fn,[0,7],0,caller=self+'(fitsfn) ', default='') then return
   if badpar(crsnum,[0,2,3,4,5],0,caller=self+'(crsnum) ' ) then return
   if badpar(crsmax,[0,2,3,4,5],0,caller=self+'(crsmax) ' ) then return
   if badpar(crsmin,[0,2,3,4,5],0,caller=self+'(crsmin) ') then return
   if badpar(crsout,[0,2,3,4,5],0,caller=self+'(crsout) ', default=0.0) then return

   sz = size(biasbase)
   nx = sz[1]
   ny  =sz[2]

   dsz = size(darkbase)
   if sz[1] ne nx or sz[2] ne ny then begin
      printf,self,' ERROR! dark and bias base images have differing dimensions.'
      return
   endif

   ; start with a biased (null) image and tack on overscan
   synbias,biasbase,biaslevel,gain,rdnoise,seed,darkframe,OVERSCAN=oscsize, $
           CRSNUM=crsnum,CRSMAX=crsmax,CRSMIN=crsmin,CRSOUT=crsout

   ;  add the dark value to the business section of the image. Each dark
   ;  pixel is a Poisson deviant with mean equal to the dark base rate
   ;  multiplied by the exposure time to yield
   ;  effective photons (ie, electrons), then scaled back to D/N by the gain.
   justdark = poidev(darkbase*gain*etime,SEED=seed)/gain 
   darkframe[0:nx-1,*] += fix( (justdark<32767.0) + 0.5 )

   if fn ne '' then begin
      mkhdr, hdr,darkframe
      sxaddpar,hdr,'RDNOISE',rdnoise,' Read-noise in image in electrons'
      sxaddpar,hdr,'BIASVAL',biaslevel,' bias level in counts'
      sxaddpar,hdr,'GAIN',gain,' Gain in e-/ADU'
      sxaddpar,hdr,'EXPTIME',etime
      sxaddpar,hdr,'FILPOS',0
      writefits,fn,darkframe,hdr
   endif

end

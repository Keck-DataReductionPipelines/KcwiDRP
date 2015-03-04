;+
; NAME:
;   synbias
; PURPOSE:
;   Create a synthetic CCD bias frame with optional overscan.
; DESCRIPTION:
;   Adds a bias level (overscan level) and simulated read noise to an
;   arbitrary base image- an optional overscan area contains bias and
;   read noise only. The sigma is specified through the read noise and gain, 
;   and is normally distributed wih a mean of 0. The output is an
;   integer array.
; CATEGORY:
;   CCD data processing
; CALLING SEQUENCE:
;   synbias,biasbase,biaslevel,gain,rdnoise,seed,biasframe
; INPUTS:
;   biasbase = 2-D input array.  This is conceptually a constant, normally
;              floating point, with each pixel in units of D/N [counts].
;              It represent the bias structure introduced by the electronics.
;   biaslevel  integer or float, bias (overscan) level, in units of D/N [counts]
;   gain       hypothetical gain [e-/DN] for statistics.
;   rdnoise    integer or float, [e-].  Sigma for bias is rdnoise/gain
;   seed       for random number generator.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;   OVERSCAN - integer, number of columns for the explicit overscan area.
;              if less than or equal to 0, there is no overscan area generated
;              and the dimensions of the output array match biasbase.
;   FITSFN  -  Fits file for the output dark frame. The default is to generate 
;              no file.
;   CRSNUM  -  average number of cosmic ray strikes per ccd area of image.
;   CRSMAX  -  maximum D/N for cosmic ray strikes that will be added to the 
;              output. Intensity of each crs is drawn uniformly over the
;              range [CRSMIN,CRSMAX].
;   CRSMIN  -  minimum D/N for cosmic ray strikes that will be added to the 
;              output.
;
; KEYWORD OUTPUT PARAMETERS:
;   CRSOUT  -  Output, number of cosmic rays added to the image.
;
; OUTPUTS:
;   biasframe - 2-D array that is the synthetic bias image. It is dimensioned
;               n x (m + o) where  n, m are the dimensions of biasbase and
;               o is the number of overscan columns created. The overscan 
;               area of the bias is biasframe[0:n-1, *] This is an array
;               of ints and the units are D/N [counts]. Pixel values are
;               calculated in floating point and rounded to nearest integer.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   2006 June 28, Written by Peter L. Collins, Lowell Observatory  
;   2006/7/10,     PLC  add COSMIC keyword
;   2006/7/12,     PLC  CRS keywords- other documentation and argument cleanups.
;   2006/07/13,    PLC, add keywords CRSOUT and STARSOUT.
;   2006/07/14, MWB, final cleanup and addition to library.
;-

pro synbias,biasbase,biaslevel,gain,rdnoise,seed,biasframe,overscan=oscsize, $
            CRSNUM=crsnum,CRSMIN=crsmin,CRSMAX=crsmax,CRSOUT=crsout,FITSFN=fn

   self='SYNBIAS: '
   if badpar(biasbase,[2,3,4,5],2,caller=self+'(biasbase) ') then return
   if badpar(biaslevel,[2,3,4,5],0,caller=self+'(biaslevel) ') then return
   if badpar(gain,[4,5],0,caller=self+'(gain) ') then return
   if badpar(rdnoise,[2,3,4,5],0,caller=self+'(rdnoise) ') then return
   if badpar(seed,[0,1,2,3,4,5],[0,1],caller=self+'(seed) ') then return
   if badpar(oscsize,[0,2,3],0,caller=self+'(OSCSIZE) ', $
             default=0) then return
   if badpar(crsnum,[0,2,3,4,5],0,caller=self+'(CRSNUM) ',  $
             default=10.0) then return
   if badpar(crsmax,[0,2,3,4,5],0,caller=self+'(CRSMAX) ',  $
             default=2000.0) then return
   if badpar(crsmin,[0,2,3,4,5],0,caller=self+'(CRSMIN) ',  $
             default=1000.0) then return
   if badpar(fn,[0,7],0,caller=self+'(FITSFN) ', default='') then return

   ; nx,ny for final image dimensions
   sz = size(biasbase)
   nx = sz[1]
   ny  =sz[2]

   oscsize=oscsize > 0
   sigma = rdnoise/gain

   biasframe = randomn(seed,(nx+oscsize),ny) * sigma + biaslevel

   ; add the base image to the array.
   biasframe[0:nx-1,*] = biasframe[0:nx-1,*] + biasbase
   biasframe = fix( (biasframe<32767.) + 0.5 )

   ; cosmic rays anyone?
   if crsnum gt 0.0 then $
      addcrs,crsnum,crsmin,crsmax,seed,biasframe,OVERSCAN=oscsize,CRSOUT=crsout

   if fn ne '' then begin
      mkhdr, hdr,biasframe
      sxaddpar,hdr,'RDNOISE',rdnoise,' Read-noise in image in electrons'
      sxaddpar,hdr,'BIASVAL',biaslevel,' bias level in counts'
      sxaddpar,hdr,'GAIN',gain,' Gain in e-/ADU'
      sxaddpar,hdr,'EXPTIME',0.
      sxaddpar,hdr,'FILPOS',0
      writefits,fn,biasframe,hdr
   endif

end

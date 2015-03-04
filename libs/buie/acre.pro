;+
; NAME: 
;  acre
; PURPOSE: 
;  Automatic Cosmic Ray Extraction
; DESCRIPTION:
;  This program will attempt to identify and remove Cosmic Ray strikes from
;    an image.  This program was developed and tested on HST PC data prior
;    to the refurbishment mission.  It may work for other types of data but
;    it is as of now untested elsewhere.
;  The simplest usage is the single pass mode where the same parameters are
;    used for the entire image.  First, the image is smoothed with a median
;    filter using a box filter size given by width.  This smoothed image is
;    then subtracted from the original image.  A robust mean of a portion of
;    the image is calculated and subtracted from the image though this mean
;    should be near zero.  Any pixel see to deviate by THRESH standard
;    deviations from this average is marked for removal and replaced by its
;    corresponding value in the smoothed image.
;  This initial step works very well on the sky.  I've found that THRESH=3
;    WIDTH=7 work pretty well on all but the largest CRS's.  Using a value
;    for width less than 7 seems to leave residual "rings" of hot pixels from
;    around the edges of a strike.
;  The draw back to these parameters is that it is much too agressive in and
;    around actual objects in the frame.  The cores of the PSF will be removed
;    and numerous pixels will be tagged in the wings of the PSF.
;  To get around this problem, use the EXCLUDE keyword.  This is a 5xN array
;    containing circular regions to scan with different parameters.  The array
;    holds N such regions.  For each region you must specify the following:
;       (0,n) - x location of region
;       (1,n) - y location of region
;       (2,n) - radius of region
;       (3,n) - sigma threshold to use in this region
;       (4,n) - width of median smoothing, (no smoothing if set to 0).
;    The procedure used is to first restore all the pixels in the region to
;    their original values (in case they were changed in the first step).
;    If the width is set to zero, nothing more is done.  If the width is a
;    meaningful value, then the original image is smoothed with that width
;    and the region is scanned for deviant pixels again and replaces any
;    found.
;  The effects of each region are cumulative on the image and done in order
;    they appear in the array.  So any final steps of restoring small locations
;    should be done last.  Also, the step of smoothing the image is very cpu
;    intensive.  It will run much faster if you can group all regions with
;    similar smoothing values together.
;  In practice, a smaller width (~3) and higher thresh (~4) seems to work in
;    the wings of the PSF, but it will still take out the core.  So, you
;    need to specify two zones, one for cleaning that is nearly the size
;    of the outer fringes of the PSF and one for pretecting the image that
;    is smaller and centered on the core.  If you happen to get a strike
;    near the core of the PSF, this routine won't help and you're probably
;    screwed anyway.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  pro acre,dirty_im,clean_im,thresh,width, $
;              BLFINAL=blfinal,BLMASK=blmask, $
;              EXCLUDE=exclude,MASK=mask,VERBOSE=verbose
; INPUTS:
;  dirty_im - Original input image to be cleaned.
;  thresh   - Deviation threshold, in sigma, from background to cause
;                pixel to be fixed.
;  width    - Median smoothing width to get local background reference.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BLFINAL  - Flag, if true, brings up ITOOL to blink between the original
;                and cleaned images.
;  BLMASK   - Flag, if true, brings up ITOOL to blink between the original
;                and the mask showing pixels that are being replaced.
;  EXCLUDE  - Array that controls special extraction behavior in select
;                regions of the image.  See DESCRIPTION for details.
;  VERBOSE  - Flag, if true, generates a wordy output of progress and
;                action as routine progresses.
; OUTPUTS:
;  clean_im - Final cleaned up image.
; KEYWORD OUTPUT PARAMETERS:
;  MASK     - Return of the mask image.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/04/05 - Written by Marc W. Buie, Lowell Observatory
;-
pro acre,dirty_im,clean_im,thresh,width, $
         BLFINAL=blfinal,BLMASK=blmask, $
         EXCLUDE=exclude,MASK=mask,VERBOSE=verbose

   if badpar(dirty_im,[1,2,3,4,5],  2,CALLER='acre (dirty_im)',dimen=dirtdim) then return
   if badpar(thresh,  [1,2,3,4,5],  0,CALLER='acre (thresh)')                 then return
   if badpar(width,   [1,2,3,4,5],  0,CALLER='acre (width)')                  then return
   if badpar(exclude, [0,1,2,3,4,5],2,CALLER='acre (EXCLUDE)', dimen=exdim  ) then return

   if keyword_set(exclude) then begin
      if exdim[0] ne 5 then begin
         help,exclude
         print,'ACRE: Exclude information array must be 5 by N.'
         return
      endif
   endif

   ; First, use median smoothing on the entire image with the width specified
   ;  by the user.  This image will track the background of the image with a
   ;  curvature commensurate with the smoothing width.

   if keyword_set(verbose) then $
      print,'Median smoothing with a width of ',strtrim(string(width),2)

   smoo = median(dirty_im,width)

   if keyword_set(verbose) then $
      print,'  Subtract off smoothed image and find mean and standard deviation.'

   diff = dirty_im-smoo
   sample=diff[where(diff[0:min([99999,n_elements(diff)-1])] ne 0.)]
   robomean,sample,5.0,0.5,avg,avgdev,stddev,var,skew,kurt,nfinal,new

   if keyword_set(verbose) then $
      print,'  Flattened background: ',avg,' +/- ',stddev,format='(a,f6.3,a,f6.3)'

   z=where(abs(diff-avg) gt thresh*stddev,count)

   if keyword_set(verbose) then $
      print,'  Replacing ',strtrim(string(count),2),' pixels in full image'

   clean_im=dirty_im
   clean_im[z]=smoo[z]
   mask = dirty_im*0
   mask[z] = 1

   if keyword_set(exclude) then begin

      if keyword_set(verbose) then $
         print,'Setting up for exclusion regions.'

      xarr = indgen(dirtdim[0])#replicate(1,dirtdim[1])
      yarr = replicate(1,dirtdim[0])#indgen(dirtdim[1])
      lastwid = -1

      for i=0,exdim[1]-1 do begin

         if keyword_set(verbose) then $
            print,'Blanking mask at ',strcompress(exclude[0:2,i])

         rsq = (xarr-exclude[0,i])^2 + (yarr-exclude[1,i])^2
         z1 = where(rsq le exclude[2,i]^2,count1)
         if count1 ne 0 then mask[z1] = 0
         clean_im[z1]=dirty_im[z1]

         if keyword_set(verbose) then $
            print,'  Reset ',strtrim(string(count1),2),' pixels in mask back to 0 (off).'

         if exclude[4,i] ge 1 then begin

            if keyword_set(verbose) then $
               print,'  New extraction threshold and width, ',strcompress(exclude[3:4,i])

            if exclude[4,i] ne lastwid then begin

               if keyword_set(verbose) then $
                  print,'  Median smoothing with a width of ',strtrim(string(exclude[4,i]),2)

               smoo = median(dirty_im,exclude[4,i])

               if keyword_set(verbose) then $
                  print,'  Subtract off smoothed image and find mean and standard deviation.'

               diff = dirty_im-smoo
               sample=diff[where(diff[0:min([99999,n_elements(diff)-1])] ne 0.)]
               robomean,sample,5.0,0.5,avg,avgdev,stddev,var,skew,kurt,nfinal,new

               if keyword_set(verbose) then $
                  print,'  Flattened background: ',avg,' +/- ',stddev,format='(a,f6.3,a,f6.3)'

            endif

            z2=where(abs(diff[z1]-avg) gt exclude[3,i]*stddev,count2)

            if keyword_set(verbose) then $
               print,'  Replacing ',strtrim(string(count2),2),' pixels in full image'

            if count2 ne 0 then begin
               clean_im[z1[z2]]=smoo[z1[z2]]
               mask[z1[z2]] = 1
               lastwid=exclude[4,i]
            endif

         endif

      endfor

   endif

   if keyword_set(blfinal) then itool,[[[dirty_im]],[[clean_im]]]
   if keyword_set(blmask)  then itool,[[[dirty_im]],[[mask]]]

end

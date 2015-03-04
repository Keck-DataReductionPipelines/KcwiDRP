;+
; NAME:
;   clean
; PURPOSE: 
;   Remove a PSF from an image via the ``clean'' algorithm.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;   clean,image,psf,xloc,yloc,maxdist,iters,new_image,resid $
;       DISPLAY=display,VERBOSE=verbose
; INPUTS:
;   image     - Original source image to be cleaned.
;   psf       - PSF image at same sampling resolution as image.
;   xloc      - X location of "object"
;   yloc      - Y location of "object"
;   maxdist   - Maximum distance from xyloc to look for local max.
;   iters     - Number of cleaning iterations to perform.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;   CROSS     - If set to a number greater than or equal to 0, the peak at
;               each iteration is found by using a cross-correlation
;               of the psf against the image rather than just using raw DN.
;               If 0, then the full size of the psf is used.  If 1 or greater,
;               a sub-region from the center that is 2*CROSS+1 pixels square
;               is used as the convolution kernal.
;   DISPLAY   - Display intermediate results, if 0, this is suppressed,
;                 if non-zero, this is the interval for the display, that is,
;                 DISPLAY=10 would cause a display every 10th iteration.
;   GAIN      - "Gain" of the clean process, the default value is 0.05 and
;                 is the scaled amount of the psf removed at each step.
;   VERBOSE   - Verbose printout of intermediate steps to the screen.  Just
;                 like display, VERBOSE=0 suppresses output, VERBOSE=n will
;                 print information every nth iteration.
; OUTPUTS:
;   new_image - Clean-ed image result.
;   resid     - Remains of the original image after clean-ed image is removed.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   1/26/94, written by Marc W. Buie, Lowell Observatory, algorithmic insight
;              graciously provided by Tod Lauer (NOAO, Tucson).
;   2/25/94, MWB, added GAIN and CROSS keywords.
;   5/19/94, MWB, changed psf normalization from peak to total.  Should
;              now be strictly flux conserving.
;   5/21/94, MWB, Changed CROSS to allow for only a portion of psf for
;              convolution.
;-
pro clean,image,psf,xloc,yloc,maxdist,iters,new_image,resid, $
       CROSS=cross,DISPLAY=display,GAIN=gain,VERBOSE=verbose

   self='CLEAN: '
   if badpar(image,  [1,2,3,4,5],2,caller=self+'(image) ') then return
   if badpar(psf,    [1,2,3,4,5],2,caller=self+'(psf) ') then return
   if badpar(xloc,   [1,2,3],    0,caller=self+'(xloc) ') then return
   if badpar(yloc,   [1,2,3],    0,caller=self+'(yloc) ') then return
   if badpar(maxdist,[1,2,3],    0,caller=self+'(maxdist) ') then return
   if badpar(iters,  [1,2,3],    0,caller=self+'(iters) ') then return
   if badpar(display,[0,1,2,3],  0,caller=self+'(display) ',default=0) then return
   if badpar(verbose,[0,1,2,3],  0,caller=self+'(verbose) ',default=0) then return
   if badpar(gain,   [0,4,5],    0,caller=self+'(GAIN) ',default=0.05) then return
   if badpar(cross,  [0,1,2,3],  0,caller=self+'(CROSS) ',default=-1)  then return

   ; Set up information on the input image and make a working copy.
   work = image
   s=size(work)
   imw = s[1]
   imh = s[2]

   ; Set up the new image
   new_image = fltarr(imw,imh)

   ; Set up information on the psf array.
   npsf = psf / max(psf)
   psfnorm = total(npsf)
   s=size(psf)
   psfw = s[1]
   psfh = s[2]
   boxm,npsf,psfw/2,psfh/2,psfw/2,psfh/2,psfx,psfy

   if cross eq 0 then begin
      kernal = psf
      if (verbose gt 0) or (psfx ne psfw-psfx) or (psfy ne psfh-psfy) then begin
         print,'Complete psf used for cross correlation.'
         if psfx ne psfw-psfx then $
            print,'Warning: Cross correlation not symmetric in X'
         if psfy ne psfh-psfy then $
            print,'Warning: Cross correlation not symmetric in Y'
      endif
   endif else if cross gt 0 then begin
      dw = min([cross,psfx,psfy,psfw-psfx,psfh-psfy])
      x1 = psfx - dw
      x2 = psfx + dw
      y1 = psfy - dw
      y2 = psfy + dw
      kernal = psf[x1:x2,y1:y2]
      if (verbose gt 0) or (dw ne cross) then begin
         print,'PSF core used for cross correlation.'
         if dw ne cross then $
            print,'Warning, cross too big, reduced from ',cross,' to ',dw $
         else $
            print,'Half-width of core used is ',dw
      endif
   endif

   for i=0L,iters do begin

      if cross gt 0 then begin
         workc = convol(work,kernal)
         boxm,workc,xloc,yloc,maxdist,maxdist,xpos,ypos,/ABSMAX
      endif else begin
         boxm,work,xloc,yloc,maxdist,maxdist,xpos,ypos,/ABSMAX
      endelse

      peak = work[xpos,ypos]

      scale_fac = peak * gain

      ; Increment the output image.
      new_image[xpos,ypos] = new_image[xpos,ypos] + scale_fac*psfnorm

      ; Determine the sub-array for subtraction
      il = max([0,xpos-psfx])
      pl = max([0,psfx-xpos])
      ir = min([imw-1,xpos+(psfw-1-psfx)])
      pr = min([psfw-1,psfx+(imw-1-xpos)])
      ib = max([0,ypos-psfy])
      pb = max([0,psfy-ypos])
      it = min([imh-1,ypos+(psfh-1-psfy)])
      pt = min([psfh-1,psfy+(imh-1-ypos)])

      ; Subtract 5% at the max location
      work[il:ir,ib:it] = work[il:ir,ib:it] - npsf[pl:pr,pb:pt]*scale_fac

      if keyword_set(verbose) then begin
         if i mod verbose eq 0 then begin
            print,'#',i,' x,y ',xpos,ypos,' sf ',scale_fac,' (',il,':',ir,',', $
                  ib,':',it,') (',pl,':',pr,',',pb,',',pt,')', $
                  peak,work[xpos,ypos], $
                  format='(a,i6.6,a,i3,1x,i3,a,f6.1,a,i3.3,a,i3.3,a,' + $
                        'i3.3,a,i3.3,a,i3.3,a,i3.3,a,i3.3,a,i3.3,a,1x,f6.1,1x,f6.1)'
         endif
      endif

      ; Optional display
      if display ne 0 then begin
         if i mod display eq 0 then begin
            mn = min(work[il:ir,ib:it])
            mx = max(work[il:ir,ib:it])

            sz=20 ; 35 ; 20
            zf= 6 ;  4 ; 6
            new_wid = (2*sz+1)*zf

            tmp1 = work[xloc-sz:xloc+sz,yloc-sz:yloc+sz]
            tmp2 = new_image[xloc-sz:xloc+sz,yloc-sz:yloc+sz]

            if !d.x_size lt new_wid or !d.y_size lt new_wid then $
               setwin,0,xsize=new_wid*2,ysize=new_wid

            ypos  = (!d.y_size - new_wid)/2
            xpos1 = (!d.x_size)/2 - new_wid
            xpos2 = (!d.x_size)/2

            tvscl,rebin(tmp1,new_wid,new_wid,/sample),xpos1,ypos
            tvscl,rebin(tmp2,new_wid,new_wid,/sample),xpos2,ypos
         endif
      endif

   endfor

   resid = work

end

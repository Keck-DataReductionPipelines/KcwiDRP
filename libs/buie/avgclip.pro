;+
; NAME:
;   avgclip
; PURPOSE:
;   Average over a 3-D array, clipping unusual deviants.
; DESCRIPTION:
;   Calculate the average value of an array, or calculate the average
;   value over one dimension of an array as a function of all the other
;   dimensions.
; CATEGORY:
;   CCD data processing
; CALLING SEQUENCE:
;   avgclip,array,average
; INPUTS:
;   array = 3-D input array.  May be any type except string.  This cube of
;              data is MODIFIED by this routine.  What's left on return may
;              or may not be useful.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;   BAD       - Bad flag cube that matches the input 3-D array.  0 means the
;                  value is not known to be bad.  Anything else and the pixel
;                  should not be used in the average.  If not provided, the
;                  bad flag array is initially set to all good.  This cube
;                  is modified to add any additional pixels that were flagged
;                  in the process of creating the final image.
;   CLEANMEAN - array (or name of fits file) that should be used to filter
;                  outliers.  Normally, this image is computed from the median
;                  of the image stack.  But, if you happen to have better
;                  information provide it via this keyword.  The image size
;                  must be consistent with the cropping region.  If the images
;                  are to be scaled the mean of this image is not used in any
;                  way so you are free to provide a normalized image if you
;                  wish.  This feature is typically used when combining
;                  flat field images.
;
;   CLEANSIG - array (or name of fits file) that should be used as the sigma
;                  image array.  By default, this is computed from a clipped
;                  sqrt(cleanmean) which is normally the median of the stack.
;                  If you provide this array you must also provide CLEANMEAN
;                  or this keyword will be ignored.  Note that the units of
;                  CLEANSIG and CLEANMEAN should match.  In other words, if
;                  you do provide a normalized clean mean, then CLEANSIG
;                  should bear the stamp of that normalization as well.  This
;                  feature is typically used when combining flat field images.
;
;   JUSTMEDIAN - Flag, if set will stop processing after the initial median
;                  combination of the image cube and this will be the final
;                  answer.  However, this is only honored if the NORMALIZE and
;                  PRESCALE flags are both set.  This should only be used if
;                  the intrinsic noise level of the result can never drop
;                  below 1 DN.
;
;   SCALE - 4 element vector which, if provide, defines the region of the
;           array dimensions that are used to scale the mean
;           of the arrays before combining.  If combined in this
;           manner, the arrays are combined weighted by the means.
;                 [x1,x2,y1,y2]
;           If the region contains more than 20000 pixels, then only this
;           many randomly chosen pixels will be used to derive the robust
;           mean of the scaling region.
;
;   NORMALIZE - Flag, if set and SCALE used, leaves the output average
;                 normalized by the SCALE region.
;
;   SILENT - Flat, if set will suppress all messages to screen.
;
;   THRESH - Threshold, in units of a standard deviation, to flag and thus
;               remove outliers.  The default is 3.0 sigma.
;
;   NOISEMIN - Noise floor to use when combining images.  Typically this would
;                be set to the readout noise so that data with unusually low
;                noise will be treated more realistically. (default=1)
;
;   NONEWBADFLAGS - Flag, if set will suppress searching for new bad pixels
;                in the stack of images.  If you are providing a bad mask
;                then it will be used as is.  If you are not providing a bad
;                mask this will cause avgclip to return a straight average
;                of the stack.
;
; OUTPUTS:
;   average - 2-D array that is the robust average of the stack.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   1992 Dec 30, Marc W. Buie, Lowell Observatory, cloned from AVG
;      and added average sigma clipping.
;   95/03/10, MWB, extensive re-write to optimize.
;   97/06/19, MWB, added SILENT keyword
;   2000/09/22, MWB, added THRESH keyword
;   2006/03/18, MWB, fixed problem with bias frame averaging when using
;                      overscan correction.
;   2006/04/04, MWB, fixed problem with flat averaging caused by bias fix
;
;   2006/06/29 Peter L. Collins, Lowell Observatory experiments on noise array
;   2006/06/30 PLC, added NOISEMETHOD and READNOISE keywords.
;   2006/07/14, MWB, final checkout and reincorporation into library.
;   2006/07/25, MWB, slight change to the normalization scaling process.  Also
;                  added CLEANMEAN and CLEANSIG keywords.
;   2006/07/27, MWB, added JUSTMEDIAN keyword
;   2007/08/08, MWB, added BAD keyword
;   2007/09/04, MWB, fixed problem with CLEANMEAN and CLEANSIG
;   2007/09/07, MWB, fixed more errors with handling of incoming bad arrays
;                      also added the NONEWBADFLAGS keyword and pass BAD
;                      through to medarr_mwb
;-

pro avgclip,arr,average,SCALE=scale,NORMALIZE=normalize,SILENT=silent, $
       THRESH=thresh,NOISEMIN=noisemin,CLEANMEAN=cleanmean,CLEANSIG=cleansig, $
       VIEWIT=viewit,JUSTMEDIAN=justmedian,BAD=bad,NONEWBADFLAGS=nonewbadflags

   ;on_error,2
   self='AVGCLIP: '

   ;Verify correct number of parameters.
   if n_params() ne 2 then begin
      print,'avgclip,arr,result,[SCALE=region],[/NORMALIZE]'
      return
   endif

   if badpar(arr,[2,3,4,5,12],3,caller=self+'(arr) ',dimen=s) then return

   nx   = s[0]
   ny   = s[1]
   nimg = s[2]
   if nimg le 2 then begin
      print,self,'Error *** There must be at least 3 images in cube.'
      return
   endif

   if badpar(bad,[0,1,2,3],3,caller=self+'(BAD) ', $
                    default=bytarr(nx,ny,nimg),dimen=sb) then return
   if sb[0] ne nx or sb[1] ne ny or sb[2] ne nimg then begin
      print,self,'Dimensions of BAD array must match input cube.'
      return
   endif

   if badpar(thresh,[0,2,3,4,5],0,caller=self+'(THRESH) ', $
                    default=3.0) then return

   if badpar(normalize,[0,1,2,3],0,caller=self+'(NORMALIZE) ', $
                    default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                    default=0) then return
   if badpar(viewit,[0,1,2,3],0,caller=self+'(VIEWIT) ', $
                    default=0) then return
   if badpar(justmedian,[0,1,2,3],0,caller=self+'(JUSTMEDIAN) ', $
                    default=0) then return
   if badpar(noisemin,[0,1,2,3,4,5],0,caller=self+'(NOISEMIN) ', $
                    default=1.0) then return
   if badpar(nonewbadflags,[0,1,2,3],0,caller=self+'(NONEWBADFLAGS) ', $
                    default=0) then return

   if noisemin lt 0.0 then begin 
      print, self + ' READNOISE (', noisemin, ') invalid, must be ge 0.0'
      return
   endif

   ;Verify the scaling region, if passed.
   if keyword_set(scale) then begin
      prescale = 1
      if n_elements(scale) ne 4 then begin
         print,self,' Error *** scaling region must be a four element vector'
         return
      endif
      x1 = scale[0]
      x2 = scale[1]
      y1 = scale[2]
      y2 = scale[3]

      if scale[0] lt 0  then message,'Start of X region is less than zero.'
      if scale[0] ge nx then message,'Start of X region greater than img size.'
      if scale[1] lt 0  then message,'End of X region is less than zero.'
      if scale[1] ge nx then message,'End of X region greater than img size.'
      if scale[2] lt 0  then message,'Start of Y region is less than zero.'
      if scale[2] ge ny then message,'Start of Y region greater than img size.'
      if scale[3] lt 0  then message,'End of Y region is less than zero.'
      if scale[3] ge ny then message,'End of Y region greater than img size.'
      if scale[0] gt scale[1] then message,'Start of X region greater than end.'
      if scale[2] gt scale[3] then message,'Start of Y region greater than end.'
   endif else begin
      prescale = 0
   endelse

   ; Take care of the cleanmean and cleansig keywords
   if badpar(cleanmean,[0,2,3,4,5,12],2,caller=self+'(CLEANMEAN) ', $
                default=0,dimen=dim1) then return
   if badpar(cleansig,[0,2,3,4,5,12],2,caller=self+'(CLEANSIG) ', $
                default=0,dimen=dim2) then return
   if n_elements(cleanmean) gt 1 then externmean=1 else externmean=0
   if n_elements(cleansig)  gt 1 then externsig=1 else externsig=0
   if externmean then begin
      if dim1[0] ne nx then begin
         print,self,' x size of CLEANMEAN does not match crop region.'
         return
      endif
      if dim1[1] ne ny then begin
         print,self,' y size of CLEANMEAN does not match crop region.'
         return
      endif
      if externsig then begin
         if dim2[0] ne nx then begin
            print,self,' x size of CLEANSIG does not match crop region.'
            return
         endif
         if dim2[1] ne ny then begin
            print,self,' y size of CLEANSIG does not match crop region.'
            return
         endif
      endif
      if not prescale and normalize then begin
         skysclim,cleanmean,lowval,hival,meanval,sigma,npts=20000
         cleanmean /= meanval
         if externsig then cleansig /= meanval
      endif else if prescale then begin
         skysclim,cleanmean[x1:x2,y1:y2],lowval,hival,meanval,sigma,npts=20000
         if externsig then cleansig /= meanval
      endif
   endif else begin
      externsig=0
   endelse

   ; pre-allocate the result array, no need to pre-init array
   average = fltarr( nx, ny, /nozero)

   ;Do scaled robust averaging, if requested
   if prescale then begin
      means = fltarr( nimg )
      for i=0,nimg-1 do begin
         skysclim,arr[x1:x2,y1:y2,i],lowval,hival,meanval,sigma,npts=20000
         means[i] = meanval
         arr[*,*,i]=arr[*,*,i]/means[i]
         if not silent then print,'Frame ',i,'  scaled by ',means[i]
      endfor
      normfac = median(means)
   endif

   ; Generate a median average of stack, this is the first pass.
   if externmean then begin
      if not silent then print,systime(0),' use external clean image'
      avg = cleanmean
   endif else begin
      if not silent then print,systime(0),' compute median average of stack'
      medarr_mwb,arr,avg,bad=bad
; not sure why this was so restrictive
;      if prescale and normalize and justmedian then begin
      if justmedian then begin
         if not silent then print,self,'  returning just the median of the stack'
         average=avg
         return
      endif
   endelse

   if externsig then begin
      sigma = cleansig
   endif else begin
      ; Generate an approximate noise array, this is NOT a rigorous value
      ;   but is mean to be close.  Start with the square root of the signal.
      ;   In cases of real signal to be measured (ie., flats) this will be
      ;   close and will follow the contour of the flat.  If this is a dark
      ;   or especially a bias frame where there really isn't a signal, make
      ;   sure the sigma value doesn't go below the intrisic noise in the
      ;   array
      if prescale then begin
         sigma=sqrt(avg*normfac) > noisemin
;         sigma /= normfac
      endif else begin
         sigma=sqrt(avg) > noisemin
      endelse
      z=where(finite(sigma) eq 0,badcount)
      if badcount ne 0 then begin
         sigma[z]=noisemin
         if not silent then print, badcount, ' negative values found in avg '
      endif
   endelse

   if not silent then $
      print,systime(0),' create residual image, ', $
         ' sigma clipping threshold=', $
         strn(thresh,format='(f10.1)')

   asum = fltarr(nx,ny)
   acnt = fltarr(nx,ny)
   npts = long(nx) * long(ny)
   npixclipped = 0
   for i=0,nimg-1 do begin
      new = arr[*,*,i]
      if not nonewbadflags then begin
         tmpbad = bad[*,*,i]
         if prescale then begin
            resid = (new-avg)/(sigma/means[i])
         endif else begin
            resid = (new-avg)/sigma
         endelse
         z = where(abs(resid) ge thresh and tmpbad eq 0B,count)
         if count ne 0 then begin
            tmpbad[z] = 1B
            bad[*,*,i] = tmpbad
         endif
      endif
      z = where(bad[*,*,i] eq 0B,count)
      if count ne 0 then begin
         asum[z] = asum[z]+new[z]
         acnt[z] = acnt[z] + 1.0
      endif else begin
         print,self,' Serious Warning!  all pixels in image ',strn(i), $
                    ' were considered bad.'
      endelse
      npixclipped = npixclipped + (npts - count)
      if viewit then begin
         if count eq 0 then begin
            print,self,'  This image was considered all bad (',strn(i),').'
            itool,new,/block
         endif else begin
            print,self,'  Showing image ',strn(i)
            z = where(bad[*,*,i] ne 0B,count)
            if count eq 0 then begin
               print,self,'  This image was considered all good (',strn(i),').'
               itool,new,/block
            endif else begin
               print,self,'  Image ',strn(i),' has ',strn(count), $
                          ' pixels marked as bad (set to 1000 below min).'
               new[z] = min(new)-1000.0
               itool,new,/block
            endelse
         endelse
      endif
   endfor

   if not silent and npixclipped gt 0 then $
      print,'        ',strn(npixclipped),' total pixels were clipped from ', $
                             strn(nimg),' original images'
   ; safety check, for good data you should not get this warning
   z = where(acnt eq 0,count)
   if count ne 0 then $
      print,self,'warning, ',strn(count),' pixels had nothing survive.'

   if not silent then print,systime(0),' create final robust average image'
   ; generate average array
   z = where(acnt ne 0,count)
   if count ne 0 then average[z]= asum[z]/acnt[z]

   if viewit then begin
      print,self,' Viewing the final summation image.'
      itool,asum,/block
      print,self,' Viewing the final summation count image.'
      itool,acnt,/block
      print,self,' Viewing the final average image.'
      itool,average,/block
   endif

   if prescale and not normalize then average = average*normfac

end

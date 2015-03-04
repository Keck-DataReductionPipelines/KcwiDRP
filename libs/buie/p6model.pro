;+
; NAME:
;    p6model
; PURPOSE: (one line)
;    Generate synthetic PSF images for the HST WFPC Planetary Camera, Chip 6.
; DESCRIPTION:
;    A generated PSF is added to the output array.  If the array is not
; defined, it is created with dimensions 800 by 800.  Successive calls
; will add objects (PSF's) to the array.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;    p6model, xsrc, ysrc, inten, date, filter, bmvnum, back, image
; INPUTS:
;    xsrc, ysrc : Position of the PSF in the output array.
;    inten      : Intensity of PSF relative to TinyTim output PSF
;    date       : Date of observation (YYMMDD format).
;    filter     : Filter name in the form Fxxx(W,LP,M,N).
;    bmvnum     : B-V list value (TinyTim V6.2 values):
;                1           O5      -0.34
;                2           O8F     -0.32
;                3           O6      -0.31
;                4           B1V     -0.27
;                5           B3V     -0.21
;                6           B6V     -0.12
;                7           A0V     -0.04
;                8           A5V      0.12
;                9           F6V      0.37
;               10           F8V      0.48
;               11           G2V      0.56
;               12           G5V      0.66
;               13           G8V      0.75
;               14           K4V      0.92
;               15           K7V      1.28
;               16           M1.5V    1.45
;               17           M3V      1.44

;    back    : Background to be added.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;    JITTER  : If set contains the gaussian smearing to apply to the image.
;                This is in units of 1/e half width.  If not set, no smearing
;                is applied.  This calculation is done in double precision
;                or float according to the type of image. The values passed
;                to the exp function ( e - (((x/j)^2 + y/j)^2/2)) are 
;                restricted to a maximum based on the computational precision
;                to avoid floating point underflow.
;    OBJRAD  : Radius of object in pixels.  If not set, assumed to be a point
;              source.  If set and radius is greater than 0.5, then PSF is
;              convolved with a circular function of this radius.  This models
;              the object as a Lambert disk, crude but useful for barely
;              resolved objects.
;    NEW     : If set, clears the output array to zeros before building the
;              PSF image(s).
;    HSTPATH : Alternate path for HST PSF's.
;    VERBOSE : If set, prints informational message.
; OUTPUTS:
;    image  : The image array into which the PSF is placed.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    The Tiny Tim programs are used, via the procedure HSTPSF, to generate
; a 2X (7.5 micron spacing) PSF at the nearest grid location to the requested
; position. (This grid is defined in hstpsf).
;    From the interpolated maximum returned by hstpsf, shift amounts for x
; and y are computed.
;    The PSF is shifted by these amounts and then compressed to a 1X
; (15 micron spacing) PSF.
;    Finally, the resulting PSF is multiplied by the intensity parameter and
; added into the output array.
;    Calls external procedures BADPAR, BOXM, HSTPSF, and SSHIFT2D.
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, December, 1993.
;    12/10/93, DWL, Modified to allow the first three parameters to be
; vectors.
;    12/16/93, DWL, Added VERBOSE keyword.
;    1/19/94, DWL, Added HSTPATH keyword.
;    1/21/94, DWL, Added x-maximum and y-maximum parameters to hstpsf call.
;    2003/1/15, DWL, Modified to call the updated routine HSTPSF, which
; uses TinyTim Version 6.1 Alpha. A PSF is returned from HSTPSF, only if
; the requested PSF file is found in disk cache.
;   2006/11/20, Peter L. Collins, Lowell Observatory, removed superflous
;                defaulting in badpar call for HSTPATH.
;   2007/07/24, PLC, add some changes made earlier to pc2model.
;   2007/07/25, PLC, propagate changes made to pc2model 2007/07/08 by Marc Buie.
;                    Z4 keyword left out for now (pending hstpsf test).
;-

pro p6model, xsrc, ysrc, inten, date, filter, bmvnum, back, image,$
             HSTPATH=hstpath,$
             JITTER=jitter_in,$
             NEW=new,$
             OBJRAD=objrad,$
             VERBOSE=verbose

   if n_params() lt 8 then begin
      print, 'p6model, x, y, inten, date, filter, bmvnum, back, image'
      return
   endif

   self = 'P6MODEL: '
   if badpar(xsrc,   [2,3,4,5], [0,1], caller=self+'(x) ', npts=n1) then return
   if badpar(ysrc,   [2,3,4,5], [0,1], caller=self+'(y) ', npts=n2) then return
   if badpar(inten, [2,3,4,5], [0,1], caller=self+'(inten) ', npts=n3 ) $
      then return

   ; Check arguments.
   if badpar(date,   7,       0,     caller=self+'(date) ') then return
   if badpar(filter, 7,       0,     caller=self+'(filter) ') then return
   if badpar(bmvnum, [2,3],   0,     caller=self+'(bmvnum) ') then return
   if badpar(back,   [2,3,4,5], 0,   caller=self+'(back) ') then return

   ; Check keyword parameters.
   if badpar(hstpath, [0,7], 0, caller=self+'(hstpath) ') then return
   if badpar(jitter_in,   [0,4,5], 0,   caller=self+'(JITTER) ', $
                 default=0.0) then return
   if badpar(new, [0,2], 0, caller=self+'(new) ', default=0) then return

   if badpar(objrad, [0,2,3,4,5], [0,1], caller=self+'(objrad) ',$
             npts=n4) then return

   if badpar(verbose, [0,2], 0, caller=self+'(verbose) ', default=0) then return


   ; Check that the first three parameters have equal length.
   t = [n1, n2, n3]
   w = where(max(t) ne min(t), count)

   if count ne 0 then begin
      msg=self + 'xsrc, ysrc, and intensity parameters must be of equal length.'
      print, msg
      return
   endif

   ; Verify valid OBJRAD if supplied
   if n_elements(objrad) ne 0 and n4 ne n1 then begin
      msg = self + 'OBJRAD must have the same length as the x and y parameters.'
      print, msg
      return
   endif

;if verbose then begin
;   print,xsrc, ysrc
;   print, inten
;   print, date, filter
;   print, 'back', back
;   print, objrad
;   print, jitter
;endif

   stat = size(image)
   imgtype = size(image,/TYPE)
   ndim = stat[0]

   if ndim ne 2 then begin
      ; The output array must be created.
      im_xsize = 800
      im_ysize = 800
      image = fltarr(im_xsize, im_ysize)
   endif else begin
      ; It exists.  Get its dimensions.
      im_xsize = stat[1]
      im_ysize = stat[2]
   endelse

   if new then image[0:im_xsize-1, 0:im_ysize-1]=0

   for j=0, n1-1 do begin
      if verbose then begin
         print, 'Working on object at ' + string(xsrc[j], ysrc[j], $
         format='(2F12.4)')
      endif

      sampfact=3
      psfsize=5
      hstpsf, xsrc[j], ysrc[j], date, filter, bmvnum, psf, xm, ym, $
              HSTPATH=hstpath, CAMERA=2, CHIP=6, $
              PSFSIZE=psfsize, SAMPFACT=sampfact, VERBOSE=verbose

      psfstat = size(psf)

      if psfstat[0] lt 2 then begin
         print, self + 'Procedure HSTPSF did not return a PSF array '
         return
      endif

      ; size of the sub-sampled PSF
      psf_xsizef = psfstat[1]
      psf_ysizef = psfstat[2]
      ; xm,ym tells us the location of the peak of the numerical PSF within the
      ;  sub-sampled pixel grid in the array "psf".  Now, compute where that
      ;  peak would fall if the PSF were to be downsampled to the output image
      ;  grid scale.
      xm0 = (xm - float(sampfact)/2.0 + 0.5)/float(sampfact)
      ym0 = (ym - float(sampfact)/2.0 + 0.5)/float(sampfact)


      ; This is the desired location for the new source, just copying variables
      ;   here to make the code look simpler later on.
      x = xsrc[j]
      y = ysrc[j]

      ; Some rudimentary error checking
      if (x lt 0) or (x ge im_xsize) then begin
         t = string(im_xsize, format='(G0.0)')
         print, self+'Error. X-position out of bounds. (0 <= X < ' + t + ').'
         continue
      endif
      if (y lt 0) or (y ge im_ysize) then begin
         t = string(im_ysize, format='(G0.0)')
         print, self, 'Error. Y-position out of bounds. (0 <= X < ' + t + ').'
         continue
      endif

      ; Compute the fractional shift needed (in the original pixel space)
      xf = (x - xm0)
      xf = xf - fix(xf)
      yf = (y - ym0)
      yf = yf - fix(yf)
      ; Create the shift vector, this shift is in the sub-sampled units.
      svec = [xf,yf]*sampfact

      ; Shift the psf.
      spsf = sshift2d(psf, svec)

      ; Convolve PSF with circular function if requested.
      if n_elements(objrad) ne 0 then begin
         ; Compute size of kernel.
         nk = ceil(objrad[j]/sampfact - 0.5) * 2 + 1

         ; If kernel is bigger than one pixel then do the convolution.
         if nk gt 1 then begin
            kc = nk / 2
            idx=indgen(nk)
            unit=replicate(1,nk)
            tmpx=idx#unit
            tmpy=unit#idx
            kernel=pixwt(kc,kc,objrad[j]/sampfact,tmpx,tmpy)
            kernel=kernel/total(kernel)
            spsf=convol(spsf,kernel)
         endif
      endif

      ; Rebin the psf.
      fpsf = rebin(spsf, psf_xsizef/sampfact, psf_ysizef/sampfact)
      psfstat  = size(fpsf)

      ; Size of the rebinned PSF (matches output image grid).
      psf_xsize = psfstat[1]
      psf_ysize = psfstat[2]

      ; compute the two matching image areas for the paste
      i0 = fix(x-xm0)
      j0 = fix(y-ym0)
      i1 = i0+psf_xsize-1
      j1 = j0+psf_ysize-1
      ip0 = 0
      ip1 = psf_xsize-1
      jp0 = 0
      jp1 = psf_ysize-1
      if i0 lt 0 then begin
         adjust = abs(i0)
         i0  += adjust
         ip0 += adjust
      endif
      if j0 lt 0 then begin
         adjust = abs(j0)
         j0  += adjust
         jp0 += adjust
      endif

      ; Add the appropriate sub-section of the PSF to the output array.
      image[i0:i1,j0:j1] += inten[j]*fpsf[ip0:ip1,jp0:jp1]

   endfor

   if jitter_in gt 0.0 then begin
      if imgtype eq 4 then begin
         x=[-2.0,-1.,0.,1.0,2.0]
         y=[-2.0,-1.,0.,1.0,2.0]
         i=[1.0,1.0,1.0,1.0,1.0]
         jitter= jitter_in
         maxexp = 32./alog10(exp(1.0))
      endif else begin
         x=[-2.0D0,-1.D0,0.D0,1.0D0,2.0D0]
         y=[-2.0D0,-1.D0,0.D0,1.0D0,2.0D0]
         i=[1.0D0,1.0D0,1.0D0,1.0D0,1.0D0]
         jitter = double(jitter_in)
         maxexp = 302./alog10(exp(1.0))
      endelse
         rsq = (x^2#i + i#y^2)/jitter^2
         maxexp=2.0*maxexp
         rsq = rsq < maxexp
         z = where ( rsq eq maxexp, count)
         if verbose then begin
            print, 'rsq: ', rsq
            if count ne 0 then print, 'rsq: ', count, ' values truncated.'
         endif
      kernel = exp(-rsq/2.)
      kernel=kernel/total(kernel)
      if verbose then help,kernel
      if verbose then print,kernel
      image=convol(image, kernel)
   endif

   image = image + back
end

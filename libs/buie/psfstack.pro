;+
; NAME:
;  psfstack
; PURPOSE:
;  Generate an average numerical psf by stacking observed images.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
;
; CALLING SEQUENCE:
;  psfstack,image,x,y,psf
;
; INPUTS:
;  image - Input image from which psf is collected.
;  x     - Scalar or vector list of positions of stars (x coordinate).
;  y     - Scalar or vector list of positions of stars (y coordinate).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  DW    - half-width of box centered around PSF, final size is 2*DW+1,
;             default size is DW=9 pixels.
;  SILENT- Flag, if set suppresses all printed output.
;  SNRTHRESH - Signal-to-noise ratio threshold.  Any source pointed to by x,y
;                 that has a lower SNR than this will be ignored.  The default
;                 is 25.
;
; OUTPUTS:
;  psf   - Final average psf, normalized to unit _volume_
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  97/10/22, Written by Marc W. Buie, Lowell Observatory
;
;-
PRO psfstack,image,x,y,psf,DW=dw,SNR=snrthresh,SILENT=silent

   if badpar(image,[2,3,4,5],2,caller='PSFSTACK: (image) ') then return
   if badpar(x,[2,3,4,5],1,caller='PSFSTACK: (x) ') then return
   if badpar(y,[2,3,4,5],1,caller='PSFSTACK: (y) ') then return
   if badpar(dw,[0,2,3,4,5],0,caller='PSFSTACK: (DW) ',default=9) then return
   if badpar(snrthresh,[0,2,3,4,5],0,caller='PSFSTACK: (SNR) ',default=25.0) then return
   if badpar(silent,[0,1,2,3],0,caller='PSFSTACK: (SILENT) ',default=0) then return

   imsz = size(image)
   pad = 10
   sz = 2*dw+1
   psfcount = n_elements(x)
   psfcount0=psfcount
   ix = fix(x+0.5)
   iy = fix(y+0.5)
   xg = x
   yg = y

   ; Weed out objects too close to the edge.
   z = where( ix ge dw+pad and ix lt imsz[1]-dw-pad and $
              iy ge dw+pad and iy lt imsz[2]-dw-pad,count)
   IF count ne 0 THEN BEGIN
      ix = ix[z]
      iy = iy[z]
      xg = xg[z]
      yg = yg[z]
      psfcount=count
   ENDIF
   edge=psfcount0-count

   ; First, get the sky and signal to noise ratio on each psf.
   snr = fltarr(psfcount)
   sky = fltarr(psfcount)
   skyerr = fltarr(psfcount)
   FOR i=0,psfcount-1 DO BEGIN

      ; find local sky level
      getannul,image, xg[i], yg[i], 10.0, 40.0, skybuf
      robomean, skybuf, 3.0, 0.5, skymean, stdmean=stdmean
      sky[i] = skymean
      skyerr[i] = stdmean

      ; get region around psf
      psf = image[ix[i]-dw:ix[i]+dw,iy[i]-dw:iy[i]+dw]

      psf = psf - skymean
      snr[i] = total(psf)/(sz^2*stdmean)

   ENDFOR

   ; Weed out faint stars.
   z = where(snr gt snrthresh,count)
   IF count ne 0 THEN BEGIN
      snr = snr[z]
      sky = sky[z]
      skyerr = sky[z]
      ix = ix[z]
      iy = iy[z]
      xg = xg[z]
      yg = yg[z]
      psfcount=count
   ENDIF
   faint=psfcount0-count

   ; Collect stack of good psf stars.
   psfstack=fltarr(sz,sz,psfcount)

   FOR i=0,psfcount-1 DO BEGIN

      ; extract slightly larger array than desired psf
      tmparr = image[ix[i]-dw-pad:ix[i]+dw+pad,iy[i]-dw-pad:iy[i]+dw+pad]

      ; Shift image to center star on center pixel
      dx = ix[i] - xg[i]
      dy = iy[i] - yg[i]
      arr = sshift2d(tmparr,[dx,dy])
      psf = arr[pad:pad+sz-1,pad:pad+sz-1]

      psfstack[*,*,i] = psf - sky[i]
      snr[i] = total(psfstack[*,*,i])/(sz^2*stdmean)
      psfstack[*,*,i] = psfstack[*,*,i] / max(psfstack[*,*,i])

   ENDFOR

   ; Compute first stacked psf average
   avgclip,psfstack,psf,/silent

   ; Compute the image residuals
   ttl   = fltarr(psfcount)
   FOR i=0,psfcount-1 DO $
      ttl[i] = total(psfstack[*,*,i]-psf)

   ; Filter out those with unusual residual levels.
   bad = intarr(psfcount)
   robomean,ttl,2.0,0.5,bad=bad
   z = where(bad eq 0, count)
   badresid=psfcount-count
   IF count ne 0 THEN BEGIN
      psfstack=psfstack[*,*,z]
      psfcount=count
   ENDIF

   ; Recompute the stacked psf average
   avgclip,psfstack,psf,/silent

   ; Compute x and y FWHM for diagnostics.
   xsum=fltarr(sz)
   ysum=fltarr(sz)
   xwd=fltarr(psfcount)
   ywd=fltarr(psfcount)
   FOR i=0,psfcount-1 DO BEGIN
      xsum[*]=0.0
      ysum[*]=0.0
      FOR ii=0,sz-1 DO xsum = xsum + psfstack[*,ii,i]
      FOR ii=0,sz-1 DO ysum = ysum + psfstack[ii,*,i]
      xwd[i] = total(xsum)/max(xsum)
      ywd[i] = total(ysum)/max(ysum)
   ENDFOR

   psf = psf/total(psf)

   if not silent then $
      print,psfcount0,edge,faint,badresid,mean(xwd),mean(ywd),psfcount, $
         format='(i4," total, ",i4," edge, ",i4," faint, ",i4," bad, ",f4.1,' + $
                '" xFWHM, ",f4.1," yFWHM, ",i4," total")'

END

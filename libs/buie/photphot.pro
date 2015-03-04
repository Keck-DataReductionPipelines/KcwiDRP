;+
; NAME:
;  photphot
; PURPOSE:   (one line only)
;  Photometry from photographic image data.
; DESCRIPTION:
;  Uses marginal distributions and a pair of 1-D gaussian fits in each
;    axis to determine flux and position from an image that derives from
;    a photographic medium.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  photphot,img,x0,y0,dw, $
;           xcen,ycen,xfwhm,yfwhm,xflux,yflux,xback,yback, $
;           xcenerr,ycenerr,xfluxerr,yfluxerr
; INPUTS:
;  img        2-d image array. Assumed to have 1 star only.
;  x0         vector of starting source x positions
;  y0         vector of starting source y positions
;  dw         scalar or vector of box size for fit.
; KEYWORD INPUT PARAMETERS:
;  SKY - Scalar or vector, if provided is used for the sky background and no
;          fit on sky is performed.
;
;  DEBUG - Flag, if set will generate debug plots and printouts.  This is
;            not designed to be used when you are providing vector input.
;            Implies verbose as well.
;  SILENT - Flag, if set will suppress all non-error output.
;  VERBOSE - Flag, if set will generate chatty out (but not nearly as much
;             as if the debug flag is set).
; 
; OUTPUTS:
;  xcen  -    fitted x position of source
;  ycen  -    fitted y position of source
;  xfwhm -    fitted FWHM of source in x direction.
;  yfwhm -    fitted FWHM of source in y direction.
;  xflux -    fitted flux of source in x direction.
;  yflux -    fitted flux of source in y direction.
;  xback -    fitted image background in x direction.
;  yback -    fitted image background in y direction.
;  xcenerr -  uncertainty on fitted x position of source
;  ycenerr -  uncertainty on fitted y position of source
;  xfluxerr - uncertainty on fitted flux of source in x direction.
;  yfluxerr - uncertainty on fitted flux of source in y direction.
;
; KEYWORD OUTPUT PARAMETERS:
;  FWHM -    full width to half maximum (scaled from sigma of fit).
;  AMPL -    max value of gaussian at xcen.
;  BACKGR -  backgr (constant added to gaussian)
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2007 Oct 15, Written by Peter L. Collins, Lowell Observatory
;               Based on Fortran code kindly supplied by Larry Wasserman.
;  2007/10/16,  add SKY keyword and input vectors of source positions.
;  2007/10/21, MWB, converted to use IDL builtin, gaussfit
;  2007/10/29, MWB, integrated into library
;
;-
pro photphot,img,x0,y0,dw, $
         xcen,ycen,xfwhm,yfwhm,xflux,yflux,xback,yback, $
         xcenerr,ycenerr,xfluxerr,yfluxerr, $
         SKY=sky,DEBUG=debug,VERBOSE=verbose,SILENT=silent

   self = 'PHOTPHOT: '
   if badpar(img,[1,2,3,4,5],2,caller=self+'(img)') then return
   if badpar(x0,[4,5],[0,1],caller=self+'(x0)',npts=nx0) then return
   if badpar(y0,[4,5],[0,1],caller=self+'(y0)',npts=ny0) then return
   if badpar(dw,[1,2,3,4,5],[0,1],caller=self+'(dw)',npts=ndw) then return
   if badpar(sky,[0,1,2,3,4,5],[0,1],caller=self+'(SKY)',npts=nsky) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG)',default=0) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE)',default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT)',default=0) then return

   if ndw eq 1 then begin
      dw=replicate(dw[0],nx0)
      ndw=nx0
   endif

   if nsky eq 0 then begin
      nterms=4
   endif else if nsky eq 1 then begin
      sky=replicate(sky[0],nx0)
      nsky=nx0
      nterms=3
   endif else begin
      nterms=3
   endelse

   if max([nx0,ny0,ndw]) ne min([nx0,ny0,ndw]) then begin
      print,self,' number of src (x0,y0) and dw elements inconsistent.'
      return
   endif

   if nsky gt 0 and nsky ne nx0 then begin
      print,self,' number of sky background values inconsistent.'
      return
   endif

   sz    = size(img,/dimen)
   imtyp = size(img,/type)
   xsz   = sz[0]
   ysz   = sz[1]

   ; Setup output vectors
   xcen     = fltarr(nx0)
   ycen     = fltarr(nx0)
   xcenerr  = fltarr(nx0)
   ycenerr  = fltarr(nx0)
   xfwhm    = fltarr(nx0)
   yfwhm    = fltarr(nx0)
   xflux    = fltarr(nx0)
   yflux    = fltarr(nx0)
   xfluxerr = fltarr(nx0)
   yfluxerr = fltarr(nx0)
   xback    = fltarr(nx0)
   yback    = fltarr(nx0)
   fwhm     = fltarr(nx0)

   xx0 = round(x0)
   yy0 = round(y0)

   for j=0, nx0-1 do begin

      ; size of sub-array that will be fitted for this position
      nx = dw[j]*2+1
      ny=nx

      ; check to see if position is too close to edge, if it is, then just
      ;  mark the output values so it doesn't look like good data.
      if xx0[j]-dw[j] lt 0 or xx0[j]+dw[j] ge xsz or $
         yy0[j]-dw[j] lt 0 or yy0[j]+dw[j] ge ysz then begin
         xcen[j]     = x0[j]
         xcenerr[j]  =   100.0
         ycen[j]     = y0[j]
         ycenerr[j]  =   100.0
         xfwhm[j]    =   100.0
         yfwhm[j]    =   200.0
         xflux[j]    = -1000.0
         xfluxerr[j] =  1000.0
         yflux[j]    = -1000.0
         yfluxerr[j] =  1000.0
         if nsky gt 0 then begin
            xback[j] = sky[j]
            yback[j] = sky[j]
         endif else begin
            xback[j] = 0.
            yback[j] = 0.
         endelse
         continue
      endif

      ; Grab sub-image. (Promote byte data to int)
      if imtyp eq 1 then begin
         subimg = fix(img[xx0[j]-dw[j]:xx0[j]+dw[j],yy0[j]-dw[j]:yy0[j]+dw[j]])
      endif else begin
         subimg = img[xx0[j]-dw[j]:xx0[j]+dw[j],yy0[j]-dw[j]:yy0[j]+dw[j]]
      endelse

      ; Subtract external sky if supplied
      if nterms eq 3 then subimg -= sky[j]

      ; independent variables
      xind=indgen(nx)+xx0[j]-dw[j]
      yind=indgen(ny)+yy0[j]-dw[j]

      ; collapse sub-image in x and y
      xmarg = total(subimg,2)   ; sum over y for each xmarg
      ymarg = total(subimg,1)   ; sum over x for each ymarg

      ; do xfit
      xfit=gaussfit(xind,xmarg,xterms,chisq=xchisq,nterms=nterms,sigma=xsig)
      xterms[2] = abs(xterms[2])
      if verbose or debug then begin
         print,'from gaussfit to xmarg'
         print,'xterms',xterms
         print,'xchisq',xchisq
         print,'xsig',xsig
      endif

      ; do yfit
      yfit=gaussfit(yind,ymarg,yterms,chisq=ychisq,nterms=nterms,sigma=ysig)
      yterms[2] = abs(yterms[2])
      if verbose or debug then begin
         print,'from gaussfit to ymarg'
         print,'yterms',yterms
         print,'ychisq',ychisq
         print,'ysig',ysig
      endif

      ; copy results to the output vectors
      xcen[j]     = xterms[1]
      xcenerr[j]  = xsig[1]
      ycen[j]     = yterms[1]
      ycenerr[j]  = ysig[1]
      xfwhm[j]    = sqrt(2.0)*xterms[2]*2.35482
      yfwhm[j]    = sqrt(2.0)*yterms[2]*2.35482
      xflux[j]    = xterms[0]*sqrt(2.0*!pi)*xterms[2]
      xflvar      = xterms[0]*sqrt(2.0*!pi)*xsig[2]^2 + $
                       xsig[0]^2*sqrt(2.0*!pi)*xterms[2]
      if xflvar ge 0.0 then begin
         xfluxerr[j] = sqrt(xterms[0]*sqrt(2.0*!pi)*xsig[2]^2 + $
                            xsig[0]^2*sqrt(2.0*!pi)*xterms[2])
      endif else begin
         xfluxerr[j] = 0.0
      endelse
      yflux[j]    = yterms[0]*sqrt(2.0*!pi)*yterms[2]
      yfluxerr[j] = sqrt(yterms[0]*sqrt(2.0*!pi)*ysig[2]^2 + $
                         ysig[0]^2*sqrt(2.0*!pi)*yterms[2])

      if nterms eq 3 then begin
         xback[j] = sky[j]
         yback[j] = sky[j]
      endif else begin
         xback[j] = xterms[3]/nx
         yback[j] = yterms[3]/ny
      endelse

      if debug then begin
         setwin,1,xsize=400,ysize=200
         plot,xind,xmarg,psym=-4
         oplot,xind,xfit,color='0000ff'xl
         setwin,2,xsize=400,ysize=200
         plot,yind,ymarg,psym=-4
         oplot,yind,yfit,color='0000ff'xl
         setwin,3,xsize=400,ysize=200
         plot,xind,xmarg-xfit,psym=-4
         setwin,4,xsize=400,ysize=200
         plot,yind,ymarg-yfit,psym=-4
      endif

   endfor

   if nx0 eq 1 then begin
      xcen=xcen[0]
      ycen=ycen[0]
      xfwhm=xfwhm[0]
      yfwhm=yfwhm[0]
      xflux=xflux[0]
      yflux=yflux[0]
      xback=xback[0]
      yback=yback[0]
      xcenerr=xcenerr[0]
      ycenerr=ycenerr[0]
      xfluxerr=xfluxerr[0]
      yfluxerr=yfluxerr[0]
   endif
end


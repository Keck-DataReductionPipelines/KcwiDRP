;+
; NAME:
;  seeing
; PURPOSE:   (one line only)
;  Given an astronomical image, determine the image quality (seeing).
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  seeing,image,fwhm,objrad,nstars
; INPUTS:
;  image - Array containing image to be examined.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FWHMGUESS - Initial guess of the FWHM in the image.  Default=3.  If this
;                 is significantly smaller than the actual seeing you will
;                 get an answer that is much too small.  The penalty for
;                 guessing a larger than optimal value appears to be weak.
;  MINSIGNAL - minimum signal to consider when looking for sources
;                 (default=0)
;  MAXSIGNAL - maximum signal to conisder when looking for sources
;                 (default=60000)
;  NODISPLAY - Flag, if set will suppress plot and image outputs
; OUTPUTS:
;  fwhm  - Image quality, FWHM in pixels for the image.
;  objrad - Object aperture radius used to determine fwhm
;  nstars - Number of stars that were averaged to get fwhm
; KEYWORD OUTPUT PARAMETERS:
;  ERROR - set to 1 (true) if an error occurred, zero otherwise.  If the
;            error flag is set, the return values may or may not be useful.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2004/07/03
;  2005/06/28, MWB, added extra error trapping and ERROR keyword
;  2008/07/23, MWB, added keyword for initial FWHM guess
;-
pro seeing,image,fwhm,objrad,nstars,ERROR=error, $
       MINSIGNAL=minsignal,MAXSIGNAL=maxsignal,NODISPLAY=nodisplay, $
       FWHMGUESS=fwhmguess

   error=1
   self='SEEING: '
   if badpar(image,[1,2,3,4,5,12,13,14,15],2, $
             caller=self+'(image): ',dimen=dimen) then return

   if badpar(minsignal,[0,1,2,3,4,5,12,13,14,15],0, $
             caller=self+'(MINSIGNAL): ',  default=0.) then return

   if badpar(maxsignal,[0,1,2,3,4,5,12,13,14,15],0, $
             caller=self+'(MAXSIGNAL): ',  default=60000.) then return

   if badpar(fwhmguess,[0,2,3,4,5],0, $
             caller=self+'(FWHMGUESS): ',  default=3) then return

   if badpar(nodisplay,[0,1,2,3],0, $
             caller=self+'(NODISPLAY): ',  default=0) then return
   error=0

   ; Do an initial pass with an initial guess on the fwhm.
   fwhm0 = fwhmguess
   find,image,x,y,flux,sharp,roundness,minsignal,fwhm0,[-1,1],[.2,1.],/silent

   if size(x,/type) eq 0 then begin
      fwhm = -1.0
      objrad=-1.0
      nstars=0
      print,self,'FIND could not detect any sources.'
      error=1
      return
   endif

   z=where(image[fix(x+0.5),fix(y+0.5)] lt maxsignal,count)

   if count eq 0 then begin
      fwhm = -1.0
      objrad=-1.0
      nstars=0
      print,self,'No sources detected.  Aborting.'
      error=1
      return
   endif

   x=x[z]
   y=y[z]

   bad = bytarr(n_elements(x))
   robomean,roundness,2.0,0.5,meanround,bad=bad
   robomean,sharp,2.0,0.5,meansharp,bad=bad
   z=where(bad eq 0,count)

   if count eq 0 then begin
      fwhm = -1.0
      objrad=-1.0
      nstars=0
      print,self,'No sources survived.  Aborting.'
      error=1
      return
   endif

   if not nodisplay then begin
      print,'mean sharpness, roundness',meansharp,meanround,' count = ',count

      setwin,0
      plot,sharp,roundness,psym=3,xtitle='Sharpness',ytitle='Roundness'
      oplot,sharp[z],roundness[z],color='ff00ff'xl,psym=3
   endif

   x=x[z]
   y=y[z]

   objrad = fwhm0
   basphote,1.0,image,1.0,x,y,objrad,objrad+5,objrad+20, $
      /silent,/nolog,fwhm=fwhm1,xcen=newx1,ycen=newy1
   bad = bytarr(n_elements(x))
   z=where(abs(newx1-x) gt 0.5 and bad eq 0,count)
   if count ne 0 then bad[z] = 1B
   z=where(abs(newy1-y) gt 0.5 and bad eq 0,count)
   if count ne 0 then bad[z] = 1B
   robomean,fwhm1,2.0,0.5,meanfwhm1,bad=bad
   z=where(bad eq 0,count)

   if count eq 0 then begin
      fwhm = -1.0
      objrad=-1.0
      nstars=0
      print,self,'No sources survived.  Aborting.'
      error=1
      return
   endif

   x=x[z]
   y=y[z]
   fwhm1=fwhm1[z]
   newx1=newx1[z]
   newy1=newy1[z]

   objrad = meanfwhm1

   if objrad lt 1.0 then begin
      fwhm = -1.0
      objrad=-1.0
      nstars=0
      print,self,'objrad too small, apparently there are no sources.  Aborting.'
      error=1
      return
   endif

   if not nodisplay then $
      print,'Setting object aperture radius to ',objrad,'  count=',count

   basphote,1.0,image,1.0,x,y,objrad,objrad+5,objrad+20, $
      /silent,/nolog,fwhm=fwhm2,xcen=newx2,ycen=newy2

   bad = bytarr(n_elements(x))
   z=where(abs(newx2-x) gt 0.5 and bad eq 0,count)
   if count ne 0 then bad[z] = 1B
   z=where(abs(newy2-y) gt 0.5 and bad eq 0,count)
   if count ne 0 then bad[z] = 1B
   z=where(abs(newx2-newx1) gt 0.3 and bad eq 0,count)
   if count ne 0 then bad[z] = 1B
   z=where(abs(newy2-newy1) gt 0.3 and bad eq 0,count)
   if count ne 0 then bad[z] = 1B
   robomean,fwhm1-fwhm2,2.0,0.5,meanfwhmdiff,bad=bad
   robomean,fwhm1,2.0,0.5,meanfwhm1,bad=bad
   robomean,fwhm2,2.0,0.5,meanfwhm2,bad=bad
   z=where(bad eq 0,count)

   if count eq 0 then begin
      fwhm = -1.0
      objrad=-1.0
      nstars=0
      print,self,'No sources survived.  Aborting.'
      error=1
      return
   endif

   if not nodisplay then $
      print,'mean FWHM1, FWHM2, FWHMdiff ',meanfwhm1,meanfwhm2,meanfwhmdiff, $
         ' count=',count

   x=x[z]
   y=y[z]
   newx1=newx1[z]
   newy1=newy1[z]
   newx2=newx2[z]
   newy2=newy2[z]

   if not nodisplay then begin
      sz=size(image,/dimen)
      skysclim,image,loval,hival
      setwin,1,xsize=sz[0],ysize=sz[1]
      tvscl,bytscl(image,min=loval,max=hival,top=255)
      plot,[0],[1],/nodata,/noerase,xr=[0,sz[0]],yr=[0,sz[1]], $
         xstyle=5,ystyle=5,xmargin=[0,0],ymargin=[0,0]
      oplot,x,y,psym=4,symsize=3,color='0000ff'xl
      oplot,newx1,newy1,psym=4,symsize=3,color='ffff00'xl
      oplot,newx2,newy2,psym=4,symsize=3,color='ff00ff'xl
   endif

   fwhm = meanfwhm2
   nstars = count

end

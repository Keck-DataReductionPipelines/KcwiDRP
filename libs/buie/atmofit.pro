;+
; NAME:
;	atmofit
; PURPOSE: (one line)
;	Fit 1 or 2 gaussians to an astronomical image that is seeing limited.
; DESCRIPTION:
; CATEGORY:
;	Function fitting
; CALLING SEQUENCE:
;  atmofit,image,loc,guess,sub,resa,a,sigmaa,chisq
; INPUTS:
;	image - 2-d data array to be fitted
;
;	loc   - 4 element vector that specifies where to look in the image
;	          for an object to fit.  This is passed to boxm to look for
;	          the image maximum.  The components are:
;	            loc(0) - X center of box.
;	            loc(1) - Y center of box.
;	            loc(2) - Half width of box in the X direction.
;	            loc(3) - Half width of box in the Y direction.
;
;	guess - Initial value guesses for the image fitting process.  There
;	          are two possibilites:
;	          1) one star (object) fit - guess is a scalar value that
;	                                       is a guess at the 1/e width
;	          2) two star (object) fit - guess is a three element vector
;	                guess(0) = 1/e width of seeing
;	                guess(1) = X location of fainter star relative to the
;	                              center of the brighter object
;	                guess(2) = Y location of fainter star relative to the
;	                              center of the brighter object
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;	FILE   - string containing the FITS file name to read for image.
;	           If read, then input parameter image is overwritten and
;	           returned.
;	FORCE  - If guess is set up for two stars, this forces the solution
;	           for the second position relative to the first if set.
;	WIDTH  - Number of pixels to attempt to extract from around the peak
;	           pixel.  Subarray will be at most 2*WIDTH+1 square.
;	           Extraction is truncated by the edge of the array.
;		   Default is FIX( GUESS(0)*5.0 + 0.5 )
;	DISPLAY - Show extraction, model fit and residual images on the
;	           current display window.  Value indicates the zoom factor
;	           for the displayed images.
; OUTPUTS:
;	sub    - Extracted piece of image.
;	resa   - Model fit to sub.
;	a      - Coefficients of the fit.
;	sigmaa - Uncertainties of the fitting coefficients.
;	chisq  - Reduced chi-squared of the fit to sub.
;
; KEYWORD OUTPUTS:
;	ERR    - uncertainty on MAG.
;	MAG    - returns the magnitude of the object (scalar for a star,
;	           three element vector for 2 gaussians [total, bigger, smaller]
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-
pro atmofit,image,loc,guess,sub,resa,a,sigmaa,chisq, $
              MAG=mag,ERR=err,FILE=file,WIDTH=width,DISPLAY=display,FORCE=force

;Validate the input values, first number of literal parameters must be right.
if n_params() lt 3 then begin
   print,'atmofit,image,loc,guess,sub,resa,a,sigmaa,chisq'
   return
endif

if not keyword_set(force) then force = 0

;Second argument.
sz=size(loc)
if sz[0] ne 1 or sz[1] ne 4 or sz[2] gt 5 then $
   message,'Location must be a four element numeric vector'

;Third argument.
sz=size(guess)
onestar = sz[0] eq 0 and sz[1] le 5
twostar = sz[0] eq 1 and sz[2] le 5 and sz[1] eq 3
if not onestar and not twostar then $
   message,'Guess must be a three element numeric vector or scalar'
if guess[0] lt 0.5 then $
   message,'Bad guess for 1/e width, should be greater than 1'

;if not keyword_set(width) then width = fix(guess(0)*5.0 + 0.5)

;Read the file if FILE is set.
if keyword_set(file) then image=readfits(file,/silent)

;Validate the image (passed or read)
sz=size(image)
if sz[0] ne 2 then message,'Image must be a 2-d numeric array'

;Get the maximum near loc.
boxm,image,loc[0],loc[1],loc[2],loc[3],xmax,ymax
fnx=sz[1]
fny=sz[2]

;Determine the extraction region from the new found maximum.
x1=max([xmax-width,0])
x2=min([xmax+width,fnx-1])
y1=max([ymax-width,0])
y2=min([ymax+width,fny-1])

sub=image[x1:x2,y1:y2]

sz=size(sub)
nx=sz[1]
ny=sz[2]

if onestar then begin
   starfit,sub,guess,resa,a,sigmaa
   a[0]=a[0]+x1
   a[1]=a[1]+y1
   sep=0.0
endif

if twostar then begin
   plchfit,sub,guess,resa,a,sigmaa,force=force
   a[0]=a[0]+x1
   a[1]=a[1]+y1
   if force eq 1 then begin
      sep = sqrt(guess[1]^2+guess[2]^2)
   endif else begin
      a[6]=a[6]+x1
      a[7]=a[7]+y1
      sep = sqrt((a[0]-a[6])^2 + (a[1]-a[7])^2)
   endelse
endif

chisq= total((sub-resa)^2)/(n_elements(sub)-n_elements(a))

if onestar then begin
   inten=a[2]*a[3]
   dinten=sqrt( a[3]^2*sigmaa[2]^2 + a[2]^2*sigmaa[3]^2 )
   flx2mag,inten,dinten,mag,err,zeropt=21.0
endif

if twostar then begin
   inten=[(a[2]+a[5])*a[3], a[2]*a[3], a[5]*a[3]]
   dinten=[ $
      sqrt( a[3]^2*sigmaa[2]^2 + a[2]^2*sigmaa[3]^2 ), $
      sqrt( a[3]^2*sigmaa[5]^2 + a[5]^2*sigmaa[3]^2 ) ]
   dinten = [ sqrt(dinten[0]^2 + dinten[1]^2), dinten ]
   flx2mag,inten,dinten,mag,err,zeropt=21.0
endif

if keyword_set(display) then begin
   rebinfac=display
   setwin,1,xsize=nx*rebinfac,ysize=ny*rebinfac
   tvscl,rebin(sub,nx*rebinfac,ny*rebinfac,/sample)
   setwin,2,xsize=nx*rebinfac,ysize=ny*rebinfac
   tvscl,rebin(sub-resa,nx*rebinfac,ny*rebinfac,/sample)
   setwin,3,xsize=nx*rebinfac,ysize=ny*rebinfac
   tvscl,rebin(resa,nx*rebinfac,ny*rebinfac,/sample)
   all=[sub,sub-resa]
   low=min(all)
   high=max(all)
   setwin,4,xsize=nx*rebinfac,ysize=ny*rebinfac
   tv,rebin(bytscl(sub,min=low,max=high,top=!d.n_colors),nx*rebinfac,ny*rebinfac,/sample)
   setwin,5,xsize=nx*rebinfac,ysize=ny*rebinfac
   tv,rebin(bytscl(sub-resa,min=low,max=high,top=!d.n_colors),nx*rebinfac,ny*rebinfac,/sample)

   if onestar then begin
      print,'Star x position    ',a[0],' +/- ',sigmaa[0]
      print,'Star y position    ',a[1],' +/- ',sigmaa[1]
      print,'Star peak          ',a[2],' +/- ',sigmaa[2]
      print,'1/e width          ',a[3],' +/- ',sigmaa[3]
      print,'Constant term      ',a[4],' +/- ',sigmaa[4]
      print,'Intensity          ',inten,' +/- ',dinten
      print,'Instrumental mag   ',mag, ' +/- ',err
   endif

   if twostar then begin
      print,'Obj #1 x position  ',a[0],' +/- ',sigmaa[0]
      print,'Obj #1 y position  ',a[1],' +/- ',sigmaa[1]
      print,'Obj #1 peak        ',a[2],' +/- ',sigmaa[2]
      print,'Obj #1 Intensity   ',inten[1],' +/- ',dinten[1]
      print,'Obj #1 mag         ',mag[1], ' +/- ',err[1]
      print,'1/e width          ',a[3],' +/- ',sigmaa[3]
      print,'Constant term      ',a[4],' +/- ',sigmaa[4]
      if force eq 1 then begin
         print,'Obj #2 x position  ',a[0]+guess[1],' solution forced.'
         print,'Obj #2 y position  ',a[1]+guess[2],' solution forced.'
      endif else begin
         print,'Obj #2 x position  ',a[6],' +/- ',sigmaa[6]
         print,'Obj #2 y position  ',a[7],' +/- ',sigmaa[7]
      endelse
      print,'Obj #2 peak        ',a[5],' +/- ',sigmaa[5]
      print,'Obj #2 Intensity   ',inten[2],' +/- ',dinten[2]
      print,'Obj #2 mag         ',mag[2], ' +/- ',err[2]
      print,'Combined Intensity ',inten[0],' +/- ',dinten[0]
      print,'Combined mag       ',mag[0], ' +/- ',err[0]
      print,'Separation         ',sep
   endif

   print,'Chisq              ',chisq
endif

end

;+
; NAME:
;  ois
; PURPOSE:   (one line only)
;  Optimal image subtraction
; DESCRIPTION:
;
;  STAMP - Definition - This term refers to a localized section of the array
;    that contains an image of point-source.  This could either be an
;    actual sub-array or a description of a sub-array in an larger image.
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  ois,image,reference,diffimage
; INPUTS:
;  image - Image to be processed.  If this is a string it is taken to be
;             a file name that will be read.  The other option is to provide
;             the array to be processed.
;  reference - Reference image to be subtracted from image.  Just as with
;                 image, the input can be a string or an array.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  CONSTPHOT - Flag, if set turns on the constant photometric ratio constraint
;  CHISQSTAT - Threshold for excluding stamps that don't fit well.
;                 Default=0.0 (ie., no exclusions)
;  GAUSSIAN  - Keyword that controls the basis set used.  If not provided,
;                a Delta-function basis set is used.  To use a gaussian
;                function basis set either give the name of the basis set
;                to use or privide the description yourself.  If you provide
;                'astier' as the name, you will get the following:
;              gaussian={nc:3, degree:[6,4,2], sigmas:[0.7,1.5,2.0]}
;              You can provide your own description by providing a
;              similar structure.  Tag nc is the number of components.
;              The length of degree and sigmas must match nc.
;  PATH - Directory location to find the input images.  Default is the
;           current directory.
;  IMGHDR - String array containing FITS header information for input image.
;             This is used only when the input image is provided directly,
;             rather than to be read from a file.  This array is used as
;             a starting point for the saved output image (if desired).
;  FWHM - FWHM of the input image.  Used only if image provided
;             directly and the header is not.  This value is
;             otherwise read from the header from the 'SEEING'
;             keyword in the header.
;  MAXPHOTSIG- Maximum DN value for a useful signal.  Any source with a peak
;                above this level is passed over.  Default=60000.0 DN
;  MINFLUX - Threshold for sources to be included in the reference PSFs
;               to support the image subtraction.  This value is relative
;               to the brightest source found in the image.  Default=0.25
;  DEGREE - Degree of polynomial used to fit for the space varying kernel.
;             Default=0 (constant kernel)
;  DELP   - Grid size in pixels for recalculating the space-varying kernel.
;             Default=1 (doesn't matter for a constant kernel)
;  OUTPATH - String, name of directory to save data in.
;                Default = current directory.
;                NOTE: PATH and OUTPATH must not be the same.  If they are
;                  then the SAVEIT flag will be ignored and the output will
;                  not be generated.
;  SILENT - Flag that will determine whether various relevant information will
;           be printed. Default=0. By default, it will print the statistics
;  NODISPLAY - Flag that will determine whether various relevant plots will be
;              created. Default=0. By default, the plots will be created.
;  SEED    - value for the seed used by the random call. Default is undefined
;              causing random to use the system time as the seed
; OUTPUTS:
;  diffimage - the subtracted image 
; KEYWORD OUTPUT PARAMETERS:
;  CREFERENCE - the convolved reference image
;  FAILED - boolean indicating whether or not the subtraction failed
;  DIFFINFO - anonymous structure containing lots of useful information about
;                the difference image and what happened along the way.
;                This structure is undefined if the output keyword FAILED is set
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Note that the input to this routine is expected to already be registered
;    and be of the same size.  The routine, dewarp, is usually used to
;    do this registration step.
; PROCEDURE:
;   Internal helper routines:
;     ois_filtpix
; MODIFICATION HISTORY:
;   Written by SwRI clinic team, 2009/11/09, optimized version derived from
;     the original code written by Patrick Miller. (ref)
;   2010/03/05, FS, added IMGSKY and REFSKY keywords
;   2010/03/12, FS, added SILENT, NODISPLAY, and SEED keywords
;   2010/04/21, FS, added CREFERENCE, FAILED keywords
;   2010/04/28, FS, changed shift calls in ois_coef to sshift2d with edge_zero
;   2010/06/10, MWB, fixed a display bug with the number of stamps changing.
;                     Also changed default on DEGREE to 0.
;   2010/09/23, MWB, maxphotsig was not getting all the way to the findsrc
;                     call that does the stamp selection.  This has been
;                     changed.  Also, kernel size determination was tweaked
;                     downward.
;   2010/10/13, MWB, rework of the image/reference flux ratio calculation
;   2010/10/21, MWB, fixed a bug where the FWHM value read from the header
;                      was not used properly.
;   2010/10/22, MWB, cleaded up ois_stamps routine.  Flux ratio and fwhm
;                      determinations are much better now.
;   2010/11/02, MWB, added DIFFINFO output keyword
;   2010/11/03, MWB, minor modification to ois_convref to hold the output
;                      image type to float (was returning a double).  This
;                      ensures the final difference image is a float.
;-

;---Various helper routines
forward_function ois_chidist
forward_function ois_chisq
forward_function ois_convbas
forward_function poly

;----------------------------------------------
; NAME:
;  ois_chisq
; PURPOSE:   (one line only)
;  This function computes the chi-square value between two stamps
; DESCRIPTION:
;  This functions takes in 2 stamps and computes the chi-square value between
;  them.
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ans = ois_chisq(csubref,subimg)
; INPUTS:
; csubref - the convolved reference stamp.<flt>[2*delw+1,2*delw+1]
; subimg - the original image.<flt>[2*delw+1,2*delw+1]
;              
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  answer - the chi-square value of the given pair of stamps.<flt>
;      
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2009/11/8, Originally written by J. Patrick Miller, Hardin-Simmons
;    University, December 2004.
;  2009/12/02 - This version is a re-write to optimize the code.
;               This version produced by Harvey Mudd College Clinic Team

function ois_chisq,csubref,subimg

   temp=fltarr(2)
   temp[0]=min(csubref)
   temp[1]=min(subimg)
   minimum=min(temp)
   temp[0]=max(csubref)
   temp[1]=max(subimg)
   maximum=max(temp)
   delw=sqrt(n_elements(subimg))
   bins=round((2.0*alog(float(delw))/alog(2.0))+1.0)
   hcsubref=histogram(csubref, nbins=bins, min=minimum, max=maximum)
   hsubimg=histogram(subimg, nbins=bins, min=minimum, max=maximum)
   answer=xsq_test(hcsubref,hsubimg)
;setwin,0
;plot,hsubimg
;oplot,hcsubref,color='0000ff'xl
;tvscl,subimg,0
;tvscl,csubref,1
;asdf

   return, answer[1]

end

;----------------------------------------------
; NAME:
;  ois_chidist
; PURPOSE:   (one line only)
;  Top-level function that calls chisq for each stamp pair
; DESCRIPTION:
;  this function takes in the image and reference image and a list of all the 
;  stamps. Then it calculates the chi-square value between each pair of stamps.
;
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  chisqdist=ois_chidist(creference,image,xstamp,ystamp,wstamp)
; INPUTS:
; creference - the convolved reference image.<flt>[nx,ny]
; image - the original image.<flt>[nx,ny]
; xstamp - vector of the x-location of each stamp.<int>[pmax]
; ystamp - vector of the y-location of each stamp.<int>[pmax]
; wstamp - vector of the width of each stamp.<int>[pmax]
;              
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  chisqdist - vector of the chi square value for each stamp.<flt>[pmax]
;      
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2009/11/8, Originally written by J. Patrick Miller, Hardin-Simmons
;    University, December 2004.  This version is a re-write optimize
;    the code.  This version produced by Harvey Mudd College Clinic Team

function ois_chidist,creference,image,xstamp,ystamp,wstamp

   p=n_elements(wstamp)
   chisqdist=fltarr(p)

;help,p,xstamp,ystamp,wstamp
   for k=0,p-1 do begin
      xctr=xstamp[k]
      yctr=ystamp[k]
      delw=wstamp[k]
      istamp=image[xctr-delw:xctr+delw,yctr-delw:yctr+delw]
      crstamp=creference[xctr-delw:xctr+delw,yctr-delw:yctr+delw]
      chisqdist[k]=ois_chisq(crstamp,istamp)
;print,k,xctr,yctr,delw,chisqdist[k]
   endfor

   return, chisqdist

end

;----------------------------------------------
; NAME:
;  ois_coef
; PURPOSE:   (one line only)
;  Create alpha, beta and gamma arrays for solving spacevarying kernel
; DESCRIPTION:
;  This function will generate the alpha, beta, and gamma arrays which are
;  used to solve for the ideal kernel by varkern.
;
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_coef,iratio,img,ref,deg,kernsz,nbasis,kbasis, $
;       xstamp,ystamp,wstamp,ibasis,alpha,beta,gamma,pmax
; INPUTS:
;  iratio - flag specifying whether a constant photometric rati should be 
;           assumed <str>
;  img - the template image. This image does not get changed. <flt>[row,col]
;  ref - the referenence image. This image gets convolved with the kernel to 
;        math the template image. <flt>,[row,col]
;  deg - the degree of the polynomial used for the space-varying kernel. <int>
;  kernsz - size of the square kernel used. <int>
;  nbasis - the total number of basis vectors used for the kernel. <int>
;  kbasis - the actual bases vectors for the kernel.
;           <flt>[nbasis,kernsz,kernsz]
;  xstamp - vector containing the x location of the stamps. <int>[pmax]
;  ystamp - vector containing the y location of the stamps. <int>[pmax]
;  wstamp - vector containing the width of the stamps. <int>[pmax]
;  ibasis - string identifying which basis function is used for kernel.<str>
;           either 'DELTA' or 'GC'         
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  alpha - this 3D array contains the x^u*y^v terms for each stamp
;          the first index corresponds to the stamp
;          the second index corresponds to the x terms
;          the third term corresponds to the y terms
;          both are increasing in exponent as it decreases
;          alpha[3,2,5]- x^2*y^5 for the 3rd stamp
;  beta - this 3D array contains the 
;          total((R_k x K_n)*(R_k x K_q) terms
;          beta[k,n,q]- the convolution of the kth stamp with the nth kernel
;                        times the convolution of the stamp with the qth kernel
;                        summed up over the stamp
;  gamma - this 2D array contains the 
;          total(I_k*(R_k x K_q))
;          gamma[k,q]- the convolution of the kth stamp with the qth kernel
;                      times the kth stamp of the template
;  pmax - total number of stamps
;      
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2009/11/8, Originally written by J. Patrick Miller, Hardin-Simmons
;    University, December 2004.  This version is a re-write optimize
;    the code.  This version produced by Harvey Mudd College Clinic Team
;   2010/04/28, FS, changed shift calls to sshift2d with edge_zero
;-
pro ois_coef,iratio,img,ref,deg,kernsz,nbasis,kbasis, $
       xstamp,ystamp,wstamp,ibasis,alpha,beta,gamma,pmax

   ; Define the Arrays
   pmax=n_elements(wstamp)
   alpha=fltarr(pmax,deg+1,deg+1)
   beta=fltarr(pmax,nbasis,nbasis)
   gamma=fltarr(pmax,nbasis)

   for u=0,deg do $
     for v=0,deg-u do $
       alpha[*,u,v]=xstamp^u*ystamp^v
    
   ; Build the Beta & Gamma Arrays
   for k=0,pmax-1 do begin
   
      xctr=xstamp[k]
      yctr=ystamp[k]
      delw=wstamp[k]
      istamp=img[xctr-delw:xctr+delw,yctr-delw:yctr+delw]
      istampr=reform(istamp,(2*delw+1)^2)
      rstamp=ref[xctr-delw:xctr+delw,yctr-delw:yctr+delw]
      
      if ibasis eq 'DELTA' then begin
        ckbasis=fltarr(nbasis,2*delw+1,2*delw+1,/nozero)
        if iratio eq 'NO' then begin
          for i=0,kernsz-1 do $
            for j=0,kernsz-1 do $
              ckbasis[i*kernsz+j,*,*] = $
                 sshift2d(rstamp,[(kernsz-1)/2-i,(kernsz-1)/2-j],/edge_zero)
        endif else begin
          for i=0,kernsz-1 do begin
            for j=0,kernsz-1 do begin
              ckbasis[i*kernsz+j,*,*] = $
                 sshift2d(rstamp,[(kernsz-1)/2-i,(kernsz-1)/2-j],/edge_zero) $ 
                            -rstamp
            endfor
          endfor
          ckbasis[0:(kernsz^2-1)/2,*,*]=shift(ckbasis[0:(kernsz^2-1)/2,*,*], $
                                              [1,0,0])
          ckbasis[0,*,*]=rstamp
        endelse
      endif else begin
        ckbasis=ois_convbas(rstamp,kbasis)
      endelse
      
      ckbasisr=reform(ckbasis,[nbasis,(2*delw+1)^2])

      beta[k,*,*]=matrix_multiply(ckbasisr,ckbasisr,/btranspose)  
      gamma[k,*]=matrix_multiply(istampr,ckbasisr,/btranspose)
   endfor

end

;----------------------------------------------
; NAME:
;  ois_convbas
; PURPOSE:   (one line only)
;  Convols all the kernel bases with a single stamp
; DESCRIPTION:
;  This function takes in a single stamp and the basis vectors for the kernel
;  and convolves them to create the ckbasis array. This function is only called
;  when a Gaussian basis function is used. Note that the convolution is run
;  with the edge_zero flag on to avoid wrapping around erroneous data.
;
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_convbas, rstamp,kbasis
; INPUTS:
; rstamp - the reference stamp.<flt>[2*delw+1,2*delw+1]
; kbasis - 3D array of kernel bases.<flt>[n,kernsz,kernsz]
;              
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  ckbasis - 3D array of the convolved reference stamp.
;            <flt>[n,2*delw+1,2*delw+1]
;      
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2009/11/8, Originally written by J. Patrick Miller, Hardin-Simmons
;    University, December 2004.  This version is a re-write optimize
;    the code.  This version produced by Harvey Mudd College Clinic Team

function ois_convbas,rstamp,kbasis
   kbsize=size(kbasis,/dimensions)
   rsize=size(rstamp,/dimensions)
   ckbasis=fltarr(kbsize[0],rsize[0],rsize[0])
   for u=0,kbsize[0]-1 do $
      ckbasis[u,*,*]=convol(rstamp,reform(kbasis[u,*,*]), $
                            /center,/edge_zero)
   return, ckbasis
end

;----------------------------------------------
; NAME:
;  ois_convref
; PURPOSE:   (one line only)
;  Generate kernel, convolve with reference, and subtract
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_convref,nbasis,kbasis,kernelsize,degree,ckern,image,reference, $
;       creference,subtraction,kernel,delp
; INPUTS:
;  nbasis      - number of kernel bases
;  kbasis      - a 3D array of 2D bases. 
;                Dimensions are [nbasis, kernelsize, kernelsize]
;  kernelsize  - number of pixels along the edge of a kernel
;  degree      - degree of the non-constant kernel polynomial
;  ckern       - 3D array of kernel coefficients.
;                Dimensions are [nbasis, degree, degree]
;  image       - template image to be subtracted
;  reference   - reference image to be convolved
;  delp        - grid at which to calculate kernels
;  iratio      - constant photometric ratio flag
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag that will determine whether various relevant information will
;           be printed. Default=0. By default, it will print the statistics
;  NODISPLAY - Flag that will determine whether various relevant plots will be
;              created. Default=0. By default, the plots will be created.
; OUTPUTS:
;  creference  - convolved reference image
;  subtraction - final subtracted image = creference - image
;  kernel      - generated kernel (of the final pixel for non-constant kernel)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by J. Patrick Miller
;  2009/11/09, Austin Lee, Harvey Mudd College
;    Unwrapped basis loop, added custom convolution code to improve performance

pro ois_convref,nbasis,kbasis,kernelsize,degree,ckern,image,reference, $
       creference,subtraction,kernel,delp,poly,ibasis,iratio,SILENT=silent, $
       NODISPLAY=nodisplay
   
   self='ois_convref: '
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                   default=0) then return
   if badpar(nodisplay,[0,1,2,3],0,caller=self+'(NODISPLAY) ', $
                                   default=0) then return

   ; determine the image and kernel dimensions
   isize=size(image, /dimensions)
   irow=isize[0]
   icol=isize[1]
   creference=fltarr(irow,icol)
   kernelwidth=(float(kernelsize)-1.0)/2.0

   ; check for constant kernel solution
   if degree le 0 then begin

      ; Gaussian Basis
      if ibasis eq 'GC' then begin
        ; build the kernel, size (kernelwidth,kernelwidth)
        l=kernelsize
        kernel=fltarr(l,l)
        for n=0,nbasis-1 do $
           kernel[*,*]=kernel[*,*]+ckern[n,0,0]*kbasis[n,*,*]
  
        ; convolve the reference
        creference=convol(reference,kernel,/center,/edge_zero)
        
      ; Delta Basis  
      endif else begin
        if iratio eq 'NO' then begin
          for i=0,kernelsize-1 do begin
            for j=0,kernelsize-1 do begin
              creference=creference+float(ckern[i*kernelsize+j,0,0]* $
              shift(reference,[(kernelsize-1)/2-i,(kernelsize-1)/2-j]))
            endfor
          endfor
        endif else begin
          ckern[0:(nbasis-1)/2]=shift(ckern[0:(nbasis-1)/2],-1)
          for i=0,kernelsize-1 do begin
            for j=0,kernelsize-1 do begin
              creference=creference+float(ckern[i*kernelsize+j,0,0]* $
              shift(reference,[(kernelsize-1)/2-i,(kernelsize-1)/2-j]))
            endfor
          endfor
          creference=creference-float(total(ckern[*,0,0])-$
                     ckern[(nbasis-1)/2,0,0])*reference
        endelse
      endelse

      if ibasis eq 'GC' then begin
        if not silent then $
           print,'   --> kernelsize=',strn(kernelsize),'  min/max ',minmax(kernel)
        if not nodisplay then begin
           setwin,6,xsize=kernelsize*15,ysize=kernelsize*15
           tvscl,rebin(kernel,kernelsize*15,kernelsize*15,/sample)
        endif
      endif

   ; varying kernel
   endif else begin

      ; build the kernel and convolve the reference (at each pixel)
      icnt=delp
      jcnt=delp
      for i=kernelwidth,irow-1-kernelwidth do begin

         if (icnt mod delp) eq 0 then icalc="Yes" else icalc="No"
         icnt=icnt+1

         for j=kernelwidth,icol-1-kernelwidth do begin

            if (jcnt mod delp) eq 0 then jcalc="Yes" else jcalc="No"
            jcnt=jcnt+1

            ; build the polynomial Factors at the pixel
            if (icalc eq "Yes") and (jcalc eq "Yes") then begin
               poly=fltarr(nbasis)
               for m=0,degree do begin
                  for l=0,degree-m do begin
                     poly[*]=poly[*]+ckern[*,m,l] * i^m * j^l
                  endfor
               endfor
               l=kernelsize
               if (ibasis eq 'DELTA') then begin
                  if (iratio eq 'NO') then begin
;                     kernel=reform(poly,l,l)
                     kernel=transpose(reform(poly,l,l))
                  endif else begin
                     polytmp=poly
                     polytmp[0:(nbasis-1)/2]=shift(poly[0:(nbasis-1)/2],-1)
                     kernel=transpose(reform(polytmp,l,l))
                     kernel[(nbasis-1)/2]=kernel[(nbasis-1)/2]-total(poly[1:*])
                  endelse
                     
                  
               endif else begin   
                  poly=rebin(poly,nbasis,l,l,/SAMPLE)
      
                  ; build the kernel at the pixel
                  kernel=fltarr(l,l)
                  kernel[0:l-1,0:l-1]=total(poly*kbasis,1)
               endelse
            endif

            ; build the subimage of the reference (at the pixel)
            refsub=reference[i-kernelwidth:i+kernelwidth, $
                             j-kernelwidth:j+kernelwidth]

            ; store the convolution calculation
            creference[i,j]=total(refsub*kernel)

         endfor

      endfor

   endelse

   ; build the subtracted image
   subtraction=image-creference

end

;----------------------------------------------
; NAME:
;  ois_edgecheck
; PURPOSE:   (one line only)
;  Check a list of sub-array to make sure none overlap the edges of array
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_edgecheck,nx,ny,xc,yc,dw,bad
; INPUTS:
;  nx - X-size of array
;  ny - Y-size of array
;  xc - X-positions of sub-array center        (these should NOT be
;  yc - Y-positions of sub-array center         provided as floating point)
;  dw - half-width of sub-array (full width is 2*dw+1)
;  bad - Flag array, 0 - consider entry good, 1 - consider entry bad
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  bad - Flag array.  Anything that was good on input is marked bad if
;           the defined sub-array falls outside the array.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2008/06/11
;    This code was designed to replace a portion of the OIS system originally
;    written by J. Patrick Miller

pro ois_edgecheck,nx,ny,xc,yc,dw,bad

   z=where(bad eq 0 and $
           (xc-dw lt 0 or yc-dw lt 0 or xc+dw ge nx or yc+dw ge ny),count)
   if count ne 0 then bad[z] = 1

end

;----------------------------------------------
; NAME:
;  ois_filtpix
; PURPOSE:   (one line only)
;  Removes discontinuities in the subtracted image
; DESCRIPTION:
;  Takes in the subtracted image produced after the convolution and masks the 
;  discontinuities at the edges and unused or saturated pixels.  Bad pixels are
;  masked with mean sky values of the subtracted image.
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;   ois_filtpix,kernwd,image,mask,cimage,skymean,skysig
; INPUTS:
;  kernwd - the full width of the convolution kernel
;  image  - the subtracted image with blemishes
;  mask   - Array of the same size as image and ref that indicates something
;            about the pixel values.  
;             3 = pixel is flagged as "do not use"
;             4 = pixel is flagged as at or above saturation level
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SEED   - value for the seed used by the random call. Default is undefined
;             causing random to use the system time as the seed
; OUTPUTS:
;  cimage  - the final image with pixel mask applied
;  skymean - the mean sky value of the good pixels in image
;  skysig  - the standard deviation of the mean sky value
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY: 
;   Written by Harvey Mudd - SwRI clinic team, 2009/11/09
;   2009/12/15, MWB, modified to use library sky determinator
;   2010/03/13, FS, added SEED keyword

pro ois_filtpix,kernwd,image,mask,cimage,skymean,skysig,SEED=seed

   self='ois_filtpix: '
   if badpar(seed,[0,2,3,4,5],[0,1],caller=self+'(SEED) ') then return

   delw=(kernwd-1)/2
   sz=size(image,/dimensions)
   nx=sz[0]
   ny=sz[1]

   cimage=image 

   skysclim,cimage,lowval,hival,skymean,skysig,npts=30000,SEED=seed
    
   ; Set the flagged pixels to a background level (with noise)
   z=where(mask eq 3 or mask eq 4, count)

   if count ne 0 then cimage[z]=skysig*randomn(seed,count)+skymean

   ; Blot out a perimeter around the edge by replacing with background
   cimage[0:delw-1,*]    =skysig*randomn(seed,delw*ny)+skymean
   cimage[nx-delw:nx-1,*]=skysig*randomn(seed,delw*ny)+skymean
   cimage[*,0:delw-1]    =skysig*randomn(seed,delw*nx)+skymean
   cimage[*,ny-delw:ny-1]=skysig*randomn(seed,delw*nx)+skymean

end

;----------------------------------------------
; NAME:
;  ois_fluxcheck
; PURPOSE:   (one line only)
;  Filter a list of star images keep those with a good brightness
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_fluxcheck,flux,minflux,maxflux,bad
; INPUTS:
;  flux - integrated flux level for each star, sorted to put the brightest
;            first and faintest last.
;  minflux - minimum allowed peak brightness
;  maxflux - maximum allowed peak brightness
;  bad - Flag array, 0 - consider entry good, 1 - consider entry bad
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  bad - Flag array.  Anything that was good on input is marked bad if
;           image does not meet input criteria
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2008/06/11
;    This code was designed to replace a portion of the OIS system originally
;    written by J. Patrick Miller

pro ois_fluxcheck,flux,minflux,maxflux,bad

   ; first stamp is the brightest, reference the filters to this
   fmin = minflux*flux[0]
   fmax = maxflux*flux[0]
;print,'fmin,fmax ',fmin,fmax

   z=where(bad eq 0 and $
           (flux lt fmin or flux gt fmax),count)
   if count ne 0 then bad[z] = 1

end

;----------------------------------------------
; NAME:
;  ois_iterpoly
; PURPOSE:   (one line only)
;  Solve for the ideal kernel coefficients
; DESCRIPTION:
;  The functions solves for the optimal convolution kernel based on a selection
;  of stamps. First, the coefficients function is called which calculates
;  several required pieces for the optimization problem. Then, if chisqstat is
;  not set, it runs ois_varkern to solve for the kernel. If chisqstat
;  is set to a value (between 0 and 1), ois_varkern is called, the
;  reference stamps convolved with the calculated kernel and the result 
;  compared to the template image. If the chi-square statistic is sufficiently
;  high for a stamp, the stamp is kept. If it is too low, the stamp is 
;  discarded and the kernel solved for again. This process repeats until no 
;  more stamps are discarded.
;
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_iterpoly,iratio,fluxratio,img,ref,deg,kernsz,nbasis, $
;            kbasis,xstamp,ystamp,wstamp,a
; INPUTS:
;  iratio - flag specifying whether a constant photometric ratio should be 
;           assumed <str>
;  fluxratio - the photometric ratio between the reference and template image. 
;              <flt>
;  img - the template image. This image does not get changed. <flt>[row,col]
;  ref - the referenence image. This image gets convolved with the kernel to 
;        match the template image. <flt>,[row,col]
;  deg - the degree of the polynomial used for the space-varying kernel. <int>
;  kernsz - size of the square kernel used. <int>
;  nbasis - the total number of basis vectors used for the kernel. <int>
;  kbasis - the actual bases vectors for the kernel.
;           <flt>[nbasis,kernsz,kernsz]
;  xstamp - vector containing the x location of the stamps. <int>[pmax]
;  ystamp - vector containing the y location of the stamps. <int>[pmax]
;  wstamp - vector containing the width of the stamps. <int>[pmax]
;  chisqstat - the desired chi-square statistic for each stamp pair.<flt>
;  ibasis - string identifying which basis function is used for kernel.<str>
;           either 'DELTA' or 'GC'
;              
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag that will determine whether various relevant information will
;           be printed. Default=0. By default, it will print the statistics
; OUTPUTS:
;  chisqdist - 
;  ckern - kernel coefficient array. <flt>[nbasis,deg+1,deg+1]
;      The 3-d array has the kernel basis varying in the D1, the x coefficients
;      varying, in D2, and the y coefficients in D3.
;      i.e. a[5,3,1] is the coefficient for the 6th basis, with x^3*y
;      
; KEYWORD OUTPUT PARAMETERS:
;  FAILED - boolean indicating whether or not the subtraction failed
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2009/11/8, Originally written by J. Patrick Miller, Hardin-Simmons
;    University, December 2004.  This version is a re-write optimize
;    the code.  This version produced by Harvey Mudd College Clinic Team
;  2010/04/22, FS, ADDED SILENT, FAILED keywords
;  2010/10/12, MWB, added flux argument

pro ois_iterpoly, iratio,fluxratio,img,ref,deg,kernsz,nbasis, $
       kbasis,xstamp,ystamp,wstamp,flux,chisqstat,chisqdist,ibasis,ckern, $
       SILENT=silent, FAILED=failed

   self='ois_iterpoly: '
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                   default=0) then return

   ; Determine the image Sizes & Set the Stamp Width
   isize=size(img, /dimensions)
   nx=isize[0]
   ny=isize[1]

   ; Build the Stamp Coefficients

   ; Calculate the Chi^2=0 Case
   if chisqstat eq 0.0 then begin
      ois_coef,iratio,img,ref,deg,kernsz,nbasis,kbasis,xstamp,ystamp,$
            wstamp,ibasis,alpha,beta,gamma,pmax
      
      if not silent then $
         print,'   --> No post-kernel stamp filtering'
      done=1
      
      ; Derive the Optimal Kernel Coefficients
      ois_varkern,iratio,fluxratio,deg,nbasis,alpha,beta,gamma,pmax,ckern, $
               SILENT=silent

   endif else done=0

   ; Calculate the Chi^2>0 Case
   pass=0
   while not done do begin
      if not silent then $
         print,'   --> post-kernel stamp filtering, pass ',strn(pass)
      pass++
;print,'***--> Untested code.'  ; some testing has been done, could use more
  
      ois_coef,iratio,img,ref,deg,kernsz,nbasis,kbasis,xstamp,ystamp,$
            wstamp,ibasis,alpha,beta,gamma,pmax

      ; Build the Polynomial Coefficients
      ois_varkern, iratio,fluxratio,deg,nbasis,alpha,beta,gamma,pmax,ckern, $
               SILENT=silent
      ; Build the reference Stamps & Convolved reference Stamps
      cref=fltarr(nx,ny)
      for k=0,pmax-1 do begin

         ; Set the Center of the Stamp
         xctr=float(xstamp[k])
         yctr=float(ystamp[k])
         delw=wstamp[k]

         ; Build the reference Stamp
         rstamp=ref[xctr-delw:xctr+delw,yctr-delw:yctr+delw]

         ; Build the Space-Varying Kernel at the reference Stamp
         polyfactor=fltarr(nbasis)
         for m=0,deg do $
           for l=0,deg-m do $
              polyfactor=polyfactor+ckern[*,m,l]*xctr^m*yctr^l
                           
         if ibasis eq 'GC' then begin
            ; build the kernel at (kernelwidth,kernelwidth)
             kbasisr=reform(kbasis,[nbasis,kernsz^2])
             polykernel=reform(matrix_multiply(kbasisr,polyfactor,/atranspose),$
                               [kernsz,kernsz])
      
            ; convolve the reference
            crefstamp=convol(rstamp,polykernel,/center,/edge_zero)
            
            ;this is not complete and needs further work
         endif else begin
            crefstamp=fltarr(2*delw+1,2*delw+1)
            if iratio eq 'NO' then begin
              for i=0,kernsz-1 do begin
                for j=0,kernsz-1 do begin
                  crefstamp=crefstamp+polyfactor[i*kernsz+j]* $
                  sshift2d(rstamp,[(kernsz-1)/2-i,(kernsz-1)/2-j],/edge_zero)
                endfor
              endfor
            endif else begin
              polyfactor[0:(nbasis-1)/2]=shift(polyfactor[0:(nbasis-1)/2],-1)
              for i=0,kernsz-1 do begin
                for j=0,kernsz-1 do begin
                  crefstamp=crefstamp+polyfactor[i*kernsz+j]* $
                  sshift2d(rstamp,[(kernsz-1)/2-i,(kernsz-1)/2-j],/edge_zero)
                endfor
              endfor
              crefstamp=crefstamp-(total(polyfactor)-$
                         polyfactor[(nbasis-1)/2])*rstamp
            endelse
         endelse
          
         cref[xctr-delw:xctr+delw,yctr-delw:yctr+delw] = $
                       crefstamp[0:2*delw,0:2*delw]
      endfor

      chisqdist=ois_chidist(cref,img,xstamp,ystamp,wstamp)
      dimold=n_elements(chisqdist)
      index=where((chisqstat le chisqdist) and (chisqdist lt 1.0),dim)

      if dimold eq dim then begin
         done=1
      endif else begin
         if dim ne 0 then begin
            xstamp=xstamp[index]
            ystamp=ystamp[index]
            wstamp=wstamp[index]
            flux  =flux[index]
            pmax=dim
         endif else begin
            help,chisqstat,chisqdist
            print,chisqdist
            print,'All stamps were removed based on the chi-sq filtering'
            failed=1B
            return
         endelse
      endelse

   endwhile

   ; Cautionary Announcement
   q=(deg+1)*(deg+2)/2
   nstamps=n_elements(wstamp)
   if nstamps lt q then begin
      print,""
      print,"Too few stamps for a good space-varying kernel."
      print,"# of Stamps Used: ",nstamps
      print,"# of Stamps Needed: ",fix(q)
      print,""
   endif

end

;----------------------------------------------
; NAME:
;  ois_kernbas
; PURPOSE:   (one line only)
;  Creates the kernel basis vectors
; DESCRIPTION:
;  This procedure produces the gaussian kernel basis vectors. Since the delta
;  basis no longer requires an actual basis set, it is no longer included in
;  this procedure.
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_kernbas,iratio,gcinfo,kernsz,nbasis,kbasis
; INPUTS:
;  iratio      -  Constant photometric ratio flag
;  gcinfo      -  Gaussian components information (structure)
;  kernsz      -  Kernel size
;  
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag that will determine whether various relevant information will
;           be printed. Default=0. By default, it will print the statistics
; OUTPUTS:
;  nbasis      -  total number of basis used for the 
;  kbasis      -  3 element float array containing the basis matrices of the
;                 kernel.
;  
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2009/11/22, Originally written by J. Patrick Miller, Hardin-Simmons
;    University, December 2004.  This version is a re-write optimize
;    of the code.  This version produced by SwRI Clinic Team, 
;    Harvey Mudd College.

pro ois_kernbas,iratio,gcinfo,kernsz,nbasis,kbasis,SILENT=silent

   ; Define the Gaussian Components Basis vectors
   self='ois_kernbas: '
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                   default=0) then return

    if not silent then $
      print,'   --> Gaussian basis vector set chosen.'

    sigkernel=(float(kernsz)-5.0)/8.0
    sigmas=sigkernel*gcinfo.sigmas
    nbasis=long(total((gcinfo.degree+1)*(gcinfo.degree+2))/2+1)
    kbasis=fltarr(nbasis,kernsz,kernsz)
    center=(kernsz-1)/2

    index=indgen(kernsz,kernsz)
    x=float(abs(index mod kernsz-center))
    y=float(abs(index/kernsz-center))
    start=0
    for k=0,gcinfo.nc-1 do begin
      for u=0,gcinfo.degree[k]   do begin
        for v=0,gcinfo.degree[k]-u do begin  
          kbasis[start,*,*]=x^u*y^v*exp(-(x^2+y^2)/(2.0*sigmas[k]^2))
          start++
        endfor
      endfor
    endfor
    ; centered Delta Function Basis vector
    kbasis[nbasis-1,center,center]=1.0

   if iratio eq "YES" then begin

      ; Reorganize the Basis vectors (centered Delta Function set to
      ;   First vector)
      ktemp=fltarr(nbasis,kernsz,kernsz)
      ktemp=shift(kbasis,[1,0,0])

      ; Normalize the Basis vectors
      sum=fltarr(nbasis)
      for k=0,nbasis-1 do $
         sum[k]=total(ktemp[k,*,*])
      for k=0,nbasis-1 do $
         if sum[k] ne 0 then $
            ktemp[k,*,*]=ktemp[k,*,*]/sum[k]

      ; Define the new Basis
      kbasis[0,*,*]=ktemp[0,*,*]
      for k=1,nbasis-1 do $
         if sum[k] ne 0 then $
            kbasis[k,*,*]=ktemp[k,*,*]-ktemp[0,*,*] $
         else $
            kbasis[k,*,*]=ktemp[k,*,*]

   endif

end

;----------------------------------------------
; NAME:
;  ois_overlapcheck
; PURPOSE:   (one line only)
;  Remove overlapping stars
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_overlapcheck,nx,ny,xc,yc,dw,bad
; INPUTS:
;  nx - X-size of array
;  ny - Y-size of array
;  xc - X-positions of sub-array center        (these should NOT be
;  yc - Y-positions of sub-array center         provided as floating point)
;  dw - half-width of sub-array (full width is 2*dw+1)
;  bad - Flag array, 0 - consider entry good, 1 - consider entry bad
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MASK - Optional mask array that matches the size of image.  If pixel is to
;            be used the value in the mask array is set to 0.  If pixel is not
;            to be used the value is set to 1 (or greater).
; OUTPUTS:
;  bad - Flag array.  Anything that was good on input is marked bad if
;           image does not meet input criteria
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This routine expects that all sub-arrays have been checked to make sure
;    none fall outside array boundaries.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2008/06/11 - Written by Marc W. Buie, Southwest Research Institute
;    This code was designed to replace a portion of the OIS system originally
;    written by J. Patrick Miller

pro ois_overlapcheck,nx,ny,xc,yc,dw,bad,MASK=in_mask

   self='ois_overlapcheck: '
;   if badpar(nx,[2,3],0,caller=self+'(nx) ') then return
;   if badpar(ny,[2,3],0,caller=self+'(ny) ') then return
;   if badpar(xc,[2,3],1,caller=self+'(xc) ') then return
;   if badpar(yc,[2,3],1,caller=self+'(yc) ') then return
;   if badpar(dw,[2,3],1,caller=self+'(dw) ') then return
;   if badpar(bad,[1,2,3],1,caller=self+'(bad) ') then return
;   if badpar(in_mask,[0,1,2,3],2,caller=self+'(MASK) ') then return

   ; this image will record if a pixel is within a sub-array that we've
   ;   already marked good.
   mask = bytarr(nx,ny)

   ; check input mask
   info=size(in_mask,/structure)
   if info.type ne 0 then begin
      if info.dimensions[0] eq nx and info.dimensions[1] eq ny then begin
         mask += in_mask
      endif else begin
         print,self,' Error! input mask does not match nx,ny.  Ignoring mask.'
      endelse
   endif

   z=where(bad eq 0, count)

   for i=0,count-1 do begin

      i0=xc[z[i]]-dw[z[i]]
      i1=xc[z[i]]+dw[z[i]]
      j0=yc[z[i]]-dw[z[i]]
      j1=yc[z[i]]+dw[z[i]]

      ; sub-array is in the clear
      if max(mask[i0:i1,j0:j1]) eq 0B then begin
         mask[i0:i1,j0:j1] += 1B
      ; sub-array overlaps an area already used
      endif else begin
         bad[z[i]] = 1B
      endelse

   endfor

end

;----------------------------------------------
; NAME:
;  ois_removesky
; PURPOSE:   (one line only)
;  Remove mean sky level from image and update bad pixel mask 
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_removesky,img,maxphotsig,notused,mask,skyi,sigi
; INPUTS:
;  img      - input image to process
;  maxphotsig - saturation signal level for image,
;  notused  - image values used to represent a pixel that should not be used.
;  mask     - Array of the same size as image and ref that indicates something
;               about the pixel values.
;               0 = normal pixel, use it as you wish
;               1 = pixel is outside of sigma bounds
;               2 = 
;               3 = pixel is flagged as "do not use"
;               4 = pixel is flagged as at or above saturation level
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SKY    - Set this parameter when you want the sky value to be passed 
;             externally. Can be either a scalar or an array the size of img.
;  SILENT - Flag that will determine whether various relevant information will
;             be printed. Default=0. By default, it will print the statistics
;  SEED   - value for the seed used by the random call. Default is undefined
;             causing random to use the system time as the seed
; OUTPUTS:
;  img   - Image with sky subtracted
;  mask  - Modified mask
;  skyi  - Mean sky signal in image
;  sigi  - Sigma of sky signal in image
; KEYWORD OUTPUT PARAMETERS:
;  FAILED - boolean indicating whether or not the subtraction failed
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by J. Patrick Miller
;  2008/07/25, Marc W. Buie, Southwest Research Institute
;    Major modifications to increase performance
;    modified 2009/11/09 SwRI clinic team
;  2009/12/17, MWB, modified to make it more general
;  2010/04/22, FS, added SKY, SILENT, SEED, FAILED keywords

pro ois_removesky,img,maxphotsig,notused,mask,skyi,sigi,SKY=sky, $
                  SILENT=silent,SEED=seed,FAILED=failed

   self='ois_removesky: '
   if badpar(img,[2,3,4,5],2,caller=self+'(img) ') then return
   if badpar(maxphotsig,[2,3,4,5],0,caller=self+'(maxphotsig) ') then return
   if badpar(notused,[2,3,4,5],0,caller=self+'(notused) ') then return
   if badpar(mask,[1,2,3],2,caller=self+'(mask) ') then return
   if badpar(sky,[0,2,3,4,5],[0,2],caller=self+'(SKY) ', $
                                   type=skytype, rank=skyrank) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                   default=0) then return                                
   if badpar(seed,[0,2,3,4,5],[0,1],caller=self+'(SEED) ') then return
   
   ; flag saturated pixels
   z=where(img ge maxphotsig,count)
   if count ne 0 then mask[z]=4B
   if not silent then $
      print,'   --> ',strn(count),' saturated pixels flagged.'

   ; flag pixels not to be used
   z=where(img eq notused,count)
   if count ne 0 then mask[z]=3B
   if not silent then $
      print,'   --> ',strn(count),' pixels flagged as unusable.'
   
   ; Calculate the mean sky & sigma
   skysclim,img,lowval,hival,skyi,sigi,npts=60000,SEED=seed

   if skytype eq 0 then begin
      img = img - skyi
   endif else if skyrank eq 0 then begin
      img = img - sky
   endif else begin
      sz1=size(img,/dimension)
      sz2=size(sky,/dimension)
      if sz1[0] eq sz2[0] and sz1[1] eq sz2[1] then begin
         img = img - sky
      endif else begin
         print, 'Sky Array is incorrectly set.'
         failed=1B
         return
      endelse
   endelse

   ; Replaced the bad pixels with background (with noise)
   z=where(mask gt 0B, count)
   if count ne 0 then begin
      img[z]=sigi*randomn(seed,count)
   endif

end

;----------------------------------------------
; NAME:
;  ois_stamps
; PURPOSE:   (one line only)
;  Collect a set of clean stars from image to be used for PSF references
; DESCRIPTION:
;  STAMP - Definition - This term refers to a localized section of the array
;    that contains an image of point-source.  This could either be an
;    actual sub-array or a description of a sub-array in an larger image.
;
;  This routine will take an input image and return a list of stamps
;    described by a position within the array and a size of the region to
;    be used.
;
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_stamps,image,fwhm,thresh, $
;       fluxfrac,kernsz,xstamp,ystamp,wstamp,mask
; INPUTS:
;  image       -  image array from which to get PSF references
;  refer       -  Reference image array
;  fwhm        -  Estimated full-width, half-maximum for the sources (pixels)
;  thresh      -  Threshold for detection, a source must be this many sigma
;                    above sky to be considered (recommend 3 or 4).
;  fluxfrac    -  Range of flux values allowed relative to the brightest
;                    source found in image.   (typically 0.25)
;  mask - Mask array that matches the size of image.  If pixel is to be used
;            the value in the mask array is set to 0.  If pixel is not to be
;            used the value is set to 1.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag that will determine whether various relevant information will
;             be printed. Default=0. By default, it will print the statistics
;  MAXPHOTSIG - Maximum DN value for a useful signal.  Any source with a peak
;                above this level is passed over.  Default=(see findsrc.pro)
;                This is used ONLY on the image (not the reference).
; OUTPUTS:
;  kernsz      -  Kernel size (minimum Value of 7, max of 13)
;  xstamp      -  X co-ordinate of the stamp's centroid (integer)
;  ystamp      -  Y co-ordinate of the stamp's centroid (integer)
;  wstamp      -  Half-width of the centroid (integer)
;                    [Stamp dimensions = (2Wstamp+1)x(2Wstamp+1)]
;  outfluxrat  - Flux ratio of the image/reference
; KEYWORD OUTPUT PARAMETERS:
;  FAILED - boolean indicating whether or not there were sufficient stamps
;           found
;  FLUX   - Flux of the surviving keywords.  This is a proper aperture
;           photometry flux of the objects as derived from the findsrc call.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2008/06/20, Originally written by J. Patrick Miller, Hardin-Simmons
;    University, December 2004.
;  2008/07/25, MWB, re-write to optimize
;  2010/04/21, FS, added SILENT, FAILED keyword
;  2010/09/23, MWB, added MAXPHOTSIG keyword
;  2010/10/12, MWB, added flux output keyword
;  2010/10/22, MWB, changed objrad in findsrc call

pro ois_stamps,image,refer,in_fwhm,thresh, $
       fluxfrac,kernsz,xstamp,ystamp,wstamp,mask,outfluxrat,SILENT=silent, $
       FAILED=failed,MAXPHOTSIG=maxphotsig,FLUX=flux,NODISPLAY=nodisplay, $
       MEANFWHM=meanfwhm,MEANREFFWHM=meanreffwhm

   self='ois_stamps: '
   if badpar(image,[2,3,4,5,12],2,caller=self+'(image) ') then return
   if badpar(refer,[2,3,4,5,12],2,caller=self+'(refer) ') then return
   if badpar(in_fwhm,[2,3,4,5],0,caller=self+'(fwhm) ') then return
   if badpar(thresh,[4,5],0,caller=self+'(thresh) ') then return
   if badpar(fluxfrac,[4,5],0,caller=self+'(fluxfrac) ') then return
   if badpar(mask,[1,2,3],2,caller=self+'(mask) ') then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return
   if badpar(nodisplay,[0,1,2,3],0,caller=self+'(NODISPLAY) ',default=0) then return
   if badpar(maxphotsig,[0,2,3,4,5],0,caller=self+'(MAXPHOTSIG) ') then return

   ; Get the dimensions of the image
   sz=size(image,/dimensions)
   nx=sz[0]
   ny=sz[1]

   ; There is some tricky stuff going on in here.  The fwhm provided on input
   ;   is at best only a reasonable guess.  What flows from here needs to be
   ;   relatively insensitive to this guess.  As long as fwhm is not a lot
   ;   smaller than the real value this should be ok.  This initial step will
   ;   find every source in the image (not reference) that has a SNR greater
   ;   than the input threshold.  The results from this call that matter are
   ;   the positions of the sources and the FWHM.  The flux is used just to
   ;   sort the sources by brightness.
   if not silent then begin
      print,'   --> objrad for findsrc call is ',in_fwhm
   endif
   findsrc,image,gap=round(in_fwhm),/nodisplay,maxphotsig=maxphotsig, $
      objrad=in_fwhm,results=results,sigthresh=thresh,/silent

   ; protect against no useful results
   if size(results,/type) ne 8 then begin
      print,'   --> findsrc call failed'
      failed=1B
      return
   endif
   if results.nobj eq 0 then begin
      print,'   --> findsrc call failed, no sources found'
      failed=1B
      return
   endif

   ; save bits of the results that need to be worked with
   xcen = results.xc
   ycen = results.yc
   flux = results.flux
   fwhm = results.fwhm
   nsrc = results.nobj

   if not silent then begin
      print,'   --> ',strn(results.nobj),' stamps at the start, average FWHM', $
         results.avgfwhm,' pixels',format='(a,a,a,f5.2,a)'
   endif
   
   ; setup a filtering array to manage the list, everything starts out good.
   bad = bytarr(nsrc)

   ; toss those with fwhm that are too small
   zb = where(fwhm lt 1.8,countzb)
   if countzb ne 0 then bad[zb]=1B
   fwhmbadtot = countzb

   ; compute a robust mean on fwhm, this will also weed out anything more than
   ;    2 sigma from the returned mean
   robomean,fwhm,2.0,0.5,meanfwhm,bad=bad
   zb = where(bad eq 1,countzb)
   fwhmofftot = countzb - fwhmbadtot

   ; For this next part of the filtering we must sort the sources in order
   ;   of descending flux (we can leave out the bad ones found so far).
   zg = where(bad eq 0B,nsrc)
   idx=reverse(sort(flux[zg]))
   bad=bytarr(nsrc)
   xcen=xcen[zg[idx]]
   ycen=ycen[zg[idx]]

   ; setup the stamp variables
   xstamp=round(xcen)
   ystamp=round(ycen)
   wstamp=replicate(ceil(2.0*meanfwhm),nsrc)

   ; compute fwhm of reference and use to filter out stamps
   ; The next step is very important.  Accurate aperture photometry is generated
   ;   for all the sources surviving to this point.  This is done on both the
   ;   image and the reference.  The aperture is set to a larger value to get an
   ;   accurate flux and fwhm measurement.  These are both critical results for
   ;   what is to follow.  The positions, at this point, are considered perfect
   ;   and the images are assumed to already have the sky subtracted from them.
   if not silent then begin
      print,'   --> objrad for basphote call is ',2*meanfwhm
   endif
   basphote,1.0,image,1.0,xcen,ycen,2*meanfwhm,0.0,-0.0001,fwhm=fwhm, $
      flux=flux,/nolog,/silent,/exact
   basphote,1.0,refer,1.0,xcen,ycen,2*meanfwhm,0.0,-0.0001,fwhm=reffwhm, $
      flux=refflux,/nolog,/silent,/exact

   ; Filter these results by FWHM on both image and reference, again.  This
   ;   overwrites the mean fwhm for the image (should be better still).
   robomean,fwhm,2.0,0.5,meanfwhm,bad=bad
   robomean,reffwhm,2.0,0.5,meanreffwhm,bad=bad
   nrefbad = long(total(bad))
   if not silent then begin
      print,'   --> initial mean FWHM of image    ',meanfwhm
      print,'   --> initial mean FWHM of reference',meanreffwhm
   endif

   ; determine kernel size.  There is some serious voodoo here.  The old
   ;   code section takes exactly what Miller originally coded.  Other
   ;   variants appear here but may well be commented out.  Someday this
   ;   will be cleaned up...

   ; old code
   fwhmdiff = abs(meanfwhm-meanreffwhm)
   sigkernel = fwhmdiff/2.0*sqrt(alog(2.0)) ; convert to 1/e half-width
   ; original from JPM
   halfl=(fix(4.0*ceil(sigkernel)+2.0) > 3) < 26

;print,'***--> original halfl',halfl
   ; new guess from MWB
;   halfl = fix(ceil(meanfwhm^1.1)) ; heuristic guess
   ; tweak for testing
 ;   halfl = halfl-4
 ;   halfl=halfl+2
;print,'***--> new halfl',halfl

   kernsz = 2*halfl+1

   ; Adjust the Stamp Sizes to Fit the Kernel
   ; width must be at least one larger than halfl
   z=where(wstamp le halfl,count)
   if count ne 0 then wstamp[z] = halfl+1

   ; Apply a bunch of filters to the stamps and accumulate and pass forward
   ;   all of the bad flags
   ois_edgecheck,nx,ny,xstamp,ystamp,wstamp,bad
   edgebad = long(total(bad))-nrefbad
   ois_fluxcheck,flux,fluxfrac,1.00,bad
   fluxbad = long(total(bad))-edgebad-nrefbad
   ois_overlapcheck,nx,ny,xstamp,ystamp,wstamp,bad,MASK=mask
   overlapbad = long(total(bad))-fluxbad-edgebad-nrefbad

   fluxratio = flux/refflux
   robomean,fluxratio,2.0,0.5,meanfluxratio,dummy,meanfluxratiosig, $
      bad=bad,stdmean=frsm
   fluxratbad = long(total(bad))-fluxbad-edgebad-overlapbad-nrefbad

   totalbad = results.nobj-nsrc + long(total(bad))

   if not silent then begin
      print,'   --> ', $
            'RB:',strn(nrefbad), $
            ' FWHM<1.8:',strn(fwhmbadtot), $
            ' FWHMsig:',strn(fwhmofftot), $
            ' EDGE:',strn(edgebad), $
            ' FLUX:',strn(fluxbad), $
            ' LAP:',strn(overlapbad), $
            ' FR:',strn(fluxratbad)
   endif
   zb = where(bad eq 1,countzb)

   z=where(bad eq 0,count)
   if count eq 0 then begin 
      failed=1B
      print,'No stamps could be found.'
   endif else begin

      xstamp = xstamp[z]
      ystamp = ystamp[z]
      wstamp = wstamp[z]
      flux   = flux[z]
      reffwhm= reffwhm[z]

      outfluxrat = meanfluxratio

      if not silent then begin
         print,'   --> FWHM of   image   stamps after cleanup',mean(fwhm)
         print,'   --> FWHM of reference stamps after cleanup',mean(reffwhm)
         print,'   --> stamps flux ratio ',meanfluxratio,' +/- ', $
               meanfluxratiosig,'  (',frsm,')'
      endif
      if not nodisplay then begin
         setwin,10,xsize=512,ysize=512
         plot,flux[z],fluxratio[z],psym=8, $
            xtitle='Stamp flux (DN)',ytitle='I/R flux ratio'
         if countzb ne 0 then $
            oplot,flux[zb],fluxratio[zb],psym=8,color='0000ff'xl
      endif

   endelse

end

;----------------------------------------------
; NAME:
;  stampimg
; PURPOSE:   (one line only)
;  Compute statistics for all stamps in the image and convolved reference
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  stampimg,image,creference,xstamp,ystamp,wstamp,itocr,chisq
; INPUTS:
;  image       -  the template image array 
;  creference  -  the convolved reference image array
;  xstamp      -  array of X co-ordinate of the stamp's centroid (integer)
;  ystamp      -  array of Y co-ordinate of the stamp's centroid (integer)
;  wstamp      -  array of Half-width of the centroid (integer)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  itocr       -  ratio of the total of template to conv. ref. stamps
;  chisq       - Goodness of fit between image and creference for each stamp
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2009/12/2 SwRI clinic team

pro ois_stampstats,image,creference,xstamp,ystamp,wstamp,itocr,chisq

   nstamps=n_elements(wstamp) 
   itocr=fltarr(nstamps) 
   chisq=fltarr(nstamps)
    
   for i=0,nstamps-1 do begin

      i0 = xstamp[i]-wstamp[i]
      i1 = xstamp[i]+wstamp[i]
      j0 = ystamp[i]-wstamp[i]
      j1 = ystamp[i]+wstamp[i]

		itocr[i]=total(image[i0:i1,j0:j1])/total(creference[i0:i1,j0:j1])
		chisq[i]=total( (image[i0:i1,j0:j1]-creference[i0:i1,j0:j1])^2 )

   endfor

end

;----------------------------------------------
; NAME:
;  ois_varkern
; PURPOSE:   (one line only)
;  Solves the ideal coefficients for the spacevarying kernel
; DESCRIPTION:
;  This functions solves the Ma=b problem for solving the spacevarying kernel
;  First, it generates M (Matrix) and b (b)
;
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  ois_varkern,iratio,fluxratio,deg,nbasis,alpha,beta,gamma,pmax,ckern
; INPUTS:
;  iratio - flag specifying whether a constant photometric ratio should be 
;           assumed <str>
;  fluxratio - the photometric ratio between the reference and template
;                 image. <flt>
;  deg - the degree of the polynomial used for the space-varying kernel. <int>
;  nbasis - the total number of basis vectors used for the kernel. <int>
;  alpha - this 3D array contains the x^u*y^v terms for each stamp
;          the first index corresponds to the stamp
;          the second index corresponds to the x terms
;          the third term corresponds to the y terms
;          both are increasing in exponent as it decreases
;          alpha[3,2,5]- x^2*y^5 for the 3rd stamp
;  beta - this 3D array contains the 
;          total((R_k x K_n)*(R_k x K_q) terms
;          beta[k,n,q]- the convolution of the kth stamp with the nth kernel
;                        times the convolution of the stamp with the qth kernel
;                        summed up over the stamp
;  gamma - this 2D array contains the 
;          total(I_k*(R_k x K_q))
;          gamma[k,q]- the convolution of the kth stamp with the qth kernel
;                      times the kth stamp of the template 
;  pmax - total number of stamps
;              
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag that will determine whether various relevant information will
;           be printed. Default=0. By default, it will print the statistics
; OUTPUTS:
; ckern - 3D array of coefficients. The 1st dimension corresponds to the kernel
;     basis, the 2nd dimension corresponds to the x exponent increasing, 
;     and 3rd dimension corresponds to the y exponent increasing.
;     a[n,m,l]- the x^m*y^l coefficient for the nth basis.
;      
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2009/11/8, Originally written by J. Patrick Miller, Hardin-Simmons
;    University, December 2004.  This version is a re-write optimize
;    the code.  This version produced by Harvey Mudd College Clinic Team

pro ois_varkern,iratio,fluxratio,deg,nbasis,alpha,beta,gamma,pmax,ckern, $
                SILENT=silent

   self='ois_varkern: '
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                   default=0) then return

   ; Define the Arrays
   ckern=dblarr(nbasis,deg+1,deg+1)

   ; Set the Constant Photometric Ratio Flag
   start=0
   if iratio eq "YES" then begin
      if not silent then $
         print,'   --> Constant photometric ratio flag turned on.'
      start=1
      ckern[0,0,0]=fluxratio
   endif else begin
      if not silent then $,
         print,'   --> Constant photometric ratio flag is not on.'
   endelse

   ; Define the Arrays
   nterms=(deg+1)*(deg+2)/2
   nsize=(nbasis-start)*nterms
   matrix=fltarr(nsize,nsize)
   btmp=fltarr(nsize)

   ; Build the Coefficient matrix & RHS Vector
   indterm=nterms^2*(nbasis-start)
   index=lonarr((nbasis-start)^2,nterms^2)
   for i=0,nterms-1 do $
    for j=0,nterms-1 do $
      for k=0,(nbasis-1-start) do $
         index[k*(nbasis-start):(k+1)*(nbasis-start)-1,i*nterms+j]= $
            lindgen(nbasis-start)*indterm+nterms*k+nsize*i+j
            
   alptmp1=fltarr(pmax,nterms^2)
   icnt=0
   for r=0,deg do $
      for s=0,deg-r do $
         for m=0,deg do $
            for l=0,deg-m do begin
               alptmp1[*,icnt]=alpha[*,m,l]*alpha[*,r,s]
               icnt++
            endfor
   
   matrix[index]=matrix_multiply(reform(beta[*,start:*,start:*], $
                                       [pmax,(nbasis-start)^2]),$
                                             alptmp1,/atranspose)
   
   alptmp2=fltarr(pmax,nterms) 
   icnt=0      
   for r=0,deg do $
     for s=0,deg-r do begin
       alptmp2[*,icnt]=alpha[*,r,s]
       icnt++
     endfor
   
   btmp=matrix_multiply(gamma[*,start:*]-start*fluxratio*trimrank( $ 
                        beta[*,0,start:*]),alptmp2,/atranspose)  
   btmp=reform(transpose(btmp),n_elements(btmp))

   ; Calculate the Polynomial Coefficients of the Optimal Kernel
   ludc,matrix,ind,/double
   atmp=lusol(matrix,ind,btmp,/double)
   
   ; Reformat the Coefficients
   icnt=0
   for n=start,nbasis-1 do begin
      jcnt=0
      for m=0,deg do $
         for l=0,deg-m do begin
            ckern[n,m,l]=atmp[icnt+jcnt]
            jcnt=jcnt+1
         endfor
      icnt=icnt+nterms
   endfor

end

;----------------------------------------------
pro ois,image,reference,diffimage,PATH=path,IMGHDR=in_imghdr, $
       MINFLUX=minflux,FWHM=fwhm,DEGREE=degree,DELP=delp, $
       GAUSSIAN=gcinfo,CONSTPHOT=constphot,CHISQSTAT=chisqstat, $
       SAVEIT=saveit,OUTPATH=outpath,IMGSKY=imgsky,REFSKY=refsky, $
       MAXPHOTSIG=maxphotsig,SILENT=silent,NODISPLAY=nodisplay,SEED=seed, $
       CREFERENCE=creference,FAILED=failed,DIFFINFO=diffinfo
   
   failed=0B
   self='OIS: '
   if badpar(image,[2,3,4,5,7],[0,2],caller=self+'(image) ', $
                type=imagetype) then return
   if badpar(reference,[2,3,4,5,7],[0,2],caller=self+'(reference) ', $
                type=reftype) then return
   if badpar(maxphotsig,[0,2,3,4,5],0,caller=self+'(MAXPHOTSIG) ', $
                                   default=60000.0) then return
   if badpar(minflux,[0,2,3,4,5],0,caller=self+'(MINFLUX) ', $
                                   default=0.25) then return
   if badpar(degree,[0,2,3],0,caller=self+'(DEGREE) ', $
                                   default=0) then return
   if badpar(delp,[0,2,3],0,caller=self+'(DELP) ', $
                                   default=1) then return
   if badpar(path,[0,7],0,caller=self+'(PATH) ',default='') then return
   if path ne '' then path=addslash(path)
   if badpar(outpath,[0,7],0,caller=self+'(OUTPATH) ',default='') then return
   if outpath ne '' then outpath=addslash(outpath)
   if badpar(gcinfo,[0,7,8],[0,1],caller=self+'(GAUSSIAN) ', $
                                  type=gcinfotype) then return
   if badpar(constphot,[0,1,2,3],0,caller=self+'(CONSTPHOT) ', $
                                   default=0) then return
   if badpar(chisqstat,[0,2,3,4,5],0,caller=self+'(CHISQSTAT) ', $
                                   default=0.0) then return
   if badpar(saveit,[0,1,2,3],0,caller=self+'(SAVEIT) ', $
                                   default=0) then return
   if badpar(imgsky,[0,2,3,4,5],[0,2],caller=self+'(SKY) ', $
                                   type=imgskytype) then return
   if badpar(refsky,[0,2,3,4,5],[0,2],caller=self+'(SKY) ', $
                                   type=refskytype) then return
   if badpar(fwhm,[0,2,3,4,5,7],0,caller=self+'(FWHM) ', $
                                   type=fwhmtype) then return
   if badpar(in_imghdr,[0,7],1,caller=self+'(IMGHDR) ', $
                                   type=hdrtype) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                   default=0) then return
   if badpar(nodisplay,[0,1,2,3],0,caller=self+'(NODISPLAY) ', $
                                   default=0) then return
   if badpar(seed,[0,2,3,4,5],[0,1],caller=self+'(SEED) ') then return
   if not silent then time1=systime(1)
   
   if outpath eq path then saveit=0

   if constphot then iratio='YES' else iratio='NO'

   if gcinfotype eq 0 then begin
      ibasis = 'DELTA'
   endif else if gcinfotype eq 7 then begin
      ibasis = 'GC'
      if strupcase(gcinfo) eq 'ASTIER' then begin
         if not silent then print,' ----> Astier gaussian components selected'
         gcinfo={nc:3, degree:[6,4,2], sigmas:[0.7,1.5,2.0]}
      endif else begin
         print,'Gaussian component set [',gcinfo,'] is not recognized.'
         return
      endelse
   endif else begin
      ibasis = 'GC'
      if not silent then print,' ----> Manual gaussian components selected'
   endelse

   if imagetype eq 7 then begin
      if not exists(path+image) then begin
         print,self,'Image ',path+image,' not found.'
         return
      endif
      fits_read, path+image,img,imghdr 
   endif else begin
      saveit=0 ; disabled for array input mode
      img = image
   endelse
   
   if fwhmtype eq 0 then begin
      if hdrtype ne 0 then imghdr=in_imghdr
      fwhm=sxpar(imghdr,'SEEING') 
      if fwhm le 1.0 then begin
         print, 'FWHM not set properly.'
         return
      endif
   endif

   if reftype eq 7 then begin
      if not exists(path+reference) then begin
         print,self,'Reference image ',path+reference,' not found.'
         return
      endif
      fits_read, path+reference,ref
   endif else begin
      ref = reference
   endelse

   ; Identify the Not Used Pixel Values
   notused=fltarr(2)
   notused[0] = min(img)-10000.0
   notused[1] = min(ref)-10000.0

   ; Make a working copy of the input images so they don't get modified.
   wimg  = img
   refer = ref

   ; Setup the mask array
   sz=size(wimg,/dimensions)
   mask=bytarr(sz[0],sz[1])

   ; remove sky and flag bad pixels
   ois_removesky,wimg,maxphotsig,notused[0],mask,skyi,sigi,SKY=imgsky, $
                 SILENT=silent,SEED=seed,FAILED=failed
   
   ois_removesky,refer,maxphotsig,notused[1],mask,skyr,sigr,SKY=refsky, $
                 SILENT=silent,SEED=seed,FAILED=failed
   
   if failed then return
   
   if not silent then begin
      print,' ----> sky signal in image    ',skyi,' +/- ',sigi
      print,' ----> sky signal in reference',skyr,' +/- ',sigr
   endif

   ; Build the Stamps and Kernel Size (HalfL=4*SigKernel+2; L=2*HalfL+1)
   ois_stamps,wimg,refer,fwhm,15.0,minflux,kernsz,xstamp,ystamp,wstamp,mask, $
              fluxratio,silent=silent,failed=failed, $
              maxphotsig=maxphotsig-skyi,flux=sflux,nodisplay=nodisplay, $
              meanfwhm=meanfwhm,meanreffwhm=meanreffwhm
   if failed then return
   nstamps=n_elements(xstamp)
   if not silent then $
      print,' ----> ',strn(nstamps),' stamps survived from image'

;   ; Calculate the Constant Photometric Ratio
;   ois_photorat,wimg,refer,xstamp,ystamp,wstamp,sflux,fluxratio,SILENT=silent, $
;                FAILED=failed
;   if failed then return
   if not silent then $
      print,' ----> ','Flux ratio image/template = ',fluxratio
   
   if not nodisplay then begin
      ; Display the image with stamp selection overlay
      showsrc,wimg,window=1,lowsig=-1.0,hisig=10.0,lowval=lowval,hival=hival
      oplot,xstamp,ystamp,psym=4,color='7000ff'xl,symsize=2.5
      xyouts,10,10,'Image',color='7000ff'xl,charsize=2.0
      showsrc,refer,window=2,lowsig=-1.0,hisig=10.0
      oplot,xstamp,ystamp,psym=4,color='7000ff'xl,symsize=2.5
      xyouts,10,10,'Reference',color='7000ff'xl,charsize=2.0
   endif

   if not silent then print,' ----> ','Stamp size range: ',minmax(wstamp)

   ; Build the Kernel Basis
   if ibasis eq 'GC' then begin
      ois_kernbas, iratio,gcinfo,kernsz,nbasis,kbasis,SILENT=silent
   endif else begin
      nbasis=long(kernsz^2)
      if not silent then print,'   --> Delta basis vector set chosen.'
   endelse

   ; Iterate the Kernel Calculation to Find the Best Stamps
   ois_iterpoly,iratio,fluxratio,wimg,refer,degree,kernsz,nbasis,kbasis, $
      xstamp,ystamp,wstamp,sflux,chisqstat,chisqdist,ibasis,ckern, $
      SILENT=silent,FAILED=failed
   nstamps=n_elements(xstamp)
      
   if not nodisplay then begin
      ; Display same stamps from the reference
      nsx=5L
      nsy=5
      swid=min(wstamp)*2+1
      mwid=min(wstamp)
      looplim = nstamps < nsx*nsy
      if not silent then $
         print,'= = => Display ',strn(looplim),' stamps (out of ', $
               strn(nstamps),') -- Image in 3, reference in 4'
      setwin,3,xsize=swid*nsx,ysize=swid*nsy
      erase
      for i=0,looplim-1 do begin
         sub=wimg[xstamp[i]-mwid:xstamp[i]+mwid,ystamp[i]-mwid:ystamp[i]+mwid]
         tvscl,sub,i
;print,'img',i,median(sub[*,0]),median(sub[*,-1])
      endfor
      setwin,4,xsize=swid*nsx,ysize=swid*nsy
      erase
      for i=0,looplim-1 do begin
         sub=refer[xstamp[i]-mwid:xstamp[i]+mwid,ystamp[i]-mwid:ystamp[i]+mwid]
         tvscl,sub,i
;print,'ref',i,median(sub[*,0]),median(sub[*,-1])
      endfor
   endif

   if failed then return

   ; Produce the Convolved reference and Subtraction of the Original Images
   ois_convref,nbasis,kbasis,kernsz,degree,ckern,wimg,refer, $
      creference,subtraction,kernel,delp,poly,ibasis,iratio,$
      SILENT=silent,NODISPLAY=nodisplay

;Marc: I commented out the kernel display here b/c it does add real info,
;since the kernel varies across the image. This would only print out the
;info for the very last kernel that was generated.
   if not silent then begin
      print,' ----> total number of stamps used ',strn(n_elements(xstamp))
      print,' ----> kernel size ',strn(kernsz), $
;            ', integral: ',total(Kernel), $ 
            ', Flux ratio (I/R): ',fluxratio
   endif

   if not nodisplay then begin
      ; Display same stamps from the convolved reference
      nsx=5L
      nsy=5
      swid=min(wstamp)*2+1
      mwid=min(wstamp)
      setwin,5,xsize=swid*nsx,ysize=swid*nsy
      erase
      looplim = nstamps < nsx*nsy
      for i=0,looplim-1 do begin
         sub=creference[xstamp[i]-mwid:xstamp[i]+mwid, $
                        ystamp[i]-mwid:ystamp[i]+mwid]
         tvscl,sub,i
      endfor
   endif

; Produce the Stamps Images
; this module is commented out because it does not add to the 
; subtraction and is only used for subtraction statistics.  It
; runs properly if uncommented.
;   stampimg, wimg,refer,creference, $
;      xstamp,ystamp,wstamp,istamps,rstamps,crstamps,sistamps,itocr

   ; Filter Out the masked pixels and the edge of the image
   ois_filtpix,kernsz,subtraction,mask,diffimage,sub_sky,sub_sig,SEED=seed
   if not nodisplay then begin
      ; show the convolved reference image
      showsrc,creference,window=8,/forcestretch,lowval=lowval,hival=hival
   
      ; show the final difference image
      showsrc,diffimage,window=9,lowsig=-3.0,hisig=3.0
   endif

   ois_stampstats,wimg,creference,xstamp,ystamp,wstamp,itocr,stamp_chisq
   stamp_chisq=ois_chidist(creference,wimg,xstamp,ystamp,wstamp)
   if not nodisplay then begin
      setwin,7
      !p.multi=[0,1,2]
      coeff=trimrank(poly_fit(sflux,itocr,1,sigma=csig))
      frange=minmax(sflux)
      fitval=poly(frange,coeff)
      print,'   --> itocr slope is ',coeff[1],' +/- ',csig[1]
      plot,sflux,itocr,psym=8, $
         xtitle='Stamp flux (DN)',ytitle='ITOCR'
      oplot,frange,fitval,color='00ffff'xl
      plot,sflux,stamp_chisq,psym=8, $
         xtitle='Stamp flux (DN)',ytitle='STAMP_CHISQ'
      !p.multi=0
   endif

   ; compute chisq for those pixels in the image that are greater than
   ;   3*sigma above sky  (remember, the sky mean has already been removed)
   if not silent then begin
      z=where(wimg gt 3.0*sigi,count)
      imchisq = total(diffimage[z]^2)/float(count)
      print,' ----> total image chisq ',imchisq,' over ',strn(count),' pixels.'
   endif

   ; Write the Subtracted Image
   if saveit then begin
      sxaddpar,imghdr,'REFIMAGE',reference,' Image reference for difference'
      fits_write,outpath+image,diffimage,imghdr
   endif

   ; Write the Statistics
   if not silent then begin 
      print,'Flux ratio statistics:  This concerns the flux ratio between the'
      print,'stamps in the image divided by the convolved reference.  If'
      print,'everything is good, the ratio is unity.'
      result=moment(itocr,maxmoment=2)
      print,'Mean ratio is                      ',result[0]
      print,'Standard deviation of the ratio is ',result[1]
      print,'Standard deviation of the mean is  ', $
            result[1]/sqrt(n_elements(itocr))
      print,""
      print, ' ----> Total Processing Time: '+strn(systime(1)-time1),+' sec'
   endif

   diffinfo = { $
      fluxratio: fluxratio, $
      kernsz:    kernsz, $
      objrad:    2*meanfwhm, $
      imgfwhm:   meanfwhm, $
      nstamps:   nstamps, $
      reffwhm:   meanreffwhm $
      }

end

;+
; NAME:
;  spec2cie
; PURPOSE:   (one line only)
;  Convert from reflectance spectrum to CIE Chromaticity coordinates
; DESCRIPTION:
;  Conversion from reflectance spectrum to CIE Chromaticity (D65).
;    The color conversion is based on a Fortran subroutine
;    from A. T. Young, ;    SDSU
;    See:  http://mintaka.sdsu.edu/GF/explain/optics/color/color.html
;
;  This routine is designed to let you feed it spectral information that
;    may or may not be encoded against other dimensions (most commonly
;    position) and convert the spectral dimension to its CIE equivalent.
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  spec2cie,wave,spec,x,y,bigy
; INPUTS:
;  wave - scalar or vector, wavelengths, in nanometers for spectrum
;          The spectral range of interest is 380-770 nm (visual range)
;          This does not need to be regularly spaced but should be monotonical
;          increasing.
;  spec - Spectrum or spectral image data, this information is expected
;         to be in the range of 0-1 where 1 is perfectly reflecting and
;         0 is perfectly absorbing.
;         rank(spec) cannot be < rank(wave).  Last array index
;            for spec matches the index for wave.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SUBSAMPLE - Allows for finer resolution input spectra to be accurately
;             transformed.  The default value is 1 and should work on anything
;             with spectral structure coarser than 10 nm.  To properly capture
;             the nuances of your input spectra, divide 10 by the sampling
;             interval of your spectra.  Round this number up to the next
;             integer and this is the value you'd give as EXPAND.
;  SHOWSUMMARY - Flag, if set will print and plot information for the
;                   scalar case.  Ignored for vector or array data.
; OUTPUTS:
;  x,y -  CIE chromaticity, rank is one less than spec.
;  bigy  - intensity
;    Note: white is x=0.3127, y=0.3290, bigy=Y=1.0
;    (z=0.3583 because x+y+z=1)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Sorry, this is not an elegant or general coding solution.  This could
;   be more general but I dont' have the time to figure out the IDL syntax
;   to maximize vectorability.  This won't be too fast if wave is short and
;   spec is really big.
; PROCEDURE:
;  The tricky part in using this routine effectively is to understand how
;   the wavelength information is imbedded in the input variables.
;  Consider the simplest possible case, two wavelenths of information
;   and just one points to calculate this for:
;      wave=[450,600]   ; two wavelengths, don't have to match end points
;      spec=[0.5,0.6]   ; slight red slope
;   will return scalar values for x,y,bigy
;  Another case, same wavelength vector, but now two "scans"
;      spec -> 100x2 array
;   second dimension matches wave vector
;   result x,y,bigy are each 100 elements long
;  Last case, saveme wavelength vector, but now two images
;      spec -> 100x100x2 array
;   third dimension matches wave vector
;   result x,y,bigy are each 100x100 arrays
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2009/01/05
;-
pro spec2cie,wave,spec,x,y,bigy,SUBSAMPLE=subsample,SHOWSUMMARY=showsummary

   self='SPEC2CIE'
   if badpar(wave,[2,3,4,5],1, caller=self+'(wave) ') then return
   if badpar(spec,[4,5],[1,2,3],caller=self+'(spec) ', $
                  dimen=dimen,type=type,rank=rank) then return
   if badpar(subsample,[0,2,3],0, caller=self+'(SUBSAMPLE) ', $
                  default=1) then return
   if badpar(showsummary,[0,1,2,3],0, caller=self+'(SHOWSUMMARY) ', $
                  default=0) then return

   refw=findgen(40)*10.0 + 380.0

   xbar=[0.004,0.019,0.085,0.329,1.238,2.997,3.975,3.915,3.362,2.272, $
         1.112,0.363,0.052,0.089,0.576,1.523,2.785,4.282,5.880,7.322, $
         8.417,8.984,8.949,8.325,7.070,5.309,3.693,2.349,1.361,0.708, $
         0.369,0.171,0.082,0.039,0.019,0.008,0.004,0.002,0.001,0.001]/100.0

   ybar=[0.000,0.000,0.002,0.009,0.037,0.122,0.262,0.443,0.694,1.058, $
         1.618,2.358,3.401,4.833,6.462,7.934,9.149,9.832,9.841,9.147, $
         7.992,6.627,5.316,4.176,3.153,2.190,1.443,0.886,0.504,0.259, $
         0.134,0.062,0.029,0.014,0.006,0.003,0.002,0.001,0.001,0.000]/100.0

   zbar=[0.020,0.089,0.404,1.570,5.949,14.628,19.938,20.638,19.299, $
         14.972,9.461,5.274,2.864,1.52,0.712,0.388,0.195,0.086,0.039,0.02, $
         0.016,0.01,0.007,0.002,0.002,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
         0.,0.,0.]/100.0

   if subsample gt 1 then begin
      idx=findgen(40*subsample+1)/float(subsample)
      xbarp = sint(idx,xbar)/float(subsample)
      ybarp = sint(idx,ybar)/float(subsample)
      zbarp = sint(idx,zbar)/float(subsample)
      refwp=findgen(40*subsample+1)*10.0/subsample + 380.0
   endif else begin
      xbarp = xbar
      ybarp = ybar
      zbarp = zbar
      refwp = refw
   endelse

   ; wave is vector
   ; spec is vector
   ; result is a scalar
   if rank eq 1 then begin
      interp,wave,spec,refwp,ispec
      x = total(ispec*xbarp)   ; reduces rank here
      y = total(ispec*ybarp)   ; reduces rank here
      z = total(ispec*zbarp)   ; reduces rank here
      bigy = y
      sum = x+y+z
      x = x/sum
      y = y/sum
      setwin,0
      allmax=max([xbarp,ybarp,zbarp])
      cie2rgb,x,y,bigy,r,g,b
      color = ishft(long(b),16) + ishft(long(g),8) + long(r)
      if showsummary then begin
         print,'CIE x,y,Y ',x,y,bigy
         print,'RGB values ',r,g,b
         print,'BGR in hex ',b,g,r,format='(a,1x,3Z2.2)'
         print,'color hex value ',color,format='(a,1x,Z6.6)'
         plot,refwp,ispec,yr=[0.0,max([ispec,1.0])],/nodata, $
            xtitle='Wavelength (nm)',ytitle='Reflectance'
         oplot,refwp,ispec,color=color,thick=7
         oplot,wave,spec,psym=4
         oplot,refw,xbar/allmax/subsample,color='0000ff'xl,psym=4
         oplot,refwp,xbarp/allmax,color='0000ff'xl
         oplot,refw,ybar/allmax/subsample,color='00ff00'xl,psym=4
         oplot,refwp,ybarp/allmax,color='00ff00'xl
         oplot,refw,zbar/allmax/subsample,color='ff0000'xl,psym=4
         oplot,refwp,zbarp/allmax,color='ff0000'xl
      endif
   endif

   ; wave is vector
   ; spec is array
   ; result is a vector
   if rank eq 2 then begin
      bigy=make_array(dimen[0],type=type)
      x=make_array(dimen[0],type=type)
      y=make_array(dimen[0],type=type)
      z=make_array(dimen[0],type=type)
      for i=0L,dimen[0]-1 do begin
         interp,wave,reform(spec[i,*]),refwp,ispec
         x[i] = total(ispec*xbarp)
         y[i] = total(ispec*ybarp)
         z[i] = total(ispec*zbarp)
         bigy[i] = y[i]
         sum = x[i]+y[i]+z[i]
         if sum le 0. then begin
            x[i] = 0.3127 ; set color to white, intensity will be 0.
            y[i] = 0.3290
         endif else begin
            x[i] = x[i]/sum
            y[i] = y[i]/sum
         endelse
      endfor
   endif

   ; wave is vector
   ; spec is cube
   ; result is an array
   if rank eq 3 then begin
      bigy=make_array(dimen[0:1],type=type)
      x=make_array(dimen[0:1],type=type)
      y=make_array(dimen[0:1],type=type)
      z=make_array(dimen[0:1],type=type)
      for i=0L,dimen[0]-1 do begin
         for j=0L,dimen[1]-1 do begin
            interp,wave,reform(spec[i,j,*]),refwp,ispec
            x[i,j] = total(ispec*xbarp)
            y[i,j] = total(ispec*ybarp)
            z[i,j] = total(ispec*zbarp)
            bigy[i,j] = y[i,j]
            sum = x[i,j]+y[i,j]+z[i,j]
            if sum le 0. then begin
               x[i,j] = 0.3127 ; set color to white, intensity will be 0.
               y[i,j] = 0.3290
            endif else begin
               x[i,j] = x[i,j]/sum
               y[i,j] = y[i,j]/sum
            endelse
         endfor
      endfor
   endif

end

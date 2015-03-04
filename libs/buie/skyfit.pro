;+
; NAME:
;  skyfit
; PURPOSE:   (one line only)
;  Determine a 2-d polynomial fit to sky background in an image.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  skyfit,image,skyimage
; INPUTS:
;  image - Array which is the image to be analyzed
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DISPLAY - Flag, if set will generate lots of plotting output.
;  XORDER  - order of fit to X direction (default=0, constant)
;  YORDER  - order of fit to Y direction (default=0, constant)
;  LOWCLIP - fraction of random sample to clip at the low end of the signal.
;              If lowclip=.1 and npts=100, then the 10 lowest values in the
;              random sample are excluded BEFORE the robust mean is computed
;              for the stretch range.  This option will probably be just a bit
;              slower if invoked.  This option will likely be more robust
;              against extreme values in the image.  Default=0.0 (no clipping)
;  HICLIP  - fraction of random sample to clip at the high end of the signal.
;              If hiclip=.9 and npts=100, then the 10 highest values in the
;              random sample are excluded BEFORE the robust mean is computed
;              for the stretch range.  This option will probably be just a bit
;              slower if invoked.  This option will likely be more robust
;              against extreme values in the image.  Default=1.0 (no clipping)
;  NPTS   - Number of pixels to use in fit (default=601)
;  SILENT - Flag, if set will suppress information output to screen.
; OUTPUTS:
;  skyimage - Smooth image of sky in image.
; KEYWORD OUTPUT PARAMETERS:
;  COEFF   - Coefficients of fit
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2004/07/02
;  2005/06/22, MWB, Added error traps
;-
pro skyfit,image,skyimage,COEFF=coeff,SKYSIG=skysig,SILENT=silent, $
       XORDER=xorder,YORDER=yorder,HICLIP=hiclip,NPTS=npts,DISPLAY=display, $
       ERROR=error

   self='SKYFIT: '
   if badpar(image,[1,2,3,4,5,12,13,14,15],2, $
             caller=self+'(image): ',dimen=dimen) then return

   if badpar(npts,[0,2,3],0, $
             caller=self+'(NPTS): ',  default=601) then return
   if badpar(xorder,[0,2,3],0, $
             caller=self+'(XORDER): ',default=0  ) then return
   if badpar(yorder,[0,2,3],0, $
             caller=self+'(YORDER): ',default=0  ) then return
   if badpar(lowclip,[0,4,5],0, $
             caller=self+'(LOWCLIP): ',default=0.0) then return
   if badpar(hiclip,[0,4,5],0, $
             caller=self+'(HICLIP): ',default=1.0) then return
   if badpar(display,[0,2,3],0, $
             caller=self+'(DISPLAY): ',default=0  ) then return
   if badpar(silent,[0,2,3],0, $
             caller=self+'(SILENT): ',default=0  ) then return

   error=0

   ; Limit to proper range
   lowclip=(lowclip > 0.0) < 1.0
   hiclip =( hiclip < 1.0) > 0.0

   ; error check
   if lowclip ge hiclip then begin
      print,self,' lowclip must be less than hiclip. Aborting.'
      error=1
      return
   endif

   cnpts = min([npts,n_elements(image)])

   xidx = indgen(dimen[0])
   yone = replicate(1,dimen[1])
   xarr = xidx#yone

   yidx = indgen(dimen[1])
   xone = replicate(1,dimen[0])
   yarr = xone#yidx

   ; generate random indicies to get a random sample of the image
   idx=randomu(seed,cnpts)*(n_elements(image)-1)

   ; eliminate high and low values (before fitting!)
   if lowclip gt 0.0 or hiclip lt 1.0 then begin

      ; extract portion
      timage = image[idx]

      ; sort
      s = sort( timage )

      ; get index bounds of clipped region
      t1 = long(lowclip * cnpts) > 0L
      t2 = long(hiclip * cnpts) < long(cnpts-1)

      idx = idx[s[t1:t2]]

   endif

   ; eliminate NaNs
   good = where(finite(image[idx]) eq 1)
   idx = idx[good]

   nvals = n_elements(idx)

   if display then begin
      setwin,0
      stats,image[idx],/silent,title='original, post-hiclip'
   endif

   if xorder eq 0 and yorder eq 0 then begin
      bad = bytarr(nvals)
      robomean,image[idx],3.0,0.5,meanval,dummy,sigma,bad=bad
      if not silent then print,'Constant ',meanval,' +/- ',sigma
      zg=where(bad eq 0,countng)
      idx = idx[zg]
      nvals = n_elements(idx)
      yfit = replicate(meanval,nvals)
      coeff = meanval
   endif else begin

      nterms = (xorder+1)*(yorder+1)

      pass=0
      done=0
      while pass lt 10 and not done do begin

         if pass gt 0 then begin
            idx = idx[zg]
            nvals = n_elements(idx)
         endif

         ind = fltarr(nterms,nvals)
         k = 0
         for j=0,xorder do begin
            for i=0,yorder do begin
               if i eq 0 and j eq 0 then begin
                  ind[k,*] = 1.0
               endif else if i eq 0 then begin
                  ind[k,*] = yarr[idx]^j
               endif else if j eq 0 then begin
                  ind[k,*] = xarr[idx]^i
               endif else begin
                  ind[k,*] = xarr[idx]^i*yarr[idx]^j
               endelse
               k++
            endfor
         endfor

         yfit = fltarr(nvals)
         coeff = mysvdfit(ind,image[idx],1,yfit=yfit,singular=singular)
         if singular ne 0 then $
            print,self,'SINGULAR values found (',strn(singular),')'


         bad = bytarr(nvals)
         robomean,image[idx]-yfit,3.0,0.5,meanval,avgdev,sigma,bad=bad

         zg=where(bad eq 0,count)
         if count eq nvals then done=1

         pass++

      endwhile

      if not silent then begin
         print,'number of passes=',strn(pass)
         print,'coeff ',coeff
         print,'residuals ',meanval,' +/- ',sigma,'  avgdev = ',avgdev, $
            format='(a,f7.1,a,f5.1,a,f5.1)'
      endif

   endelse

   resid = image[idx]-yfit

   if display then begin
      setwin,1
      plot,xarr[idx],image[idx],psym=4,xr=[0,dimen[0]-1],xtitle='X'
      setwin,2
      plot,yarr[idx],image[idx],psym=4,xr=[0,dimen[1]-1],xtitle='Y'
      setwin,3
      plot,xarr[idx],resid,psym=4,xr=[0,dimen[0]-1],xtitle='X'
      setwin,4
      plot,yarr[idx],resid,psym=4,xr=[0,dimen[1]-1],xtitle='Y'

      setwin,5
      stats,image[idx],/silent,title='Final values'
      setwin,6
      stats,resid,/silent,title='Final values (residuals)'
   endif

   ; Now to determine a confusion correction.  For this must calculate the
   ;  mode of the surviving distribution.
   nbins = nvals/40
   min_data = min(resid)
   max_data = max(resid)
   binsz = (max_data-min_data)/float(nbins)
   if not silent then print,'bin size for correction',binsz
   hist = histogram(resid,binsize=binsz,min=min_data,max=max_data)
   xidx = findgen(n_elements(hist))*binsz+min_data

   ; smooth the "rough" distribution
   lowess,xidx,hist,binsz*20,shist

   ; peak of smoothed distribution is the mode
   z=where(shist eq max(shist))
   z=z[0]
   mode = xidx[z]
   if not silent then print,'Mode of residuals ',mode

   ; Compute and estimate of sky sigma, find 1/e of peak on low-DN side.
   z=where(shist lt max(shist)/exp(1.0) and xidx lt mode,count)
   if count eq 0 then begin
      print,self,' Something wrong with data, unable to compute sky fit.'
      error=1
      return
   endif
   skysig = mode - xidx[z[count-1]]
   if not silent then print,'Sigma estimate ',skysig

   if display then begin
      setwin,7
      plot,xidx,hist
      oplot,xidx,shist,color='0000ff'xl
      oplot,mode*[1,1],[0,max(hist)],color='00ff00'xl
      oplot,(mode-sigma)*[1,1],[0,max(hist)],color='00ff00'xl
   endif

   ; Apply correction to fit
   coeff[0] += mode

   if not silent then print,'final coeff ',coeff

   skyim,dimen,coeff,xorder,yorder,skyimage

end

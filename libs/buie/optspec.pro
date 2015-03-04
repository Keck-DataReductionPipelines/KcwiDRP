;+
; NAME: 
;  optspec
; PURPOSE:
;  Optimal extraction of a point source spectrum from OSIRIS XD data.
; DESCRIPTION:
;  This program is custom built to extract spectra from OSIRIS XD data.
;    The spectral images contain usually 3 or 4 cross-dispersed orders.
;    Because of the high and variable sky background signal, all images are
;    reduced as image pairs to permit excluding most sky signal at the
;    very first step.
;
;  As the program progresses, numerous plot and display windows are opened.
;    Window 0: Image window that contain 4 rows of strip images.  The strip
;              images come from extracting the relevant portions of the
;              original image such that the object spectrum will be
;              parallel to a row in the resampled image.  All pixels outside
;              the slit are discarded.  The 4 rows contain:
;                0 (top)    - Original image, scaled from image min to max.
;                1          - Original image, scaled from +/- 3 sigma about the
;                                robust mean of the image.
;                2          - Image after final sky subtraction scaled on same
;                                range and row #1.  This strip should have no
;                                remnant sky structure if everything works.
;                3 (bottom) - Image from row #2 after subtracting out the
;                                postive spectrum.  Ideally, you should not
;                                see a remnant of the positive spectral image.
;
;    Window 1: Trivial sum over the positive image aperture.
;
;    Window 2: Average column profile computed by summing all columns.  This
;                 summation is robust against bad pixels, no data, etc.
;                 Superimposed on the profile are (1) the +/- threshold
;                 that is used to locate the positive and negative image.
;                 The lines are drawn at +/- 3 sigma from the background (fit
;                 by a linear fuction). (2) red diamonds show those pixels
;                 identified as pure sky. (3) orange '*' show the positive
;                 image pixels. (4) brownish-green '*' show the negative image
;                 pixels.
;
;    Window 5,6,7,8: Plot of the residuals of pixel location of spectrum vs.
;                 column.  Each order is plotted in a different window.
;
;    Window 10: Plot of the optimally extracted spectrum.  It's most useful
;                 to compare this against the plot in window 1.
;
;    Window 11: Ratio of the optimal spectrum to the summation spectrum.
;
;    Window 12: Plot of spectrum location in all orders in the original
;                 image coordinates.  White points are those considered good
;                 during the process of tracing the centerlines.  Red points
;                 are those flagged as bad because the position was outside
;                 the positive aperture.  The brownish-green points
;                 are those flagged as bad because the FWHM at that column
;                 was abberant.
;    Window 13: Plot of the true image profiles in each order.  If not FINAL
;                 this shows the actual profile with the gaussian approximation
;                 overlain.  When FINAL is set, you will also see an overlay
;                 of the actual numerical image profile that was generated.
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  optspec,calib,root,i1,i2,spec,all
; INPUTS:
;  calib- Anonymous structure containing all pertinent calibration
;           information.  This structure is usually loaded beforehand using
;           the routine, "ldcalir"
;  root - string containing the root of the file name (with leading path
;         if desired).  DO NOT include the . between the root and suffix.
;  i1   - Frame id (integer) of first image.
;  i2   - Frame id (integer) of second image.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FINAL   - Flag, if set, indicates that this is a final pass extraction.
;              If it is not "final" then the optimal extractor uses a
;              gaussian profile fitted to the data.  This option is most
;              often used when the bad pixel mask does not yet exist.
;              When FINAL is set, a full numerical profile is generated and
;              used for the optimal extraction.  In this case, the gaussian
;              profile is used to help with locating the centerline but not
;              for the spectral extraction.
;  FINDBAD - Flag, if set causes program to look for bad pixels in the sky
;              region.  Upon completion, a strip array is written to disk with
;              the file name, mask.NNN, where the bad pixels have been set to 1.
;  GAUSSCOR- Correction factor from the directly computed image FWHM (per order)
;              to the final optimal weighting profile.  (Default = 1.3, this
;              number is only approximately correct but seems to be pretty
;              close.  A number smaller than this will often lead to spurious
;              periodic noise, often quite a lot of noise.  A number that is
;              too large will add a small amount of extra noise.  Ideally this
;              should be set for each and every spectrum but this has not yet
;              proved tractable.)
;  NONEG   - Flag, if true will inhibit all treatment of the negative image.
;              This should only be used if you know that there is no negative
;              spectral image and will preserve the maximum amount of sky in
;              the process.
;  RAW     - Flag, if true will inhibit the column-wise background subtraction.
;              The default is to fit the background in each column (along the
;              slit) and subtract the fitted background.  Use a linear function
;              for the background.
;  MASK    - Strip image mask.  Good pixels are 1, bad pixels are 0.  Bad pixels
;              are passed over during the profile weighting summation.
;  SAVE    - Flag, if true, final spectrum will be saved to a file.  The output
;              file name is root+'s'+suffix.  Thus 950911.003 would be saved to
;              the file 950911s.003.  Also, a file with the average spatial
;              profile is saved to root+'p'+suffix.
;  OUTPATH - The path to the location where data is to be saved, default is
;              the current directory.
;  PATH    - optional string that points to the directory containing the data.
;              This information is not used if the root already begins with '/'.
;              If root is not an absolute pathname, then PATH is prepended to
;              root for READ operations.  This path is not used for saving.
;              This allows reading from one directory (possible a read only area)
;              and then saving to the current directory.
;  PLOTS   - Which plots to show.
;              0 - Show them all (default).
;              1 - Don't show any plots or images.
;              2 - Just show windows 0, 2, and 13 (strip image, average column
;                     profile, actual profile of each order).
;  CLEAN   - Flag, if set, calls clnspec to allow removing bad points before
;              saving spectrum (this is actually obsolete but left in for now).
;
; OUTPUTS:
;  spec - 1-D spectrum extracted from spectral image.
;  all  - i1-i2 after XD spectra extracted to strip image. (see getpair)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Specifically written for OSIRIS cross-dispersed spectral data.  MASK
;  does not work particularly well.
; PROCEDURE:
; MODIFICATION HISTORY:
;  97/03, Written by Marc W. Buie, Lowell Observatory, cloned from getspec
;  97/08/07, MWB, massive reworking and debugging.
;  97/12/10, MWB, finalize new version, added OUTPATH keyword
;  98/06/09, MWB, added GAUSSCOR keyword.  Also numerous additions for
;                   improved bad pixel flagging.
;  98/08/11, MWB, removed obsolete keywords and updated the documentation.
;  98/08/25, MWB, added PLOTS=2 option.  Added NONEG flag.
;-

PRO optspec_f3,x,a1,a2,f
   dxl = x - a1 - 0.025
   dxr = x - a1 + 0.025
   const = sqrt(2.0)/a2
   f = const*(gaussint(const*dxr)-gaussint(const*dxl))
END

PRO optspec_fn,xn,yn,xthresh,x,y
   y=fltarr(n_elements(x))
   z=where(abs(x) le xthresh,count)
   if count ne 0 then begin
      interp,xn,yn,x[z],newy
      y[z] = newy
   endif
END

pro optspec,calib,root,i1,i2,spec,all, $
   MASK=mask,SAVE=save,PATH=path,CLEAN=clean, $
   FINDBAD=findbad,NONEG=noneg,OUTPATH=in_outpath, $
   GAUSSCOR=gausscor,FINAL=final,PLOTS=plots

   if badpar(calib,8,1,CALLER='optspec (calib) ') then return
   if badpar(root,7,0,CALLER='optspec (root) ') then return
   if badpar(i1,[2,3],0,CALLER='optspec (i1) ') then return
   if badpar(i2,[2,3],0,CALLER='optspec (i2) ') then return
   if badpar(save,[0,1,2,3],0,CALLER='optspec (SAVE) ',DEFAULT=0) then return
   if badpar(clean,[0,1,2,3],0,CALLER='optspec (CLEAN) ',DEFAULT=0) then return
   if badpar(mask,[0,1,2,3],2,CALLER='optspec (MASK) ',DEFAULT=-1) then return
   if badpar(noneg,[0,1,2,3],0,CALLER='optspec (NONEG) ',DEFAULT=0) then return
   if badpar(path,[0,7],0,caller='optspec: (PATH) ',default='./') then return
   if badpar(in_outpath,[0,7],0,caller='optspec: (OUTPATH) ',default='./') then return
   if badpar(findbad,[0,1,2,3],0,CALLER='optspec (FINDBAD) ',DEFAULT=0) then return
   if badpar(gausscor,[0,4,5],0,CALLER='optspec (GAUSSCOR) ',DEFAULT=1.3) then return
   if badpar(final,[0,1,2,3],0,CALLER='optspec (FINAL) ',DEFAULT=0) then return
   if badpar(plots,[0,1,2,3],0,CALLER='optspec (PLOTS) ',DEFAULT=0) then return
   if badpar(gausscor,[0,4,5],0,CALLER='optspec (GAUSSCOR) ',DEFAULT=1.3) then return

   IF mask[0] eq -1 THEN inmask=0 else inmask=1

   if plots ne 1 then loadct,5,/silent

   nmaxwin = 20
   if plots eq 0 then $
      dowin=replicate(1B,nmaxwin) $
   else if plots eq 1 then $
      dowin=bytarr(nmaxwin) $
   else if plots eq 2 then begin
      dowin=bytarr(nmaxwin)
      dowin[0] =1B
      dowin[2] =1B
      dowin[13]=1B
   endif else $
      dowin=replicate(1B,nmaxwin)


   if strmid(root,0,1) eq '/' then begin
      newroot = root
      outpath = ''
   endif else begin
      newroot = addslash(path)+root
      outpath = addslash(in_outpath)
   endelse

   ; Load two images and display
   print,root,i1,'-',i2,format='($,a,".",i3.3,a,i3.3)'
   getpair,calib,newroot,i1,i2,all,hdr,/raw
   all = all * 10.0 ; convert to electrons from counts
   all0= all
   sz=size(all)
   nrows=sz[2]
   ncols=sz[1]
   if dowin[0] then begin
      setwin,0,/show,xsize=ncols,ysize=nrows*5
      tvscl,bytscl(all,min=-10000.0,max=10000.0,top=!d.n_colors-1),0
   endif

   ; If there isn't an input mask, then create a blank one.  Flag all pixels
   ;   that are very low.  These have been marked by getpair as falling off
   ;   the detector.
   IF not inmask THEN BEGIN
      mask=bytarr(ncols,nrows)
      z=where(all lt -1.0e6,count)
      IF count ne 0 THEN mask[z] = 1
   ENDIF

   ; Do a first pass sky cleaning.  At this point we don't know where the
   ;   object is and can't protect the sky subtraction from it.  But, something
   ;   needs to be done to get the initial profile.
   backsub,all,/col,order=1,min_value=-1.0e6,mask=mask

   ; show image, scaled on sky
   print,'.',format='($,a)'
   tmpbad=mask
   robomean,all,3,.5,avg,dummy,stddev,bad=tmpbad
   if dowin[0] then $
      tv,bytscl(all,min=avg-3.0*stddev,max=avg+3.0*stddev,top=127),1

   ; Select some random columns to get a first approximation of the
   ;   average column profile.  This will locate the positive and negative
   ;   images.
   nrand=60
   idx=randomu(seed,nrand)*(ncols-1)
   
   print,'.',format='($,a)'
   avgcol=fltarr(nrows)
   cut=all[idx,*]*(1.0-mask[idx,*])
   ; Normalize the columns
   for i=0,nrand-1 do begin
      cut[i,*]=cut[i,*]/max(cut[i,*])
   endfor
   ; find the mean in the row direction, masking off bad pixels.
   for i=0,calib.height-1 do begin
      robomean,cut[*,i],3,.5,avg,bad=mask[idx,i]
      avgcol[i]=avg
   endfor
   avgcol0=avgcol

   ; Plot the initial average column profile.
   if dowin[2] then begin
      setwin,2,xsize=580,ysize=380,/show
      yr=[min([avgcol,-1.0]),max([avgcol,1.0])]
      if min(yr) eq max(yr) then yr[1]=yr[0]+1.0
      plot,avgcol,yr=yr
   endif

   ; Find the positive and negative peaks in the average profile.
   print,'.',format='($,a)'
   peakrow=where(avgcol eq max(avgcol[2:calib.height-3]))
   peakrow=peakrow[0]
   if not noneg then begin
      minrow=where(avgcol eq min(avgcol[2:calib.height-3]))
      minrow=minrow[0]
   endif else begin
      minrow=-1
   endelse

   ; Second pass on finding average profile.
   cut=all*(1.0-mask)
   ; Normalize on max within one pixel of the average profile peak.
   for i=0,ncols-1 do begin
      cut[i,*] = cut[i,*] / $
                 max(cut[i,max([peakrow-1,0]):min([peakrow+1,calib.height-1])])
   endfor
   ; find the mean in the row direction, masking off bad pixels.
   for i=0,calib.height-1 do begin
      robomean,cut[*,i],3,.5,avg,bad=mask[*,i]
      avgcol[i]=avg
   endfor

   ; Find the slope of the background in the averaged profile.
   print,'.',format='($,a)'
   colno=findgen(calib.height)
   coeff=goodpoly(colno[0:calib.height-2],avgcol[0:calib.height-2], $
                                      1,2.2,f_col,newx,newy,EXCLUDE=6)
   robomean,newy-poly(newx,coeff),3,.5,bavg,dummy,bstddev

   ;Now we know where the object is so do background subtraction while
   ;  leaving out the object rows.  Also, pixels marked as bad are also
   ;  left out.
   print,'s',format='($,a)'
   posprofile = avgcol
   FOR i=max([peakrow+1,1]),calib.height-1 DO BEGIN
      IF posprofile[i] gt posprofile[i-1] or posprofile[i] lt 0.0 THEN $
         posprofile[i]=0.0
   ENDFOR
   FOR i=min([peakrow-1,calib.height-2]),0,-1 DO BEGIN
      IF posprofile[i] gt posprofile[i+1] or posprofile[i] lt 0.0 THEN $
         posprofile[i]=0.0
   ENDFOR
   zpos=where(posprofile gt 0.0,numpos)

   negprofile = avgcol
   if minrow ne -1 then begin
      FOR i=max([minrow+1,1]),calib.height-1 DO BEGIN
         IF negprofile[i] lt negprofile[i-1] or negprofile[i] gt 0.0 THEN $
            negprofile[i]=0.0
      ENDFOR
      FOR i=min([minrow-1,calib.height-2]),0,-1 DO BEGIN
         IF negprofile[i] lt negprofile[i+1] or negprofile[i] gt 0.0 THEN $
            negprofile[i]=0.0
      ENDFOR
      zneg=where(negprofile lt 0.0,numneg)
   endif else begin
      zneg=-1
      numneg=0
   endelse

   countbad=numpos+numneg
   IF numpos ne 0 and numneg ne 0 THEN BEGIN
      badloc=[zpos,zneg]
   ENDIF ELSE IF numpos ne 0 THEN BEGIN
      badloc=zpos
   ENDIF ELSE IF numneg ne 0 THEN BEGIN
      badloc=zneg
   ENDIF
   if countbad ne 0 then begin
      all=all0
      backsub,all,/col,order=1,exclude=badloc,min_value=-1.0e6,mask=mask
      if dowin[0] then begin
         setwin,0
         tv,bytscl(all,min=avg-3.0*stddev,max=avg+3.0*stddev,top=!d.n_colors-1),2
      endif
   endif else begin
      if dowin[0] then $
         tv,bytarr(ncols,nrows),2
   endelse

   ; Now do the profile again after final sky cleaning
   print,'.',format='($,a)'
   cut=all*(1.0-mask)
   for i=0,ncols-1 do begin
      cut[i,*] = cut[i,*] / $
                 max(cut[i,max([peakrow-1,0]):min([peakrow+1,calib.height-1])])
   endfor
   for i=0,calib.height-1 do begin
      robomean,cut[*,i],3,.5,avg,bad=mask[*,i]
      avgcol[i]=avg
   endfor

   ; Re-find the positive and negative peaks
   print,'.',format='($,a)'
   peakrow=where(avgcol eq max(avgcol[2:calib.height-3]))
   peakrow=peakrow[0]
   if not noneg then begin
      minrow=where(avgcol eq min(avgcol[2:calib.height-3]))
      minrow=minrow[0]
   endif

   ; From the averaged profile, find the positive definite portion of the
   ;   profile centered on the object.  In other words, scan the profile
   ;   from the peak outward.  The profile should always decrease (or stay
   ;   the same) but never dip below zero.  This will form the template
   ;   for the extraction of the standard spectrum.
   posprofile = avgcol
   FOR i=max([peakrow+1,1]),calib.height-1 DO BEGIN
      IF posprofile[i] gt posprofile[i-1] or posprofile[i] lt 0.0 THEN $
         posprofile[i]=0.0
   ENDFOR
   FOR i=min([peakrow-1,calib.height-2]),0,-1 DO BEGIN
      IF posprofile[i] gt posprofile[i+1] or posprofile[i] lt 0.0 THEN $
         posprofile[i]=0.0
   ENDFOR
   zpos=where(posprofile gt 0.0,numpos)

   negprofile = avgcol
   if not noneg then begin
      ; Do the same for the negative image, just to find out where it is.
      print,'.',format='($,a)'
      FOR i=max([minrow+1,1]),calib.height-1 DO BEGIN
         IF negprofile[i] lt negprofile[i-1] or negprofile[i] gt 0.0 THEN $
            negprofile[i]=0.0
      ENDFOR
      FOR i=min([minrow-1,calib.height-2]),0,-1 DO BEGIN
         IF negprofile[i] lt negprofile[i+1] or negprofile[i] gt 0.0 THEN $
            negprofile[i]=0.0
      ENDFOR
      zneg=where(negprofile lt 0.0,numneg)
   endif else begin
      negprofile[*] = 0.0
   endelse

   zsky=where(negprofile eq 0.0 and posprofile eq 0.0)

   ; If requested, look at sky for the bad pixels.  Save results to disk.
   IF findbad and zsky[0] ne -1 THEN BEGIN
      print,'b',format='($,a)'
      FOR i=0,ncols-1 DO BEGIN
         tmpbad=mask[i,zsky]
         robomean,all[i,zsky],3.0,0.5,bad=tmpbad
         mask[i,zsky]=tmpbad
      ENDFOR
      badname = 'mask.'+string(i1,form='(i3.3)')
      writefits,badname,mask
   ENDIF

   ; plot profile
   coeff=goodpoly(colno,avgcol,1,2.2,f_col,newx,newy,EXCLUDE=6)
   robomean,newy-poly(newx,coeff),3,.5,bavg,dummy,bstddev
   if dowin[2] then begin
      setwin,2,xsize=580,ysize=380,/show
      yr=[min([avgcol,-1.0]),max([avgcol,1.0])]
      plot,colno,avgcol,xtitle='Row number',ytitle='Relative signal strength', $
                        title='Average spatial profile',yr=yr
      oplot,colno,f_col+bavg+3.0*bstddev,color=55
      oplot,colno,f_col+bavg-3.0*bstddev,color=55
      oplot,colno[zpos],posprofile[zpos],color=70,psym=2,symsize=1.5
      if numneg ne 0 then $
         oplot,colno[zneg],negprofile[zneg],color=87,psym=2,symsize=1.5
      oplot,colno[zsky],avgcol[zsky],psym=4,color=63,symsize=1.5
   endif

   ; extract standard spectrum
   print,'.',format='($,a)'
   spec = fltarr(ncols)
   FOR i=0,numpos-1 DO $
      spec=spec+all[*,zpos[i]]
   if dowin[1] then begin
      setwin,1,xsize=600,ysize=490
      plotspec,calib,spec,yr=[0,6e4],title='Simple sum over positive aperture'
   endif

   ; compute the gaussian moments to each column over the positive object,
   ;  finding position and width in this pass.
   print,'.',format='($,a)'
   a0=fltarr(ncols)
   a1=fltarr(ncols)
   a2=fltarr(ncols)
   wt=replicate(1.0,numpos)
   FOR i=0,ncols-1 DO BEGIN
      a1[i]=total(all[i,zpos]*colno[zpos])/total(all[i,zpos])
      a2[i]=total(all[i,zpos])/max(all[i,zpos])
      IF a1[i] lt min(colno[zpos]) THEN a1[i] = min(colno[zpos])
      IF a1[i] gt max(colno[zpos]) THEN a1[i] = max(colno[zpos])
      a0[i]=all[i,fix(a1[i]+0.5)]
   ENDFOR

   ; Find mean width of object
   print,'.',format='($,a)'
   robomean,a2,2.0,0.5,meanfwhm,dummy1,meanfwhmsig

   ; Re-construct original chip positions.
   print,'.',format='($,a)'
   xpos=fltarr(ncols)
   ybot=fltarr(ncols)
   FOR i=0,calib.nor-1 DO BEGIN
      xpos[calib.o[i,0]:calib.o[i,1]]= $
         findgen(calib.o[i,1]-calib.o[i,0]+1)+calib.x1[i,0]
      ybot[calib.o[i,0]:calib.o[i,1]]= $
         fix(xpos[calib.o[i,0]:calib.o[i,1]]*calib.slope[i] + calib.y0[i] + 0.5)
   ENDFOR
   yloc=a1+ybot
   if dowin[12] then begin
      setwin,12,xsize=500,ysize=500
      plot,xpos,yloc,xr=[0,255],yr=[0,255],psym=3,xtitle='Column number', $
         ytitle='Row number',title='Centerline in original image coordinates'
   endif

   ; Weed out those columns that can now be recognized as bad, two rules,
   ;    if position at either bottom or top of slit --> bad
   ;    if bad FWHM -- > bad
   print,'.',format='($,a)'
   a1bad=intarr(ncols)
   z1bad=where(fix(a1+0.5) eq min(zpos) or fix(a1+0.5) eq max(zpos),count)
   IF count ne 0 THEN BEGIN
      a1bad[z1bad]=1
      if dowin[12] then oplot,xpos[z1bad],yloc[z1bad],psym=3,color=55
   ENDIF
   z1bad=where(a2 lt meanfwhm-2.0*meanfwhmsig or $
               a2 gt meanfwhm+2.0*meanfwhmsig and a1bad eq 0,count)
   IF count ne 0 THEN BEGIN
      a1bad[z1bad]=1
      if dowin[12] then oplot,xpos[z1bad],yloc[z1bad],psym=3,color=87
   ENDIF

   ; Given the locations, fit a 2nd order polynomial to the y location
   print,'.',format='($,a)'
   yfit=fltarr(ncols)
   badloc=replicate(1,ncols)
   FOR i=0,calib.nor-1 DO BEGIN
      xp = xpos[calib.o[i,0]:calib.o[i,1]]
      yp = yloc[calib.o[i,0]:calib.o[i,1]]
      good=where(a1bad[calib.o[i,0]:calib.o[i,1]] eq 0,countgood)
      IF countgood lt 2 THEN BEGIN
         a1bad[calib.o[i,0]:calib.o[i,1]] = 0
         good=where(a1bad[calib.o[i,0]:calib.o[i,1]] eq 0)
      ENDIF
      coeff=goodpoly(xp[good],yp[good], $
                     2,2.2,yfit0,newx,newy)
      yfit[calib.o[i,0]:calib.o[i,1]]=poly(xp,coeff)
      good=where(abs(yp-poly(xp,coeff)) le 1.0,countgood)
      if dowin[5+i] then begin
         setwin,5+i,xsize=600,ysize=490
         plot,xp,yp-poly(xp,coeff),yr=[-0.5,0.5],xtitle='Column number', $
            ytitle='Row # residual (pixels)',title='Order '+string(i,format='(i1)')
      endif
      IF countgood ne 0 THEN BEGIN
         if dowin[5+i] then $
            oplot,xp[good],yp[good]-poly(xp[good],coeff), $
               color=65,psym=8,symsize=0.7
         badloc[good+calib.o[i,0]] = 0
      ENDIF
   ENDFOR
   ypos=yfit-ybot

   ; Find mean gaussian width for each order.
   print,'.',format='($,a)'
   a2mean=fltarr(calib.nor)
   a2msig=fltarr(calib.nor)
   FOR i=0,calib.nor-1 DO BEGIN
      tmpbad=badloc[calib.o[i,0]:calib.o[i,1]]
      robomean,a2[calib.o[i,0]:calib.o[i,1]],2,.5,sigmean,dummy,sigsig, $
         bad=tmpbad
      badloc[calib.o[i,0]:calib.o[i,1]]=tmpbad
      a2mean[i]=sigmean
      a2msig[i]=sigsig
   ENDFOR
   ; Convert from FWHM to 1/e half-width and enforce a floor of 1.1 pix FWHM
   a2mean = (a2mean > 1.1) / 2.35482  * gausscor
   a2msig = (a2msig > 1.1) / 2.35482

   ; Compute the optimal spectral extraction, including a model image.
   if dowin[13] then begin
      setwin,13            ; for order image profile plot
      !p.multi=[0,2,2]
   endif

   optspec=fltarr(ncols)
   prof = fltarr(ncols,calib.height)
   modim= fltarr(ncols,calib.height)

   ysmofac = 1.0
   FOR i=0,calib.nor-1 DO BEGIN

      print,'^',format='($,a)'
      if dowin[13] then $
         plot,[0],[1],/nodata,xr=[-10,10],yr=[-0.25,0.75], $
            xtitle='Rows from centerline', $
            ytitle='Relative flux', $
            title='Order '+string(i,format='(i1)')

      xp=0
      yp=0
      FOR j=calib.o[i,0],calib.o[i,1] DO BEGIN

         optspec_f3,colno,ypos[j],a2mean[i],xfit
         xfit=xfit/total(xfit)*(1.0-mask[j,*])
         prof[j,*]=xfit
         if numneg ne 0 then prof[j,zneg]=0.0
         optspec[j]=total(all[j,*]*xfit)/total(xfit^2)
         modim[j,*]=prof[j,*]*optspec[j]
         IF not badloc[j] THEN BEGIN
            zg = where(mask[j,*] eq 0,countg)
            if countg gt 1 then begin
               newx = colno[zg]-ypos[j]
               newy = all[j,zg]/optspec[j]
               if dowin[13] then oplot,newx,newy,psym=3,symsize=0.5
               if final then begin
                  if n_elements(xp) eq 1 then begin
                     xp = newx[*]
                     yp = newy[*]
                  endif else begin
                     xp = [xp,newx[*]]
                     yp = [yp,newy[*]]
                  endelse
               endif
            endif
         ENDIF
      ENDFOR  ; end of loop on order's points

      ; Compute gaussian profile used in first pass and over plot it.
      xtest=(findgen(1001)-500.0)/50.0
      optspec_f3,xtest,0.0,a2mean[i],xfit
      if dowin[13] then oplot,xtest,xfit/total(xfit)*50.0,color=55

      ; On a final pass, generate a numerical order profile and generate a
      ;   new spectral extraction.
      if final then begin

         ; Select those points in the order near the centerline.
         zg = where(abs(xp) lt a2mean[i] * 4.0 + ysmofac)
         xp = xp[zg]
         yp = yp[zg]
         if dowin[13] then oplot,xp,yp,psym=3,color=25

         ; Construct a grid of points over which the profile will be determined.
         nxpg = 151
         xpg = (findgen(nxpg)-float(nxpg/2))/float(nxpg/2)*4.0*a2mean[i]

         ; First pass smoothing of the profile, this gives a general trend
         ;   from which the bad pixels can be identified and eliminated.
         lowess,xp,yp,ysmofac,yps,order=2,newx=xpg

         ; Clip off negative values in the smoothed profile.  Then compute
         ;   the deviation of the data from the smoothed profile.  Eliminate
         ;   those points more than 3 sigma from the smoothed profile.
         yps=yps > 0
         optspec_fn,xpg,yps,a2mean[i]*4.0,xp,yfit
         sbad=bytarr(n_elements(yfit))
         robomean,yp-yfit,3.0,0.5,savg,dummy,sstdev,bad=sbad
         print,sstdev,format='($,f4.2)'
         zg = where(sbad eq 0)
         xp = xp[zg]
         yp = yp[zg]

         ; Second pass smoothing of the profile.  Bad values have been
         ;   eliminated so this should be a better profile.
         lowess,xp,yp,ysmofac,yps,order=2,newx=xpg
         yps=yps > 0

         if dowin[13] then oplot,xpg,yps,color=80
         ; Scan the left side of the profile from 1/e to edge.  The profile
         ;    must descend, enforce this.
         left = max(where(xpg lt -a2mean[i]))
         for is=left-1,0,-1 do $
            if yps[is] gt yps[is+1] then yps[is]=yps[is+1]

         ; Do the same for the right side of the profile.
         right = min(where(xpg gt a2mean[i]))
         for is=right+1,n_elements(xpg)-1 do $
            if yps[is] gt yps[is-1] then yps[is]=yps[is-1]

         if dowin[13] then begin
            oplot,xp,yp,psym=3,color=48
            oplot,xpg,yps,color=96
         endif

         ; Normalize the profile
         yps=yps/int_tabulated(xpg,yps)

         ; Second pass using the numerical profile.
         FOR j=calib.o[i,0],calib.o[i,1] DO BEGIN
            newx = colno-ypos[j]
            optspec_fn,xpg,yps,a2mean[i]*4.0,newx,yfit
            yfit=yfit*(1.0-mask[j,*])
            if numneg ne 0 then yfit[zneg] = 0.0
            prof[j,*]=yfit
            optspec[j]=total(all[j,*]*yfit)/total(yfit^2)
            modim[j,*]=prof[j,*]*optspec[j]
         ENDFOR  ; end of loop on order's points

      endif

   ENDFOR ; end of loop over all orders

   if dowin[0] then begin
      !p.multi=0
      setwin,0
      tv,bytscl(all-modim,min=avg-3.0*stddev,max=avg+3.0*stddev, $
                top=!d.n_colors-1),3
      tvscl,mask,4
   endif

   if dowin[10] then begin
      setwin,10,xsize=600,ysize=490
      plotspec,calib,optspec,yr=[0,6e4],title='Optimally extracted spectrum'
   endif

   if dowin[11] then begin
      setwin,11,xsize=600,ysize=490
      plotspec,calib,optspec/spec,yr=[0.8,1.2], $
         title='Ratio of optimal to non-optimal spectrum'
   endif

   spec=optspec

; Clean spectrum if requested.
   if clean then clnspec,calib,spec

; Save spectrum and average profile if requested.
   if save then begin

      print,'w',format='($,a)'
      outname = outpath+root+'s.'+string(i1,form='(i3.3)')
      sxaddpar,hdr,'FRAMMATE',i2,' Frame used for background subtraction'
      sxaddpar,hdr,'NAXIS',1
      sxaddpar,hdr,'NAXIS1',n_elements(spec)
      sxdelpar,hdr,'NAXIS2'
      writefits,outname,spec,hdr

      outname = outpath+root+'p.'+string(i1,form='(i3.3)')
      openw,lun,outname,/get_lun
      avgcol = avgcol > (-9.9999)
      FOR i=0,calib.height-1 DO BEGIN
         tag='*'
         zp = where(i eq zpos)
         zn = where(i eq zneg)
         zs = where(i eq zsky)
         IF zp[0] ne -1 THEN tag='p'
         IF zn[0] ne -1 THEN tag='n'
         IF zs[0] ne -1 THEN tag='s'
         printf,lun,i,avgcol[i],tag,f_col[i]+bavg-3.0*bstddev, $
            f_col[i]+bavg+3.0*bstddev,format='(i2,1x,f8.5,1x,a1,2(1x,f8.5))'
      ENDFOR
      free_lun,lun

   endif

   print,'FWHM',meanfwhm,'pix',format='(2x,a,1x,f5.2,1x,a)'

end

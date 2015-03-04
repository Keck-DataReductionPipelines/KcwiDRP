;+
; NAME: 
;  getspec
; PURPOSE:
;  Extract a point source spectrum from OSIRIS XD data.
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  getspec,calib,root,i1,i2,spec,all,
;     RAW=raw,NONEG=noneg,AUTO=auto,MASK=mask
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
;  AUTO - Flag, if true, enables automatic aperture location for extracting
;         the spectra.   The image is collapsed into a single column.  Any
;         pixel more than 3 sigma from the background is flagged for
;         extraction.  Default is to require clicking on the image displayed
;         in window 0.  Three rows centered on the clicked location are
;         extracted for the spetrum.  /AUTO is the preferred option.
;  APLOC- Override to specify the positive object location.  Overrides AUTO.
;  APSIZE-Override to force the size of the extraction aperture.  The
;           will extend from the location (auto or APLOC) + and - by APSIZE.
;           Thus the total aperture is 2*APSIZE+1 rows.
;  NONEG - Flag, if true, inhibits using the "negative" spectrum in the
;          background image (i2).  Default is to extact the negative
;          spectrum, negate it, and add to the positive spectrum.
;  RAW  - Flag, if true will inhibit the column-wise background subtraction.
;           The default is to fit the background in each column (along the
;           slit) and subtract the fitted background.  Use a linear function
;           for the background.
;  MASK - Indicies in strip image.  These pixels will be replaced by the
;           average of their nearest neighbors in the x direction.
;  SAVE - Flag, if true, final spectrum will be saved to a file.  The output
;           file name is root+'s'+suffix.  Thus 950911.003 would be saved to
;           the file 950911s.003
;  PATH - optional string that points to the directory containing the data.
;           This information is not used if the root already begins with '/'.
;           If root is not an absolute pathname, then PATH is prepended to
;           root for READ operations.  This path is not used for saving.
;           This allows reading from one directory (possible a read only area)
;           and then saving to the current directory.
;  CLEAN - Flag, if set, calls clnspec to allow removing bad points before
;           saving spectrum.
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
;  95/03/24, Written by Marc W. Buie, Lowell Observatory
;  95/06/21, MWB, modified aperture algorithm.
;  95/09/14, MWB, added calib structure pass through, added SAVE flag, added
;                 PATH keyword, added CLEAN flag.
;  95/09/18, MWB, improved auto-aperture locating algorithm
;  95/09/21, MWB, added APLOC and APSIZE keywords
;-
pro getspec,calib,root,i1,i2,spec,all, $
   RAW=raw,NONEG=noneg,AUTO=auto,MASK=mask,SAVE=save,PATH=path,CLEAN=clean, $
   APSIZE=apsize,APLOC=aploc

   if badpar(calib,8,1,CALLER='getspec (calib) ') then return
   if badpar(root,7,0,CALLER='getspec (root) ') then return
   if badpar(i1,[2,3],0,CALLER='getspec (i1) ') then return
   if badpar(i2,[2,3],0,CALLER='getspec (i2) ') then return
   if badpar(raw,[0,1,2,3],0,CALLER='getspec (RAW) ',DEFAULT=0) then return
   if badpar(save,[0,1,2,3],0,CALLER='getspec (SAVE) ',DEFAULT=0) then return
   if badpar(apsize,[0,1,2,3],0,CALLER='getspec (APSIZE) ',DEFAULT=0) then return
   if badpar(aploc,[0,1,2,3],0,CALLER='getspec (APLOC) ',DEFAULT=-1) then return
   if badpar(clean,[0,1,2,3],0,CALLER='getspec (CLEAN) ',DEFAULT=0) then return
   if badpar(auto,[0,1,2,3],0,CALLER='getspec (AUTO) ',DEFAULT=0) then return
   if badpar(mask,[0,2,3],[0,1],CALLER='getspec (MASK) ',DEFAULT=-1) then return
   if badpar(path,[0,7],0,caller='getspec: (path) ',default='./') then return

   if aploc ne -1 then auto=0

   loadct,5,/silent

   if strmid(root,0,1) eq '/' then begin
      newroot = root
   endif else begin
      newroot = addslash(path)+root
   endelse

; Load two images and display
   print,'positive image ',i1,'   negative image ',i2
   getpair,calib,newroot,i1,i2,all,hdr,raw=raw,mask=mask
   sz=size(all)
   nrows=sz[2]
   ncols=sz[1]
   setwin,0,/show,xsize=ncols,ysize=nrows*3
   erase
   tvscl,all,0

; construct average column profile, done over a restricted range for S/N
   avgcol=fltarr(nrows)
   cut=all[220:380,*]
   for i=0,380-220-1 do begin
      cut[i,*]=cut[i,*]/max(cut[i,*])
   endfor
   for i=0,calib.height-1 do begin
      robomean,cut[*,i],3,.5,avg
      avgcol[i]=avg
   endfor

; plot profile
   setwin,2,/show
   plot,avgcol

; show image, scaled on sky
   setwin,0
   robomean,all,3,.5,avg,dummy,stddev
   tv,bytscl(all,min=avg-3.0*stddev,max=avg+3.0*stddev,top=127),1

; Determine range of rows to sum

; Automatic location of object spectra
   if auto then begin
	   posrowpos=where(avgcol eq max(avgcol[2:33]))
      posrowpos=posrowpos[0]
	   if not keyword_set(noneg) then begin
   	   negrowpos=where(avgcol eq min(avgcol[2:33]))
         negrowpos=negrowpos[0]
	   endif else begin
	      negrowpos = -1
	   endelse

; Manual location for object spectra
   endif else begin
      if aploc eq -1 then begin
         print,'Click on positive object spectrum'
         cursor,x,posrowpos,/wait,/change,/device
      endif else begin
         posrowpos = aploc
      endelse
      posrowpos = posrowpos mod calib.height
      print,'Positive object at row ',posrowpos
	   if not keyword_set(noneg) then begin
	      print,'Click on negative object spectrum'
	      cursor,x,negrowpos,/wait,/up,/device
	      print,'Negative object at row ',negrowpos
	   endif else begin
	      negrowpos = -1
	   endelse
	endelse

; Replot the average column with mean background +- 3 sigma limits overlaid
   setwin,2
   idx=indgen(n_elements(avgcol))
   coeff=goodpoly(idx,avgcol,1,2.2,f_col,newx,newy,EXCLUDE=6)
   robomean,newy-poly(newx,coeff),3,.5,bavg,dummy,bstddev
   plot,avgcol
   for i=230,240 do $
      oplot,idx,all[i,*]/max(abs(all[i,*])),color=45
   oplot,avgcol
   oplot,avgcol,psym=8,symsize=0.7,color=70
   oplot,idx,f_col+bavg+3.0*bstddev,color=55
   oplot,idx,f_col+bavg-3.0*bstddev,color=55
   posap=where( (avgcol-f_col-bavg gt 3.0*bstddev) and $
                (abs(idx-posrowpos) lt 4)         ,count)
   if apsize ne 0 then begin
      apleft  = posrowpos - apsize
      apright = posrowpos + apsize
      print,'NOTE: manual aperture size used.'
   endif else if count ne 0 then begin
      apleft=min(posap)-1
      apright=max(posap)+1
   endif else begin
      apleft=posrowpos-2
      apright=posrowpos+2
      print,'WARNING: normal aperture location find failed.'
   endelse
   oplot,newx,newy,psym=8
   setusym,-1
   oplot,idx[apleft:apright],avgcol[apleft:apright], $
      psym=8,color=96,symsize=1.5
   setusym,1

; Add annotation for negative image if wanted.
	if not keyword_set(noneg) then begin
	   negap=where( (avgcol lt f_col-3.0*bstddev) and $
                   (abs(idx-negrowpos) lt 4)         ,count)
	   if (count ne 0) then begin
	      oplot,idx[negap],avgcol[negap],psym=4,color=96,symsize=1.5
	   endif
   endif

;Now we know where the object is so redo background subtraction while
;  leaving out the object rows.
   if save or clean then begin
	   badloc=where( abs(avgcol-f_col-bavg) gt 3.0*bstddev, countbad )
	   if countbad ne 0 then begin
	      setwin,0
	      backsub,all,/col,order=1,exclude=badloc
	      tv,bytscl(all,min=avg-3.0*stddev,max=avg+3.0*stddev,top=127),2
	   endif
   endif

;  Sum up rows in the aperture.
   posobj=fltarr(ncols)
   negobj=fltarr(ncols)
   for ii=apleft,apright do posobj=posobj+all[*,ii]
	if negrowpos ge 0 then $
      for ii=0,n_elements(negap)-1 do negobj=negobj+all[*,negap[ii]]

; Compute final spectrum for output and plot it against pixels and wavelength
   spec = posobj-negobj
   setwin,1,/show
   plotspec,calib,spec
   setwin,3
   plot,posobj
   if not keyword_set(noneg) then oplot,-negobj,color=60
   setwin,1

; Clean spectrum if requested.
   if clean then clnspec,calib,spec

; Save spectrum if requested.
   if save then begin
      outname = root+'s.'+string(i1,form='(i3.3)')
      sxaddpar,hdr,'HISTORY','Background from frame '+string(i2)
      writefits,outname,spec,hdr
   endif

end

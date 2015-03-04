;+
; NAME:
;   avger
; PURPOSE: (one line)
;   Temporal averaging of time-series data.
; DESCRIPTION:
;   This program was written to perform N point averaging of raw photometry
;   data.  It will work on any temporal data streams or data that has clumpy
;   independent variable values.  The data are grouped together into bin
;   that are specified by the THRESH input.  Thresh specifies the size of a
;   gap that will cause the group to be broken.  The value for thresh is taken
;   to be a multiple of the 'normal' spacing between points.  If THRESH=2,
;   then any gap twice as long as the previous point spacing will cause a
;   break.  Any number equal to or less than 1 will prevent all averaging.
;   To prevent too much binning for long uniform data runs, MAXBIN puts an
;   upper limit on the number of points that can be grouped together and
;   XSPREAD limits the xspan within a single group.
;
;   The THRESH criterion is applied to the data first for grouping,
;   then MAXBIN and XSPREAD are used simultaneously to break up long
;   binning strings.
;
;   The data are averaged together using a weighted average (see MEANERR).
;   The uncertainty returned is the standard deviation of the mean.
;
; CATEGORY:
;   Numerical
; CALLING SEQUENCE:
;   pro avger,x,y,err,maxbin,thresh,avgx,avgy,sigy
; INPUTS:
;      x      - Independent variable.
;      y      - Dependent variable.
;      err    - Uncertainty on y in units of standard deviation.
;               This can be a scalar or a vector but must not be zero.
;      maxbin - Maximum number of points to average together.
;      thresh - Gap that will break grouping of data as a fraction of normal.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;      DATAERR - Output vector of sigma of the mean computed directly
;                 from the scatter in the data.  If the number of points
;                 in an output bin is one, then the output error is the
;                 same as the input error.
;      FORCEIT - 2xN vector, First column is a point number and the second
;                 column is a flag, 0 means force this point to bin, 1 means
;                 force a break at this point.  Point numbers outside of
;                 the valid data range are silently ignored.  This info
;                 if supplied overrides the breaking controlled by THRESH,
;                 MAXBIN, and XSPREAD allowing a direct modification of
;                 binning for pathalogical cases.
;                    Example:
;                       forceit=[[13,0],[14,1],[19,0]]
;                    would force points 13 and 19 to NOT end the binning
;                    and would force point 14 to be the end of a bin.
;                    When using this option, VERBOSE is especially useful.
;      XSPREAD - maximum range of x allowed in a single averaged point.
;                 (default = no limit).
;      VERBOSE - Flag, if true will cause a complete printout of how the
;                 vector is being binned.
; OUTPUTS:
;      avgx   - X value after binning.
;      avgy   - Y value after binning.
;      sigy   - New uncertainty.
; KEYWORD OUTPUT PARAMETERS:
;      NPTS   - Vector that contains the number of points averaged for each
;                 output point.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   The input vectors must have equal length and should be greater in length
;   than 3.  If the vectors are of length 2 or 3, the program will return a
;   straight average of all input values.  Scalar inputs are not allowed and
;   will generate an error.
; PROCEDURE:
; MODIFICATION HISTORY:
;    1993/05/11 - Written by Marc W. Buie, Lowell Observatory.
;    1994/03/21, MWB, modified to output double precision if input is double.
;    1994/02/25, MWB, added XSPREAD control over binning.
;    1998/01/16, MWB, added DATAERR keyword
;    2009/02/12, MWB, fixed error in DATAERR calculation.
;    2010/12/13, MWB, added NPTS output keyword
;-
pro avger,x,y,in_err,maxbin,thresh,avgx,avgy,sigy, $
       DATAERR=dsigy,FORCEIT=forceit,XSPREAD=xspread,VERBOSE=verbose,NPTS=npts

   if badpar(x,[2,3,4,5],1,caller='AVGER: (x) ',npts=nx,type=xtype) then return
   if badpar(y,[2,3,4,5],1,caller='AVGER: (y) ',npts=ny,type=ytype) then return
   if badpar(in_err,[0,2,3,4,5],[0,1],caller='AVGER: (err) ', $
                npts=nerr,default=1) then return
   if badpar(maxbin,[2,3],0,caller='AVGER: (maxbin) ') then return
   if badpar(thresh,[2,3,4,5],0,caller='AVGER: (thresh) ') then return
   if badpar(xspread,[0,2,3,4,5],0,caller='AVGER: (XSPREAD) ', $
      default=x[nx-1]-x[0]) then return
   if badpar(forceit,[0,2,3],2,caller='AVGER: (FORCEIT) ') then return
   if badpar(verbose,[0,1,2,3],0,caller='AVGER: (VERBOSE) ',default=0) then return

   if nerr eq 1 then err=replicate(in_err,ny) else err = in_err

   test_zero=where(err eq 0.,count_zero)
   if count_zero ne 0 then begin
      print,'AVGER: the input sigma array must not contain zeros.'
      return
   endif

   if nx ne ny or nx ne n_elements(err) then begin
      print,'AVGER: Error, the input arrays must have the same lengths.'
      return
   endif

   nx = n_elements(x)
   if nx eq 1 then $
      message,'Input must be vectors longer than one element.'

   if nx lt 4 then begin

      meanerr,x,err,avgx
      meanerr,y,err,avgy,sigy

   endif else begin

      ;Compute the pre and post dx values, doesn't include first and last point.
      predx  = x[1:nx-2] - x[0:nx-3]
      postdx = x[2:nx-1] - x[1:nx-2]

      ;Find break points for averaging based on dx test.
      ;    First never breaks, last always breaks.
      breakit = [0, (postdx gt thresh*predx), 1]

      if verbose then begin
         print,'AVGER: breakit after dx test'
         print,breakit
      endif

      ; Check length of averaging run and break if length hits maxbin
      count=0
      istart=0
      for i=0,nx-2 do begin
         if breakit[i] then begin
            count=0
            istart=i+1
         endif else begin
            count=count+1
         endelse
         if (count ge maxbin) or (x[i+1]-x[istart]) gt xspread then begin
            breakit[i] = 1
            count=0
            istart=i+1
         endif
      endfor

      if verbose then begin
         print,'AVGER: xspread = ',xspread
         print,'AVGER: breakit after run length and xspread check'
         print,breakit
      endif

      ; Apply FORCEIT if specified.
      oldbreakit=breakit
      if keyword_set(forceit) then begin
         for i=0,n_elements(forceit)/2-1 do begin
            idx = forceit[0,i]
            flag = forceit[1,i] eq 1
            if idx ge 0 and idx lt nx then $
               breakit[idx] = flag
         endfor
      endif

      if verbose then begin
         for i=0,nx-1 do $
            print,i,oldbreakit[i],breakit[i],x[i],y[i], $
               format='(i3,1x,i1,1x,i1,1x,g,1x,g)'
      endif

      ;Find length of output vectors and initialize.
      z=where(breakit eq 1, nout)
      if verbose then $
         print,'Output vector will have ',strn(nout),' points.'
      if xtype ne 5 and ytype ne 5 then begin
         avgx = fltarr(nout,/nozero)
         avgy = fltarr(nout,/nozero)
         sigy = fltarr(nout,/nozero)
         dsigy = fltarr(nout,/nozero)
      endif else begin
         avgx = dblarr(nout,/nozero)
         avgy = dblarr(nout,/nozero)
         sigy = dblarr(nout,/nozero)
         dsigy = dblarr(nout,/nozero)
      endelse
      npts=lonarr(nout)

      ; This is the major working loop.  Thumb through breakit and average points
      ;   whenever it is true.  Store input if length of run is 1.  Otherwise,
      ;   average the points.
      j=0
      nstart=0
      for i=0L,nx-1 do begin
         if breakit[i] then begin
            if nstart eq i then begin
               avgx[j] = x[i]
               avgy[j] = y[i]
               sigy[j] = err[i]
               dsigy[j] = err[i]
               npts[j] = 1
            endif else begin
               if verbose then $
                  print,j,'  ',strn(nstart),'-',strn(i),i-nstart+1
               meanerr,x[nstart:i]-x[nstart],err[nstart:i],avgxval
               meanerr,y[nstart:i],err[nstart:i],avgyval,sigyval,sigdata
               avgx[j]  = avgxval+x[nstart]
               avgy[j]  = avgyval
               sigy[j]  = sigyval
               dsigy[j] = sigdata/sqrt(float(i-nstart))  ; this is N-1
               npts[j] = i-nstart+1
            endelse
            nstart = i+1
            j=j+1
         endif
      endfor

   endelse

end

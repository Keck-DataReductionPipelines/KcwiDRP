;+
; NAME:
;  robomean
; PURPOSE: (one line)
;  Robust statistical moments of the data.
; DESCRIPTION:
;  This routine computes the average, average deviation, standard
;  deviation, variance, skew and kurtosis of the input data.  The various
;  output quantities are always returned as floating point scalars.
;  The statistics are computed with no regard for the dimensionality of
;  the input data.
;
;  The statistics are robust in that the data is searched for outliers
;  after the moments are computed.  If outliers are found they are
;  removed and the statistics are computed again.  This continues until
;  either no outliers are found or if the removal of outliers has an
;  insignificant effect on the statistics.
; CATEGORY:
;  Statistics
; CALLING SEQUENCE:
;  robomean,data,thresh,eps,avg,avgdev,stddev,var,skew,kurt,nfinal
; INPUTS:
;  data   - Input data to be analyzed.
;  thresh - Deviation from the mean to signify an outlier.
;  eps    - Smallest significant change in mean in units of std dev.
; OPTIONAL INPUT PARAMETERS:
;  None.
; INPUT KEYWORD PARAMETERS:
;  BAD    - Array of flags, 0=good, 1=bad.  Any points removed from the sample
;              will have this flag set to 1 upon return.  Note that if you
;              do not supply this array, you will not get the resulting bad
;              flag output even if you supply the keyword.
; OUTPUTS:
;  avg    - Sample mean.
;  avgdev - Average deviation of the data from the mean.
;  stddev - Standard deviation of the data from the mean.
;  var    - Variance of the data from the mean.
;  skew   - Skewness, third statistical moment.
;  kurt   - Kurtosis, fourth statistical moment.
;  nfinal - Number of points used in the final result.
;  new    - Vector of 'clean' pixels (optional).
; OUTPUT KEYWORD PARAMETERS:
;  STDMEAN - Optional return of standard deviation of the mean.
;  ERROR   - Flag, if set indicates that all points were removed.  This can
;              be caused by a many things but usually is an indication of some
;              pathology in the data.  The most common is to have a true
;              mean and standard deviation in the data that is very distant
;              from an initial approximation so that all data are excluded on
;              a pass (usually the first).
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
;  None.
; RESTRICTIONS:
;  None.
; PROCEDURE:
;  Standard techniques, see section 13.1 in Numerical Recipies.  The
;  thresh and eps values are not tremendously important.  Thresh=5.0
;  and eps=0.5 appear to work pretty well most of the time.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1992 Jan 20.
;  Fix - nfinal was not being returned to calling program.
;        Doug Loucks, Lowell Observatory, 1992 Oct 22.
;  Fix - Sense of test to determine additional refinement was
;        not correct for some cases.  Changed logic to parallel
;        the C version, since the WHILE statement is now available
;        in IDL.
;  Mod - Added by Marc Buie: Argument 'new,' allowing the 'clean' pixels
;        to be returned to the caller.
;  Mod - 2/11/94, DWL, Added keyword STDMEAN to permit return of this
;        value to the caller.
;  1997/03/23 - MWB, added BAD keyword and operation.  This program's operation
;        should not be any different than what it was if you don't supply BAD.
;  1998/07/08, MWB, changed so that NaN values are automatically flagged as bad.
;  2000/11/2, MWB, cosmetic rewrite.
;  2002/01/15, MWB, fixed a subtle case where the data leads to stdev=0.0
;               that was causing the program to crash.
;  2005/06/21, MWB, added ERROR return flag.
;  2009/12/30, MWB, mod from 2002 caused the program to return the wrong
;               answer for noiseless input data.  Changed to allow stdev=0.0
;               on return but this case avoids almost all of the internal
;               processing in this routine.
;-
pro robomean, data, thresh, eps, avg, avgdev, stddev, var, skew, kurt, $
              nfinal, new, STDMEAN=r_stdmean, BAD=bad, ERROR=error

   ; figure out if there are input bad flags, if there are input bad flags
   ;   use them, and save any changes to be returned.  Without input flags
   ;   the work to remember what's trimmed out is not done.
   savebad = n_elements(bad) eq n_elements(data)

   error=0

   ; First, bypass anything in the input data that is not finite.
   if savebad then begin
      nans = where(finite(data) eq 0,count)
      if count ne 0 then bad[nans]=1
      good = where(bad eq 0,count)
   endif else begin
      good = where(finite(data) eq 1,count)
   endelse

   ; Do the first pass, watch out for pathological cases too.
   if count eq 0 then begin
      print, 'All points removed.'
      avg=0.
      avgdev=0.
      stddev=0.
      var=0.
      skew=0.
      kurt=0.
      error=1
      return
   endif else if count eq 1 then begin
      avg=data[good[0]]
      avgdev=0.
      stddev=0.
      var=0.
      skew=0.
      kurt=0.
      return
   endif else begin
      Moment4, data[good], avg, avgdev, stddev, var, skew, kurt
      n = count
   endelse

   ; Compute errors on the statistical moments.
   skewerr = SQRT(6.0/n)
   kurterr = SQRT(24.0/n)

   ; compute standard deviation of the mean
   if n gt 1 then $
      stdmean = stddev / SQRT(n-1.0) $
   else $
      stdmean = stddev

   ; Safety, stddev should really never be zero.
   if stddev eq 0 then begin
      nfinal=n
      r_stdmean = 0.0
      return
   endif

   ; First weeding pass, anything that is more than thresh sigma from the
   ;    mean is marked as bad.  The sigma used is the standard deviation of
   ;    the data.
   if savebad then begin
      nogood = where( abs(data[good]-avg)/stddev gt thresh, badcount)
      if badcount ne 0 then bad[good[nogood]]=1
      good = WHERE( bad eq 0, count)
   endif else begin
      if stddev gt 0.0 then begin
         good0 = WHERE( ABS( data[good]-avg ) / stddev  LE thresh, count)
         good  = good[good0]
      endif
   endelse

   ; Safety valve, if nothing good here, return results prior to the
   ;    first weeding pass.
   if count eq 0 then begin
      new = data
      nfinal = n_elements(new)
      r_stdmean = stddev
      error=1
      return
   endif

   ; Restrict data to just the good data.
   new = data[ good ]

   ; Set nfinal to the number before first weeding
   nfinal = n
   
   ; set the new length to the nuber after first weeding
   newlen = count

   ; set the oldmean to a high value to get things started, this value
   ;    ensures that oldmean won't pass the test and the following loop
   ;    will run at least once as long as something was pulled out during
   ;    the first weeding pass.
   oldmean = avg + 2.0*eps*stddev

   ; Loop to whittle away at the data.  This loop will continue to run as
   ;   long as there is data to work with and the termination conditions have
   ;   not yet been met.  If the number of points has changed AND the average
   ;   has changed by more than eps*stdmean then another iteration will be
   ;   done.  Another iteration will also be done if the kurtosis changes by
   ;   more than thresh times the kurtosis error.  The kurtosis based test is
   ;   quite important in this process.  In some cases, you would quit too
   ;   early if you just looked at the change in the mean.
   while ( (newlen ne n) and ( (ABS( oldmean-avg ) / stdmean gt eps) or $
         ( kurt gt thresh*kurterr ) ) ) do begin

      ; save the results from the last pass
      oldmean = avg
      n = newlen

      ; Compute new statistical moments
      Moment4, new, avg, avgdev, stddev, var, skew, kurt

      ; safety valve
      if stddev eq 0 then stddev=max(data)

      ; errors
      skewerr = SQRT(6.0/n)
      kurterr = SQRT(24.0/n)
      if n gt 1 then $
         stdmean = stddev / SQRT(n-1.0) $
      else $
         stdmean = stddev

      ; The next filtering pass
      if savebad then begin
         nogood = where( abs(data[good]-avg)/stddev gt thresh, badcount)
         if badcount ne 0 then bad[good[nogood]]=1
         good = WHERE( bad eq 0, count)
         if count ne 0 then new = data[good]
      endif else begin
         good = WHERE( ABS( new-avg ) / stddev le thresh, count)
         if count ne 0 then new = new[ good ]
      endelse

      if count eq 0 then begin
         print, 'robomean: All points removed.'
         error=1
         return
      endif
      newlen = count

   endwhile

   nfinal = n
   r_stdmean = stdmean

end

;+
; NAME:
;  lowess
; PURPOSE:
;  Lowess smoothing of data.
; DESCRIPTION:
;
;  This algorithm was gleaned from a description of LOWESS, standing
;  for LOcally WEighted Scatterplot Smoother, found in "The Elements of
;  Graphing Data", by William S. Cleveland, Wadsworth Advanced Books and
;  Software.  This implementation is probably not the same as the one
;  described.  I have tried to include the provision for using different
;  weighting functions.  At the time of writing I don't know what effect
;  different functions have upon the smoothing process.  This procedure
;  in itself is not intended to be robust (as defined by Cleveland). By
;  including the possiblity of varying weights for the data points it is
;  possible to acheive robustness by multiple calls of this routine.
;
; CATEGORY:
;  Numerical
; CALLING SEQUENCE:
;  lowess,x,y,width,ysmoo,WEIGHT=weight
; INPUTS:
;  x      - Independant variable values.
;  y      - Dependant variable values.
;  width  - Width of smoothing function (in same units sa X).
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  NEWX   - If provided, the smoothed curve is computed over this input
;             range of x values rather than the input x range.  ysmoo
;             will have the same length as NEWX if it is provided.
;  ORDER  - Order of polynomial fit during the lowess smoothing. (default=1)
;  WEIGHT - Weight to use for each point (default is to have equal weight)
;
; OUTPUTS:
;  ysmoo  - Smoothed version of Y, same number of points as input x and y.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;  By default, the weighting function is triangular where the weight is
;  1 at the output point location, and drops linearly to zero +/- width from
;  the output point.
;
; MODIFICATION HISTORY:
;  98/06/16, Written by Marc W. Buie, Lowell Observatory
;  2001/02/01, MWB, changed Polyfitw call to poly_fit equivalent
;  2006/11/13, MWB, forced loop variables to LONG
;
;-
pro lowess,x,y,width,ysmoo,WEIGHT=weight,ORDER=order,NEWX=newx

   if badpar(x,[2,3,4,5],1,CALLER='LOWESS: (x) ',npts=nx) then return
   if badpar(y,[2,3,4,5],1,CALLER='LOWESS: (y) ',npts=ny) then return
   if badpar(width,[2,3,4,5],0,CALLER='LOWESS: (width) ') then return
   if badpar(weight,[0,2,3,4,5],1,CALLER='LOWESS: (WEIGHT) ', $
                       default=replicate(1.0,nx),npts=nw) then return
   if badpar(order,[0,2,3],0,CALLER='LOWESS: (ORDER) ',default=1) then return
   if badpar(newx,[0,2,3,4,5],1,CALLER='LOWESS: (NEWX) ',default=-1) then return

   if nx ne ny then begin
      print,'LOWESS: lengths of x and y must match'
      return
   endif

   TRIBASE=0.01

   if nw ne nx then begin
      print,'LOWESS: length of weight vector must match x and y'
      return
   endif

   if newx[0] eq -1 and n_elements(newx) eq 1 then $
      nnx=0 $
    else $
      nnx=n_elements(newx)

   ; return smoothed points for all input X
   if nnx eq 0 then begin
      ysmoo = y * 0.
      for i=0L,nx-1 do begin

         ; Set limits on smoothing width
         xl = x[i] - width
         xr = x[i] + width

         z=where(x ge xl and x le xr, count)

         if count le order+1 then begin
            ysmoo[i] = y[i]
         endif else begin
            w = weight[z]*(1.0 - ((1.0-TRIBASE)/width) * abs(x[z]-x[i]))
            coeff=poly_fit(x[z]-x[i],y[z],order, $
                           measure_errors=sqrt(1.0/w),status=status)
            ysmoo[i] = poly(0.0,coeff)
         endelse
      endfor

   ; return smoothed points for only the requested X locations
   endif else begin
      ysmoo = fltarr(nnx)
      for i=0L,nnx-1 do begin

         ; Set limits on smoothing width
         xl = newx[i] - width
         xr = newx[i] + width

         z=where(x ge xl and x le xr, count)

         if count eq 1 then begin
            ysmoo[i] = y[z[0]]
         endif else if count le order+1 then begin
            interp,x,y,newx[i],newy
            ysmoo[i] = newy
         endif else begin
            w = weight[z]*(1.0 - ((1.0-TRIBASE)/width) * abs(x[z]-newx[i]))
            coeff=poly_fit(x[z]-newx[i],y[z],order, $
                           measure_errors=sqrt(1.0/w),status=status)
            ysmoo[i] = poly(0.0,coeff)
         endelse
      endfor
   endelse

end

;+
; NAME:
;   mysvdfit
;
; PURPOSE:
;   Perform a general least squares fit with optional error estimates.
;
; DESCRIPTION:
;   This version uses SVD.  A user-supplied function or a built-in
;   polynomial is fit to the data.
;
; CATEGORY:
;   Function fitting
;
; CALLING SEQUENCE:
;   Result = SVDFIT(X, Y, M)
;
; INPUTS:
;   X:   A vector representing the independent variable.  If this an array,
;           the columns are taken to be the precomputed independant vectors
;           and no actual function is computed here.
;
;   Y:   Dependent variable vector.  This vector should be same length 
;      as X.
;
;   M:   The number of coefficients in the fitting function.  For 
;      polynomials, M is equal to the degree of the polynomial + 1.
;
; OPTIONAL INPUTS:
;   Weight:   A vector of weights for Y(i).  This vector should be the same
;      length as X and Y.
;
;      If this parameter is ommitted, 1 is assumed.  The error for 
;      each term is weighted by Weight(i) when computing the fit.  
;      Frequently, Weight(i) = 1./Sigma(i) where Sigma is the 
;      measurement error or standard deviation of Y(i).
;
;   Funct:   A string that contains the name of an optional user-supplied 
;      basis function with M coefficients. If omitted, polynomials
;      are used.
;
;      The function is called:
;         R = FUNCT(X,M)
;      where X is an N element vector, and the function value is an 
;      (N, M) array of the N inputs, evaluated with the M basis 
;      functions.  M is analogous to the degree of the polynomial +1 
;      if the basis function is polynomials.  For example, see the 
;      function COSINES, in the IDL User Library, which returns a 
;      basis function of:
;         R(i,j) = cos(j*x(i)).
;      For more examples, see Numerical Recipes, page 519.
;
;      The basis function for polynomials, is R(i,j) = x(i)^j.
;      
; OUTPUTS:
;   SVDFIT returns a vector of M coefficients.
;
; OPTIONAL OUTPUT PARAMETERS:
;   NOTE:  In order for an optional keyword output parameter
;   to be returned, it must be defined before calling SVDFIT.
;   The value or structure doesn't matter.  For example:
;
;      YF = 1            ;Define output variable yf.
;      C = SVDFIT(X, Y, M, YFIT = YF)    ;Do SVD, fitted Y vector is now
;                  ;returned in variable YF.
;
;   YFIT:   Vector of calculated Y's.
;
;   CHISQ:   Sum of squared errors multiplied by weights if weights
;      are specified.
;
;   COVAR:   Covariance matrix of the coefficients.
;
;    VARIANCE:   Sigma squared in estimate of each coeff(M).
;
;    SINGULAR:   The number of singular values returned.  This value should
;      be 0.  If not, the basis functions do not accurately
;      characterize the data.
;
; COMMON BLOCKS:
;   None.
;
; SIDE EFFECTS:
;   None.
;
; MODIFICATION HISTORY:
;   Adapted from SVDFIT, from the book Numerical Recipes, Press,
;   et. al., Page 518.
;   minor error corrected April, 1992 (J.Murthy)
;   93/10/12, Marc W. Buie, Lowell Observatory.  Added option to make this
;             work similar to "regress".
;   97/03/20, MWB, Changed to use SVDC and SVSOL and everything is now in
;               double precision.
;   2005/02/02, MWB, Changed thresh from 10^-9 to 2x10^-12
;   2005/06/21, MWB, added error trap keyword
;   2009/07/14, MWB, fixed a bug in the original covar calculation
;-

function mysvdfit,x,y,m, YFIT = yfit, WEIGHT = weight, CHISQ = chisq, $
   SINGULAR = sing, VARIANCE = var, COVAR = covar, Funct = funct, ERROR=error

   error=0

   thresh = 2.0e-12      ;Threshold used in editing singular values

   xx = x*1.      ;Be sure x is floating or double

; Get size and rank for use later.
   size_x = size(x)
   xrank = size_x[0]
   if xrank eq 2 then begin
      n = size_x[2]
      m = size_x[1]
   endif else begin
      n = size_x[1]
   endelse

   if n ne n_elements(y) then begin
      print,'MYSVDFIT: X and Y must have same # of elements.'
      error=1
      return,-1
   endif

   if n_elements(weight) ne 0 then begin
      if n_elements(weight) ne n then begin
         print,'MYSVDFIT: Weights have wrong number of elements.'
         error=1
         return,-1
      endif
      b = y * weight   ;Apply weights
   endif else b = y   ;No weights

   no_funct = n_elements(funct) eq 0

; Use user function with 1-d x
   if xrank eq 1 and no_funct then begin
      z = execute('a='+funct+'(x,m)')
      if z ne 1 then begin
         print,'MYSVDFIT: Error calling user fcn: ' + funct
         error=1
         return,-1
      endif
      if n_elements(weight) ne 0 then $
         a = a * (weight # replicate(1.,m))

   endif else if xrank eq 2 then begin
      a = rotate(x,4)
      if n_elements(weight) ne 0 then $
         a = a * (weight # replicate(1.,m))

   endif else begin      ;Call user's function
      a = dblarr(n,m)         ;coeff matrix
      if n_elements(weight) ne 0 then xx = float(weight) $
      else xx = replicate(1.,n)   ;Weights are 1.
      for i=0,m-1 do begin      ;Make design matrix
         a[0,i] = xx
         xx = xx * x
      endfor
   endelse

   svdc,a,w,u,v,/double,/column         ;Do the svd

;Find all the non-singular values
   good = where(w gt (max(w) * thresh), ng)
   sing = m - ng      ;# of singular values

   if sing ne 0 then begin
      small=where(w le max(w)*thresh)
      print, 'MYSVDFIT: Warning: ' + strcompress(sing,/remove) + $
         ' singular values found: ' + strcompress(string(small))
      w[small]=0
      if ng eq 0 then begin
         error=1
         return,-1
      endif
   endif

   coeff=svsol(u,w,v,b,/double,/column)

   wt = dblarr(m)
   wt[good]=1./w[good]

   if (n_elements(weight) eq 0) then xx=replicate(1.,n) else xx=weight
   if (n_elements(yfit) ne 0) or (n_elements(chisq) ne 0) then begin
      if no_funct and xrank eq 1 then yfit = poly(x,coeff) $
      else begin 
         yfit = dblarr(n)
         for i=0,m-1 do yfit = yfit + coeff[i] * a[*,i] / xx
      endelse
   endif

   if n_elements(chisq) ne 0 then begin   ;Compute chisq?
      chisq = (y - yfit)
      if n_elements(weight) ne 0 then chisq = chisq * weight
      chisq = total(chisq ^ 2)
   endif

   wt = wt*wt      ;Use squared w

   if n_elements(covar) ne 0 then begin   ;Get covariance?
      covar = dblarr(m,m)
      for i=0,m-1 do for j=0,i do begin
        s = 0.
;        for k=0,m-1 do s = s + wt[k] * v[i,k] * v[k,j]
        for k=0,m-1 do s = s + wt[k] * v[i,k] * v[j,k]
        covar[i,j] = s
        covar[j,i] = s
      endfor
   endif

   if n_elements(var) ne 0 then begin
      var = dblarr(m)
      for j=0,m-1 do for i=0,m-1 do $
         var[j] = var[j] + v[j,i]^2 * wt[i]
   endif

   return,coeff
end



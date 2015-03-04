;+
; NAME:
;   robosvd
; PURPOSE:   (one line only)
;  Robust SVD linear regression fit using mysvdfit
; DESCRIPTION:
; CATEGORY:
;  Function fitting
; CALLING SEQUENCE:
;   Result = ROBOSVD(X, Y, M)
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
; OPTIONAL INPUT PARAMETERS:
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
; KEYWORD INPUT PARAMETERS:
;   BAD - byte array of the same length as Y.  If 0, value is considered
;           good and used in the fit.  If 1, value is considered bad and
;           is not used.  If additional points are seen as bad by this
;           program, then those flags are modified in the input array.
;   SILENT - Suppress all printed output.
; OUTPUTS:
;   SVDFIT returns a vector of M coefficients.
; KEYWORD OUTPUT PARAMETERS:
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
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2004/07/07
;  2005/02/17, MWB, added NFINAL keyword
;  2005/02/22, MWB, added BAD keyword
;  2005/06/21, MWB, added error trapping keyword
;-
function robosvd,in_x,in_y,m,thresh,eps, $
   YFIT = yfit, WEIGHT = weight, CHISQ = chisq, $
   SINGULAR = sing, VARIANCE = var, COVAR = covar, Funct = funct, $
   NFINAL=nfinal, BAD=in_bad, SILENT=silent, ERROR=error

   initial_npts = n_elements(in_y)

   error=0

   self='ROBOSVD: '
   if badpar(in_bad,[0,1,2,3],1,caller=self+'(BAD) ', $
             default=bytarr(initial_npts)) then return,-1
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
             default=0) then return,-1

   x = in_x
   y = in_y

   bad = in_bad
   idx = lindgen(initial_npts)

   zg = where(bad eq 0,goodstart)
   if goodstart ne initial_npts then begin
      x = x[*,zg]
      y = y[zg]
      zb=where(bad eq 1)
      idx=idx[zg]
      bad=bad[zg]
   endif

   pass=0

   repeat begin
      npts = n_elements(y)

      ; make sure to get the fitted values
      yfit = y*0.

      ; initial fit
      coeff = mysvdfit(x,y,m,yfit=yfit,weight=weight,chisq=chisq,error=error, $
                         singular=sing,variance=var,covar=covar,funct=funct)
      if error then return,-1
      
      yfit=float(yfit)
      robomean,y-yfit,thresh,eps,bad=bad,meandiff,avgerr,sig,error=error
      if error then return,-1

      zg = where(bad eq 0,count)

      done = count eq npts

      if not done then begin
         x = x[*,zg]
         y = y[zg]
         zb=where(bad eq 1)
         in_bad[idx[zb]] = 1B
         idx=idx[zg]
         bad=bad[zg]
      endif

      pass++

   endrep until done or pass gt 20

   if pass ne 1 and not silent then $
      print,self,strn(pass),' passes, started with ',strn(goodstart), $
                 ' points and ended up with ',strn(count)

   nfinal = count

   size_x = size(in_x)
   xrank = size_x[0]
   if xrank eq 2 then begin
      n = size_x[2]
      m = size_x[1]
   endif else begin
      n = size_x[1]
   endelse
   no_funct = n_elements(funct) eq 0

   if no_funct and xrank eq 1 then yfit = poly(in_x,coeff) $
   else begin 
      yfit = dblarr(n)
      for i=0,m-1 do yfit = yfit + coeff[i] * in_x[i,*]
   endelse

   return,coeff

end

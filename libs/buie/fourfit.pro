;+
; NAME: 
;  fourfit
; PURPOSE: 
;  Fit one or more fourier terms to discrete (periodic) data.
; DESCRIPTION:
; CATEGORY:
;  Function fitting
; CALLING SEQUENCE:
;  fourfit,phase,data,sig,nterms,c,csig,YFIT=yfit,CHISQ=chisq
; INPUTS:
;  phase  - Fraction of period (mod or linearly increasing).
;  data   - Measured values.
;  sig    - Uncertainties of the data.
;  nterms - Number of fourier terms to fit.  (2*nterms+1 unknowns)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  c - fourier series coefficients (see fourfunc)
;  csig - uncertainties of the coefficients
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/10/10, Written by Marc W. Buie, Lowell Observatory
;  2009/02/12, MWB, found and fixed a serious bug that caused the chisq
;    value to be WRONG.  The old value was basically a reduced chisq
;    as if the number of free parameters were zero.   Ouch.
;-
pro fourfit,phase,data,sig,nterms,c,csig,YFIT=yfit,CHISQ=chisq

   w = 1.0/sig^2
   guess = fltarr(nterms*2+1)
   guess[0] = mean(data)

   yfit = curvefit(phase,data,w,guess,sigterms,function_name="FOURFUNC")

   chisq = total( ((data-yfit)/sig)^2 ) / float(n_elements(data)-n_elements(guess))

   c=guess
   csig=sigterms
   
end

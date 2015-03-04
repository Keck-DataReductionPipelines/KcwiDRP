;+
; NAME:
;	meanerr2
; PURPOSE: (one line)
;	Calculate the mean and estimated error for a set of weighted data points
; DESCRIPTION:
;	This routine is adapted from Program 5-1, XFIT, from "Data Reduction
;	and Error Analysis for the Physical Sciences", p. 76, by Philip R.
;	Bevington, McGraw Hill.  This routine computes the weighted mean using
;	Instrumental weights (w=1/sigma^2).  The final uncertainty is
;	insensitive to a multiplicative constant on the weights.
; CATEGORY:
;	Statistics
; CALLING SEQUENCE:
;	meanerr2,x,weight,xmean,xsigma
; INPUTS:
;	x      - Array of data points
;	weight - array of weights for data points
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	xmean   - weighted mean
;	xsigma  - uncertainty of xmean
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 2000/10/01
;  2000/10/3, MWB, mostly WMG, it now works.
;-
pro meanerr2,x,weight,xmean,xsigma

   if n_elements(x) eq 1 then begin
      xmean  = x[0]
      xsigma = sqrt(1.0/weight)
   endif else begin
      sum    = total(weight)
      if sum eq 0.0 then print,'MEANERR2: sum is zero.'
      sumx   = total(weight*x)
      xmean  = sumx/sum
      xsigma = sqrt( total( ((x-xmean)^2)*weight*n_elements(x) ) / $
                  ( float(n_elements(x)-1) * total(weight)) )
   endelse

end

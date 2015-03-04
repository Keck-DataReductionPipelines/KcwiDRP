pro meanerr,x,sigmax,xmean,sigmam,sigmad
;+
; NAME:
;	meanerr
; PURPOSE: (one line)
;	Calculate the mean and estimated errors for a set of data points
; DESCRIPTION:
;	This routine is adapted from Program 5-1, XFIT, from "Data Reduction
;	and Error Analysis for the Physical Sciences", p. 76, by Philip R.
;	Bevington, McGraw Hill.  This routine computes the weighted mean using
;	Instrumental weights (w=1/sigma^2).
; CATEGORY:
;	Statistics
; CALLING SEQUENCE:
;	meanerr,x,sigmax,xmean,sigmam,sigmad
; INPUTS:
;	x      - Array of data points
;	sigmax - array of standard deviations for data points
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	xmean  - weighted mean
;	sigmam - standard deviation of mean
;	sigmad - standard deviation of data
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1992 Feb 20
;-

if n_elements(x) eq 1 then begin
   xmean  = x[0]
   sigmam = sigmax[0]
   sigmad = sigmax[0]
endif else begin
   weight = 1.0/sigmax^2
   sum    = total(weight)
   if sum eq 0.0 then print,'MEANERR: sum is zero.'
   sumx   = total(weight*x)
   xmean  = sumx/sum
   sigmam = sqrt(1.0/sum)
   sigmad = sqrt(total((x-xmean)^2)/(n_elements(x)-1))
endelse

end

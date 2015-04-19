; $Id: ccpeak.pro,v 1.2 2014/09/26 00:27:34 neill Exp $
;+
; NAME:
;           CCPEAK
;
; PURPOSE:
;       Locates the precise location of the peak in the
;       cross-correlation function between two vectors.
;       (Locates LAG_max)
;
; CALLING SEQUENCE:
;
;       LAG_max = CCPEAK(VEC1, VEC2 [, RADIUS ])
;
; INPUTS:
;
;       VEC1, VEC2 - Functions to be cross-correlated

; OPTIONAL INPUTS:
;
;       RADIUS -  How many array elements around the 
;                 nominal peak where polynomial fit 
;                 should be performed.
;
; OUTPUTS:
;
;       LAG_max - Lag at which cross-correlation 
;                 function is maximized
;
; RESTRICTIONS:
;
;       Uses my POLYFIT procedure, not IDL's
;       POLY_FIT
;
;       Uses C_CORRELATE to perform cross-correlation
;
; MODIFICATION HISTORY:
; Written sometime in December 2002 by JohnJohn
; 07 June 2003 JJ - Fixed bug where CF was being indexed out of
; range. Also limited the minimum and maximum lag returned to be
; min(lag) and max(lag), respectively.
; 26 June 2003 JJ - Default radius is now 50 rather than 10
; 25 Sept 2014 JDN - Added offset keyword to use as prior
;-

function ccpeak,arr1, arr2, radius, ccf=cf, lag=lag, offset=offset
on_error,2		;Return to caller if an error occurs
n = n_elements(arr1)
if n_elements(radius) eq 0 then radius = 50
if keyword_set(offset) then off = offset else off = 0
lag = fillarr(1,-radius,radius) + off
cf = c_correlate(arr1,arr2,lag)
dum = max(cf, ind)

srad = 3
sublag = lag[(ind-srad) > 0:(ind+srad) < (2*radius)]
subcf  = cf[(ind-srad) > 0:(ind+srad) < (2*radius)]
a = polyfit(sublag,subcf,2)
maxlag = - a[1]/(2.*a[2])
nlag = n_elements(lag)
if maxlag lt lag[0] then maxlag = lag[0]
if maxlag gt lag[nlag-1] then maxlag = lag[nlag-1]
return,maxlag
end


function mode, array, binsz

;+
; NAME:
;       MODE
;
; PURPOSE:
;       Return the mode of an array.
;
; CALLING SEQUENCE:
;       mod = mode(x)
;
; INPUTS:
;       array    the array of interest
;
; OPTIONAL INPUTS:
;       binsz    if set, the size of the histogram bins.  Default = 1
;
; OUTPUTS:
;       The value of the array where the maximum number of elements of 
;       array are located given the bin size.
;
; PROCEDURE
;       Uses histogram, stolen code from histo.pro
;
; COMMON BLOCKS:
;       None.
;
; NOTES
;
; REFERENCES
; 
; AUTHOR and DATE:
;     Jeff Hicke     11/21/93
;
; MODIFICATION HISTORY:
;
;-
;

   if (keyword_set(binsz) eq 0) then binsz = 1

   if not finite(binsz) then binsz = 1

   hist = histogram(array,binsize=binsz)

   ifin=n_elements(hist)

   xvalues = findgen(ifin)*binsz+min(array)+binsz/2.

   t=where(hist eq max(hist),nt)
   if nt gt 1 then $
	   ret=avg(xvalues(t)) $
   else	   ret=xvalues(t)

   return, ret

end


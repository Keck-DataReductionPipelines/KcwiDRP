;+
; NAME: 
;  num_rate
; PURPOSE:   (one line only)
;  Gives the relative number vs. apparent motion rate(arcsec/hr) of KBOs.
; DESCRIPTION:
;  This is a crude approximation of the number of objects as a
;  function of distance, and rate is given as a proxy for distance.
;  The intended range of the rate is from 2arcsec/hr to 5arcsec/hr, it
;  is valid to evaluate it at any rate, but will be poorly representatibe
;  outside of 2 to 5.  This function is meant to be probabalistically
;  sampled with smplprb.pro.  The normalization of this function is
;  arbritrary.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  array=num_rate(x)
; INPUTS:
;  x        :An array or scalar value in arcsec/hr
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  array    :Returns a float array of the same size as x.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/13, Written by Maureen Teyssier, REU Lowell Observatory
;  2004/07/15, MWB, incorporated into library.
;  2004/07/18, MWB, changed to a more realistic function based on real data.
;-
function num_rate,x

   self='NUM_RATE: '
   if badpar(x,[1,2,3,4,5,6],[0,1],CALLER=self+'(x) ',npts=npts,rank=rank) $
             then return,-1

   if rank eq 0 then y=3.0 else y=replicate(3.0,npts)

; old, piecewise continuous function
;   z1=where(x lt 2,c1) 
;   z2=where(x ge 2 and x le 3,c2)
;   z3=where(x gt 3 and x le 5,c3)
;   z4=where(x gt 5,c4)
;   if c1 gt 0 then y[z1]=0.
;   if c2 gt 0 then y[z2]=.5*x[z2]-.5
;   if c3 gt 0 then y[z3]=2.35-.45*x[z3]
;   if c4 gt 0 then y[z4]=0.

   ; four quassians plus a constant term in the initialization
   r  = [2.9,2.9,3.6,3.9]
   hw = [0.15,0.45,0.25,0.55]
   pk = [40.0,100.0,5.0,10.0]

   for i=0,n_elements(r)-1 do begin
      arg = ((x - r[i])/hw[i])^2
      z = where(arg lt 50.0,count)
      if count ne 0 then $
         y[z] += 1.0/exp(((x[z] - r[i])/hw[i])^2) * pk[i]
   endfor

   return,y

end

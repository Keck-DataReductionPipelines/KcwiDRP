;+
; NAME: 
;  num_mag
; PURPOSE:   (one line only)
;  Gives the relative number vs. magnitude of KBOs.
; DESCRIPTION:
;  This is an approximation of the number of objects as a function of
;  apparent magnitude.  The function is a power law and is intended to
;  represent the large-size distribution of KBOs.  This function will be
;  reasonable for a magnitude brighter than 26.
;  This function is meant to be probabalistically sampled with smplprb.pro.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  array=num_mag(m)
; INPUTS:
;  m      :Vector or scalar of magnitude values
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  array  :Returns an array of the same size as m. (when plotted it
;             looks like an exponential function)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/13, Written by Maureen Teyssier, REU Lowell Observatory
;  2004/07/15, MWB, incorporated into library.
;-
function num_mag,m,f

   self='num_mag: '
   if badpar(m,[1,2,3,4,5,6,7],[0,1],CALLER=self+'(m) ') then return,-1

   ; slope of the power-law density function, eg., Bernstein et al., AJ, 2004
   alpha=0.83

   ; m0=22.7-2.5*alog10(f)
   m0 = 20.2  ; set to give 10 objects (f=10) per sq.-degree at R=22.7

   p=alpha*alog(10)*10^(alpha*(m-m0))

   return,p

end

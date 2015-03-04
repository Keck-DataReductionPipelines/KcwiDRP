;+
; NAME:
;	pdmshif
; PURPOSE: (one line)
;	Zero-point shift search by phase dispersion minimization ($\theta$ based).
; DESCRIPTION:
;	This routine computes the Theta statistic to look for the optimum
;	zero-point shift between two datasets for a given period.
;	The theta statistic is the same as used for period searching
;	in time-series data using the technique described by Stellingwerf,
;	ApJ, 224, pp. 953-960 (1978).  The shift reported is that which
;	is added to x2 to register with respect to x1.
; CATEGORY:
;       Photometry
; CALLING SEQUENCE:
;	pdmshif,t1,x1,sig1,t2,x2,sig2,period,shif1,shif2,dshif,shiftv,theta
; INPUTS:
;	t1    - independent variable for first dataset (usually time)
;	x1    - dependent variable for first dataset
;                   (usually magnitude or intensity)
;	sig1  - Uncertainty on x for first dataset.
;	t2    - independent variable for first dataset (usually time)
;	x2    - dependent variable for first dataset
;                   (usually magnitude or intensity)
;	sig2  - Uncertainty on x for first dataset.
;	period - Lightcurve period. (same units as t1 and t2).
;	shif1 - Lower limit to zero-point shift (units=[x1])
;	shif2 - Upper limit to zero-point shift (units=[x1])
;	dshif - Frequency interval.  (units=[1/t])
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	shiftv  - Zero-point shift vector.
;	theta - PDM statistic (1 ==> insignificant, 0 ==> significant).
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1992 Feb 13, by Marc W. Buie, Lowell Observatory
;  95/02/23 - MWB - added NBINS keyword
;-
pro pdmshif,t1,x1,sig1,t2,x2,sig2,period,shif1,shif2,dshif,shiftv,theta, $
      NBINS=in_nbins

if badpar(in_nbins,[0,1,2,3,4,5],0,CALLER='PDM2 (nbins) ',DEFAULT=80) then return
nbins = fix(in_nbins)

if (shif1 eq shif2) then begin
   print,"Shift start and stop values must be different."
   return
endif

if (shif1 gt shif2) then begin
   tmp=shif1
   shif1=shif2
   shif2=tmp
endif

shiftv = findgen((shif2-shif1)/dshif)
shiftv = shiftv/(n_elements(shiftv)-1)
shiftv = shiftv * (shif2-shif1) + shif1
theta = fltarr(n_elements(shiftv))

width = 1.0/(nbins/2.0)
pbinl = findgen(nbins)/float(nbins)
pbinr = pbinl + width

meanerr,[x1,x2],[sig1,sig2],mx,dummy,sigsamp
varsamp = sigsamp^2

; Compute the phase, fixed for all computations.
lphase = [t1,t2] / period
lphase = lphase - fix(lphase)
lphase = [lphase,lphase+1.0]

;Setup the sigma vector (unaffected), double it as well.
sigx=[sig1,sig2]
sigx=[sigx,sigx]

for i=0,n_elements(shiftv)-1 do begin

;Construct the new shifted data for this pass through the loop
   x = x2+shiftv[i]
   x = [x1,x]
   x = [x,x]

   samp = intarr(nbins)
   sig  = fltarr(nbins)

   for p=0,nbins-1 do begin
      z = where( lphase gt pbinl[p] and lphase le pbinr[p], count)
      if (count gt 1) then begin
         meanerr,x[z],sigx[z],m,dummy,val
         sig[p] = val
         samp[p] = count
      endif
   endfor

   z = where(samp gt 0)
   s = total(sig[z]^2*(samp[z]-1))/float(total(samp)-nbins)
   theta[i] = s/varsamp
endfor

end

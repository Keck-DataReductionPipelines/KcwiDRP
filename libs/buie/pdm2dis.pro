;+
; NAME:
;	pdm2dis
; PURPOSE: (one line)
;	Phase disconnect shift search by phase dispersion minimization ($\theta$ based).
; DESCRIPTION:
;	This routine computes the Theta statistic to look for the optimum
;	zero-point shift between two datasets for a given period.
;	The theta statistic is the same as used for period searching
;	in time-series data using the technique described by Stellingwerf,
;	ApJ, 224, pp. 953-960 (1978).  The shift reported is that which
;	is added to the phase of x2 to register with respect to x1.
; CATEGORY:
;       Photometry
; CALLING SEQUENCE:
;	pdm2dis,t1,x1,sig1,t2,x2,sig2,period,shif1,shif2,dshif,shiftv,chi2
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
;	shif1 - Lower limit to phase shift in fractional rotations
;	shif2 - Upper limit to phase shift in fractional rotations
;	dshif - phase shift interval.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	shiftv  - Zero-point shift vector.
;	chi2    - PDM statistic (1 ==> perfect, >>1 is bad).
;	avgyes  - Average number of points contained within non-empty bins.
;	nempty  - Total number of empty bins.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1992 Feb 13, by Marc W. Buie, Lowell Observatory
;  95/02/23 - MWB - added NBINS keyword
;  95/08/03 - MWB, cloned from pdm2shif
;-
pro pdm2dis,t1,x1,sig1,t2,x2,sig2,period,shif1,shif2,dshif,shiftv,chi2, $
      avgyes,nempty,NBINS=in_nbins

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
chi2   = fltarr(n_elements(shiftv))
avgyes = fltarr(n_elements(shiftv))
nempty = intarr(n_elements(shiftv))

width = 1.0/(nbins/2.0)
pbinl = findgen(nbins)/float(nbins)
pbinr = pbinl + width

;Setup the data vector, fixed for all computations.
x = [x1,x2]
x = [x,x]

;Setup the sigma vector (unaffected), double it as well.
sigx=[sig1,sig2]
sigx=[sigx,sigx]

for i=0,n_elements(shiftv)-1 do begin

	; Compute the phase, fixed for all computations.
	lphase = [t1,t2+shiftv[i]*period] / period
	lphase = lphase - fix(lphase)
	lphase = [lphase,lphase+1.0]

   samp = intarr(nbins)
   chi  = fltarr(nbins)

   for p=0,nbins-1 do begin
      z = where( lphase gt pbinl[p] and lphase le pbinr[p], count)
      if (count gt 1) then begin
         avgyes[i] = avgyes[i]+count
         meanerr,x[z],sigx[z],m,dummy,val
         chi[p] = total(((x[z] - m)/sigx[z])^2)/(count-1)
         samp[p] = count
      endif
   endfor

   s = total(chi)/float(total(samp)-nbins)
   chi2[i] = s
   z = where(samp ne 0)
   avgyes[i] = avgyes[i] / float(n_elements(z))
   nempty[i] = nbins - n_elements(z)
endfor

end

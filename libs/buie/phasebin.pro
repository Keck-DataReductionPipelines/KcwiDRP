pro phasebin,t,x0,sigx0,t0,period,nbins,rotphase,x,sig,chi2,pop
;+
; NAME:
;	phasebin
; PURPOSE: (one line)
;	Compute a phase binned average lightcurve
; DESCRIPTION:
;	This routine bins lightcurve data as a function of time into a
;	binned composite lightcurve.  Bins with no data are not returned.
; CATEGORY:
;       Photometry
; CALLING SEQUENCE:
;	phasebin,t,x0,sigx0,t0,period,nbins,rotphase,x,sig
; INPUTS:
;	t     - independent variable (usually time)
;	x0    - dependent variable (usually magnitude or intensity)
;	sigx0 - Uncertainty on x.
;	t0    - Reference time for 0 phase.
;	period- Rotational period to use when binning (same units as t).
;	nbins - Number of bins to break rotation into.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	rotphase - Rotational phase for composite lightcurve.
;	x        - composite lightcurve.
;	sig      - Standard deviation of the mean in each bin.
;	chi2     - Goodness of fit to data within bin.
;	pop      - Number of points in each bin.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1992 Apr 22, by Marc W. Buie, Lowell Observatory
;  2000/11/08, MWB, removed use of obsolete ()
;-

pbinl = findgen(nbins)/float(nbins)
pbinr = (findgen(nbins)+1)/float(nbins)
lphase = (t - t0)/period
lphase = lphase - fix(lphase)

x     = fltarr(nbins)
pop   = intarr(nbins)
sig   = fltarr(nbins)
chi2  = fltarr(nbins)

for p=0,nbins-1 do begin
   z = where( lphase gt pbinl[p] and lphase le pbinr[p], count)
   pop[p] = count
   if (count gt 1) then begin
      meanerr,x0[z],sigx0[z],m,dummy,vals
      chi2[p] = count
      sig[p]  = dummy
      x[p]    = m
   endif else if (count eq 1) then begin
      x[p]    = x0[z[0]]
      sig[p]  = sigx0[z[0]]
      chi2[p] = 1.0
   endif
endfor

rotphase = (pbinl+pbinr)/2.0

z=where(pop ne 0)

rotphase=rotphase[z]
x=x[z]
pop=pop[z]
sig=sig[z]
chi2=chi2[z]

end

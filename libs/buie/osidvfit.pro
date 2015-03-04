;+
; NAME:
;  osidvfit
; PURPOSE:
;  Find the linear fit coefficients for a spectrum for each order
; DESCRIPTION:
;  The spectrum and a reference to be divided by are input.  The spectrum is
;  divided by the reference and this divided spectrum is then filtered for noise
;  and deviant points.  A linear fit is then applied to each order of the
;  filtered divided spectrum.
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  osidvfit,calib,divspec
; INPUTS:
;  calib    - The calibration information for the spectrum
;  spectrum - The spectrum to be fit
;  avgspec  - The spectrum to be used as a divisor
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BAD - The bad flags associated with the average spectrum
; OUTPUTS:
;  fit - The linear fit coefficients for each order; this is in the form of a
;        2-d array with the fit[0,i]=yint and fit[1,i]=slope where i is the
;        order of the spectrum
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  98/06/24 - Written by Chris Dalla Piazza, Lycoming College
;-

pro osidvfit,calib,spectrum,avgspec,fit,BAD=bad

; Check inputs for validity

if n_params() eq 0 then begin
  print,'osidvfit,calib,spectrum,avgspec,fit,BAD=bad'
  return
endif

if badpar(calib,8,1,CALLER='OSIDVFIT (calib) ') then return
if badpar(spectrum,[4,5],1,CALLER='OSIDVFIT (spectrum) ') then return
if badpar(avgspec,[4,5],1,CALLER='OSIDVFIT (avgspec) ') then return

; Define the bad flag filter and other variables for later use

fit=fltarr(2,calib.nor)
z=where(bad eq 1)
z1=where(bad eq 0)

; Create the divided spectrum

divspec=fltarr(calib.npts)
divspec[z1]=spectrum[z1]/avgspec[z1]

;
; Find the linear fit to the spectrum
;

for i=0,calib.nor-1 do begin

; Create an index and a subset of the spectrum for this order

  idx=indgen(calib.o[i,1]-calib.o[i,0]+1)+calib.o[i,0]
  zord=where(z1 ge min(idx) and z1 le max(idx))
  zord=z1[zord]
  spec=spectrum[idx]

; Find the maximum value of the signal.  This is by definition the tenth
; greatest point in the order

  sorted=sort(spec[zord])
  maxidx=sorted[n_elements(zord)-10]
  max=spectrum[zord[maxidx]]

; Do the same for the average spectrum

  avspec=avgspec[idx]
  avsorted=sort(avspec)
  avmaxidx=avsorted[n_elements(idx)-10]
  avmax=avspec[avmaxidx]

; Make a filter with the points that pass the test

  filter=where(spec gt 0.1*max and avspec gt 0.1*avmax and bad[idx] eq 0)

; Reindex the filter so that it is scaled the same as the original idx variable

  filter=filter+min(idx)

; Use column 128 of the CCD as the new origin for the coordinate transformation

  origin=poly(128.0,calib.cof[i,*])

; Find the linear fit coefficients for the filtered divided spectra

  fit[*,i]=goodpoly(calib.w[filter]-origin,divspec[filter],1,2,yfit,newx,newy)

;*******************************************************************************
; This commented out section will display the points that are being used to make
; the fit as well as the fits!

;if i eq 0 then begin
;if !D.WINDOW gt 30 then window,0 else window,!D.WINDOW+1
;endif
;newx=newx+origin
;if i eq 0 then plot,newx,newy,xr=[calib.w[0],calib.w[calib.npts-1]] else ;oplot,newx,newy
;oplot,newx,yfit

endfor

end

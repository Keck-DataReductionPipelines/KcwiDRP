;+
; NAME:
;  osislope
; PURPOSE:
;  Correct an IR OSIRIS spectrum for the slope problem.
; DESCRIPTION:
;  Each order of an IR spectrum from OSIRIS is corrected for the "sloping"
;  problem given the linear fit coefficients for each order of the spectrum.
;  The origin of this linear fit for each order is column 128 of the CCD
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  osislope,calib,spec,fit,bad
; INPUTS:
;  calib - Calibration structure variable
;  spec  - Spectrum to be slope corrected
;  fit   - Linear fit information to be applied to spectrum
;  bad   - Badflags for the spectrum
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  outspec - Slope corrected spectrum
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  98/06/05, Written by Chris Dalla Piazza, Lycoming College
;  98/07/17, CDP Added the badflags for the spectrum
;-

pro osislope,calib,spec,fit,outspec

; Check the validity of the inputs

if n_params() eq 0 then begin
   print,'osislope,calib,spec,fit,outspec'
   return
endif

if badpar(calib,8,1,CALLER='OSISLOPE (calib) ') then return
if badpar(spec,[4,5],1,CALLER='OSISLOPE (spec) ') then return
if badpar(fit,[4,5],2,CALLER='OSISLOPE (fit) ') then return

; Define variables for later use

origin=fltarr(calib.nor)
correct=fltarr(calib.npts)

; Define the origin of the coordinate system to be column 128 of the CCD

for i=0,calib.nor-1 do origin[i]=poly(128.0,calib.cof[i,*])

; Build the correction vector

for i=0,calib.nor-1 do begin
  for j=calib.o[i,0],calib.o[i,1] do begin

    correct[j]=fit[1,i]*(calib.w[j]-origin[i])+fit[0,i]

  endfor
endfor

; Apply the correction

outspec=fltarr(calib.npts)
outspec=spec/correct

end

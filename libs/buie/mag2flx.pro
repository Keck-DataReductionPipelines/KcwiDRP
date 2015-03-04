;+
; NAME:
;     mag2flx
; PURPOSE: (one line)
;     Convert from magnitudes to flux units with errors.
; DESCRIPTION:
; CATEGORY:
;     Astronomy
; CALLING SEQUENCE:
;     mag2flx,mag,magerr,flux,fluxerr
; INPUTS:
;     mag    - Magnitudes.
;     magerr - Uncertainties on the magnitudes.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     ZEROPT  - Magnitude that corresponds to a flux of 1. (default=0)
; OUTPUTS:
;     flux    - Flux values for the magnitudes.
;     fluxerr - Uncertainties on the fluxes.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-
pro mag2flx,mag,magerr,flux,fluxerr, ZEROPT = zeropt

if badpar(mag,[2,3,4,5],[0,1,2,3,4,5,6,7,8],caller='MAG2FLX: (mag) ') then return
if badpar(magerr,[2,3,4,5],[0,1,2,3,4,5,6,7,8],caller='MAG2FLX: (magerr) ') then return
if badpar(zeropt,[0,2,3,4,5],0,caller='MAG2FLX: (zeropt) ',default=0.0) then return

flux = 10.0^((zeropt - mag)/2.5)
fluxerr = flux*magerr/ (2.5*alog10(exp(1.0)))

end

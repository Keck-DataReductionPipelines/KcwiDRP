;+
; NAME:
;	difref
; PURPOSE: (one line)
;	Compute amount of atmospheric refraction relative to 5000 Angstroms.
; DESCRIPTION:
;	this function is based on the formulas in Filippenko, PASP, v. 94,
;	pp. 715-721 for the index of refraction of air (routine REFRAC).
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;	ans = difref(wave,pressure,temp,water,zenith)
; INPUTS:
;	wave     - Wavelength of light, in microns.
;	pressure - Atmospheric pressure, in mm of Hg.
;	temp     - Atmospheric temperature, in degrees C.
;	water    - Water vapor pressure, in mm of Hg.
;	zenith   - Angle from the zenith, in radians.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	Returns the amount of differential refraction in arcseconds.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1991 Feb, by Marc W. Buie, Lowell Observatory
;-

function difref,wave,pressure,temp,water,zenith

dr = 206265.0 * $
       (refrac(wave,pressure,temp,water)-refrac(0.5,pressure,temp,water)) * $
       tan(zenith)

return,dr
end

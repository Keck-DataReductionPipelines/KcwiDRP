;+
; NAME:
;  refrac
; PURPOSE:
;  Apply atmospheric refraction to a ``true'' zenith angle
; DESCRIPTION:
;  This calculation is based on a few different sources.  First, it is
;  assumed that the index of refraction of air at the base of the atmosphere
;  can be calculated (see AIRINDEX).  From the index of refraction, the
;  bending is computed from the formula on p.55 of the old Explanatory Supplment
;  to the Nautical Almanac.  This formula has been modified by removing the
;  h/rho term.  The explanatory supplement doesn't indicate that this is
;  legitimate but I've validated this computation against a more emperical
;  formalism from Eisele and Shannon (NRL memo 3058, May 1975).  Eisele and
;  Shannon don't indicate the wavelength of light used but if I use 0.56 microns
;  and compare for the same input conditions (dry air only), the refraction
;  computed agrees to within 1 arcsec down to 51 degrees zenith angle and
;  is good to 10 arcsec down to 80 degrees.
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  zref = refrac(z,wave,pressure,temp,relhum)
; INPUTS:
;  z        - true zenith angle (as if there were no atmosphere), in radians
;	wave     - wavelength of light, in microns
;	pressure - atmospheric pressure in mm of Hg
;	temp     - atmospheric temperature in degrees C
;	relhum   - Relative humidity (in percent)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  returned is the refracted (or apparent) zenith distance.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  Not accurate (nor useful) for z > 85 degrees.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  97/03/03, Written by Marc W. Buie, Lowell Observatory
;
;-

function refrac,z,wave,pressure,temp,relhum

n = airindex(wave,pressure,temp,relhum)

zref = asin(sin(z)/n)

return,zref

end

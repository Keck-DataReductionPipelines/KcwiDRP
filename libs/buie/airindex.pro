;+
;NAME:
;	airindex
;PURPOSE: (one line)
;	Compute the real part of the refractive index of air.
;DESCRIPTION:
;	This function is based on the formulas in Filippenko, PASP, v. 94,
;	pp. 715-721 for the index of refraction of air.  The conversion from
;  relative humidity to vapor pressure is from the Handbook of Chemistry
;  and Physics.
;CATEGORY
;  Miscellaneous
;CALLING SEQUENCE:
;	n = airindex(wave,pressure,temp,relhum)
;INPUTS:
;	wave     - wavelength of light, in microns
;	pressure - atmospheric pressure in mm of Hg
;	temp     - atmospheric temperature in degrees Celcius
;	relhum   - Relative humidity (in percent)
;OUTPUTS:
;	return value is the index of refraction for the input conditions.
;REVISION HISTORY:
;	Written by Marc W. Buie, STScI, 1991/02/28
;  1997/03/03, MWB, changed name from REFRAC, changed to relative humidity
;  2005/08/23, MWB, fixed bug in one internal equation
;COMMENTS:
;
;-

function airindex,wave,pressure,temp,relhum

   n = double(64.328) + $
           29498.1/(146.0 - (1.0/wave)^2) + 255.4/(41.0 - (1.0/wave)^2)

   pfac = double(pressure) * (1.0+(1.049-0.0157*temp)*1.0e-6*pressure) / $
                                 (720.883*(1.0+0.003661*temp))

   dt   = 100.0 - temp
   logp = 2.8808 - 5.67*dt/(274.1+temp-0.15*dt)
   f    = (relhum/100.0) * 10.0^logp

   water = (double(0.0624) - 0.000680/wave^2)*f/(1.0+0.003661*temp)

   n = ( n - water ) * pfac

   n = float(1.0 + n * 1.0e-6)

   return,n

end

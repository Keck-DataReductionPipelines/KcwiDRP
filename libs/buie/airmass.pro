;+
; NAME:
;  airmass
; PURPOSE: (one line)
;  Compute airmass for one or more times.
; DESCRIPTION:
;  This is should be a pretty good function for computing the air mass factor.
;  The default is to use the cosine based formula derived by David Tholen
;  but the older secant based formula from Hardie is still available.  The
;  zenith angle is corrected for refraction before using either formula (see
;  REFRAC).  The defaults on the atmospheric conditions are STP.  This function
;  should be quite good up to 5 airmasses.  This formula will work up to
;  a zenith angle of 80 degrees after which the computation id not done.
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  am = airmass(jd,ra,dec,lat,lon,wave,pressure,temp,relhum)
; INPUTS:
;  jd  - Julian date (must be double precision to get nearest second).
;  ra  - Right ascension (of date) in radians.
;  dec - Declination (of date) in radians.
;  lat - Latitude of observatory in radians.
;  lon - West longitude of observatory in radians.
; OPTIONAL INPUT PARAMETERS:
;  wave     - wavelength of light, in microns (default=0.56)
;  pressure - atmospheric pressure in mm of Hg (default=760.0)
;  temp     - atmospheric temperature in degrees C (default=0.0)
;  relhum   - Relative humidity (in percent) (default=0.0)
; KEYWORD INPUT PARAMETERS:
;  UT  - Time, in hours to add to JD to get the correct Universal Time.
;  HARDIE - Flag, if set causes Hardie formula to be used.
; KEYWORD OUTPUT PARAMETERS:
;  ALT - Optional return of the altitude for each airmass.
;  LHA - Optional return of the local hour angle.
;  LST - Optional return of the local sidereal time.
;  AZI - Optional return of the azimuth (west from south).
; OUTPUTS:
;  Return value is the airmass in single precision.
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
; RESTRICTIONS:
;  Any input may be a vector.  If more than one is a vector then the
;  lengths must match.  The return will have the same dimensions as
;  the input.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written 1992 March, by Marc W. Buie, Lowell Observatory
;  94/05/05 - MWB, modified to split out the LST calculation, added LST and
;                LHA optional keyword outputs and UT keyword input.'
;  97/03/03 - MWB, added the Tholen airmass equation as the default.  Hardie
;                is still included as an option.  Also added refraction
;                to the calculation which added numerous inputs.
;  97/10/23 - MWB, added the AZI keyword.
;  2002/01/08, MWB, now allows 2-d inputs on jd,ra,dec
;-
function airmass,jd,ra,dec,lat,lon, $
            wave,pressure,temp,relhum, $
            ALT=alt,HARDIE=hardie,LHA=lha,LST=lst,UT=ut, AZI=azi

   if badpar(jd,[5],[0,1,2],CALLER='airmass: (jd) ') then return,jd*0.
   if badpar(ra,[4,5],[0,1,2],CALLER='airmass: (ra) ') then return,jd*0.
   if badpar(dec,[4,5],[0,1,2],CALLER='airmass: (dec) ') then return,jd*0.
   if badpar(lat,[4,5],0,CALLER='airmass: (lat) ') then return,jd*0.
   if badpar(lon,[4,5],0,CALLER='airmass: (lon) ') then return,jd*0.
   if badpar(wave,[0,2,3,4,5],0,CALLER='airmass: (wave) ', $
                                DEFAULT=0.56) then return,jd*0.
   if badpar(pressure,[0,2,3,4,5],0,CALLER='airmass: (pressure) ', $
                                DEFAULT=760.0) then return,jd*0.
   if badpar(temp,[0,2,3,4,5],0,CALLER='airmass: (temp) ', $
                                DEFAULT=0.0) then return,jd*0.
   if badpar(relhum,[0,2,3,4,5],0,CALLER='airmass: (relhum) ', $
                                DEFAULT=0.0) then return,jd*0.
   if badpar(ut,[0,2,3,4,5],0,CALLER='airmass: (UT) ', $
                              DEFAULT=0) then return,jd*0.

   hangle,jd,ra,dec,lat,lon,lha,lst,UT=ut

   alt = asin( sin(lat)*sin(dec) + cos(lat)*cos(dec) *cos(lha) )

;   azi = atan( sin(lha), cos(lha)*sin(lat) - tan(dec)*cos(lat) )
   azi = prival(atan( -sin(lha), -cos(lha)*sin(lat) + tan(dec)*cos(lat) ))

   zenith = !pi/2.0-alt
   z = where(zenith le 1.521,count)

   am = replicate(1000000.0,n_elements(jd))
   if (count ne 0) then begin
      zenith[z] = refrac(zenith[z],wave,pressure,temp,relhum)
      IF keyword_set(hardie) THEN BEGIN
         x = 1.0/cos(zenith[z])-1
         am[z] = float(1.0+(0.9981833d0-(0.002875d0+0.0008083d0*x)*x)*x)
      ENDIF ELSE BEGIN
         cz = cos(zenith[z])
         am[z] = sqrt(235225.0*cz*cz + 970.0 + 1.0) - 485*cz
      ENDELSE
   endif

   return,float(am)

end

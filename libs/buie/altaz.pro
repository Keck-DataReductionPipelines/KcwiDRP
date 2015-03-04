;+
; NAME:
;  altaz
; PURPOSE:   (one line only)
;  Compute altitude and azimuth on sky given equatorial coordinates
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  altaz,ha,lat,dec,alt,az
; INPUTS:
;  ha - hour angle of position on sky (radians)
;  lat - Latitude of observing vantage point (radians)
;  dec - Declination of position on sky (radians)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  alt - Altitude of position above horizon (radians)
;  az  - Azimuth of position (radians)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2010/07/02, Written by Marc W. Buie, Southwest Research Institute.  Ported
;                from altaz.c in my C-library.
;-
pro altaz,ha,lat,dec,alt,az

   cha  = cos(ha)
   sha  = sin(ha)
   cdec = cos(dec)
   sdec = sin(dec)
   clat = cos(lat)
   slat = sin(lat)

   arg = slat*sdec + clat*cdec*cha

   alt=dblarr(n_elements(arg))
   z=where(arg ge -1.0 and arg le 1.0,count)
   if count ne 0 then alt[z]=asin(arg[z])

   ; special case near north celstial pole
   az=atan(-sha,sdec*clat/cdec - cha*slat)
   z=where(cdec le 1.0e-6 and dec > 0.0,count)
   if count ne 0 then az[z] = 0.0
   z=where(cdec le 1.0e-6 and dec < 0.0,count)
   if count ne 0 then az[z] = !dpi
   z=where(abs(ha) le 1.0e-6 and (dec-lat) ge 0.0,count)
   if count ne 0 then az[z] = 0.0

   alt=trimrank(alt,/overwrite)
   az=prival(trimrank(az,/overwrite))

end

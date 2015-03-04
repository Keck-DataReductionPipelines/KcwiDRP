;+
; NAME:
;  lcltoeq
; PURPOSE:   (one line only)
;  Convert from local horizon coordinates to equatorial coordinates
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  lcltoeq,alt,az,lat,ha,dec
; INPUTS:
;  alt  - Altitude above horizon (radians)
;  az   - Azimuth (radians)
;  lat  - Latitude of viewpoint (radians)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  ha   - Local hour angle relative to the prime meridian (radians)
;  dec  - Declination of position (radians)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2010/07/02, Written by Marc W. Buie, Southwest Research Institute.  Ported
;                from lcltoeq.c in my C-library.
;-
pro lcltoeq,alt,az,lat,ha,dec

   calt = cos(alt)
   salt = sin(alt)
   caz  = cos(az)
   saz  = sin(az)
   clat = cos(lat)
   slat = sin(lat)

   sdec = ((slat*salt + clat*calt*caz) < 1.0) > (-1.0)

;   dec=dblarr(n_elements(arg))
;   z=where(arg ge -1.0 and arg le 1.0,count)
;   if count ne 0 then dec[z]=asin(arg[z])
   dec = asin(sdec)
   cdec=cos(dec)

;   ha = atan(-saz,salt*clat/calt - caz*clat)

   sha = -calt*saz/cdec
   cha = (sdec*clat-calt*caz)/(cdec*slat)
   ha = atan(sha,cha)

   ha =trimrank(ha ,/overwrite)
   dec=trimrank(dec,/overwrite)

end

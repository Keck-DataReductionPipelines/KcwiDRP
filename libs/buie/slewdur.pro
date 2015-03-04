;+
; NAME:
;  slewdur
; PURPOSE:
;  Estimate the time to slew the telescope from one location to another.
; DESCRIPTION:
; CATEGORY:
;  Data Acquisition
; CALLING SEQUENCE:
;  ans=slewdur(jd,ra0,dec0,ra1,dec1)
; INPUTS:
;  jd   - Julian date at time of slew
;  ra0  - Right ascension of starting point
;  dec0 - Declination of starting point
;  ra1  - Right ascension of destination
;  dec1 - Declination of destination
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is estimated time to complete a slew over the requested
;    distance (time is in seconds).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/01/27
;-
function slewdur,jd,ra0,dec0,ra1,dec1

   ; This is the GPS position for the 42", derived 1993 Sep 08
   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi

   ; Relevant motion rates (full tilt motion, no ramps).  These numbers are
   ;  all in degrees per second.
   rarate = 2.0
   decrate = 1.5
   domerate = 2.5

   ; How far to move in RA?
   radist = abs(ra0-ra1)*!radeg
   ratime = radist/rarate

   ; How far to move in Dec?
   decdist = abs(dec0-dec1)*!radeg
   dectime = decdist/decrate

   ; How long to move dome?
   hangle,jd,ra0,dec0,lat,lon,ha0
   hangle,jd,ra1,dec1,lat,lon,ha1
   altaz,ha0,lat,dec0,alt0,az0
   altaz,ha1,lat,dec1,alt1,az1
   domedist = abs(az0-az1)*!radeg
   if domedist gt 180 then domedist=360-domedist
   dometime = domedist/domerate

   ; Time for slew is the largest of the three times
   slewtime = max([ratime,dectime,dometime])

   return,slewtime

end

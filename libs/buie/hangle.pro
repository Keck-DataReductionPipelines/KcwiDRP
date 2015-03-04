;+
; NAME:
;  hangle
; PURPOSE: (one line)
;  Compute the local hour angle of an object.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  hangle,jd,ra,dec,lat,lon,lha,lst [,UT=ut]
; INPUTS:
;  JD  - Julian date (must be double precision to get nearest second).
;  RA  - Right ascension (of date) in radians.
;  DEC - Declination (of date) in radians.
;  LAT - Latitude of observatory in radians.
;  LON - West longitude of observatory in radians.
; OPTIONAL INPUT PARAMETERS:
;  None.
; KEYWORD INPUT PARAMETERS:
;  UT  - Time, in hours to add to JD to get the correct Universal Time.
;           That the same as Universal Time minus the Local Time.
; KEYWORD OUTPUT PARAMETERS:
; OUTPUTS:
;  LHA - Return of the local hour angle.
;  LST - Return of the local sidereal time.
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
; RESTRICTIONS:
;  Any input may be a vector.  If more than one is a vector then the
;  lengths must match.  The return will have the same dimensions as
;  the input.
; PROCEDURE:
; MODIFICATION HISTORY:
;
;  94/05/05 - Written by Marc W. Buie, Lowell Observatory
;  2002/01/08, MWB, now allows 2-d input for jd,ra,dec
;-
pro hangle,jd,ra,dec,lat,lon,lha,lst,UT=ut

   if badpar(jd,[5],[0,1,2],CALLER='hangle: (jd) ') then return
   if badpar(ra,[4,5],[0,1,2],CALLER='hangle: (ra) ') then return
   if badpar(dec,[4,5],[0,1,2],CALLER='hangle: (dec) ') then return
   if badpar(lat,[4,5],0,CALLER='hangle: (lat) ') then return
   if badpar(lon,[4,5],0,CALLER='hangle: (lon) ') then return
   if badpar(ut,[0,2,3,4,5],0,CALLER='hangle: (UT) ',DEFAULT=0) then return

   lsidtim,jd,lon,lst,UT=ut

   lha = lst - ra
   z = where(lha gt !pi,count)
   if (count ne 0) then lha[z] = lha[z] - 2.0*!pi
   z = where(lha le -!pi,count)
   if (count ne 0) then lha[z] = lha[z] + 2.0*!pi

end

;+
; NAME: 
;  lsidtim
; PURPOSE: 
;  Compute local sidereal time at a given longitude and time.
; DESCRIPTION:
;  This routine is based on the algorithms from p. 83 of "Astronomical
;  Algorithms" by J. Meeus, 1st edition.
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  lsidtim,jd,lon,sidtim [,UT=ut]
; INPUTS:
;  jd  - Julian Date (double precision), scalar or vector.
;  lon - West longitude of observatory in radians (scalar).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  UT  - Time, in hours to add to JD to get the correct Universal Time.
;           That the same as Universal Time minus the Local Time.
; OUTPUTS:
;  lst - Local sidereal time for each of the input times (radians).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/05/05 - Written by Marc W. Buie, Lowell Observatory
;  2002/01/08, MWB, 2-d input on jd is now allowed.
;  2002/09/05, MWB, total rewrite to use modern formula
;-
pro lsidtim,in_jd,lon,lst,UT=ut

   if badpar(in_jd,[5],[0,1,2],CALLER='lsidtim: (jd) ') then return
   if badpar(lon,[4,5],0,CALLER='lsidtim: (lon) ') then return
   if badpar(ut,[0,2,3,4,5],0,CALLER='lsidtim: (UT) ',DEFAULT=0) then return

   ; add time offset to what ever JD is provided (need not be at 0h.
   jd = in_jd + double(ut)/24.0d0

   ; Extract JD at 0h UT
   jd0 = long(jd+0.5d0) - 0.5d0

   ; compute auxillary quantity
   t = (jd0-2451545.0d0)/36525.0d0

   ; compute mean sidereal time at Greenwich at 0h UT
   gst = t*(8640184.812866d0 + $
         t*(    0.093104d0   - $
         t*     6.2d-6 ))
   gst = 1.753368559233d0 + gst/43200.0d0*!dpi

   hour = (jd-jd0)*2.0d0*!dpi * 1.00273790935d0

   st = gst + hour

   lst = st - lon

   lst = lst mod (2.0d0*!dpi)

end

;+
; NAME: 
;  lst2jd
; PURPOSE: 
;  Compute UT given local sidereal time, longitude and approximate time.
; DESCRIPTION:
;  This routine is based on the algorithms from p. 39 of "Astronomical
;  Formulae for Calculators" by J. Meeus.
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  lst2jd,jd0,lon,sidtim,jd
; INPUTS:
;  jd0 - Julian Date (double precision), at 0h UT, scalar or vector.
;          The "fractional" part of the jd is ignored.
;  lon - West longitude of observatory in radians (scalar).
;  sidtim - Local sidereal time (radians).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  jd     - Julian Date that matches sidtim.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/09/03 - Written by Marc W. Buie, Lowell Observatory
;-
pro lst2jd,in_jd0,lon,lst,jd

   if badpar(in_jd0,[5],[0,1,2],CALLER='lsidtim: (jd) ') then return
   if badpar(lon,[4,5],0,CALLER='lsidtim: (lon) ') then return
   if badpar(lst,[4,5],[0,1,2],CALLER='lsidtim: (lst) ') then return

   ; Extract JD at 0h UT
   jd0 = long(in_jd0+0.5d0) - 0.5d0

   ; compute auxillary quantity
   t = (jd0-2451545.0d0)/36525.0d0

   ; compute mean sidereal time at Greenwich at 0h UT
   gst = t*(8640184.812866d0 + $
         t*(    0.093104d0   - $
         t*     6.2d-6 ))
   gst = 1.753368559233d0 + gst/43200.0d0*!dpi
   gst = gst mod (2.0d0*!dpi)

   hour = ( lst + lon - gst )
   z=where(hour lt 0, count)
   if count ne 0 then hour[z]=hour[z]+2.0d0*!dpi
;   hour = hour mod (2.0d0*!dpi)
   hour = hour / 1.00273790935d0

   jd = jd0+hour/!dpi*0.5d0

end

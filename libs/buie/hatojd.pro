;+
; NAME:
;  hatojd
; PURPOSE: (one line)
;  Find the nearest Julian date for a given hour angle and date.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-
pro hatojd,ha,ra,lst,jd0,jd

   dt = (ha + ra - lst) / 1.002737908d0

   z = where(dt lt -!dpi,count)
   if count ne 0 then $
      dt[z] = (ha[z] + 2.0d0*!dpi + ra[z] - lst[z]) / 1.002737908d0

   z = where(dt gt !dpi,count)
   if count ne 0 then $
      dt[z] = (ha[z] - 2.0d0*!dpi + ra[z] - lst[z]) / 1.002737908d0

   jd = jd0 + dt/2.0d0/!dpi

end

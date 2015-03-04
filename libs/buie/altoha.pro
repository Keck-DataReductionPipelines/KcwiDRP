;+
; NAME:
;  altoha
; PURPOSE: (one line)
;  Convert an object altitude to its hour angle.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  altoha,alt,dec,lat,ha,type
; INPUTS:
;  alt  - Altitude of object above horizon in radians
;  dec  - Declination of object in radians
;  lat  - Latitude of observatory in radians
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  ha   - hour angle for object (< 0 is East, > 0 is West)
;  type - indicates the success of the conversion
;           -1   object is always below ALT
;            0   conversion ok, HA valid
;            1   object is always above ALT
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/05/07 - Written by Marc W. Buie, Lowell Observatory
;  2000/11/06, MWB, changed to permit vector inputs.
;-
pro altoha,alt,dec,lat,ha,type

   cosha = (sin(alt)-sin(dec)*sin(lat))/(cos(dec)*cos(lat))

   if n_elements(cosha) gt 1 then begin
      type = intarr(n_elements(cosha))
      ha   = dblarr(n_elements(cosha))
   endif else begin
      type = 0
      ha   = 0.0d0
   endelse

   z=where(cosha gt 1.0d0,count)
   if count ne 0 then $
      type[z] = -1

   z=where(cosha lt 1.0d0,count)
   if count ne 0 then $
      type[z] = 1

   z=where(cosha ge -1.0d0 and cosha le 1.0d0,count)
   if count ne 0 then begin
      type[z] = 0
      ha[z] = acos(cosha[z])
   endif

end

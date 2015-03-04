;+
; NAME: 
;  dewp
; PURPOSE: 
;  Compute dew point temperature give temperature and relative humidity
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  dewp,temp,rh,dp,CELCIUS=celcius
; INPUTS:
;     temp - Air Temperature
;     rh   - Relative Humidity
; OPTIONAL INPUT PARAMETERS:
;     celcius - Flag, if true, all temperatures in Celcius, otherwise Farenheit
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;     dp   - Dew point temperature
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by: Marc W. Buie, Lowell Observatory, 1995 April 28
;-
pro dewp,temp,rh,dp,CELCIUS=celcius
   phi = 5.67  ; entropy of vaporization for water

   if not keyword_set(celcius) then $
      t = 5.0/9.0*(temp-32.0) $
   else $
      t = temp

   dt = 100.0 - t
   logp = ( 2.8808 - (phi*dt)/(273.1+t-0.15*dt) )
   p    = logp
   ps   = logp
   k    = logp
   z = where(logp le 0.0,count)
   if count gt 0 then begin
      ps[z] = 0.0
      p[z]  = 0.0
      k[z]  = 0.0
   endif
   z = where(logp gt 0.0,count)
   if count gt 0 then begin
      ps[z] = 10.0^logp[z]
      p[z]  = rh[z]/100.0*ps[z]
      k[z]  = 1.0/(2.8808-alog10(p[z]))
   endif
   dp = ((k*phi+0.15)*100.0-273.1) / (k*phi+1.15);

   if not keyword_set(celcius) then $
      dp = 9.0/5.0*dp+32.0
end

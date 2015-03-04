;+
; NAME:
;  windstr
; PURPOSE:   (one line only)
;  Convert a wind direction angle to a string name (or back).
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  wname = windstr(wdir)
; INPUTS:
;  wdir - Wind direction in degrees, scalar or vector
;            if numbers are provided, these are converted to strings
;            if strings are provided, they are converted to integers
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the name of the wind direction (string) or the numeric
;    value depending in the input.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Note that the names are coarser than numbers. If you start with numbers,
;    convert to names and then back to numbers you probably won't get the same
;    answer.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2005/03/26
;  2008/02/03, MWB, changed to allow reverse transformation.
;-
function windstr,wdir

   self='WINDSTR: '
   if badpar(wdir,[2,3,4,5,7],[0,1],caller=self+'(wdir) ',type=type) then return,''
   direction=[ $
      "N","NNE","NE","ENE", $
      "E","ESE","SE","SSE", $
      "S","SSW","SW","WSW", $
      "W","WNW","NW","NNW"  $
      ]
   value=findgen(16)*22.5

   if type eq 7 then begin
      if n_elements(wdir) eq 1 then begin
         z=where(wdir eq direction,count)
         if count eq 0 then return,0. else return,value[z[0]]
      endif else begin
         idx = intarr(n_elements(wdir))
         for i=0,15 do begin
            z=where(wdir eq direction[i],count)
            if count ne 0 then idx[z] = i
         endfor
         return,value[idx]
      endelse
   endif else begin

      z = (wdir + 11.25) mod 360.0
      zz = where(z lt 0.,count)
      if count ne 0 then z[zz] += 360.0
      z = fix(z/22.5)

      return,direction[z]
   endelse

end

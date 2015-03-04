;+
; NAME: 
;  tvtil
; PURPOSE: 
;  Display a Pluto/Charon .til map as an image on the current display.
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
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
;  Sep 1991, Written by Marc W. Buie, Lowell Observatory
;-
pro tvtil,map,xoff,yoff,scale
  nann1 = fix(sqrt(n_elements(map)/4))
  nann=nann1*2
  start=0
  stop=3
  tv,rebin(map[start:stop],4*scale,scale),((nann1-1)*2)*scale+xoff,(nann-1)*scale+yoff,/order
  for i=2,nann1 do begin
     start=stop+1
     stop = start - 1 + i*4
     tv,rebin(map[start:stop],i*4*scale,scale),((nann1-i)*2)*scale+xoff,(nann-i)*scale+yoff,/order
  endfor
  for i=nann1,1,-1 do begin
     start=stop+1
     stop = start - 1 + i*4
     tv,rebin(map[start:stop],i*4*scale,scale),((nann1-i)*2)*scale+xoff,(i-1)*scale+yoff,/order
  endfor
end

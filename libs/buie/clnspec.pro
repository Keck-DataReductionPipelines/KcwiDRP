;+
; NAME: 
;  clnspec
; PURPOSE: 
;  Interactive cleaning of bad pixels in an OSIRIS XD spectrum.
; DESCRIPTION:
;  Plots one order at a time and allows cleaning up bad pixels.  The left
;    mouse button is used to alternately mark bad pixels and then their new
;    new value for visual interpolating.  The middle button causes the
;    nearest point to be replaced by the average of its two nearest
;    neighbors.  CLEANDAT is called on each order.
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  clnspec,calib,spec
; INPUTS:
;  calib- Anonymous structure containing all pertinent calibration
;           information.  This structure is usually loaded beforehand using
;           the routine, "ldcalir"
;  spec - 1-D spectrum vector to be cleanded (will be modified in place).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  spec - Cleaned 1-D spectrum vector
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  Uses graphics window 6 and forces its size.  !p.multi is also set to 0.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/09/15, Written by Marc W. Buie, Lowell Observatory
;  96/05/28, MWB, added changes for new calib structure
;-
pro clnspec,calib,spec

if badpar(calib,8,1,CALLER='cleanspec (calib) ') then return
if badpar(spec,[1,2,3,4,5],1,CALLER='cleanspec (spec) ') then return

for i=0,calib.nor-1 do begin

   if i ne calib.nor-1 then $
      print,'Cleaning segment ',i,'   Click right to go to next segment' $
   else $
      print,'Cleaning segment ',i,'   Click right to complete cleaning'

   x=findgen(calib.o[i,1]-calib.o[i,0]+1)
   y=spec[calib.o[i,0]:calib.o[i,1]]
   cleandat,x,y,TITLE='Order '+string(i,format='(i1)')
   spec[calib.o[i,0]:calib.o[i,1]]=y

endfor

end

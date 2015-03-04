;+
; NAME: 
;  wedge
; PURPOSE: 
;  Compute and return a gray scale step wedge.
; DESCRIPTION:
; CATEGORY:
;  Image display
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
; Oct 1991, Written by Marc W. Buie, Lowell Observatory
;-
function wedge
w=20
ramp=bytarr(!d.n_colors,5*w)
for i=0,w-1 do begin
   for j=0,!d.n_colors-1 do begin
      ramp[j,i]=j/16*16
      ramp[j,i+w]=j/8*8
      ramp[j,i+2*w]=j/4*4
      ramp[j,i+3*w]=j/2*2
      ramp[j,i+4*w]=j
   endfor
endfor
ramp=[ramp,ramp]
return,ramp
end

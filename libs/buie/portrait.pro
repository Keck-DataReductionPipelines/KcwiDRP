;+
; NAME:
;	portrait
; PURPOSE: (one line)
;	IDL procedure for initializing the PS device for portrait mode plotting.
; DESCRIPTION:
;	Output plotting device is changed to PS in portrait mode with a
;	Helvetica font.
; CATEGORY:
;	Utility
; CALLING SEQUENCE:
;	portrait
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	None.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory
;-

pro portrait

   set_plot,'ps'
   device,/portrait,ysize=25.,yoffset=1.3
   device,/Helvetica

   return
end

;+
; NAME:
;	landscap
; PURPOSE: (one line)
;	Procedure for initializing the PS device for plotting.
; DESCRIPTION:
;	Output plotting device is change to PS in landscape mode with a
;	Helvetica font.
; CATEGORY:
;	Utility
; CALLING SEQUENCE:
;	landscape
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
;	Written by Marc W. Buie, Lowell Observatory.
;-

pro landscap

set_plot,'ps'
setpage,/landscape,xsize=24.0,ysize=18.0
device,/Helvetica

return
end

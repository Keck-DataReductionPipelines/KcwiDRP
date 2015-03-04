;+
; NAME:
;	display
; PURPOSE: (one line)
;	IDL procedure for initializing the X window display for plotting.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-

pro display

case !version.os of
   'Win32': device = 'WIN'
   else: device = 'X'
endcase
set_plot,device

return
end


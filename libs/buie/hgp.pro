;+
; NAME:
;	hgp
; PURPOSE: (one line)
;	Compute a single parameter Henyey-Greenstein phase function.
; DESCRIPTION:
; CATEGORY:
;	Mathematical
; CALLING SEQUENCE:
;	ans = hgp(phase,c)
; INPUTS:
;	phase - Angle between the incident and emitted flux (in degrees).
;	c     - Henyey-Greenstein functional value.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	Return value is the phase function value(s).  Same dimension as
;	the inputs.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1991 Feb.,by Marc W. Buie, Lowell Observatory
;-
function hgp,phase,c
   return,(1.-c^2)/(1.0+c^2+2.0*c*cos(phase/!radeg))^1.5
end

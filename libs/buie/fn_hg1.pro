;+
; NAME:
;  fn_hg1
;
; PURPOSE:
;  Evaluate the 1-parameter Henyey-Greenstein phase function.
;
; DESCRIPTION:
;
; CATEGORY:
;  Mathematical
;
; CALLING SEQUENCE:
;  fn_hg1, g, a, F, /radians
;
; INPUTS:
;  g:  Phase angle(s) in degrees.
;  a:  Asymmetry parameter (negative is back, positive is forward).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  radians:  Flag, if true, phase angle is in radians instead of default degrees.
;
; OUTPUTS:
;  F:  The value of the function at each g.
;
; KEYWORD OUTPUT PARAMETERS:
;
; OPTIONAL OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;  None.
;
; SIDE EFFECTS:
;  None.
;
; RESTRICTIONS:
;  None.
;
; PROCEDURE:
; MODIFICATION HISTORY:
;  1999-Nov-13 Created by Will Grundy
;  2001-Oct-29 WMG added radians keyword
;-
pro fn_hg1,g,a,F,radians=radians

    on_error,2  ; Return to caller if an error occurs

    aa = a*a
    if keyword_set(radians) then $
       F = (1.0 - aa)/((1.0 + 2.0*a*cos(g) + aa)^1.5) $
    else $
       F = (1.0 - aa)/((1.0 + 2.0*a*cos(!DPI*g/180.0) + aa)^1.5)
    return

end

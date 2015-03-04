;+
; NAME:
;  fn_hg3
;
; PURPOSE:
;  Evaluate the 3-parameter Henyey-Greenstein phase function.
;
; DESCRIPTION:
;
; CATEGORY:
;  Mathematical
;
; CALLING SEQUENCE:
;  fn_hg3, g, a, F, /radians
;
; INPUTS:
;     g: Phase angle(s) in degrees.
;     a: Array of parameters:
;         A[0] = forward asymmetry (should be positive)
;         A[1] = backward asymmetry (should be negative)
;         A[2] = forward fraction (between 0 and 1)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;     radians:  Phase angle is in radians instead of default degrees.
;
; OUTPUTS:
;     F: The value of the function at each g.
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
pro fn_hg3,g,a,F,radians=radians

    on_error,2  ; Return to caller if an error occurs

    fn_hg1,g,a[0],fwd,radians=radians
    fn_hg1,g,a[1],bkwd,radians=radians

    F = a[2]*fwd + (1.0-a[2])*bkwd

    return

end

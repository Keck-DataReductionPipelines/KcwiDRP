;+
; NAME:
;  fn_hg2
;
; PURPOSE:
;  Evaluate the 2-parameter Henyey-Greenstein phase function.
;
; DESCRIPTION:
;
; CATEGORY:
;  Mathematical
;
; CALLING SEQUENCE:
;  FN_HG3, g, a, F, /radians
;
; INPUTS:
;     g: Phase angle(s) in degrees.
;     a: Array of parameters:
;          A[0] = asymmetry (ought to be positive)
;          A[1] = forward fraction (between 0 and 1)
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
;  2001-Nov-19 Created by Will Grundy (based on fn_HG3.pro)
;-
pro fn_hg2,g,a,F,radians=radians

    on_error,2  ; Return to caller if an error occurs

    fn_hg1,g,abs(a[0]),fwd,radians=radians
    fn_hg1,g,-abs(a[0]),bkwd,radians=radians

    F = a[1]*fwd + (1.0-a[1])*bkwd

    return

end

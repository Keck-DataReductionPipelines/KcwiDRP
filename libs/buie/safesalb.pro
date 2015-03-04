;+
; NAME:
;  safesalb
; PURPOSE:   (one line only)
;  Filter to enforce a valid range of single scattering albedo [0,1]
; DESCRIPTION:
;  This routine recasts an input value (or set of values) and upon return
;    the values are certain to be in the inclusive range of [0,1].  This
;    is done in following order and maps the infinite number range down
;    onto the finite range:
;    safesalb = |salb|
;    safesalb = safesalb mod 2.0
;    safesalb = 2.0 - safesalb where safesalb > 1.0
;
;  In the range "near" the original range this function basically implements
;    a mirror at 0 and 1.  This is most useful for ameoba-like fitting routines
;    that don't know what to do with a finite range fitting variable.
;
; CATEGORY:
;  Function fitting
; CALLING SEQUENCE:
;  return = safesalb(salb)
; INPUTS:
;  salb - input values to process (any rank allowed)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  returns the single scattering albedo reduced to its principle range, it
;    is allowed to return to the same variable, ie., salb=safesalb(salb).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   2009/02/01, Written by Marc W. Buie, Southwest Research Institute
;-
function safesalb,salb

   newsalb = salb

   ; first deal with negative values
   z=where(newsalb lt 0.0, count)
   if count ne 0 then newsalb[z] = abs(newsalb[z])

   ; reduce the range down to 0-2
   newsalb = newsalb mod 2.0

   ; flip the range 1-2 down
   z = where(newsalb gt 1.0, count)
   if count ne 0 then newsalb[z] = 2.0 - newsalb[z]

   return,newsalb

end

;+
; NAME:
;   getsalb
; PURPOSE: (one line)
;   Solve for single scattering albedo given bi-directional reflectance
; DESCRIPTION:
; CATEGORY:
;   Miscellaneous
; CALLING SEQUENCE:
;   ans = getsalb(bidref,emu,smu,holes,pzero)
; INPUTS:
;   bidref - The bi-directional reflectance.
;   emu    - Cosine of the emission angle.
;   smu    - Cosine of the incidence angle.
;   holes  - Compaction parameter value (1981 formalism).
;   pzero  - Value of the single particle phase function.
; OPTIONAL INPUT PARAMETERS:
;   None.
; KEYWORD PARAMETERS:
;   None.
; OUTPUTS:
;   returns the single scattering albedo.
; COMMON BLOCKS:
;   None.
; SIDE EFFECTS:
;   None.
; RESTRICTIONS:
;   This works on scalar input only.
; PROCEDURE:
; MODIFICATION HISTORY:
;   Written 1995 Jan 13, by Marc W. Buie, Lowell Observatory
;-
function getsalb,bidref,emu,smu,holes,pzero

;Set the boundaries for the binary search
salb_low  = 0.0
salb_high = 1.0
bidr_low  = 0.0  ; trivial
bidr_high = bidr(1.0,emu,smu,holes,pzero)

if (bidref lt bidr_low) or (bidref gt bidr_high) or $
   (emu lt 0.0) or (smu lt 0.0) then return,-1.0

eps = 0.005
maxiter=40

iter=0
while ((bidr_high-bidr_low)/bidr_high gt eps) and (iter lt maxiter) do begin
;   print,iter,salb_low,salb_high,bidr_low,bidr_high,bidref
   salb = (salb_low + salb_high)/2.0
   newbidr = bidr(salb,emu,smu,holes,pzero)
   if newbidr gt bidref then begin
      bidr_high = newbidr
      salb_high = salb
   endif else begin
      bidr_low = newbidr
      salb_low = salb
   endelse
   iter = iter + 1
endwhile

return,(salb_low+salb_high)/2.0

end

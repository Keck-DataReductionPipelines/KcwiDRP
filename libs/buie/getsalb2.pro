;+
; NAME:
;   getsalb2
; PURPOSE: (one line)
;   Solve for single scattering albedo given bi-directional reflectance
; DESCRIPTION:
; CATEGORY:
;   Miscellaneous
; CALLING SEQUENCE:
;   ans = getsalb2(bidref,emu,smu,g,holes,p,b0,theta)
; INPUTS:
;   bidref - The bi-directional reflectance.
;   emu    - Cosine of the emission angle.
;   smu    - Cosine of the incidence angle.
;   g      - Phase angle, in radians.
;   holes  - Compaction parameter value (1981 formalism).
;   p      - Value of the single particle phase function.
;   theta  - Surface roughness value.  (radians)
; OPTIONAL INPUT PARAMETERS:
;   None.
; KEYWORD PARAMETERS:
;  H93   - Flag, if set, uses the 1993 version of Hapke's approximation to the
;            Chandresekar H function.  The 1993 version is more accurate but
;            considerably slower to compute.
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
;   Written 2003/10/23, by Marc W. Buie, Lowell Observatory, cloned
;     from getsalb.pro
;-
function getsalb2,bidref,emu,smu,g,holes,p,b0,theta,H93=h93

;Set the boundaries for the binary search
salb_low  = 0.0
salb_high = 1.0
bidr_low  = 0.0  ; trivial
bidr_high = bidr2(1.0,emu,smu,g,holes,p,b0,theta,h93=h93)

; invalid input
if (emu lt 0.0) or (smu lt 0.0) then return,10.0

; desired flux is too low (w would need to be < 0)
if bidref lt bidr_low then return,-1.0

if bidref gt bidr_high then return,2.0

eps = 0.005
maxiter=40

iter=0
while ((bidr_high-bidr_low)/bidr_high gt eps) and (iter lt maxiter) do begin
;   print,iter,salb_low,salb_high,bidr_low,bidr_high,bidref
   salb = (salb_low + salb_high)/2.0
   newbidr = bidr2(salb,emu,smu,g,holes,p,b0,theta,h93=h93)
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

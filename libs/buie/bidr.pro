;+
; NAME:
;	bidr
; PURPOSE: (one line)
;	Compute the bi-directional reflectance (old Hapke formula).
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;	ans = bidr(w,emu,smu,g,holes,pzero)
; INPUTS:
;	w     - Single scattering albedo.
;	emu   - Cosine of the emission angle.
;	smu   - Cosine of the incidence angle.
;  g     - Phase angle, in radians.
;	holes - Compaction parameter value (1981 formalism).
;	pzero - Value of the single particle phase function.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Return value is the bi-directional reflectance.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;       Any input may be a vector.  If more than one is a vector then the
;       lengths must match.  The return will have the same dimensions as
;       the input.
; PROCEDURE:
; MODIFICATION HISTORY:
;       Written 1991 December, by Marc W. Buie, Lowell Observatory
;       97/08/21, MWB, fixed phase angle bug, added g as input argument.
;-
function bidr,w,emu,smu,g,holes,pzero

gamma = sqrt(1-w)
bscat0=g
m = where( g ge !pi/2, count)
if count ne 0 then bscat0[m] = 0
m = where( g lt 1e-5*holes, count)
if count ne 0 then bscat0[m] = 1 - 1.5*g[m]/holes
m = where( g lt !pi/2 and g ge 1e-5*holes, count)
if count ne 0 then begin
   x = holes/tan(g[m])
   ex = exp(-x)
   bscat0[m] = 1 - (3-ex) * (1-ex) / (x+x)
end
bscat = bscat0*exp(-0.5*w^2)
twoemu = emu + emu
twosmu = smu + smu

hobs = (1+twoemu) / (1 + gamma * twoemu)
hsun = (1+twosmu) / (1 + gamma * twosmu)

bidr = w*smu/(smu+emu)*((1+bscat)*pzero+hobs*hsun-1)/(4*!pi)

return,bidr

end

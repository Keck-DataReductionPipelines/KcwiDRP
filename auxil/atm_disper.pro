function atm_disper,w0,w1,airmass
;+
;-
z = acos(1.0/airmass)
n0 = refractivity(w0/10.)
n1 = refractivity(w1/10.)
return,206265.d0 * (n0 - n1) * tan(z)
end

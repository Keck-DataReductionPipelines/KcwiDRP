function atm_disper,w0,w1,airmass, $
	temperature=temperature, $
	pressure_pa=pressure_pa, $
	humidity=humidity, xc=xc
;+
;-
; Check keywords
if keyword_set(temperature) then $
	temp = temperature $
else	temp = 10.0
if keyword_set(pressure_pa) then $
	press = pressure_pa $
else	press = 61100.0
if keyword_set(humidity) then $
	hum = humidity $
else	hum = 50.0
if keyword_set(xc) then $
	co2 = xc $
else	co2 = 400.0
z = acos(1.0/airmass)
n0 = refractivity(w0/10.,temp,press,hum,co2)
n1 = refractivity(w1/10.,temp,press,hum,co2)
return,206265.d0 * (n0 - n1) * tan(z)
end


FUNCTION atmospheric_density, temperature, pressure_pa, humidity, xc, $
                              force_xw=force_xw, water_vapor=water_vapor, $
                              dry_air=dry_air, verbose=verbose, $
                              atmosphere_values=atmosphere_values

;+
; NAME:
; atmospheric_density
;
; PURPOSE:
; Calculate the atmospheric density of air at the supplied conditions
; EXPLANATION:
; This procedure will take as inputs a set of physical conditions
; (temperature, density, humidity, and CO2 concentration) and
; calculate the refractivity (the index of refraction minus 1)
; of air at a specific wavelength.
; CALLING SEQUENCE:
; r = refractivity(wavelength, temp, pressure, humidity, xc)
;
; INPUTS:
; temperature = temperature of air, in Celsius (C)
; (default = 20)
; pressure_pa = local air pressure, in pascals (P)
; (default = 100000)
; humidity = relative humidity, in percent (%)
; (default = 75)
; xc = concentration of CO2, in parts per million (ppm)
; (default = 380)
;
; OPTIONAL INPUT KEYWORDS:
; force_xw = use the value of this keyword as the molar fraction
; of water vapor instead of calculating x_w from the
; supplied conditions
; water_vapor = if supplied, then calculated the partial pressure
; of pure water vapor
; dry_air = if supplied, then calculated the partial pressure
; of completely dry air
; verbose = indicates level of output information
; 0 = no output
; 1 = standard output
; 2 = extended output
;
; OPTIONAL OUTPUT KEYWORD:
; atmosphere_values = structure containing calculated atmospheric
; parameters for given conditions
; R = the gas constant
; Z = compressibility of moist air
; Ma = molar mass of dry air (at a given xc)
; Mw = molar mass of water vapor
; svp = saturation vapor pressure of water vapor in air
; at temperature TT_K
; f = enhancement factor of water vapor in air
; density = calculated density
; TT_C = temperature of air, in Celsius (C)
; TT_K = temperature of air, in Kelvin (K) = TT_C + 273.15
; pressure_Pa = local air pressure, in pascals (P)
; humidity = relative humidity, in percent (%)
; xw = concentration of CO2, in parts per million (ppm)
;
; RESULTS:
; density = calculated density
;
; EXAMPLE:
; density_a = atmospheric_density(12, 70000, 35, 400, /dry_air)
; COMMON BLOCKS:
; none
;
; PROCEDURE:
; Ciddor, P.E., 1996, "Refractive index of air: new equations for
; the visible and near infrared", Applied Optics LP, vol. 35,
; Issue 9, p.1566
;
; MODIFICATION HISTORY:
; Written, Kevin Reardon, INAF/Osservatorio Astrofisico di Arcetri, 2005
;-

IF NOT KEYWORD_SET(temperature) THEN temperature = 20
TT_C = temperature
TT_K = TT_C + 273.16

IF N_ELEMENTS(humidity) NE 1 THEN humidity = 75
humidity_partial = humidity / 100.

IF N_ELEMENTS(pressure_pa) NE 1 THEN pressure_pa = 100000
IF N_ELEMENTS(xc) NE 1 THEN xc = 450
IF NOT KEYWORD_SET(verbose) THEN verbose = 0

IF verbose GE 2 THEN BEGIN
    PRINT
    PRINT, FORMAT='(%"Temp: %6.2f C <> Pressure - %8.1f (Pa) <> ' + $
                  'Humidity - %6.1f (\%) <> CO2 - %3.3d (ppm)")',$
           tt_C, pressure_Pa, humidity, xc
ENDIF

; ***************** Constants *******************
; Ciddor, 1996, Appendix A
AA = 1.2378847d * 1e-5 ; K^(-2)
BB = -1.9121316d * 1e-2 ; K^(-1)
CC = 33.93711047d ;
DD = -6.3431645d * 1e3 ; K

alpha = 1.00062d
beta = 3.14d * 1e-8 ; Pa^(-1)
gamma = 5.6d * 1e-7 ; deg C^(-2)

a0 = 1.58123d * 1e-6 ; K Pa^(-1)
a1 = -2.9331d * 1e-8 ; Pa^(-1)
a2 = 1.1043d * 1e-10 ; K^(-1) Pa^(-1)
b0 = 5.707d * 1e-6 ; K Pa^(-1)
b1 = -2.051d * 1e-8 ; Pa^(-1)
c0 = 1.9898d * 1e-4 ; K Pa^(-1)
c1 = -2.376d * 1e-6 ; Pa^(-1)
d = 1.83d * 1e-11 ; K^2 Pa^(-2)
e = -0.765d * 1e-8 ; K^2 Pa^(-2)

; Ciddor, 1996, Section 3
; gas constant
R = 8.314510d ; J mol^(-1) K^(-1) - gas constant
; molar mass of water vapor
Mw = 0.018015d ; kg/mol - molar mass of water vapor
; molar mass of dry air containing a concentration of CO2 of Xc ppm
Malpha = 1e-3 * (28.9635 + 12.011 * 1e-6 * (xc - 400))

; ***************** *******************

; saturation vapor pressure of water vapor in air at temperature T
; Ciddor, 1996, Section 3
svp = EXP( AA * TT_K^2 + BB * TT_K + CC + DD/TT_K )

; enhancement factor of water vapor in air
f = (alpha + beta * pressure_Pa + gamma * TT_C^2)

IF N_ELEMENTS(force_xw) LT 1 THEN BEGIN
    xw = f * humidity_partial * svp / pressure_Pa
ENDIF ELSE BEGIN
    xw = force_xw
ENDELSE
 
; Ciddor, 1996, Appendix
IF N_ELEMENTS(Z) LT 1 THEN BEGIN
    Z = 1 - (pressure_Pa/TT_K) * (a0 + a1 * TT_C + a2 * TT_C^2 + $
                                   (b0 + b1 * TT_C) * xw + $
                                   (c0 + c1 * TT_C) * xw^2 ) + $
            (pressure_Pa/TT_K)^2 * (d + e * xw^2)
ENDIF

IF KEYWORD_SET(water_vapor) THEN BEGIN
   density = (pressure_Pa * Mw * xw / (Z * R * TT_K))
   ;PRINT,pressure_Pa,Mw,xw,Z,R,TT_K
ENDIF ELSE IF KEYWORD_SET(dry_air) THEN BEGIN
   density = (pressure_Pa * Malpha / (Z * R * TT_K)) * (1 - xw)
ENDIF ELSE BEGIN
   density = (pressure_Pa * Malpha / (Z * R * TT_K)) * (1 - xw * (1 - Mw/Malpha))
ENDELSE

IF verbose GE 2 THEN BEGIN
    PRINT, FORMAT='(%"svp: %8.2f <> f: %9.7f <> xw: %12.4f <> ' + $
                  'Z: %14.6f <> density: %8.4f")',$
           svp, f, xw, Z, density
ENDIF

atmosphere_values = CREATE_STRUCT('R', R, 'Z', Z, 'Ma', Malpha, 'Mw', Mw, $
                                  'svp', svp, 'f', f, 'density', density, $
                                  'TT_C', TT_C, 'TT_K', TT_K, 'pressure_Pa', pressure_Pa, $
                                  'humidity', humidity, 'xw', xw)

RETURN, density

END


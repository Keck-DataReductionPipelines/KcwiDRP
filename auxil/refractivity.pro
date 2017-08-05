
FUNCTION refractivity, wavelength, temperature, pressure_pa, humidity, xc, verbose=verbose
;+
; NAME:
; refractivity
;
; PURPOSE:
; Calculate the refractivity of air at the supplied conditions
; EXPLANATION:
; This procedure will take as inputs a set of physical conditions
; (temperature, density, humidity, and CO2 concentration) and
; calculate the refractivity (the index of refraction minus 1)
; of air at a specific wavelength.
; CALLING SEQUENCE:
; r = refractivity(wavelength, temp, pressure, humidity, xc)
;
; INPUTS:
; wavelength = wavelength at which to calculate refractivity,
; in nanometers (nm)
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
; verbose = indicates level of output information
; 0 = no output
; 1 = standard output
; 2 = extended output
;
; OPTIONAL OUTPUT KEYWORD:
;
; RESULTS:
; nprop = calculated refractivity
;
; EXAMPLE:
; r = refractivity(630, 12, 70000, 35, 400)
; COMMON BLOCKS:
; none
;
; PROCEDURE:
; Ciddor, P.E.: 1996, "Refractive index of air: new equations for
; the visible and near infrared", Applied Optics LP vol. 35,
; Issue 9, p.1566
;
; Ciddor, P.E. and Hill, R.J.: 1999, Applied Optics vol. 38, p. 1663.
;
; MODIFICATION HISTORY:
; Written, Kevin Reardon, INAF/Osservatorio Astrofisico di Arcetri, 2005
;-

wavelength_nm = 633.0d ; nm
wavelength_nm = wavelength ; nm
wavelength_mic = wavelength_nm / 1000.
; convert wavelengths in air to vacuum wavelengths [ lamda(air) = lamba(vacuum) / n(air) ]
; using mean index of refraction of air = 1.00027
wavelength_vac = wavelength_mic * 1.00027
wavenumber = 1 / wavelength_vac

IF N_ELEMENTS(temperature) NE 1 THEN temperature = 20
IF N_ELEMENTS(humidity) NE 1 THEN humidity = 75
IF N_ELEMENTS(pressure_pa) NE 1 THEN pressure_pa = 100000
IF N_ELEMENTS(xc) NE 1 THEN xc = 380
IF NOT KEYWORD_SET(verbose) THEN verbose = 0

; ***************** Constants *******************
; Ciddor, 1996, Appendix A
; from Peck and Reeder (1962)
k0 = 238.0185d ; microns^(-2)
k1 = 5792105.0d ; microns^(-2)
k2 = 57.362d ; microns^(-2)
k3 = 167917.0d ; microns^(-2)

; from Owens (1967)
w0 = 295.235d ; microns^(-2)
w1 = 2.6422d ; microns^(-2)
w2 = -0.032380d ; microns^(-4)
w3 = 0.004028d ; microns^(-6)
; ***************** *******************

; Refractivity of Air at 15 deg C, 101325 Pa, 0% humidity, and
; a fixed concentration of 450 ppm of CO2
; Ciddor, 1996, Eq. (1)
nas = ( 1e-8 * ( k1 / (k0 - wavenumber^2) + k3 / (k2 - wavenumber^2) ) ) + 1

; Refractivity of Air at 15 deg C, 101325 Pa, 0% humidity, and
; a variable concentration of CO2 of Xc ppm
; Ciddor, 1996, Eq. (2)
naxs = ((nas - 1) * ( 1 + 0.534e-6 * (xc - 450)) ) + 1

; Refractivity of water vapor at 20 deg C, 1333 Pa, 0% humidity
; correction factor derived by Ciddor, 1996 by fitting to measurements
cf = 1.022
; Ciddor, 1996, Eq. (3)
nws = ( 1e-8 * cf * ( w0 + (w1 * wavenumber^2) + (w2 * wavenumber^4) + $
                         (w3 * wavenumber^6) ) ) + 1

; density of dry air at standard conditions
density_axs = atmospheric_density( 15, 101325, 0, xc, force_xw=0, /dry_air)
; density of water vapor at standard conditions
density_ws = atmospheric_density( 20, 1333, 100, xc, force_xw=1)

; density of dry air at input/actual conditions
density_a = atmospheric_density( temperature , pressure_pa, humidity, xc, /dry_air)
; density of water vapor at input/actual conditions
density_w = atmospheric_density( temperature , pressure_pa, humidity, xc, /water)

density = atmospheric_density (temperature, pressure_pa, humidity, xc)

; Ciddor, 1996, Eq. (5)

IF (density_ws NE 0) THEN nprop_a = (density_a/density_axs) * (naxs - 1) ELSE nprop_a = 0
IF (density_ws NE 0) THEN nprop_w = (density_w/density_ws) * (nws - 1) ELSE nprop_w = 0
nprop = nprop_a + nprop_w


;PRINT, density_a, density_axs, density_w, density_ws
IF (verbose GE 1) THEN PRINT, FORMAT='(%"n(axs): %8.1f <> n(ws): %8.1f <> rho(a/axs): %15.6f <> rho(w/ws):: %15.6f <> n(prop): %8.1f")',$
       (naxs-1)*1e8, (nws-1)*1e8, (density_a/density_axs), (density_w/density_ws), nprop * 1e8
IF (verbose GE 2) THEN PRINT,FORMAT='(%"n(air): %8.1f <> n(water): %8.1f")', $
       (density_a/density_axs) * (naxs - 1) * 1e8, (density_w/density_ws) * (nws - 1) * 1e8

RETURN, nprop

END


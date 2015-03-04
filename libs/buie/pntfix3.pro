;+
; NAME:
;	pntfix3
; PURPOSE: (one line)
;	Compute the pointing correction as used by MOVE at Anderson Mesa
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  pntfix3,coefs,haobs,decobs,hafix,decfix,dha,ddec
; INPUTS:
;	coefs  - Pointing coefficients.
;	           coefs(0) - Zero point in dec (")
;	           coefs(1) - Zero point in ra (")
;	           coefs(2) - Angle between true and instrumental poles (")
;	           coefs(3) - Angle between line of pole and true meridian (deg)
;	           coefs(4) - Telescope tube droop in HA and DEC (")
;	           coefs(5) - Angle between optical and telescope tube axes (")
;	           coefs(6) - Mechanical orthogonality of RA and Dec Axes (")
;	           coefs(7) - Dec axis flexure (")
;	           coefs(8) - HA encoder scale error ("/degree)
;             coefs[9] -  HA*Dec term
;             coefs[10]-  HA^2 term
;	haobs  - Observed hour angle of telescope in degrees
;	decobs - Observed declination of telescope in degrees
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	hafix  - Corrected (true) hour angle of telescope in degrees
;	decfix - Corrected (true) declination of telescope in degrees
;	dha    - Correction to hour angle in arc-sec on the sky
;	ddec   - Correction to declination in arc-sec
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;	The latitude and longitude of Anderson Mesa (42" actually) is
;	hardcoded into this procedure.
; PROCEDURE:
; MODIFICATION HISTORY:
;	2/11/93 - Written by Marc W. Buie from notes provided by Nat White.
;-
pro pntfix3,coefs,haobs,decobs,hafix,decfix,dha,ddec

   ;These are GPS values for 42" telescope
   lat =  35.0968333d0  ; Larry's MOVE values
   lon = 111.5359167d0

   ;lat=35.0967189d0     ; GPS value for 42"
   ;lon=111.5365053d0

   lat_r    = lat    / 180.0d0 * !dpi
   lon_r    = lon    / 180.0d0 * !dpi
   haobs_r  = haobs  / 180.0d0 * !dpi
   decobs_r = decobs / 180.0d0 * !dpi

   coefs_r  = coefs
   idx=[0,1,2,4,5,6,7]
   coefs_r[idx] = coefs_r[idx] / 3600.0d0 / 180.0d0 * !dpi
   coefs_r[3]   = coefs_r[3]   / 180.0d0 * !dpi

   altobs_r = asin(sin(lat_r)*sin(decobs_r)+cos(lat_r)*cos(decobs_r)*cos(haobs_r))
   azobs_r  = atan(sin(haobs_r),cos(haobs_r)*sin(lat_r)-tan(decobs_r)*cos(lat_r))

   altobs = altobs_r*!radeg
   azobs  = azobs_r*!radeg
   zenobs = 90-altobs

   a0    = coefs[0]	; Zero point in dec (")
   b0    = coefs[1]	; Zero point in ra (")
   gam   = coefs[2]	; Angle between true and instrumental poles (")
   the_r = coefs_r[3]	; Angle between line of pole and true meridian (deg)
   e     = coefs[4]	; Telescope droop in HA and DEC (")
   c     = coefs[5]	; Angle between optical and telescope tube axes (")
   im    = coefs[6]	; Mechanical orthogonality of RA and Dec axes (")
   l     = coefs[7]	; Dec axis flexure (")
   r     = coefs[8]	; HA encoder scale error ("/degree)
   hd    = coefs[9]  ; HA*Dec term
   hsq   = coefs[10] ; HA^2 term

   a1 = gam * cos(the_r)
   a2 = gam * sin(the_r)
   a3 = e

   b1 = c
   b2 = im
   b3 = l
   b4 = r
   b5 = hd
   b6 = hsq

   xx = cos(haobs_r)
   yy = sin(haobs_r)
   zz = sin(lat_r) * cos(decobs_r) - cos(lat_r) * sin(decobs_r) * xx
   ss = 1.0/cos(decobs_r)
   tt = tan(decobs_r)
   qq = sin(lat_r) * tan(decobs_r) + cos(lat_r) * cos(haobs_r)
   hh = -a1 * yy * tt + a2 * xx * tt + a3 * cos(lat_r) * ss * yy

   ddec = a0 - a1 * xx - a2 * yy - a3 * zz
   dha  = b0 + b1 * ss - b2 * tt + hh + b3 * qq + b4 * haobs + b5 * decobs * haobs + b6 * haobs^2

   ddec_r = ddec / 3600.0d0 / 180.0d0 * !dpi
   dha_r  = dha  / 3600.0d0 / 180.0d0 * !dpi

   hafix_r  = haobs_r  + dha_r
   decfix_r = decobs_r + ddec_r

   hafix    = haobs    + (dha  / 3600.0d0)
   decfix   = decobs   + (ddec / 3600.0d0)

   azfix_r=atan(sin(hafix_r),cos(hafix_r)*sin(lat_r)-tan(decfix_r)*cos(lat_r))
   altfix_r=asin(sin(lat_r)*sin(decfix_r)+cos(lat_r)*cos(decfix_r)*cos(hafix_r))
   altfix = altfix_r * !radeg
   zenfix=90-altfix

   fixsep_r=acos(sin(decobs_r)*sin(decfix_r)+cos(decobs_r)*cos(decfix_r)*cos(haobs_r-hafix_r))
   fixsep=fixsep_r*!radeg*3600.0

end

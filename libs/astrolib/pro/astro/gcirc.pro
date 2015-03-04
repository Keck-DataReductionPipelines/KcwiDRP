PRO gcirc,u,ra1,dc1,ra2,dc2,dis                         
;+
; NAME:
;     GCIRC
; PURPOSE:
;     Computes rigorous great circle arc distances.  
; EXPLANATION:
;     Input position can either be either radians, sexigesimal RA, Dec or
;     degrees.   All computations are double precision. 
;
; CALLING SEQUENCE:
;      GCIRC, U, RA1, DC1, RA2, DC2, DIS
;
; INPUTS:
;      U    -- integer = 0,1, or 2: Describes units of inputs and output:
;              0:  everything radians
;              1:  RAx in decimal hours, DCx in decimal
;                       degrees, DIS in arc seconds 
;              2:  RAx and DCx in degrees, DIS in arc seconds
;      RA1  -- Right ascension or longitude of point 1
;      DC1  -- Declination or latitude of point 1
;      RA2  -- Right ascension or longitude of point 2
;      DC2  -- Declination or latitude of point 2
;
; OUTPUTS:
;      DIS  -- Angular distance on the sky between points 1 and 2
;              See U above for units;  double precision  
;
; PROCEDURE:
;      "Cosine formula" (p. 7 of Smart's Spherical Astronomy or
;      p. 12 of Green's Spherical Astronomy)
;
; NOTES:
;       (1) If RA1,DC1 are scalars, and RA2,DC2 are vectors, then DIS is a
;       vector giving the distance of each element of RA2,DC2 to RA1,DC1.
;       Similarly, if RA1,DC1 are vectors, and RA2, DC2 are scalars, then DIS
;       is a vector giving the distance of each element of RA1, DC1 to 
;       RA2, DC2.    If both RA1,DC1 and RA2,DC2 are vectors then DIS is a
;       vector giving the distance of each element of RA1,DC1 to the 
;       corresponding element of RA2,DC2.    If the input vectors are not the 
;       same length, then excess elements of the longer ones will be ignored.
;
;       (2) Coordinates closer together than a few milliarcsec cannot
;       be distinguished.  If you are in this realm, you should be
;       using special-purpose algorithms.
;
;       (3) The function SPHDIST provides an alternate method of computing
;        a spherical distance.
;
; PROCEDURE CALLS:
;      None
;
;   MODIFICATION HISTORY:
;      Written in Fortran by R. Hill -- SASC Technologies -- January 3, 1986
;      Translated from FORTRAN to IDL, RSH, STX, 2/6/87
;      Vector arguments allowed    W. Landsman    April 1989
;      Prints result if last argument not given.  RSH, RSTX, 3 Apr. 1998
;      Remove ISARRAY(), V5.1 version        W. Landsman   August 2000
;      Added option U=2                      W. Landsman   October 2006
;      Use double precision for U=0 as advertised R. McMahon/W.L.  April 2007
;-
 compile_opt idl2
 On_error,2                            ;Return to caller

 npar = N_params()
 IF (npar ne 6) and (npar ne 5) THEN BEGIN
   print,'Calling sequence:  GCIRC,U,RA1,DC1,RA2,DC2[,DIS]'
   print,'   U = 0  ==> Everything in radians'
   print, $
   '   U = 1  ==> RAx decimal hours, DCx decimal degrees, DIS arc sec'
   print,'   U = 2  ==> RAx, DCx decimal degrees, DIS arc sec'
   RETURN
 ENDIF

 scalar = (size(ra1,/N_Dimen) EQ 0) and (size(ra2,/N_dimen) EQ 0)
 IF scalar THEN BEGIN
    IF (ra1 eq ra2) and (dc1 eq dc2) THEN BEGIN
       dis = 0.0d0
       IF npar eq 5 THEN $
           print,'Positions are equal:  ', ra1, dc1
       return
    ENDIF
 ENDIF

 d2r    = !DPI/180.0d0
 as2r   = !DPI/648000.0d0
 h2r    = !DPI/12.0d0

; Convert input to double precision radians
 CASE u OF
   0:  BEGIN
          rarad1 = double(ra1)
          rarad2 = double(ra2)
          dcrad1 = double(dc1)
          dcrad2 = double(dc2)
       END
   1:  BEGIN
          rarad1 = ra1*h2r
          rarad2 = ra2*h2r
          dcrad1 = dc1*d2r
          dcrad2 = dc2*d2r
       END
   2:  BEGIN  
          rarad1 = ra1*d2r
          rarad2 = ra2*d2r
          dcrad1 = dc1*d2r
          dcrad2 = dc2*d2r
        END
   ELSE:  MESSAGE, $
                'U must be 0 (radians), 1 ( hours, degrees) or 2 (degrees)'
 ENDCASE

 radif  = abs(rarad2-rarad1)
 pi_mod = where(radif gt !DPI, npi)         ;Make sure between 0 and 2*!PI
 IF npi gt 0 THEN radif[pi_mod] = 2.0*!DPI - radif[pi_mod]
 cosdis = sin(dcrad1)*sin(dcrad2) + cos(dcrad1)*cos(dcrad2)*cos(radif)
 dis    = acos(cosdis<1.0d0>(-1.0d0))
 IF (u ne 0) THEN dis = dis/as2r

 IF (npar eq 5) and scalar THEN BEGIN
    IF (u ne 0) and (dis ge 0.1) and (dis le 1000)  $
       THEN fmt = '(F10.4)' $
       ELSE fmt = '(E15.8)'
    IF (u ne 0) THEN units = ' arcsec' ELSE units = ' radians'
    print,'Angular separation is ' + string(dis,format=fmt) + units
 ENDIF

 RETURN
 END


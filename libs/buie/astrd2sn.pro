;+
; NAME:
;  astrd2sn
; PURPOSE:
;  Astrometry conversion from ($\alpha$,$\delta$) to ($\xi$,$\eta$)
;
; DESCRIPTION:
;  Standard coordinate conversion (see Smart, p283)
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  astrd2sn,ra,dec,racen,deccen,xi,eta
;
; INPUTS:
;  ra     - Right ascension (radians)
;  dec    - Declination (radians)
;  racen  - Right ascension of tangent point between plane and celestial sphere.
;  deccen - Declination of tangent point between plane and celestial sphere.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  ARCSEC - Flag, if set indicates that the output xi,eta values are in
;            arcseconds.  Otherwise they are returned in radians.
;
; OUTPUTS:
;  xi     - Coordinate in tangent plane.
;  eta    - Coordinate in tangent plane.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  97/04/05 - Written by Marc W. Buie, Lowell Observatory
;  2009/12/08, MWB, added ARCSEC keyword
;-
PRO astrd2sn,ra,dec,racen,deccen,xi,eta,ARCSEC=arcsec

   beta      = ra - racen
   cosbeta   = cos(beta)
   tandec    = tan(dec)
   tandeccen = tan(deccen)
   s   = cosbeta + tandec*tandeccen
   xi  = sin(beta)/cos(deccen)/s
   eta = (tandec - tandeccen*cosbeta)/s

   if keyword_set(arcsec) then begin
      xi  = xi*180.0d0/!dpi*3600.0d0
      eta = eta*180.0d0/!dpi*3600.0d0
   endif
END

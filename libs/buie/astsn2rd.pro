;+
; NAME:
;  astsn2rd
; PURPOSE:
;  Astrometry conversion from ($\xi$,$\eta$) to ($\alpha$,$\delta$)
; DESCRIPTION:
;  Standard coordinate conversion (see Smart, p283)
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  astsn2rd,xi,eta,racen,deccen,ra,dec
;
; INPUTS:
;  xi     - Coordinate in tangent plane (radians).
;  eta    - Coordinate in tangent plane (radians).
;  racen  - Right ascension of tangent point between plane and celestial
;              sphere (radians).
;  deccen - Declination of tangent point between plane and celestial
;              sphere (radians).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  ARCSEC - Flag, if set indicates that the input xi,eta values are in
;            arcseconds.  Otherwise they need to be in radians.  Does not
;            affect the output ra,dec.
;
; OUTPUTS:
;  ra     - Right ascension (radians)
;  dec    - Declination (radians)
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
;  1997/04/05 - Written by Marc W. Buie, Lowell Observatory
;  2009/06/22, MWB, modified to eliminate numerical singularities
;  2010/12/10, MWB, added ARCSEC keyword
;-
pro astsn2rd,in_xi,in_eta,racen,deccen,ra,dec,ARCSEC=arcsec
   if keyword_set(arcsec) then begin
      xi  = in_xi/180.0d0*!dpi/3600.0d0
      eta = in_eta/180.0d0*!dpi/3600.0d0
   endif else begin
      xi  = in_xi
      eta = in_eta
   endelse
   cosdeccen = cos(deccen)
   sindeccen = sin(deccen)
   tandeccen = tan(deccen)
   ra  = atan(xi,(cosdeccen-eta*sindeccen)) + racen
   dec = atan((cos(ra-racen)*(eta+tandeccen)) / (1.0d0-eta*tandeccen))
end

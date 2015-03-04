;+
; NAME:
;  eccrec
; PURPOSE:   (one line only)
;  Compute coordinates given eccentricy anomaly
; DESCRIPTION:
;  Given elements, compute heliocentric rectangular coords,
;     and heliocentric distance
;
;  NOTE
;     the epoch is implicit in the eccentric anomaly and auxiliary elements
; CATEGORY:
;  Celestial Mechanics
; CALLING SEQUENCE:
;  eccrec,a,ecc,eccanom,px,qx,py,qy,pz,qz,x,y,z
; INPUTS:
;  a    - Semi-major axis (AU)
;  ecc  - Eccentricity
;  eccanom - Eccentric Anomaly (radians)
;  px,qx,py,qy,pz,qz - auxilary orbital elements (equinox J2000)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  x,y,z - heliocentric equatorial rectangular coordinates (J2000, radians)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/05/29 - Marc W. Buie, Lowell Observatory.  Ported from fortran routines
;    supplied by Larry Wasserman and Ted Bowell.
;-
pro eccrec,a,ecc,eccanom,px,qx,py,qy,pz,qz,x,y,z

   aux2 = sqrt(1.0d+00-ecc*ecc)
   aux1 = a*(cos(eccanom)-ecc)
   aux2 = a*aux2*sin(eccanom)

   x = aux1*px+aux2*qx
   y = aux1*py+aux2*qy
   z = aux1*pz+aux2*qz

end

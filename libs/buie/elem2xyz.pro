;+
; NAME:
;  elem2xyz
; PURPOSE:   (one line only)
;  Compute position given orbital elements
; DESCRIPTION:
; CATEGORY:
;  Celestial Mechanics
; CALLING SEQUENCE:
; INPUTS:
;  m    - Mean anomaly (radians)
;  arg  - Argument of perihelion (radians)
;  node - Longitude of ascending node (radians)
;  inc  - Inclination (radians)
;  ecc  - Eccentricity
;  a    - Semi-major axis (AU)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  JDEPOCH - Julian date of mean anomaly (default=0.0, ie., doesn't matter)
;  JDPOS   - Julian date desired (default=JDEPOCH)
; OUTPUTS:
;  x    - Heliocentric x coordinates (AU)
;  y    - Heliocentric y coordinates (AU)
;  z    - Heliocentric z coordinates (AU)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/05/29 - Marc W. Buie, Lowell Observatory.  Ported from fortran routines
;    supplied by Larry Wasserman and Ted Bowell.
;-
pro elem2xyz,in_m,arg,node,inc,ecc,a,x,y,z, $
   JDEPOCH=jdepoch,JDPOS=jdpos,ECLIPTIC=ecliptic

   if badpar(jdepoch,[0,4,5],0,caller='ELEM2XYZ: (JDEPOCH) ',default=0.0d0) then return
   if badpar(jdpos,[0,4,5],0,caller='ELEM2XYZ: (JDPOS) ',default=jdepoch) then return
   if badpar(ecliptic,[0,1,2,3],0,caller='ELEM2XYZ: (ECLIPTIC) ',default=0) then return

   if jdpos ne jdepoch then begin
      newanom,jdepoch,in_m,a,jdpos,m
   endif else begin
      m = in_m
   endelse

   ; compute auxillary elements
   auxelem,node,arg,inc,px,qx,py,qy,pz,qz

   ; solve Kepler's Equation
   kepler1,m,ecc,eccanom

   ; Given eccentric anomaly, compute positions
   eccrec,a,ecc,eccanom,px,qx,py,qy,pz,qz,x,y,z

   ; Rotate to ecliptic coordinates, if desired
   if ecliptic then begin
;      obl = 0.4090926292045900525273794d0  ; J2000 obliquity in radians (Standish, 1981)
      obl = 0.4090928042223289096135375d0  ; J2000 obliquity in radians, IAU
      ye =  y*cos(obl) + z*sin(obl)
      ze = -y*sin(obl) + z*cos(obl)
      y  = ye
      z  = ze
   endif

end

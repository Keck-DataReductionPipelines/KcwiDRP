;+
; NAME:
;  auxelem
; PURPOSE:   (one line only)
;  Compute auxilary orbital elements
; DESCRIPTION:
; CATEGORY:
;  Celestial Mechanics
; CALLING SEQUENCE:
;  auxelem,node,arg,inc,px,qx,py,qy,pz,qz
; INPUTS:
;  node - Longitude of ascending node (radians)
;  arg  - Argument of perihelion (radians)
;  inc  - Inclination (radians)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  px,qx,py,qy,pz,qz - auxilary orbital elements (equinox J2000)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/05/29 - Marc W. Buie, Lowell Observatory.  Ported from fortran routines
;    supplied by Larry Wasserman and Ted Bowell.
;-
pro auxelem,node,arg,inc,px,qx,py,qy,pz,qz

   ; Constants for J2000
   ce = 0.91748206206918d+00
   se = 0.39777715593191d+00

   can=cos(node)
   san=sin(node)
   cap=cos(arg)
   sap=sin(arg)
   cai=cos(inc)
   sai=sin(inc)

   px =  can*cap - san*sap*cai
   qx = -can*sap - san*cap*cai
   u  =  san*cap + can*sap*cai
   py =    u*ce  - sap*sai*se
   v  = -san*sap + can*cap*cai
   qy =    v*ce  - cap*sai*se
   pz =    u*se  + sap*sai*ce
   qz =    v*se  + cap*sai*ce

end

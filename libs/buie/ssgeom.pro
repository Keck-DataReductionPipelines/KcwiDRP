;+
; NAME: 
;  ssgeom
; PURPOSE: 
;  Compute Sun and Earth distance and phase angle for solar system object.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  ssgeom,ephemeris,sun,earth,phang,elong
; INPUTS:
;  ephemeris - 8 by N array as returned from codes 20-22 of EPHEM
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  sun   - Object to Sun distance in AU.
;  earth - Object to Earth distance in AU.
;  phang - Sun - Object - Earth angle in degrees.
;  elong - Object - Earth - Sun angle in degrees.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  93/10/28, Written by Marc W. Buie, Lowell Observatory
;  94/08/05, MWB, fixed bug in calculation of ELONG
;  2010/08/20, MWB, minor tweak to help eliminate spurious dimensions of;
;                   returned information.
;-
pro ssgeom,ephemeris,sun,earth,phang,elong

;Validate number of parameters
if n_params() eq 0 then begin
   print,'Usage:  ssgeom,ephemeris,sun,earth,phang'
   return
end

if badpar(ephemeris,5,[1,2],caller='EPHEM: (jd) ' ,npts=npts,dimen=dimen) then return
if dimen[0] ne 8 then begin
   print,'Ephemeris input must be 8 by N elements'
   return
endif

xobj = trimrank(ephemeris[2,*])
yobj = trimrank(ephemeris[3,*])
zobj = trimrank(ephemeris[4,*])
xsun = trimrank(ephemeris[5,*])
ysun = trimrank(ephemeris[6,*])
zsun = trimrank(ephemeris[7,*])
xos  = xobj - xsun
yos  = yobj - ysun
zos  = zobj - zsun

au    = sqrt( xsun^2 + ysun^2 + zsun^2 )
earth = sqrt( xobj^2 + yobj^2 + zobj^2 )
sun   = sqrt( xos^2  + yos^2  + zos^2  )

phang = acos((xos*xobj + yos*yobj + zos*zobj)/(earth*sun)) * !radeg

elong = acos((xsun*xobj + ysun*yobj + zsun*zobj)/(earth*au)) * !radeg

end

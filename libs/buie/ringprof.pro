;+
; NAME:
;    ringprof
; PURPOSE: (one line)
;    Compute a surface brightness profile.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;    ringprof, image, xcen, ycen, rmax, dr, skyback, rout, iout
; INPUTS:
;    image      : CCD image array.
;    xcen, ycen : Coordinates of center.
;    rmax       : Maximum radius for profile.
;    dr         : Incremental value of radius.
;    skyback    : Sky background.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    rout : Radius vector of length rmax/dr+1.
;    rout : Counts vector of length rmax/dr+1.
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
;    Ported by Doug Loucks, Lowell Observatory, March 1993, from
;    the C-language version written by Marc Buie.
;-
PRO ringprof, image, xcen, ycen, rmax, dr, skyback, rout, iout

n = LONG( rmax / dr )
rout = FLTARR( n + 1 )
iout = FLTARR( n + 1 )
j = LONG( 0 )
FOR r=0.0, rmax, dr DO BEGIN
   Sumann, image, xcen, ycen, r, r+dr, 0.0, area, psum, nsum, d1, d2, d3, d4
   rout[ j ] = r + dr / 2
   iout[ j ] = ( psum + nsum ) / area - skyback
   j = j + 1
ENDFOR

END

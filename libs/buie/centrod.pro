;+
; NAME:
;    centrod
; PURPOSE: (one line)
;    Compute center of mass of an object aperture.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;    centrod, image, xcen, ycen, radius, inradius, outradius, skyback, $
;             xbar, ybar, counts
; INPUTS:
;    image          : CCD image array.
;    xcen, ycen     : Center of window.
;    radius         : Radius of window.
;    inradius       : Inner radius of sky annulus.
;    outradius      : Outer radius of sky annulus.
;    skyback        : Sky background in counts per pixel.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    ADDNEG         : Flag, if set, the position is computed using both
;                       the moments of counts above and below the background.
;                       Default is to compute the position only using those
;                       image values greater than the sky background.
;
; OUTPUTS:
;    xbar, ybar     : Position of center of mass.
;    counts         : Mass of object in counts.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Ported by Doug Loucks, Lowell Observatory, 1992 Oct, from the C-
;    language version written by Marc Buie.
;    4/1/93, DWL, Added argument validation (badpar).
;    99/08/31, MWB, added ADDNEG keyword.
;-
PRO centrod, image, xcen, ycen, radius, inradius, outradius, skyback, $
             xbar, ybar, counts, ADDNEG=addneg

; Validation code disabled to speed execution.  This routine gets called
;   a great deal in real use and the validation adds considerable overhead.

; Validate the number of arguments.
;IF N_PARAMS() NE 10 THEN BEGIN
;   MESSAGE, 'centrod,image,xcen,ycen,radius,inradius,outradius,' + $
;   'skyback,xbar,ybar,counts', /INFO
;   RETURN
;ENDIF

; Validate the input argument types.
;IF badpar( image, [1,2,3,4,5], 2, CALLER='centrod' ) THEN RETURN
;IF badpar( xcen, [1,2,3,4,5], 0, CALLER='centrod' ) THEN RETURN
;IF badpar( ycen, [1,2,3,4,5], 0, CALLER='centrod' ) THEN RETURN
;IF badpar( radius, [1,2,3,4,5], 0, CALLER='centrod' ) THEN RETURN
;IF badpar( inradius, [1,2,3,4,5], 0, CALLER='centrod' ) THEN RETURN
;IF badpar( outradius, [1,2,3,4,5], 0, CALLER='centrod' ) THEN RETURN
;IF badpar( skyback, [1,2,3,4,5], 0, CALLER='centrod' ) THEN RETURN

IF ( inradius NE 0 ) AND ( outradius NE 0 ) THEN BEGIN
; Compute the sky background.
   sumann, image, xcen, ycen, inradius, outradius, 0.0, area, $
           possum, negsum, posxmom, negxmom, posymom, negymom
   skyback = ( possum + negsum ) / area
ENDIF

; Now do the actual calculation.
;
sumann, image, xcen, ycen, 0.0, radius, skyback, area, $
        possum, negsum, posxmom, negxmom, posymom, negymom

counts = possum + negsum

xbar=xcen
ybar=ycen

IF possum NE 0.0 THEN BEGIN
   xbar = xbar + posxmom / possum
   ybar = ybar + posymom / possum
ENDIF

if keyword_set(ADDNEG) and negsum ne 0.0 then begin
   xbar = xbar + negxmom / negsum
   ybar = ybar + negymom / negsum
endif

END

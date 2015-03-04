;+
; NAME:
;  astrd2xy
; PURPOSE:
;  Astrometry conversion from ($\alpha$,$\delta$) to image (x,y)
;
; DESCRIPTION:
;  This transformation can either be based on a simple linear transformation
;    with rotation from the celestial sphere to linear CCD chip coordinates.
;    Or, it can use a full astrometric solution (including linear).
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  astrd2xy,ra,dec,info,x,y
;
; INPUTS:
;  ra     - Right ascension (radians)
;  dec    - Declination (radians)
;  info   - Transformation information held in an anonymous structure.  There
;              are two different groups of tags that can appear.  The original
;              simple linear transformation needs the following tags:
;                 pscale - Plate scale (arcsec/pixel).
;                 rang   - Rotation angle of image (radians).
;                 xflip  - -1 if image flipped in X, 1 if not.
;                 yflip  - -1 if image flipped in Y, 1 if not.
;
;              The full-up transformation requires a different set of tags:
;                 renormfac - normalization factor
;                 cxi - xi transformations coefficients (x,y -> xi)
;                 ceta - eta transformations coefficients (x,y -> eta)
;                 terms - string array with names of terms to fit
;
;              Both types need the following.
;                 xcref  - X center of image.
;                 ycref  - Y center of image.
;                 raref  - Right ascension of center of image (tangent plane).
;                 decref - Declination of center of image (tangent plane).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  FULL - Flag, if set indicates the full transformation should be used.
;            Only the tags needed must be provided.
;
; OUTPUTS:
;  x      - X coordinate in image
;  y      - Y coordinate in image
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
;  2009/06/21, MWB added full-up transformation
;  2009/11/02, MWB, split off most of code into astsn2xy
;
;-
pro astrd2xy,ra,dec,info,x,y,XI=xi,ETA=eta,FULL=FULL

   self='ASTRD2XY: '
   if badpar(ra,[2,3,4,5],[0,1,2,3],caller=self+'(ra) ') then return
   if badpar(dec,[2,3,4,5],[0,1,2,3],caller=self+'(dec) ') then return
   if badpar(info,[8],[1],caller=self+'(info) ') then return
   if badpar(full,[0,1,2,3],0,caller=self+'(FULL) ',default=0) then return

   astrd2sn,ra,dec,info.raref,info.decref,xi,eta
   astsn2xy,xi,eta,info,x,y,FULL=FULL

end


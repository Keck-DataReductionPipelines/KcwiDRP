;+
; NAME:
;  astxy2rd
; PURPOSE:
;  Astrometry conversion from image (x,y) to ($\alpha$,$\delta$)
;
; DESCRIPTION:
;  This transformation can either be based on a simple linear transformation
;    with rotation from the celestial sphere to linear CCD chip coordinates.
;    Or, it can use a full astrometric solution (including linear).  The
;    simple linear transformation is only an approximate treatment and
;    will not work for very large fields.  The best results will come from
;    the full-up treatment.
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  astxy2rd,x,y,info,ra,dec
;
; INPUTS:
;  x      - X coordinate in image
;  y      - Y coordinate in image
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
;                 terms - list of terms to fit
;                 prot - Rotation to get to standard coordinates (radians)
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
;  FULL - Flag, if set indicates the full transformation should be used.
;            Only the tags needed must be provided.
;
; OUTPUTS:
;  ra     - Right ascension (radians)
;  dec    - Declination (radians)
;
; KEYWORD OUTPUT PARAMETERS:
;  DX  - Internal transformed x value (normalized for /full)
;  DY  - Internal transformed y value (normalized for /full)
;  XI  - tangent plane coordinate (arcsec)
;  ETA - tangent plane coordinate (arcsec)
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;  The following applies to the /FULL conversion.
;
;  The solution is a two-step conversion.  You start with raw coordinates
;    that typically relate to the original position in the native coordinate
;    system for the device.  The first transformation step is to convert
;    to another system related to the first by a translation and rotation.
;     e.g.   xp = (  (x-xc)*cos(prot) + (y-yc)*sin(prot) ) / renormfac
;            yp = ( -(x-xc)*sin(prot) + (y-yc)*cos(prot) ) / renormfac
;
;    The astrometric transformation then maps xp,yp to ra,dec.
;
; MODIFICATION HISTORY:
;  1997/04/05 - Written by Marc W. Buie, Lowell Observatory
;  2009/06/22, MWB added full-up transformation and changed the simple
;                 linear transformation to use xi/eta tangent plane
;  2009/08/05, MWB, modified full transformation to include a coordinate
;                 rotation, this requires an extra tag in info structure.
;  2009/11/02, MWB, split off most of code into astxy2sn
;
;-
pro astxy2rd,x,y,info,ra,dec,FULL=full,XI=xi,ETA=eta,DX=dx,DY=dy

   self='ASTXY2RD: '
   if badpar(x,[2,3,4,5],[0,1,2,3],caller=self+'(x) ') then return
   if badpar(y,[2,3,4,5],[0,1,2,3],caller=self+'(y) ') then return
   if badpar(info,[8],[1],caller=self+'(info) ') then return
   if badpar(full,[0,1,2,3],0,caller=self+'(FULL) ',default=0) then return

   astxy2sn,x,y,info,xi,eta,FULL=full,DX=dx,DY=dy
   astsn2rd,xi,eta,info.raref,info.decref,ra,dec

end

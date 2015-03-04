;+
; NAME:
;  astxy2sn
; PURPOSE:
;  Astrometry conversion from image (x,y) to tangent plane ($\xi$,$\eta$)
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
;  astxy2sn,x,y,info,xi,eta
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
;                 terms - string array with list of terms to include
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
;  ARCSEC - Flag, if set the returned values are in arcseconds.
;
;  FULL - Flag, if set indicates the full transformation should be used.
;            Only the tags needed must be provided.
;
; OUTPUTS:
;  xi     - Tangent plane coordinates (radians)
;  eta    - Tangent plane coordinates (radians)
;
; KEYWORD OUTPUT PARAMETERS:
;  DX  - Internal transformed x value (normalized for /full)
;  DY  - Internal transformed y value (normalized for /full)
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
;  2009/11/02 - Written by Marc W. Buie, Southwest Research Institute
;  2009/12/02, MWB, consolidate xiterms and etaterms tags into one (terms)
;  2010/01/13, MWB, added ARCSEC option
;
;-
pro astxy2sn,x,y,info,xi,eta,FULL=full,DX=dx,DY=dy,ARCSEC=arcsec

   self='ASTXY2SN: '
   if badpar(x,[2,3,4,5],[0,1,2,3],caller=self+'(x) ') then return
   if badpar(y,[2,3,4,5],[0,1,2,3],caller=self+'(y) ') then return
   if badpar(info,[8],[1],caller=self+'(info) ') then return
   if badpar(full,[0,1,2,3],0,caller=self+'(FULL) ',default=0) then return
   if badpar(arcsec,[0,1,2,3],0,caller=self+'(ARCSEC) ',default=0) then return

   x0 = x-info.xcref
   y0 = y-info.ycref

   ; Full transformation
   if full then begin

      dx=( x0*cos(info.prot)+y0*sin(info.prot))/info.renormfac
      dy=(-x0*sin(info.prot)+y0*cos(info.prot))/info.renormfac
      xi = asteval(dx,dy,info.cxi,info.terms)
      eta= asteval(dx,dy,info.ceta,info.terms)

   ; Simple transformation
   endif else begin

      sx  = float(info.xflip)/info.pscale
      sy  = float(info.yflip)/info.pscale
      dx  = x0*cos(info.rang) - y0*sin(info.rang)
      dy  = x0*sin(info.rang) + y0*cos(info.rang)
      xi  = dx/sx
      eta = dy/sy

   endelse

   if not arcsec then begin
      xi = xi/3600.0d0*!dpi/180.0d0
      eta = eta/3600.0d0*!dpi/180.0d0
   endif

end

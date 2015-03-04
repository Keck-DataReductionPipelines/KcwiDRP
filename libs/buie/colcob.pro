;+
; NAME:
;  colcob
; PURPOSE:
;  Compute the offset from the center-of-light to center-of-body.
; DESCRIPTION:
;
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  colcob,map,npix,radius,pole,selat,selon,sslat,sslon,dx,dy
; INPUTS:
;  map    - Array containing a full map of surface, see RENDER.PRO for a
;               more complete description of what's needed here.
;  npix   - size of the array for the computation (pixels), the image used
;               for the computation is square and the object just fits in
;               the array.
;  radius - Radius of object in km.
;  pole   - Position angle of pole, east from north (degrees)
;  selat  - Sub-earth latitude (degrees)
;  selon  - Sub-earth longitude (degrees)
;  sslat  - Sub-solar latitude (degrees)  Only used for Hapke functions. 
;  sslon  - Sub-solar longitude (degrees)  Only used for Hapke functions.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  Accepts all keywords supported by RENDER.PRO.  Many of these are needed
;    to support the proper calculation of the surface reflectance.
;
; OUTPUTS:
;  dx     - X position of the center-of-light in km (actually this is just
;               in the same units as radius but generally should be km).
;  dy     - Y position of the center-of-light.
; KEYWORD OUTPUT PARAMETERS:
;  IMAGE  - Rendered image computed for calculation.
;  FLUX   - Total flux (I/F) of the rendered image.
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
;  1998/02/17, Written by Marc W. Buie, Lowell Observatory
;  2009/02/01, MWB, nearly a total rewrite both in documentation, coding, and
;                 operation.  This new version is NOT compatible with the old.
;
;-
pro colcob,map,npix,radius,pole,selat,selon,sslat,sslon,dx,dy, $
           IMAGE=image,FLUX=flux,_EXTRA=extra

   self='COLCOB: '
   if badpar(map,[4,5],2,caller=self+'(map) ') then return
   if badpar(npix,[2,3],0,caller=self+'(npix) ') then return
   if badpar(radius,[2,3,4,5],0,caller=self+'(radius) ') then return
   if badpar(pole,[2,3,4,5],0,caller=self+'(pole) ') then return
   if badpar(selat,[2,3,4,5],0,caller=self+'(selat) ') then return
   if badpar(selon,[2,3,4,5],0,caller=self+'(selon) ') then return
   if badpar(sslat,[2,3,4,5],0,caller=self+'(sslat) ') then return
   if badpar(sslon,[2,3,4,5],0,caller=self+'(sslon) ') then return

   ; Compute scale for calculation, trim a little off npix so that it really
   ;  does cleanly fit in the array.
   scale = 2.0*radius/(npix-1)
   render,map,radius,scale,pole,selat,selon,sslat,sslon,npix,npix,image, $
      xarr=x,yarr=y,/silent,_EXTRA=extra

   flux = total(image)
   dx = total(x*image)/flux*radius
   dy = total(y*image)/flux*radius

end

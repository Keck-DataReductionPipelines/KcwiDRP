;+
; NAME:
;    sumann
; PURPOSE: (one line)
;    Integrate over an annulus.
; DESCRIPTION:
;
;    This procedure computes the first two image moments of the pixels
;    contained within the input annulus.  The position and inner and outer
;    radii define the annulus.  Each pixel in the input image is then
;    assigned a weighting value which is its areal overlap between the pixel
;    and the annulus.  This weight varies from 0 to 1 and is the precise
;    analytic area of overlap (computed by pixwt.pro).  Of course, this
;    program doesn't do the computation for all pixels, only those near
;    the edge.
;
;    For historical reasons, the moments are split into two components:
;    The moments of all negative pixels in the annulus and the moments of
;    all positive pixels in the annulus.  Therefore, to get the true total
;    number of counts you must add possum+negsum.  Likewise, to get the
;    true center-of-mass (center-of-light) moment in either x or y, you
;    should add the positive and negative moments, ie.,
;       xmom = xcen + posxmom/possum + negxmom/negsum
;       ymom = ycen + posymom/possum + negymom/negsum
;    These numbers are returned separately because there are other reasons
;    for wanting the distributions separated.  For example, one use of this
;    routine is to compute the position of a star in an image.  For this
;    you would make two calls.  The first is a call with back=0. centered
;    (roughly) on the object.  you would then set inradious to be larger
;    than the object and outradius to something larger still.  The sky
;    (background) signal would then be possum+negsum.  Note this is NOT
;    how I use this routine for photometry, it's meant as an example ONLY.
;    The second call would then be made with inradius = 0 and outradius
;    to be just larger than the star.  This time, set back to the
;    background found in the first call.  The object brightness is then
;    possum+negsum counts above sky.  Then, I use just posxmom and posymom
;    as the position of the object.  Strictly speaking I should add the
;    negative moments, but, I know that the negative stuff is most likely
;    to be sky or bad pixels and irrelevant for the object location.
;    (see also centrod.pro)
;
;    Note: the use of this routine for computing sky signals has been
;    superceded by a very different algorithm.  basphote.pro uses a
;    combination of getannul.pro and robomean.pro to compute good sky
;    background values.  This routine is used mostly for object counts and
;    circular radial profiles.
;
; CATEGORY:
;    CCD data processing
; CALLING SEQUENCE:
;    Sumann, image, xcen, ycen, inradius, outradius, back, totweight, $
;            possum, negsum, posxmom, negxmom, posymom, negymom
;
; INPUTS:
;    image       : CCD image array.
;    xcen,ycen   : Center of annulus.
;    inradius    : Radius of inner circle.
;    outradius   : Radius of outer circle.
;    back        : Background to subtract from each pixel.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    totweight          : Area of annulus.
;    possum, negsum     : Sums of positive and negative pixels.
;    posxmom, negxmom   : Positive and negative x moments relative to xcen.
;    posymom, negymom   : Positive and negative y moments relative to ycen.
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
;    Ported by Doug Loucks, Lowell Observatory, 1992 Oct, from the
;    C-language version written by Marc Buie.
;    April, 1993. DWL. Replaced the inner FOR loop with vectors.
;    98/09/21, MWB, optimizations
;    98/09/29, MWB, fixed nasty bug introduced during optimization.
;-
PRO Sumann, image, xcen, ycen, inradius, outradius, back, totweight, $
            possum, negsum, posxmom, negxmom, posymom, negymom

   totweight = 0.0

   possum  = 0.0
   negsum  = 0.0
   posxmom = 0.0
   negxmom = 0.0
   posymom = 0.0
   negymom = 0.0

   image_size = SIZE( image )

   r2 = outradius + 2
   npts = LONG( !PI * r2*r2 )
   x = INTARR( npts, /NOZERO )
   y = INTARR( npts, /NOZERO )
   flags = INTARR( npts )

   ;  As the pixel location vectors (x, y) are assembled, the vector flags
   ;  is constructed with the following values:
   ;
   ;     0  : Trivial.  pixwt not needed.
   ;     1  : Trivial.  pixwt not needed.
   ;    -1  : On or near outer circle only.
   ;    -2  : On or near inner circle only.
   ;    -4  : On or near both circles.
   ;
   ;  Then the flags are used to calculate the final weights for each pixel
   ;  location.
   ;
   IF ( 0 LE inradius ) AND ( inradius LT outradius ) THEN BEGIN
      ; Compute the y-range limits.
      r2 = outradius
      r1 = inradius
      r2sq = r2 * r2
      r1sq = r1 * r1
      yp = ycen - r2
      outy0 = FIX( yp )
      IF outy0+0.5 LT yp THEN outy0=outy0+1
      yp = ycen + r2
      outy3 = FIX( yp + 1 )
      IF outy3-0.5 GT yp THEN outy3=outy3-1

      base = 0
      FOR yp = outy0, outy3 DO BEGIN
         cgetrng, ycen, xcen, r2, yp, outx0, outx1, outx2, outx3
         cgetrng, ycen, xcen, r1, yp, inx0, inx1, inx2, inx3
         n = outx3 - outx0 + 1

         ;  Assume on outer circle.
         xx = outx0 + INDGEN( n )
         yy = REPLICATE( yp, n )
         f  = REPLICATE( -1, n )  ; -1 = on or near outer circle

         ;  Entirely inside outer circle.   (1)
         t = WHERE( outx1 LE xx AND xx LT outx2, count )
         IF count GT 0 THEN f[t] = 1

         ;  On inner circle.
         t = WHERE( (inx0 LE xx AND xx LT inx1) OR $
                    (inx2 LE xx AND xx LT inx3), count )
         IF count GT 0 THEN f[t]=f[t]-3

         ;  Entirely inside inner circle.  (0)
         t = WHERE( inx1 LE xx AND xx LT inx2, count )
         IF count GT 0 THEN f[t]=0
         
         x[ base : base+n-1 ] = xx
         y[ base : base+n-1 ] = yy
         flags[ base : base+n-1 ] = f
         base = base + n
      ENDFOR

      ; This weeds out all non-contributing pixels as well as the extra
      ;   storage (npts-base).
      z = WHERE( flags NE 0, count )
      IF count GT 0 THEN BEGIN
         x = x[z]
         y = y[z]
         flags = flags[z]
      ENDIF

      ; Keep only those pixels which are actually in the image array.
      xmin = MIN( x, MAX=xmax )
      ymin = MIN( y, MAX=ymax )
      IF xmin LT 0 OR xmax GE image_size[1] OR $
         ymin LT 0 OR ymax GE image_size[2] THEN BEGIN
         z = WHERE( x GE 0 AND x LE image_size[ 1 ] AND $
                    y GE 0 AND y LE image_size[ 2 ], count )
         IF count EQ 0 THEN RETURN
         x = x[z]
         y = y[z]
         flags = flags[z]
      ENDIF

      wts = replicate(1.0,n_elements(flags))

      z = WHERE( flags EQ -1, count )
      IF count GT 0 THEN wts[z] = pixwt(xcen,ycen,outradius,x[z],y[z]) > 0.

      z = WHERE( flags EQ -2, count )
      IF count GT 0 THEN wts[z] = (1.0 - pixwt(xcen,ycen,inradius,x[z],y[z])) > 0.

      z = WHERE( flags EQ -4, count )
      IF count GT 0 THEN BEGIN
         wts[z] = (pixwt( xcen, ycen, outradius, x[z], y[z] ) - $
                  pixwt( xcen, ycen, inradius,  x[z], y[z] ) ) > 0.
      ENDIF

      totweight = TOTAL( wts, /DOUBLE )

      val = image[ x, y ]
      vxw = ( val - back ) * wts

      z = WHERE( vxw GT 0, count )
      IF count NE 0 THEN BEGIN
         tmp = vxw[z]
         possum  = TOTAL( tmp, /DOUBLE )
         posxmom = TOTAL( tmp * ( x[z] - xcen ), /DOUBLE )
         posymom = TOTAL( tmp * ( y[z] - ycen ), /DOUBLE )
      ENDIF

      z = WHERE( vxw LT 0, count )
      IF count NE 0 THEN BEGIN
         tmp = vxw[z]
         negsum  = TOTAL( tmp, /DOUBLE )
         negxmom = TOTAL( tmp * ( x[z] - xcen ), /DOUBLE )
         negymom = TOTAL( tmp * ( y[z] - ycen ), /DOUBLE )
      ENDIF

   ENDIF

END

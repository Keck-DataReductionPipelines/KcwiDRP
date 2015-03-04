;+
; NAME:
;    getannul
; PURPOSE: (one line)
;    Extract an annulus from a 2-D array.
; DESCRIPTION:
;
; CATEGORY:
;    CCD data processing
; CALLING SEQUENCE:
;    Getannul, image, xcen, ycen, inradius, outradius, data
;
; INPUTS:
;    image       : CCD image array.
;    xcen,ycen   : Center of annulus.
;    inradius    : Radius of inner circle.
;    outradius   : Radius of outer circle.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
; OUTPUTS:
;    data        : Array of data from the image array.
;    idx         : Optional output of 1D indices (in image) of data.
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
;    Written by Doug Loucks, Lowell Observatory, April, 1993.
;    Reference: getannul.c by Marc Buie.
;    This version is completely different than the previous version and
;    somewhat faster.
;    5/12/93, DWL, Fixed a bug involving fringe pixels.  In certain cases,
;                  the determination of the row indices led to a bad array
;                  subscript range.
;    1/5/94, DWL, Added optional output parameter idx.
;    95/06/12, MWB, Fixed bug, base needed to be LONG for large sky apertures
;-
PRO Getannul, image, xcen, ycen, inradius, outradius, data, idx

data = 0

; Get input image array size.
image_size = SIZE( image )
xsize = image_size[ 1 ]
ysize = image_size[ 2 ]

; Compute the exact area of the annulus and reserve enough space for the pixel
; index arrays.
r1 = inradius
r2 = outradius + 1
npts = LONG( !PI * ( r2*r2 - r1*r1 ) )
x = INTARR( npts )
y = INTARR( npts )
base = 0L

IF ( 0 LE inradius ) AND ( inradius LT outradius ) THEN BEGIN
   ; Compute the y-range limits.
   r2 = outradius
   r1 = inradius
   r2sq = r2 * r2
   r1sq  = r1 * r1
   yp = ycen - r2
   outy0 = FIX( yp )
   IF outy0 LT yp THEN outy0=outy0+1
   yp = ycen + r2
   outy3 = FIX( yp + 1 )
   IF outy3 GT yp THEN outy3=outy3 - 1
   FOR yp = outy0, outy3 DO BEGIN
      ; Step through each row and compute the x-range (or ranges).
      dy = yp - ycen
      dysq = dy * dy
      dx2 = SQRT( r2sq - dysq )
      xp = xcen - dx2
      outx0 = FIX( xp )
      IF outx0 LT xp THEN outx0=outx0+1
      xp = xcen + dx2
      outx3 = FIX( xp + 1 )
      IF outx3 GT xp THEN outx3=outx3-1
      IF dysq GT r1sq THEN BEGIN
         ; No intersection with inner radius.
         n = outx3 - outx0 + 1
         IF n EQ 0 THEN n = 1
         x[ base : base+n-1 ] = outx0 + INDGEN( n )
         y[ base : base+n-1 ] = REPLICATE( yp, n )
         base = base + n
      ENDIF ELSE BEGIN
         ; Passing through the inner circle.  Select two parts.
         dx1 = SQRT( r1sq - dysq )
         xp = xcen - dx1
         inx0 = FIX( xp + 1 )
         IF inx0 GT xp THEN inx0 = inx0 - 1
         xp = xcen + dx1
         inx3 = FIX( xp )
         IF inx3 LT xp THEN inx3=inx3+1
         n1 = inx0 - outx0 + 1
         n2 = outx3 - inx3 + 1
         IF n1 EQ 0 THEN n1 = 1
         IF n2 EQ 0 THEN n2 = 1
         x[ base : base+n1-1 ] = outx0 + INDGEN( n1 )
         y[ base : base+n1-1 ] = REPLICATE( yp, n1 )
         base = base + n1
         x[ base : base+n2-1 ] = inx3 + INDGEN( n2 )
         y[ base : base+n2-1 ] = REPLICATE( yp, n2 )
         base = base + n2
      ENDELSE
   ENDFOR

   ; Final verification and selection.
   t = WHERE( x[0:base-1] LT 0 OR x[0:base-1] GE xsize OR $
              y[0:base-1] LT 0 OR y[0:base-1] GE ysize, count )
   IF count GT 0 THEN BEGIN
      t = WHERE( x[0:base-1] GE 0 AND x[0:base-1] LT xsize AND $
                 y[0:base-1] GE 0 AND y[0:base-1] LT ysize, count )
      data = image[ x[t], y[t] ]
      IF N_PARAMS() EQ 7 THEN idx = x[t] + xsize * y[t]
   ENDIF ELSE BEGIN
      data = image[ x[0:base-1], y[0:base-1] ]
      IF N_PARAMS() EQ 7 THEN idx = x[0:base-1] + xsize * y[0:base-1]
   ENDELSE
ENDIF
END

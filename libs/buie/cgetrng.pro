;+
; NAME:
;    cgetrng
; PURPOSE: (one line)
;    How to integrate over a circle.
; DESCRIPTION:
;    This procedure is called to determine how to iterate when integrating
; over a circle.  The circle's center is at (xc,yc), and its radius is r.
; For pixels with x-coordinate x, those in the intervals [y0,y1) and [y2,y3)
; are on or near the circle.  Those in the interval [y1,y2) are definitely
; inside; all others are definitely outside.
;    Of course, the routine can be called to determine an interval for fixed
; y by calling it as cgetrng,yc,xc,r,y,x0,x1,x2,x3.
;    The appropriate way to integrate over a circle is therefore as follows:
; cgetrng,xc,yc,r,Round(xc),y0,y1,y2,y3
; for (y = y0; y <= y3-1; y=y+1) {
;    cgetrng, yc, xc, r, y, x0, x1, x2, x3;
;    for (x = x0; x <= x1-1; x=x+1) sum = sum + value(x,y)*pixwt(xc,yc,r,x,y);
;    for (x = x1; x <= x2-1; x=x+1) sum = sum + value(x,y);
;    for (x = x2; x <= x3-1; x=x+1) sum = sum + value(x,y)*pixwt(xc,yc,r,x,y);
;    }
; CATEGORY:
;    CCD data processing
; CALLING SEQUENCE:
;    cgetrng, xc, yc, r, x, y0, y1, y2, y3
; INPUTS:
;    xc, yc : Center of the circle.
;    r      : Radius of the circle.
;    x      : X coordinate for the intervals to be determined.
; OPTIONAL INPUT PARAMETERS:
;    None.
; KEYWORD PARAMETERS:
;    None.
; OUTPUTS:
;    y0, y1, y2, y3 : The endpoints of the three intervals of interest.
; COMMON BLOCKS:
;    None.
; SIDE EFFECTS:
; RESTRICTIONS:
;    None.
; PROCEDURE:
;    Determine three intervals along the x input coordinate:  The
;    intervals [y0,y1), [y1,y2), and [y2,y3).
; MODIFICATION HISTORY:
;    Ported by Doug Loucks, Lowell Observatory, 1992 Sep, from the
;    routine cgetrng in pixwt.c, by Marc Buie.
;    4/1/93, DWL, Added argument validation (badpar).
;-
PRO cgetrng, xc, yc, r, x, y0, y1, y2, y3

; Validate the number of arguments.
;IF N_PARAMS() NE 8 THEN BEGIN
;   MESSAGE, 'cgetrng,xc,yc,r,x,y0,y1,y2,y3', /INFO
;   RETURN
;ENDIF

; Validate input argument types.
;IF badpar( xc, [1,2,3,4,5], 0, CALLER='cgetrng' ) THEN RETURN
;IF badpar( yc, [1,2,3,4,5], 0, CALLER='cgetrng' ) THEN RETURN
;IF badpar( r, [1,2,3,4,5], 0, CALLER='cgetrng' ) THEN RETURN
;IF badpar( x, [1,2,3,4,5], 0, CALLER='cgetrng' ) THEN RETURN

sqrt2 = 1.414213563D0   ; Rounded up to increase size of uncertain areas.

IF r LE 0.0 THEN BEGIN  ; it misses completely
   outdsq = -1.0
ENDIF ELSE BEGIN
   a = r * r + 0.5 - ( x - xc ) * ( x - xc )
   b = sqrt2 * r
   outdsq = a + b
   IF b LT 1.0 THEN BEGIN  ; indsq would be invalid--say no interior
      indsq = -1.0
   ENDIF ELSE BEGIN
      indsq = a - b
   ENDELSE
ENDELSE

IF outdsq LT 0.0 THEN BEGIN  ; complete miss
   y0 = Round( yc )
   y1 = y0
   y2 = y0
   y3 = y0
ENDIF ELSE BEGIN  ; there is some intersection
   outd = SQRT( outdsq )
   y0 = Ceil( yc - outd )
   y3 = Floor( yc + outd ) + 1
   IF indsq LT 0.0 THEN BEGIN  ; no interior
      y1 = Round( yc )
      y2 = y1
   ENDIF ELSE BEGIN  ; there is a certain interior
      ind = SQRT( indsq )
      y1 = Ceil( yc - ind )
      y2 = Floor( yc + ind ) + 1
   ENDELSE
ENDELSE
END

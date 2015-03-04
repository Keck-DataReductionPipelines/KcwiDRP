;+
; NAME:
;    findmax
; PURPOSE: (one line)
;    Find the interpolated local maximum in a 2-D array.
; DESCRIPTION:
;
; CATEGORY:
;    Numerical
; CALLING SEQUENCE:
;    findmax, x, y, f, xmax, ymax, fmax, $
;             DELTA=in_delta, EPS=in_eps, GOLD=in_gold
; INPUTS:
;    x, y   : Position of the initial guess.
;    f      : The 2-D function array.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    DELTA   = Half-width of the box containing the desired maximum.
;              Default is 1.0 pixel.
;    EPS     = Stop criterion. Default=1.0E-5.
;    GOLD    = Pad value on DELTA.  Default is 1.0E-4.
; OUTPUTS:
;    xmax, ymax  : Position of computed maximum.
;    fmax        : Computed function maximum.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    Binary search (2D).
;    A guess for the location of the maximum is chosen.  The external function
; sint2d is called to obtain interpolated function values at two symmetric
; points along each axis (x and y).  For each axis, the two points are used
; to determine which way to shift the location of the maximum.  IF the
; function values at the two points are not equal, the location of the
; maximum is shifted by half the previous amount in the indicated direction,
; and a new set of four points are computed at half the offset used previously.
; This process continues until the offset falls below some small threshold
; value (epsilon).
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, September, 1993.
;    2007/06/21, MWB, promote x,y inputs to float (internally only).
;-
PRO findmax, in_x, in_y, in_f, out_xmax, out_ymax, out_fmax, $
             DELTA=in_delta, EPS=in_eps, GOLD=in_gold

IF N_PARAMS() NE 6 THEN BEGIN
   ;Display the calling sequence.
   PRINT, 'findmax, x, y, f, xmax, ymax, fmax, '
   PRINT, 'DELTA=, EPS=, GOLD='
   RETURN
ENDIF

;Verify incoming parameters.
IF badpar( in_x, [2,3,4,5], 0, CALLER='% FINDMAX: ' ) THEN RETURN
IF badpar( in_y, [2,3,4,5], 0, CALLER='% FINDMAX: ' ) THEN RETURN
IF badpar( in_f, [2,3,4,5], 2, CALLER='% FINDMAX: ' ) THEN RETURN

IF KEYWORD_SET( in_delta  ) THEN delta = in_delta ELSE delta = 1.0
IF KEYWORD_SET( in_eps ) THEN eps = in_eps ELSE eps = 1.0E-5
IF KEYWORD_SET( in_gold ) THEN gold = in_gold ELSE gold = 1.0E-4

;Set the starting values.
i = float(in_x)
j = float(in_y)
h = delta + gold
n = 0
REPEAT BEGIN
   ;Set two test points along the x-axis.
   x1 = i - h
   x2 = i + h

   ;Set two test points along the y-axis.
   y1 = j - h
   y2 = j + h

   ;Compute the function at the offset locations.
   f1 = sint2d( x2,  j, in_f )
   f2 = sint2d(  i, y2, in_f )
   f3 = sint2d( x1,  j, in_f )
   f4 = sint2d(  i, y1, in_f )

   ;Shift the x location of the next maximum.  If symmetric, no shift.
   IF f1 GT f3 THEN i = i + h
   IF f3 GT f1 THEN i = i - h

   ;Shift the y location of the next maximum.  If symmetric, no shift.
   IF f2 GT f4 THEN j = j + h
   IF f4 GT f2 THEN j = j - h

   ;Halve the search interval.
   h = 0.5 * h
   n = n + 1
   ;Test for convergence.
ENDREP UNTIL h LT eps

;MESSAGE, 'Number of iterations ' + STRING( n ), /INFO

;Store the results into the output parameters.
out_xmax = i
out_ymax = j
out_fmax = sint2d( i, j, in_f )

END

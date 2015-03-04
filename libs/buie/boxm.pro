;+
; NAME:
;    boxm
; PURPOSE: (one line)
;    Find location of a maximum within a sub-array.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;    boxm, image, xcen, ycen, deltay, deltax, xmax, ymax
; INPUTS:
;    image      : Image array.
;    xcen, ycen : Center of sub-array.
;    deltax     : Half-width of sub-array.
;    deltay     : Half-height of sub-array.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    ABSMAX     : Flag, if set, looks for pixel with greatest absolute value.
;
; OUTPUTS:
;    xmax, ymax : Coordinates, in image, of local maximum.
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
;    4/1/93, DWL, Added argument validation (badpar).
;    1/26/94, MWB, Added ABS argument.
;    12/13/95, MWB, added support for 3-d input arrays.
;    98/09/21, MWB, added NOCHECK keyword to speed up execution.
;    2002/07/12, MWB, changed function so that xcen,ycen,deltax,deltay
;                  are changed to long.  xcen,ycen are rounded, deltax/y
;                  are truncated.
;-
;
PRO boxm, image, xcen, ycen, deltax, deltay, xmax, ymax, ABSMAX=absmax, $
       NOCHECK=nocheck

if not keyword_set(nocheck) then begin
   ; Validate the number of arguments.  If not seven, print a usage line.
   IF N_PARAMS() NE 7 THEN BEGIN
      MESSAGE, 'boxm,image,xcen,ycen,deltax,deltay,xmax,ymax', /INFO
      RETURN
   ENDIF

   ; Validate the input parameters.
   IF badpar( image, [1,2,3,4,5,12,13,14,15], [2,3], CALLER='boxm',rank=imrank) THEN RETURN
   IF badpar( xcen, [1,2,3,4,5], 0, CALLER='boxm') THEN RETURN
   IF badpar( ycen, [1,2,3,4,5], 0, CALLER='boxm') THEN RETURN
   IF badpar( deltax, [1,2,3,4,5], 0, CALLER='boxm') THEN RETURN
   IF badpar( deltay, [1,2,3,4,5], 0, CALLER='boxm') THEN RETURN
endif else begin
   imrank = size(image,/n_dimensions)
endelse

deltax = long(deltax)
deltay = long(deltay)

s_image = SIZE( image )

x_size = s_image[1]
y_size = s_image[2]

startx = long(xcen - deltax + 0.5)
stopx = startx + 2*deltax
starty = long(ycen - deltay + 0.5)
stopy = starty + 2*deltay

; Make selected sub-array fit into the large array.
;
startx = 0 OR (startx GE 0 AND startx LT x_size) * startx OR $
                (startx GE x_size) * (x_size - 1)
stopx  = 0 OR (stopx GE 0 AND stopx LT x_size) * stopx OR $
                (stopx GE x_size) * (x_size - 1)
starty = 0 OR (starty GE 0 AND starty LT y_size) * starty OR $
                (starty GE y_size) * (y_size - 1)
stopy  = 0 OR (stopy GE 0 AND stopy LT y_size) * stopy OR $
                (stopy GE y_size) * (y_size - 1)

if imrank eq 2 then begin
   ; Extract the sub-array.
   ;
   t = image[ startx : stopx, starty : stopy ]

   ; Take absolute value (if requested)
   IF KEYWORD_SET( absmax ) THEN t = ABS(t)

   ; Get size info.
   ;
   t_size = SIZE( t )
   t_xsize = t_size[ 1 ]

   ; Locate the local maximum.
   ;
   t1 = WHERE( t EQ MAX( t ), count )

   ; Compute the image array coordinates of the local maximum.
   ;
   IF count NE 0 THEN BEGIN
      w = t1[0] / t_xsize
      xmax = startx + t1[0] - w * t_xsize
      ymax = starty + w
   ENDIF

endif else begin
   nframes = s_image[3]
   xmax = replicate(-10,nframes)
   ymax = replicate(-10,nframes)
   for i=0,nframes-1 do begin
      t = image[ startx : stopx, starty : stopy, i ]
      IF KEYWORD_SET( absmax ) THEN t = ABS(t)
      t_size = SIZE( t )
      t_xsize = t_size[ 1 ]
      t1 = WHERE( t EQ MAX( t ), count )
      t_size = SIZE( t )
      t_xsize = t_size[ 1 ]
      t1 = WHERE( t EQ MAX( t ), count )
	   IF count NE 0 THEN BEGIN
	      w = t1[0] / t_xsize
	      xmax[i] = startx + t1[0] - w * t_xsize
	      ymax[i] = starty + w
	   ENDIF
   endfor
endelse

END

;+
; NAME:
;  gridwt
; PURPOSE:
;  Compute circle overlap weights in a circle within an array.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  gridwt,xc,yc,radius,xsize,ysize,wtarr
; INPUTS:
;  xc     - X center of circle.
;  yc     - Y center of circle.
;  radius - Radius of the circle in pixels.
;  xsize  - X size of array
;  ysize  - Y size of array
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  wtarr  - Output array filled with overlap area of each pixel with given
;            circle.  Values range from 0 to 1.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  97/11/12 - Written by Marc W. Buie
;-
pro gridwt,xc,yc,radius,xsize,ysize,wtarr
   wtarr=fltarr(xsize,ysize)
   cgetrng,xc,yc,radius,round(xc),y0,y1,y2,y3
   ylow = max([y0,0])
   yhigh = min([y3,ysize])-1
   for y=ylow,yhigh do begin
      cgetrng,yc,xc,radius,y,x0,x1,x2,x3
      if x1 ge 0 and x2 lt xsize then begin
         if (x1 lt x2-1) then wtarr[x1:x2-1,y] = 1.0
      endif else if x2 ge 0 and x2 lt xsize then begin
         if (0 lt x2-1) then wtarr[0:x2-1,y] = 1.0
      endif else if x1 ge 0 and x2 gt xsize then begin
         if (x1 lt xsize-1) then wtarr[x1:xsize-1,y] = 1.0
      endif else begin
         if (0 lt xsize-1) then wtarr[0:xsize-1,y] = 1.0
      endelse
      for x=x0,x1-1 do begin 
         if x ge 0 and x lt xsize then wtarr[x,y] = pixwt(xc,yc,radius,x,y)
      endfor
      for x=x2,x3-1 do begin
         if x ge 0 and x lt xsize then wtarr[x,y] = pixwt(xc,yc,radius,x,y)
      endfor
   endfor
end

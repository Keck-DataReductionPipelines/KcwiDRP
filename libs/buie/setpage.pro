;+
; NAME:
;  setpage
; PURPOSE:
;  Set size and location of plot on page to center the output.
; DESCRIPTION:
;  This will do something only if the output plot device is PS or PRINTER.
;    In that case, it will setup a centered page output.  The default is
;    landscape mode with xsize=25cm, and ysize=19cm.
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  setpage
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  LANDSCAPE - Flag, if set causes a landscape orientation output
;  PORTRAIT  - Flag, if ste causes a portrait orientation output
;                If you set both, you'll get portrait
;  XSIZE     - width of output in centimeters, default for landscape is 25,
;                default for portrait is 19.
;  YSIZE     - height of output in centimeters, default for landscape is 19,
;                default for portrait is 25.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written 96/08/28, Marc W. Buie, Lowell Observatory
;-
pro setpage,LANDSCAPE=landscape,PORTRAIT=portrait,XSIZE=xsize,YSIZE=ysize

if !d.name eq 'PS' or !d.name eq 'PRINTER' then begin

   if keyword_set(landscape) or not keyword_set(portrait) then begin
      if badpar(xsize,[0,2,3,4,5],0,CALLER='SETPAGE: (xsize) ',default=25.0) then return
      if badpar(ysize,[0,2,3,4,5],0,CALLER='SETPAGE: (ysize) ',default=19.0) then return
      xoffset = (21.59 - ysize) / 2.0
      yoffset = (27.94 - xsize) / 2.0 + xsize
      device,xsize=xsize,ysize=ysize,xoffset=xoffset,yoffset=yoffset,/landscape
   endif

   if keyword_set(portrait) then begin
      if badpar(xsize,[0,2,3,4,5],0,CALLER='SETPAGE: (xsize) ',default=19.0) then return
      if badpar(ysize,[0,2,3,4,5],0,CALLER='SETPAGE: (ysize) ',default=25.0) then return
      xoffset = (21.59 - xsize) / 2.0
      yoffset = (27.94 - ysize) / 2.0
      device,xsize=xsize,ysize=ysize,xoffset=xoffset,yoffset=yoffset,/portrait
   endif

endif

end

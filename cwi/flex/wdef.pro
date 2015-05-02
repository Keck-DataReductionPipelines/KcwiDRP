pro wdef,ws,xs,ys, uright=uright, uleft=uleft, lright=lright, lleft=lleft, $
	free=free, retain=retain, delete=delete, xpos=xpos, ypos=ypos, $
	pixmap=pixmap, zbuffer=zbuffer, xwindow=xwindow, loud=loud, $
	erase=erase, title=title, already=already, image=image, $
	umid=umid
;+
; NAME:
;        WDEF
; PURPOSE:
;       Makes a window quick and easy. 
;	Allocate free window# if not specified to avoid window collisions
;
; CALLING SEQUENCE:
;          wdef [,ws,xs,ys,retain=retain]
;          wdef, ws [,/uleft,/uright,/lleft,lright] ; specify quadrant postion
;						    ; ws is allocated and 
;						    ; OUTPUT if not defined
;          wdef,dummy,xs,ys,/zbuffer		    ; zbuffer, not x-windows
;          wdef,dummy,image=image		    ; size window to fit image
;
; INPUTS:
;	ws = window number, default=0 (ws is output if not defined on input)
;				      (if defined, ws must be in range (0-31)
;	xs = xsize, defaults to 512
;	ys = ysize, defaults to xs
;
; OUTPUTS:
;       ws - returns window number allocated if not defined on input
;
; KEYWORD PARAMETERS:
;	umid   - if set, position windown in upper middle
;       uright - if set, position window in upper right 
;       uleft  - if set, position window in upper left
;       lright - if set, position window in lower right
;       lleft  - if set, position window in lower left
;	zbuffer - if set, set device to z buffer first
;	xwindow - if set, set device to x first
;	already	- If set, see if the active window is the proper size
;		  and if so, do not call "window" again
;       image   - if set, determine xsize and ysize from image dimensions
;
; MODIFICATION HISTORY:
;	First ever program written by LWA with Jim Lemen's help.
;	Made public April 1992
;	23-Sep-92 (MDM) - Modified to not use RETAIN=2 for NCD X-terminals
;       16-feb-93 (slf) - allocate from free window pools to avoid window
;			  collisions from multiple routines. Add retain 
;			  keywrd.  Added quadrant position keywords
;	 7-apr-93 (slf) - add pixmap keyword, check range of ws if defined
;	24-May-93 (MDM) - Patch so that it would work with SUN-View
;        3-sep-93 (slf) - inhibit device if batch mode
;	 4-sep-93 (slf) - Added Z-buffer support
;	 8-sep-93 (slf) - add title keyword 
;	 5-Apr-94 (MDM) - added /already switch
;       14-Jul-94 (SLF) - added IMAGE keyword (size to image), quiet zbuff
;        6-Dec-94 (SLF) - documentation
;	14-sep-95 (SLF) - allow /FREE and no window number-> free window
;        9-Jun-98 (SLF) - move quadrant code to X logic block 
;	18-May-05 (LWA) - commented out Z buffer message
;	30-Nov-05 (LWA) - added keyword /umid
;-
; setup zbuffer if device=Z, request or batch job

loud=keyword_set(loud)
if loud then begin
   print
   help,!d.name
   help,/trace
endif

if keyword_set(xwindow) and !d.name ne 'X' then begin
   message,/info,'Setting plot device to X...'
   set_plot,'x'
endif

zbuffer= keyword_set(zbuffer) or getenv('ys_batch') ne '' $
	  or (!d.name eq 'Z' and 1-keyword_set(xwindow))

pixmap=keyword_set(pixmap)

if not keyword_set(retain) then begin
   item = strupcase(getenv('DISPLAY'))
   case strmid(item,0,3) of
      'NCD':retain=1		; ncds provide backing store
      else: retain=2		; default - let idl do it
   endcase
endif

; resolve paramters
case 1 of
   n_params() eq 0 and (1-keyword_set(free)) : ws=0	; upwardly compatible
   n_elements(ws) eq 0  or keyword_set(free) : ws=-1	; force free window
   (n_elements(ws) eq 1) and (ws ge 128):begin	; assume 'ws' is really xs
   	xs=ws 
	ws=-1					; force free window
   endcase			
   else:
endcase

if keyword_set(image) and n_elements(xs) eq 0 then begin
   xs=(size(image))(1)
   ys=(size(image))(2)   
endif

; assign size defaults
if n_elements(xs) eq 0 then xs=512	; defaults to 512
if n_elements(ys) eq 0 then ys=xs	; defaults to sqare

if (keyword_set(already) and (n_elements(ws) ne 0)) then begin	;MDM added 5-Apr-94
    if ((!d.window eq ws) and (!d.x_size eq fix(xs)) and (!d.y_size eq fix(ys))) then return
end


if zbuffer then begin ; Z-buffer support, slf, 4-sep
;   -------------------- z buffer setup ------------------------------
    if !d.name ne 'Z' then begin
   ;    message,/info,'Setting plot device to Z-Buffer...'
       set_plot,'Z'
    endif
    if keyword_set(erase) then erase
    if loud then help,'Device: ' + !d.name, xs,ys
    device,set_resolution=[xs,ys]	; zbuffer equivilent of window size  
;   ------------------------------------------------------------------
endif else begin
;   determine quadrant offsets

    dxy=[0,0]
   if (!d.name eq 'X') and not zbuffer then begin
       device,get_screen=ssize		; how big is this X-monitor? 
   end else begin
       ssize=[900,900]			;take a guess since "get_screen" keyword is not valid for SUN-View
   end

   autopos=0
   case 1 of 
      keyword_set(umid) : dxy= [(ssize(0)/2)-(xs/2),ssize(1)-ys]
      keyword_set(uleft): dxy= [ 0,ssize(1)-ys]
      keyword_set(lleft): dxy= [ 0, 0]
      keyword_set(lright):dxy= [ssize(0)-xs, 0]
      keyword_set(uright):dxy= [ssize(0)-xs,ssize(1)-ys]
      else: autopos=1 - (keyword_set(xpos) or keyword_set(ypos))
   endcase   

   if keyword_set(xpos) then dxy(0)=xpos	; passed, so override
   if keyword_set(ypos) then dxy(1)=ypos   ; passed, so override

;  --------------------- x windows setup -----------------------------
;   if window number was specified, use it, otherwise use a free window
    if ws ge 0 then begin
       if ws gt 31 then begin
          message,/info,'Explicit window number input must be in range {0 - 31}'
       endif else begin
	  if n_elements(title) eq 0 then title='IDL ' + strtrim(ws,2)
          if autopos then begin
	     window,ws,xsiz=xs, ysiz=ys, retain=retain, pixmap=pixmap, title=title
          endif else begin
             window,ws,xsiz=xs, ysiz=ys, retain=retain, xpos=dxy(0), ypos=dxy(1), pixmap=pixmap , title=title
          endelse
       endelse
    endif  else begin
       ; Need if for title since null title passed to window causes error!
       if keyword_set(title) then $
          window,/free, xsiz=xs, ysiz=ys,retain=retain, $
		xpos= dxy(0), ypos=dxy(1), pixmap=pixmap, title=title else $
          window,/free, xsiz=xs, ysiz=ys,retain=retain, $
		xpos= dxy(0), ypos=dxy(1), pixmap=pixmap
       ws=!d.window				; assign to output
    endelse
;  -------------------------------------------------------------------
endelse

if loud then begin
   print
   help,!d.name
   help,/trace
endif

return
end

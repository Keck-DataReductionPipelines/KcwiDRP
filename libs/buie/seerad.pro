;+
; NAME:
;  seerad
; PURPOSE:
;  Display a radial profile at X and Y value of the cursor
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  seerad, image [, X0, Y0]
; INPUTS:
;  Image = Array containing values that are displayed.  May be
;          any type.  Rather than pixel values being read from the display
;          they are taken from this parameter, avoiding scaling difficulties.
; OPTIONAL INPUT PARAMETERS:
;  X0, Y0 = Optional location of lower left corner of image area on
;           screen.
; OUTPUTS:
;  None.
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
;  The X, Y and value of pixel under the cursor are constantly 
;    displayed.
;
;  Pressing left or center mouse button, makes a new line of output,
;    saving the old and generating a new profile.
;  Pressing right mouse button, exits the procedure.
;
; RESTRICTIONS:
;  None.
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Marc W. Buie, Lowell Observatory, 1992 March 27
;-
pro seerad,image,x0,y0
on_error,2              ;Return to caller if an error occurs
window,1
wset,0
print,'Press left or center mouse button for new plot.'
print,'... right mouse button to exit.'
s = size(image)
if s[0] ne 2 then message, 'Image parameter not 2d.'
s[1] = s[1]-1  ;To n-1
s[2] = s[2]-1
!mouse.button=0
x1=-1 & y1=-1  ;Previous values
if n_elements(x0) le 0 then x0 = 0
if n_elements(y0) le 0 then y0 = 0
if s[s[0]+1] ge 4 then form = 'F' else form = 'I'
cr = string("15b) ;"
form="($,'x=',i4,', y=',i4,', value=',"+form+",a)"
while !mouse.button ne 4 do begin
   cursor,x,y,2,/dev
   if (!mouse.button and 3) ne 0 then begin ;New line?
      print,form="($,a)",string("12b) ;"
      while (!mouse.button ne 0) do begin wait,.1 & cursor,x,y,0,/dev & end
           radp,image[x-15:x+15,y-15:y+15],15.0,15.0,r,i
           wset,1
           plot,[-r,r],[i,i],psym=3
           wset,0
   endif

   x = x-x0 & y = y - y0
   if !order ne 0 then yy = (s[2] > !d.y_vsize) - 1 - y $
   else yy = y
   if (x le s[1]) and (yy le s[2])  and (x ge 0) and $
      (y ge 0) then $
      print,form = form, x,y,Image[x,yy],cr
      x1 = x & y1=y
endwhile
print,form="(/)"
end

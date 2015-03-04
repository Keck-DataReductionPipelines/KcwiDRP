;+
; NAME:
;  rddat
; PURPOSE:
;  Display the X and Y value of the cursor in a plot
; CATEGORY:
;  2-D plotting
; CALLING SEQUENCE:
;  rddat
; INPUTS:
;  None.
; OUTPUTS:
;  None.
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
;  The X, Y and data values of plot under the cursor are constantly 
;  displayed.
;
;  Pressing left or center mouse button, makes a new line of output,
;     saving the old.
;  Pressing right mouse button, exits the procedure.
;
; RESTRICTIONS:
;  None.
; PROCEDURE:
;  Straightforward.
; MODIFICATION HISTORY:
;  DMS, Dec, 1987.
;  cloned from RDPIX by Marc W. Buie, March 1991.
;  98/06/27, MWB, converted from !err to !mouse
;  99/05/28, MWB, added kludge fix for operation on a Windows platform.
;                   Unless RSI implements <return> (no line feed) this
;                   routine (and rdpix) are not fully useful.
;-
pro rddat

   on_error,2              ;Return to caller if an error occurs

   if !version.os_family eq 'Windows' then $
      cr = string("12b) $  ;"
   else $
      cr = string("15b)  ;"

   !mouse.button=0
   print,'Press left or center mouse button for new output line.'
   print,'... right mouse button to exit.'
   fmt="($,a,'x=',f10.4,2x,'y=',f10.4)"

   while !mouse.button ne 4 do begin
      cursor,x,y,2,/data
      if !mouse.button eq 1 or !mouse.button eq 2 then begin  ;New line?
         print,''
         print,format=fmt,'',x,y
         cursor,x1,y1,4,/data
      endif

      print,format=fmt,cr,x,y
   endwhile

   if !version.os_family ne 'Windows' then print,''

end

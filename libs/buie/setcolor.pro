;+
; NAME:
;  setcolor
; PURPOSE:   (one line only)
;  Set a 24-bit color value for plotting on 24-bit direct graphics or Postscript
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  value=setcolor(input)
; INPUTS:
;  input - Input value.  Choices are:
;           Scalar long - this is taken to be the BGR triplet,
;                          example: '0000ff'xl is red.
;           Byte or integer, 3-element vector - this is taken to be RGB
;                          example: [0,0,255] is red.
;  If the input is invalid in any way !d.n_colors-1 is returned.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the color index needed.  This is intended to be passed
;    to the color keyword on many plot programs.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;   If the display is 24-bit, the color is returned as a LONG scalar.
;   If the display is Postscript, then a three color lookup table is loaded
;     where 0=white, 1=your color, 2=black.  In this case the return value
;     is set to 1.  To return the color to a normal setting after this
;     you need to do the following manually.
;   tvlct,bindgen(256),bindgen(256),bindgen(256)
;     or
;   loadct,0,/silent
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2009/01/13
;-
function setcolor,input

   if size(input,/type) eq 3 and n_elements(input) eq 1 then begin
      lvalue = input
      rd = byte(lvalue and 'ff'xl)
      gn = byte(ishft(lvalue and 'ff00'xl,-8))
      bl = byte(ishft(lvalue and 'ff0000'xl,-16))
   endif else if size(input,/type) ge 1 and size(input,/type) le 3 and $
                 n_elements(input) eq 3 then begin
      lvalue = ishft(long(input[2]),16) + $
               ishft(long(input[1]),8) + $
               long(input[0])
      rd = byte(input[2])
      gn = byte(input[1])
      bl = byte(input[0])
   endif else begin
      return,!d.n_colors-1
   endelse

   if !d.name eq 'PS' then begin
      tvlct,[0,rd,255],[0,gn,255],[0,bl,255]
      return,1
   endif else if !d.name eq 'X' and !d.n_colors eq 16777216L then begin
      return,lvalue
   endif else begin
      return,!d.n_colors-1
   endelse

end

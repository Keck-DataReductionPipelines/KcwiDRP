;+
; NAME:
;  strb62
; PURPOSE:
;  Convert an integer into a Base 62 formatted string.
; DESCRIPTION:
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  str=strb62(val)
; INPUTS:
;  val - Integer (byte, int, long) to be converted
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return is the string
; KEYWORD OUTPUT PARAMETERS:
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
;  99/02/23, Written by Marc W. Buie, Lowell Observatory
;  2000/05/03, MWB, added bi-directional conversion capability.
;-
function strb62,in_val,PAD=pad

   if badpar(in_val,[1,2,3,7],[0,1],CALLER='STRB62: (val) ',type=in_type) then return,''

   if in_type ne 7 then begin
      if badpar(pad,[0,1,2,3],0,caller='STRB62: (PAD) ',default=0) then return,''
      str=''
      val=in_val

      repeat begin
         digit = val mod 62
         val   = val / 62
         if digit le 9 then $
            str = string(48b+byte(digit))+str $
         else if digit le 35 then $
            str = string(55b+byte(digit))+str $
         else $
            str = string(61b+byte(digit))+str
      endrep until val eq 0

      for i=0,pad-strlen(str)-1 do str='0'+str
      return,str

   endif else begin
      str = in_val
      val = 0L
      while strlen(str) ne 0 do begin
         val = val*10L
         c = strmid(str,0,1)
         str = strmid(str,1,999)
         if c ge '0' and c le '9' then $
            val = val + long(c) $
         else if c ge 'A' and c le 'Z' then $
            val = val + byte(c)-55L $
         else if c ge 'a' and c le 'z' then $
            val = val + byte(c)-61L
      endwhile
      return,val[0]
   endelse

end

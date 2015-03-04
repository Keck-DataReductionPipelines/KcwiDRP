;+
; NAME:
;  strb36
; PURPOSE:
;  Convert an integer into a Base 36 formatted string.
; DESCRIPTION:
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  str=strb36(val)
; INPUTS:
;  val - input is either scalar or vector
;     if Integer (byte, int, long) convert to a string
;          or
;     if String convert to long.  Leading and trailing blanks are
;          ignored but once trimmed all characters must be letters or numbers.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  PAD - Length to pad the output string to.  Leading zeros will be prepended
;          to the string to make the string of length PAD.  If the string is
;          already longer than PAD nothing is done.  The default is zero (ie.,
;          no padding).  This keyword has no effect if the input is a string.
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
;  1998/03/16, Written by Marc W. Buie, Lowell Observatory
;  2000/02/16, MWB, added inverse function operation.
;  2010/01/14, MWB, added support for vector input.
;-
function strb36,in_val,PAD=pad

   if badpar(in_val,[1,2,3,7],[0,1],caller='STRB36: (val) ', $
                                type=valtype,npts=nvals) then return,''
   if badpar(pad,[0,1,2,3],0,caller='STRB36: (PAD) ',default=0) then return,''

   ; convert from a string to a number
   if valtype eq 7 then begin

      number = replicate(0L,nvals)
      for j=0,nvals-1 do begin
         val=strtrim(strupcase(in_val[j]),2)
         for i=0,strlen(val)-1 do begin
            c=strmid(val,i,1)
            case 1 OF
               c ge '0' and c le '9': begin
                  digit = fix(c)
               end
               c ge 'A' and c le 'Z': begin
                  digit = byte(c) - 55b
               end
               else: begin
                  print,'STRB36: Error! Illegal character in input string. [', $
                         val,']  [',c,'] is bad.'
                  return,-1
               end
            endcase
            number[j] = number[j]*36 + digit[0]
         endfor
      endfor
      return,trimrank(number)

   ; convert from a number to a string
   endif else begin

      str=replicate('',nvals)

      for j=0,nvals-1 do begin
         val=in_val[j]

         repeat begin
            digit = val mod 36
            val   = val / 36
            if digit le 9 then $
               str[j] = string(48b+byte(digit))+str[j] $
            else $
               str[j] = string(55b+byte(digit))+str[j]
         endrep until val eq 0

         for i=0,pad-strlen(str[j])-1 do str[j]='0'+str[j]

      endfor

      return,trimrank(str)

   endelse

end

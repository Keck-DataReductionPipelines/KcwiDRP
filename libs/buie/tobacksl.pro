;+
; NAME: 
;  tobacksl
; PURPOSE:
;  Convert forward slash (/) to backslash ($\backslash$) in string.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  str=tobacksl(str)
; INPUTS:
;  str - String to scan and convert
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is a copy of the string with / converted to \\
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/01/21, Written by Marc W. Buie
;-

function tobacksl,in_str
   if badpar(in_str,7,0,CALLER='TOBACKSL: (str) ') then return,''
   str = in_str
   pos = strpos(str,'/')
   while pos ne -1 do begin
      strput,str,'\',pos
      pos = strpos(str,'/')
   endwhile
   return,str
end

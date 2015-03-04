;+
; NAME:
;  repchar
; PURPOSE:
;  Replace a target string with a new string in string or string array.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  result = repchar(str,c1,c2)
; INPUTS:
;  str - input string or string array to be scanned
;  c1  - Target string, if found will be replaced with c2
; OPTIONAL INPUT PARAMETERS:
;  c2  - Replacement string for target, default = ' '
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the modified string, input not modified
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;    96/07/02 - Written by Marc W. Buie, Lowell Observatory
;-
function repchar, str, c1, c2
   
   if badpar(str,7,[0,1],CALLER='REPCHAR: (str) ') then return,str
   if badpar(c1,7,0,CALLER='REPCHAR: (c1) ') then return,str
   if badpar(c2,[0,7],0,CALLER='REPCHAR: (c2) ',default=' ') then return,str

   for i=0,n_elements(str)-1 do begin

      ; Temp copy to be modified (maybe)
      new=str[i]

      ; Loop until target string is no longer seen.
      repeat begin
         loc = strpos(new,c1)

         ; string found, now replace it
         if loc ne -1 then begin
            tmp = ''

            ; add string before target
            if loc ne 0 then tmp=tmp+strmid(new,0,loc)

            ; add replacement for target
            tmp=tmp+c2

            ; add string after target
            if loc+strlen(c1) lt strlen(new) then $
               tmp=tmp+strmid(new,loc+strlen(c1),strlen(new))

            ; may be more, not done yet.
            done = 0
            new=tmp

         ; target string not found, we're done with this string.
         endif else begin
            done = 1
         endelse
      endrep until done

      ; Add new string to output, don't make array until second string.
      if n_elements(outstr) eq 0 then $
         outstr=new $
      else $
         outstr=[outstr,new]

   endfor

   return,outstr

end

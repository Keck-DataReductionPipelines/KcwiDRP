;+
; NAME: 
;  addslash
; PURPOSE: 
;  Append a trailing / to string (if needed).
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  addslash,name
; INPUTS:
;  name - string to modify (scalar or vector)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  name - string with trailing slash
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  1995/06/08, Written by Marc W. Buie, Lowell Observatory
;  1999/02/08, MWB, added clause for Mac style directories.
;  2009/02/27, MWB, upgraded to handle vector input.
;-
function addslash,in_name

   if badpar(in_name,7,[0,1],CALLER='ADDSLASH: (name) ') then return,''

   name = in_name

   for i=0,n_elements(name)-1 do begin
      if strlen(name[i]) eq 0 then name[i] = './'
      if !version.os_family eq 'Windows' then begin
         if (strmid(name[i],strlen(name[i])-1,1) ne '/') and $
            (strmid(name[i],strlen(name[i])-1,1) ne '\') then name[i] = name[i] + '\'
      endif else if !version.os_family eq 'MacOS' then begin
         if strmid(name[i],strlen(name[i])-1,1) ne ':' then name[i] = name[i] + ':'
      endif else begin
         if strmid(name[i],strlen(name[i])-1,1) ne '/' then name[i] = name[i] + '/'
      endelse
   endfor

   return,name

end

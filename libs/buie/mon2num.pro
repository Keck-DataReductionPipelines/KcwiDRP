;+
; NAME:
;  mon2num
; PURPOSE:   (one line only)
;  Convert the name of a month to its integer equivalent.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  mon2num, name, num
; INPUTS:
;  name - String or string array with the name of a month, the case does
;            not matter and the string need only be long enough to be unique.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  num - Number of the month (1=Jan, 2=Feb, etc.)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  If the name you give is not unique, the first match is returned.
;  If the name you give is not correct, you get a 0 back for that entry.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2008/02/03
;-
pro mon2num,name,num

   self='MON2NUM: '
   if badpar(name,7,[0,1],caller=self+'(name) ') then return

   months = [ 'January', 'February', 'March', 'April', 'May', 'June', $
              'July', 'August', 'September', 'October', 'November', 'December' ]

   if n_elements(name) eq 0 then num=0 else num=intarr(n_elements(name))

   sname = name[uniq(name,sort(name))]
   for i=0,n_elements(sname)-1 do begin
      idx = where(strmatch(months,sname[i]+'*',/fold_case) eq 1)
      z=where(sname[i] eq name)
      num[z] = idx[0]+1
   endfor

end

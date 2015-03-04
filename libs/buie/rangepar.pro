;+
; NAME:
;     rangepar
; PURPOSE: (one line)
;     Parse a string with numbers and ranges to get an expanded list of numbers.
; DESCRIPTION:
;   You can provide single numbers or a range of numbers.  Ranges are
;     delineated by a hyphen (for example 120-125).  Ranges or single numbers
;     are separated by either a comma or space.  Spaces (even multiples) are
;     tolerated anywhere.  But, only one comma is allowed between numbers or
;     ranges.  The string can have an optional exclusion descriptor.  Anything
;     after the first 'x' is used as an exclusion set (ranges or numbers,
;     same syntax).
;
;   For example, the string '100-105x103' would return [100,101,102,104,105]
;      as an array of integers.
;   If the input is [100,101,102,104,105] the result would be '100-102,105-105'
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;     rangepar,input,output
; INPUTS:
;     input  - if input is a string, it is parsed to generate a list of numbers
;                (see description)
;              if input is an array of numbers it is converted to a string.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;     output - string or integer array depending on the input.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   In the reverse conversion (from numbers to a string), the routine
;     will handle negative numbers but will produce output like -5--3
;     which rangepar can't parse in the other direction.  Also, the
;     'x' construct is not used in this direction.  Thus there is no
;     promise that applying rangepar twice will return you to an
;     identical starting point (though the implied list is the same).
; PROCEDURE:
; MODIFICATION HISTORY:
;     2001/07/25 - Written by Marc W. Buie, Lowell Observatory.
;     2008/12/05 - Peter L. Collins, Lowell Observatory, added /REVERSE.
;     2009/11/19, MWB, removed REVERSE and made operation context dependent.
;-
pro rangepar_single,str,range

   ; split the string by blank delimiters
   parts = strsplit(str,' ',/extract)

   ; split each of the parts by hyphens and build the list of numbers
   nvals = 0
   for i=0,n_elements(parts)-1 do begin
      val=long(strsplit(parts[i],'-',/extract))
      if n_elements(val) eq 1 then begin
         val1 = val[0]
         val2 = val[0]
      endif else begin
         val1 = val[0]
         val2 = val[1]
      endelse

      if nvals eq 0 then begin
         range = lindgen(val2-val1+1)+val1
         nvals = val2-val1+1
      endif else begin
         range = [range,lindgen(val2-val1+1)+val1]
         nvals = nvals + val2-val1+1
      endelse
      
   endfor

end

pro rangepar,input,output

   self='rangepar: '
   if badpar(input,[2,3,7],[0,1],caller=self+'(input) ', $
                                 rank=inputrank,type=inputtype) then return
   if inputtype eq 7 and inputrank ne 0 then begin
      print,self,'Vector string input is not allowed.'
      return
   endif

   ; convert from string to array
   if inputtype eq 7 then begin
      ; make a working copy of the input string
      str=input

      ; replace any commas with a blank
      repeat begin
         pos=strpos(str ,',')
         if pos ge 0 then begin
            strput,str,' ',pos
         endif
      endrep until pos lt 0

      ; eliminate leading and trailing blanks then compress multiple
      ;   blanks to one.
      str = strcompress(strtrim(str,2))

      ; now break on any 'x', first part goes in, second part goes out.
      parts=strsplit(str,'x',/extract)

      rangepar_single,parts[0],output
      if n_elements(parts) gt 1 then begin
         rangepar_single,parts[1],remove
         intrsect,output,remove,overlap,noverlap
         if noverlap gt 0 then begin
            intrsect,output,overlap,keep,nfound,/nnot
            if nfound gt 0 then begin
               output = keep
            endif else begin
               print,'Everything is excluded, excluding nothing.'
            endelse
         endif
      endif

   ; convert from array to string
   endif else begin

      ; created sorted list without duplicates
      list = input[uniq(input,sort(input))]
      nlist=n_elements(list)
      output=''
      if nlist gt 1 then begin
         ; array of increment from ith to i+1th
         neighbor = list[1:*]-list[0:nlist-2]
         ; array of where ith is -end- of a run (which could be a run of 1).
         zspace =  where(neighbor ne 1,nsp)
         ; a final "implicit" zspace entry for the very end of list.
         if nsp++ eq 0 then zspace = nlist-1 else zspace = [zspace, nlist-1]
         srange=0  
         for i=0, nsp-1 do begin
            if srange gt zspace[i]-1 then  begin
               ; this is a run of 1.
               output += ( strn(list[srange]) + ',' )
               srange++
            endif else begin
               ; this is a run of 2 or more
               output += (strn(list[srange]) + '-' + strn(list[zspace[i]]) + ',')
               srange = zspace[i]+1
            endelse
         endfor
         output = strmid(output, 0, strlen(output)-1) ; kill trailing comma
      endif else begin
         output = strn(list)
      endelse

   endelse

end

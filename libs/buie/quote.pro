;+
; NAME:
;   quote
; PURPOSE:   (one line only)
;   Convert a string into one safe for including in a mySQL query
; DESCRIPTION:
;   Similar to the mysql function quote.  Leading and trailing blanks
;   are removed.  Leading and trailing quotes are preserved but are considered
;   to be delimiters for the string provided there is a leading and a trailing
;   quote.  If there is just one it is considered to be interior to the string.
;   An interior single quote is converted to \' and a backslash is converted
;   to \\.  Embedded newlines (string(10B)) are quoted with '\n' where n is
;   the letter n.
;   On output, the string will have leading and trailing quotes added
;   if they weren't already there. However, if the string is a scalar
;   and it is 'NULL' (4 characters) after stripping blanks 
;   it is returned as is without quotes.
;   The input can either be a single string or a string array. If an array,
;   the strings are joined with a quoted newline separator. For example,
;    s = quote(['this text output','more of the output']) produces
;    " 'this text output\nmore of the output' " as a scalar string. This 
;    feature exists to support comment fields of type TEXT or BLOB.
;   In multiline cases (input is a string array or contains 10B newline 
;   characters) empty or all whitespace lines at the begining or end
;   of the total string will be trimmed. A single such line comes back as
;   a ''.
; CATEGORY:
;   Database
; CALLING SEQUENCE:
;   result = quote(string)
; INPUTS:
;   string - String to be processed- if a string array, the strings are
;            joined with a newline quoted after each string including the
;            last. Embedded newline characters (10B) are also quoted in
;            this way.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  LITERAL- Flag, if set, some of the processing of quote is not done,
;              specifically removal of trailing/leading blanks, special
;              treatment for the string 'NULL', and removal of leading and
;              null/whitepace lines in the multiline cases. The processing 
;              of single quotes and backslashes in the input string is the same.
; OUTPUTS:
;   return value is a string that is ready for mySQL
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; Newlines are currently quoted with two characters: '\n' where the
; n is a literal lower case n. It is inadvisable to try to use an
; actual '\n' within the string to be quoted, although it will work
; currently if the string is a scalar and contains no embedded 10B characters.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/08/12
;  2006/10/27, Peter L. Collins, Lowell Observatory
;              add support for string array input (with newlines)
;  2006/11/28, PLC, allow literal (string(10B)) newlines in str.
;  2006/12/11, PLC, disconnect LITERAL flag from rank of str and
;                   (except if LITERAL) trim leading and trailing 
;                    null or all whitespace lines in the multiline cases.
;-
function quote,str,LITERAL=literal

   self='QUOTE: '
   if badpar(str,7, [0,1], caller=self+'(str) ',RANK =strrank) then return,''
   if badpar(literal,[0,1,2,3], 0, caller=self+'(LITERAL) ', $
             default=0) then return,''

   strvector = (strrank eq 1)
   multiline = strvector
   t = "'"

   ; trim leading and trailing blanks
   if not literal then newstr = strtrim(str,2) $
   else newstr = str

   ; check for special value NULL
   if not literal and not strvector and newstr[0] eq 'NULL' then return,newstr

   ; are there leading and trailing single quotes?  If so, trim them off
   sizeable = where(strlen(newstr) ge 2)
   if sizeable[0] ge 0 then begin
      strippable = where( strmid(newstr[sizeable],0,1) eq t  and $
                          strmid(newstr[sizeable],0,/REVERSE_OFFSET) eq t)
      if  strippable[0] ge 0 then begin
         strippable = sizeable[strippable]
         newstr[strippable] = strmid(newstr[strippable], 1, $
                                     strlen(newstr[strippable])-2)
      endif
   endif
       
   ; find all single quotes and \ and prepend a \ to each.
   patt = "('|\\)"
   for j = 0, n_elements(newstr)-1 do begin
      ; use the regex feature of strplit to find all pattern match
      ; boundaries
      pos = strsplit(newstr[j],patt,/regex,LENGTH=len,/PRESERVE_NULL)
      if n_elements(pos) gt 1 then begin
         ; join the substrings back appending  "\" and the delimiting "'" or "\"
         ; for the final substring, this is a no-op (pos + len out of range)
         ; based on how strmid works, but there is a spurious '\' generated
         ; which is excised.
         newstr[j] = strjoin(strmid(newstr[j],pos, len) + '\' + $
                     strmid(newstr[j], pos + len,1))
         newstr[j] = strmid(newstr[j], 0, strlen(newstr[j])-1)
      endif
      if strpos(newstr[j],string(10B)) ge 0 then begin
         multiline=1
         ; quote any embedded literal newlines (string(10B)) 
         strnl = strsplit(newstr[j], string(10B),/EXTRACT,/PRESERVE_NULL)
         newstr[j] = strjoin(strnl, '\n')
      endif
   endfor

   ; add the quoted newlines and make into a scalar string.
   if strvector then newstr = strjoin(newstr, '\n')  
   if multiline and not literal then begin
      ; processing to remove leading and trailing empty lines.
      substr = strsplit(newstr, '\\n',/EXTRACT,/PRESERVE_NULL,/REGEX)
      pos1 = -1
      for j = 0, n_elements(substr) - 1 do begin
         if  substr[j] ne '' then begin
            pos2=j
            if pos1 lt 0 then pos1=j
         endif
      endfor
      if pos1 ge 0 then newstr = strjoin( substr[pos1:pos2],'\n') $
      else newstr = ''
   endif

   newstr = t + newstr + t   ; wrap in single quotes
   return,newstr
end

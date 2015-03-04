;+
; NAME:
;   dequote
; PURPOSE:   (one line only)
;   Reconvert a string processed by QUOTE, or from the result of a MySQL query.
; DESCRIPTION:
;   In its simplest form this routine will take a string output from QUOTE
;   and convert it back to its input. This means removing leading and trailing
;   single quotes, and converting "\\" and  "\'" to "\" and "'", respectively.
;   If the input string has embedded "\n" strings it will be broken into a
;   string array on the "\n" boundaries, with the '\n" themselves removed.
;   In this case, the output is a string vector- otherwise, it is a scalar.
;   Please note that because QUOTE is a many to one mapping in general,
;   the sequence    dequote(quote(x)) returns a string that may not be
;   identical to x. (Eg, if x had leading blanks.) However, it should
;   be possible to use the output of DEQUOTE with QUOTE again and obtain
;   the same results with MySQl.
;   Using the /SELECTSQL flag allows quote to be used with the output of 
;   mysql queries, particularly in processing  TEXT and BLOB fields. The
;   main difference with SELECTSQL set is that embedded "\" and "'" fields are 
;   left alone, except for \n which is again assumed to be a line boundary 
;   and used to break the string into substring elements.
; CATEGORY:
;   Database
; CALLING SEQUENCE:
;   result = dequote(string)
; INPUTS:
;   string - String to be processed
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SELECTSQL- Flag, if set some of the processing of dequote is not done,
;             specifically removal of backlash characters as escapes.
; OUTPUTS:
;   return value is a scalar string, or a string array if the input contained
;   the "\n" escape sequence. Back to back "\n" (corresponding to empty lines)
;   in the input will result in empty substrings in the array returned.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; Newlines are currently quoted with two characters: '\n' where the
; n is a literal lower case n. DEQUOTE will not do anything with embedded
; actual newline (012') characters, whether or not they are preceded by '\'.
; Dequote has no equivalent of the LITERAL keyword for quote.
; Dequote does not remove any empty lines- for example,
;                       dequote('\n\n\string\n\n')
; returns an array of 4 elements, 3 of which are empty strings.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Peter L. Collins, 2006/10/27, Lowell Observatory
;  2006/12/11, PLC, small bug fix- to return scalar if 1 element.
;-
function dequote,str,SELECTSQL=selectsql

   self='DEQUOTE: '
   if badpar(str,              7,  0, caller=self+'(str) ') then return,''
   if badpar(selectsql,[0,1,2,3], 0, caller=self+'(SELECTSQL) ', $
             default=0) then return,''

   t = "'"
   newstr = str

   ; are there leading and trailing single quotes?  If so, trim them off
   if strlen(newstr) ge 2 then begin
      if strmid(newstr,0,1) eq t and $
         strmid(newstr,strlen(newstr)-1,1) eq t then $
            newstr = strmid(newstr,1,strlen(newstr)-2)
   endif

   ; find all single quotes and \ preceded by a \ and remove the \
   if not selectsql then begin
      patt = "(\\'|\\\\)"
      ; use the regex feature of strsplit to find all pattern match
      ; boundaries
      pos = strsplit(newstr,patt,/REGEX,LENGTH=len,/PRESERVE_NULL)
      ;print, pos,len
      if n_elements(pos) gt 1 then begin
         ; join the strings back putting in only the 2nd char from the location
         ; of each match.
         newstr = strjoin(strmid(newstr,pos, len) + $
                     strmid(newstr,(pos + len +1),1))
      endif
   endif

   ; find and break on escaped newlines.
   escapednl = '\\n'
   newstr = strsplit(newstr, escapednl, /REGEX,/EXTRACT,/PRESERVE_NULL)

   ns = n_elements(newstr)
   ; remove any final null string (artifact of strsplit, never real).
   if ns gt 1 then if newstr[ns-1] eq '' then newstr = newstr[0:ns-2]
   if n_elements( newstr) eq 1 then newstr = newstr[0]
   return,newstr
end

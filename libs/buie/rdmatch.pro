;+
; NAME:
;     rdmatch
; PURPOSE: (one line)
;     Read a standard name --- non-standard name correspondence file.
; DESCRIPTION:
;
;     This program reads a file which contains a list of matches between
;     informal and imprecise object names (typically from FITS headers)
;     and formal, well-defined object names to be used by EPHEM.
;
;     The file format has one pair of strings per line.
;     The first string starts at the start of the line.  The second string
;     starts at just after the ' character.  Each string is stripped of
;     leading and trailing blanks (and tabs).
;
;     These string pairs are collected into a pair of string arrays and
;     then returned.
;
; CATEGORY:
;     File I/O
; CALLING SEQUENCE:
;     rdmatch,file,proper,informal
; INPUTS:
;     file - String containing file name to be read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;     ERROR - error flag, if true, some error occured during file I/O.
; OUTPUTS:
;     proper   - String array of proper ephemeris names.
;     informal - String array of informal object names.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     3/25/93 - Written by Marc W. Buie, Lowell Observatory.
;    11/22/96 - MWB, changed to permit spaces in the proper names.
;    98/01/28 - MWB, changed to quietly ignore blank lines in file.
;-
pro rdmatch,file,proper,informal, $
            ERROR=error

on_error,2
on_ioerror,bad

if n_params() ne 3 then begin
   print,'Usage: rdmatch,file,proper,informal'
   return
endif

if badpar(file,7,0,caller='RDMATCH: (file) ') then return

openr,unit,file,/get_lun

newline=''
n=0

while not eof(unit) do begin
   readf,unit,newline,format='(a)'
   a = strtrim(gettok(newline,"'"),2)
   b = strtrim(newline,2)
   IF a ne '' THEN BEGIN
      if n eq 0 then begin
         proper   = a
         informal = b
      endif else begin
         proper   = [proper,a]
         informal = [informal,b]
      endelse
      n = n+1
   ENDIF
endwhile

error=0

goto,done

bad:
   print,!error_state.msg
   error=1

done:
   free_lun,unit

end

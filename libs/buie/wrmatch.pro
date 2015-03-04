;+
; NAME:
;     wrmatch
; PURPOSE: (one line)
;     Write a standard name --- non-standard name correspondence file.
; DESCRIPTION:
;
;     This program writes a file which contains a list of matches between
;     informal and imprecise object names (typically from FITS headers)
;     and formal, well-defined object names to be used by EPHEM.
;
;     The file format has one pair of strings per line.
;     The first token (or word) in the line is taken to be the standard
;     object name.  Since this name is not allowed to contain spaces,
;     the first space on the line signals the end of the standard name.
;     All characters (spaces included) following this name are ignored up
;     to the first ' character in the line.  All characters (including
;     spaces) following the quote character up to the end of the line are
;     taken to be the non-standard name.
;
;     The input string pairs are printed to the requested file.
;
; CATEGORY:
;     File I/O
; CALLING SEQUENCE:
;     wrmatch,proper,informal,file
; INPUTS:
;     proper   - String array of proper ephemeris names.
;     informal - String array of informal object names.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;     file - String containing file name to write.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     3/25/93 - Written by Marc W. Buie, Lowell Observatory.
;-
pro wrmatch,proper,informal,file

;on_error,2
on_ioerror,bad

if badpar(proper,7,[0,1],caller='WRMATCH: (proper) ',npts=n1) then return
if badpar(informal,7,[0,1],caller='WRMATCH: (informal) ',npts=n2) then return

if n1 ne n2 then begin
   print,'WRMATCH: Error.  The two input string arrays must have equal length'
   return
endif

openw,unit,file,/get_lun

for i=0,n1-1 do begin
   printf,unit,proper[i],informal[i],format='(a,"	''",a)' ; careful of tab!
endfor

goto,done

bad:
   print,!error_state.msg

done:
   free_lun,unit

end

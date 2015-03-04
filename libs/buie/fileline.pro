;+
; NAME: 
;  fileline
; PURPOSE:   (one line only)
;  Returns one line from a file at a chosen location.
; DESCRIPTION:
; CATEGORY:
;  File I/O 
; CALLING SEQUENCE:
;  line=fileline(filename,linenumber)
; INPUTS:
;  filename  :String, name of file to read
;  linenumber:The line number of the file to read (0 indexed)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  HEADER    : Optional header to be skipped in the file.  Default is no
;                 header.  If supplied, this string (or string) array must
;                 match the first N lines of the file or it will be an error.
;                 The header, if provided, is not counted in the line count
;                 for the line to be returned.
; OUTPUTS:
;  return value is the specified line of the file (string).  If an error
;    occurs, the return value is an integer value of -1.  If you get a string,
;    there was no error.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/13, Written by Maureen Teyssier, REU Lowell Observatory
;  2004/07/15, MWB, incorporated into library.
;-
function fileline,filename,num,header=header

   self='FILELINE: '
   if badpar(filename,[7],0,CALLER=self+'(filename) ') then return,-1
   if badpar(num,[1,2,3],0,CALLER=self+'(num) ') then return,-1

   if badpar(header,[0,7],[0,1],CALLER=self+'(header) ', $
                                default='') then return,-1
  
   openr,lun,filename,/get_lun
   line='' 
   i=0L
   if header[0] ne '' then begin
      while not eof(lun) and i lt n_elements(header) do begin
         readf,lun,line,format='(a)'
         if header[i] ne line then begin
            print,self,'FATAL ERROR! On line ',strn(i),' of the header:'
            print,self,header
            print,' '
            print,self,'was not found to match the same line:'
            print,self,line
            print,self,'in the file:',filename
            print,' '
            return,-1
         endif
         i++
      endwhile
      if eof(lun) and i lt n_elements(header) then begin
         print,self,'File:',filename
         print,self,'ERROR! The end of the file has been reached on line',i, $
                    ' while reading the header.'
         print,self,'The header is longer than the file.'
         print,self,'Header: ',header
         print,' '
         return,-1
      endif 
   endif

   i=0L
   while i le num and not eof(lun) do begin
     readf,lun,line,format='(a)';reading a string of unspecified length  
     i++
   endwhile

   if i lt num then begin
      print,self,'ERROR!!  The file:',filename
      print,self,'has less lines than the chosen file line number:',num
      return,-1
   endif

   free_lun,lun

   return, line

end

;+
; NAME:
;  repwrite
; PURPOSE:
;  Update file by replacing or adding line of information
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  repwrite,file,tag,line
; INPUTS:
;
;  file - string of file name to update (or create)
;
;  tag  - key to scan at the start of the line, all lines found
;          matching this key will be replaced by the new line provided.
;          Any other line matching this tag will be removed from the file.
;          (scalar or vector, length must match LINE)
;
;  line - Replacement line for file.  If tag is not found, this will be
;          added to the file.  The output is left sorted by the tags.
;          If this is an empty string (''), then all lines that match the
;          tag will be removed.
;          (scalar or vector, length must match TAG)
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;
;  HEADER  - String array to put at start of file if file is created.
;              Default is no header.  If file already exists, then this is
;              ignored.
;
;  HEADLEN - Number of lines at the start of the file to pass over.
;              File must have at least this many lines to be valid.  If no
;              header provided, the default is 0.  If you give a header, then
;              the length of the string array is used as the default header
;              length.
;           
; OUTPUTS:
;  All output is to the file.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/19, Written by Marc W. Buie, Lowell Observatory
;  97/2/6, MWB, different algorithm and file is now sorted.
;  97/10/09, MWB, added HEADER and HEADLEN keywords
;  98/03/22, MWB, added NOSORT keyword
;  2000/09/20, MWB, added support for vector input on tag and line
;-
pro repwrite,file,tag,line,HEADER=header,HEADLEN=headlen,NOSORT=nosort

   if badpar(file,7,0,caller='REPWRITE (file) ') then return
   if badpar(tag, 7,[0,1],caller='REPWRITE (tag) ',npts=nt) then return
   if badpar(line,7,[0,1],caller='REPWRITE (line) ',npts=nl) then return
   if badpar(headlen,[0,2,3],0,caller='REPWRITE (HEADLEN) ',default=-1) then return
   if badpar(header,[0,7],[0,1],caller='REPWRITE (HEADER) ',default='[[NoNe]]') then return

   if nt ne nl then begin
      print,'REPWRITE: Fatal error!  Length of "tag" and "line" are not equal.'
      help,file,tag,line
      return
   endif

   if headlen eq -1 and header[0] eq '[[NoNe]]' then headlen = 0
   if headlen eq -1 and header[0] ne '[[NoNe]]' then headlen = n_elements(header)

   if exists(file) then begin

      ; First find the number of lines in the file
      openr,lun,file,/get_lun
      in_line=''
      nlines=0L
      while(not eof(lun)) do begin
         readf,lun,in_line,format='(a1)'
         nlines=nlines+1L
      endwhile
      point_lun,lun,0

      if headlen gt nlines then begin
         print,'repwrite: file shorter than header length, aborting'
         return
      endif

      if headlen gt 0 then begin
         hdr=strarr(headlen)
         for i=0L,headlen-1 do begin
            readf,lun,in_line,format='(a)'
            hdr[i] = in_line
         endfor
         nlines = nlines - headlen
      endif

      ; now load the data from the file
      taglen=strlen(tag[0])
      if nlines gt 0 then begin
         tags=strarr(nlines)
         info=strarr(nlines)
         for i=0L,nlines-1 do begin
            readf,lun,in_line,format='(a)'
            tags[i]=strmid(in_line,0,taglen)
            info[i]=in_line
         endfor
      endif
      free_lun,lun

      for i=0L,nt-1 do begin
         if nlines ne 0 then begin
            ; Identify those lines that don't match the desired tag
            z = where(tag[i] ne tags,count)

            ; Save all non-matching lines, if all non-matching there's nothing to do
            if count ne nlines and count ne 0 then begin
               info = info[z]
               tags = tags[z]
               nlines = count
            endif
         endif else begin
            count=0
         endelse

         if nlines eq 0 then begin
            info = line[i]
            nlines = 1L
         endif else if count eq 0 and line[i] ne '' then begin
            info = line[i]
            nlines = 1L
         endif else if count eq 0 and line[i] eq '' then begin
            nlines = 0L
         endif else if line[i] ne '' then begin
            info = [info,line[i]]
            tags = [tags,tag[i]]
            nlines = nlines+1L
         endif
      endfor

      if keyword_set(nosort) then begin
         idx=lindgen(nlines)
      endif else begin
         idx=sort(tags)
      endelse

      if headlen ne 0 or nlines ne 0 then begin
         ; write the new ordered set back to the file.
         openw,lun,file,/get_lun
         if headlen ne 0 then printf,lun,hdr,format='(a)'
         for i=0L,nlines-1 do begin
            printf,lun,info[idx[i]]
         endfor
         free_lun,lun
      endif

   endif else if line ne '' then begin
      openw,lun,file,/get_lun
      if header[0] ne '[[NoNe]]' then printf,lun,header,format='(a)'
      for i=0L,nl-1 do $
         printf,lun,line[i]
      free_lun,lun
   endif

end

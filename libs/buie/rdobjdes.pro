;+
; NAME:
;  rdobjdes
; PURPOSE:
;  Read in a file in the format of newobj.dat for use
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdobjdes,filename,ncodes,objcode,field,xref,xsec
; INPUTS:
;  filename - name of file to read
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ASTPATH  - path to astrometry data (default=/net/frakir/raid/buie/astrometry)
; OUTPUTS:
;  ncodes   - number of entries read
;  objcode  - object code 
;  field    - first frame observation
;  xref     - cross reference in newobj.dat file 
;  xsec     - Indicates if the local code is a secondary designation
; KEYWORD OUTPUT PARAMETERS:
;  COMMENTS - original xref string containing all comments
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  01/01/08, Written by Susan Kern
;  2001/08/15, MWB, upgraded to use LONG for all counters.
;  2003/05/09, MWB, fixed bug on reading lines with short xref entries
;  2004/02/09, MWB, changed default for ASTPATH
;-

pro rdobjdes,filename,ncodes,objcode, field,xref,xsec, $
             ASTPATH=astpath, COMMENTS=comments

   if badpar(filename,7,0,caller='rdobjdes:(filename)') then return
   if badpar(astpath,[0,7],0,caller='rdobjdes:(ASTPATH)', $
            default='/net/frakir/raid/buie/astrometry') then return

   ; Locate the id code file and load it
   ncodes=0L
   if exists(addslash(astpath)+filename) then begin
      openr,lun,addslash(astpath)+filename,/get_lun
      line=''
      while not eof(lun) do begin
         readf,lun,line,format='(a1)'
         ncodes = ncodes+1L
      endwhile
   endif


   point_lun,lun,0L

   line=''
   objcode=strarr(ncodes)
   field=strarr(ncodes)
   xref=strarr(ncodes)
   xsec=intarr(ncodes)
   tab = string( byte(9) )

   for i=0L,ncodes-1 do begin
      readf,lun,line,format='(a)'
      words=strsplit(line,tab,/EXTRACT)
      objcode[i]=words[0]
      field[i]=words[1]
      if n_elements(words) ge 3 then begin
         xref[i]=words[2]
      endif
   endfor

   objcode=strtrim(objcode,2) 
   field=strtrim(field,2) 
   xref=strtrim(xref,2) 

   comments = xref
   xref=strcompress(xref)
   length=0
   
   for i=0L,ncodes-1 do begin
      if strlen(xref[i]) ne 0 then begin
         words=strsplit(xref[i],/EXTRACT)
         xref[i]=words[0]
         length=strlen(xref[i])

         ; note, if the second is true we know it is a secondary designation
         if strmid(xref[i],0,1) eq '(' and $
            strmid(xref[i],0,1,/REVERSE_OFFSET)  eq ')' then begin
            xref[i]=strmid(xref[i],1,length-2)
         endif else if strmid(xref[i],0,1) eq '(' then begin
            xref[i]=strmid(xref[i],1)
            xsec[i]=1
         endif
      endif
   endfor

   free_lun,lun

end

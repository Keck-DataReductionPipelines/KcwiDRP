;+
; NAME:
;  shanelist
; PURPOSE:   (one line only)
;  convert KBO target list to Lick Observatory Shane 3m telescope file
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  shanelist,file
; INPUTS:
;  file - Name of KBO target list file
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/09/08 - Written by Marc W. Buie, Lowell Observatory
;-
pro shanelist,infile

   if badpar(infile,7,0,caller='shanelist (file) ') then return

   readcol,infile,id,elong,range,ra,dec,mag,dra,ddec, $
      format='a,f,f,a,a,x,x,x,f,x,f,f'
   blanks='                     '

   words=strsplit(infile,'.',/extract)

   outfile = words[0]+'.list'
   print,outfile

   openw,lun,outfile,/get_lun

   for i=0,n_elements(id)-1 do begin
      object = id[i]
      pos = strpos(object,'=')
      if pos ge 0 then object = strmid(object,0,pos)
      object=strtrim(object,2)
      ras = ra[i]
      strput,ras,' ',2
      strput,ras,' ',5
      decs = dec[i]
      strput,decs,' ',3
      strput,decs,' ',6
      print,object+blanks,ras,decs,mag[i], $
         format='(a19,1x,a,1x,a,3x,"2000",1x,f4.1)'
      printf,lun,object+blanks,ras,decs,mag[i], $
         format='(a19,1x,a,1x,a,3x,"2000",2x,f4.1,3x,"filler")'
   endfor

   free_lun,lun
end

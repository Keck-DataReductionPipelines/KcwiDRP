;+
; NAME:
;  rdnicobs
; PURPOSE:
;  Read a NICMOS observation set description file.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  rdnicobs,file,x0,y0,obsid,ref,x,y
; INPUTS:
;  file - String with the filename to be read.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  x0 - x pixel position of the object(s) in the reference image
;  y0 - y pixel position of the object(s) in the reference image
;  obsid - observation id of all the Grism images
;  ref   - observation id of the matching Grism image, this will either
;            be an actual grism image id or 'sky'
;  x     - X pixel offset of dither (this is the offset of the object in the image).
;  y     - Y pixel offset of dither (this is the offset of the object in the image).
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1999 July 13
;  2002/09/03, MWB, changed Str_sep call to strsplit
;-
pro rdnicobs,file,x0,y0,obsid,ref,x,y

   if badpar(file,7,0,CALLER='RDNICOBS: (file) ') then return

   if not exists(file) then begin
      print,'RDNICOBS: Error!  File ',file,' not found.'
      return
   endif

   openr,lun,file,/get_lun
   line=''
   readf,lun,line,format='(a)'
   x0=float(strsplit(strtrim(strcompress(line),2),' ',/extract))
   readf,lun,line
   y0=float(strsplit(strtrim(strcompress(line),2),' ',/extract))
   n=0
   while not eof(lun) do begin
      readf,lun,line,format='(a)'
      words=strsplit(strcompress(line),' ',/extract)
      if n eq 0 then begin
         obsid = words[0]
         ref   = words[1]
         x     = float(words[2])
         y     = float(words[3])
      endif else begin
         obsid = [obsid,words[0]       ]
         ref   = [ref  ,words[1]       ]
         x     = [x    ,float(words[2])]
         y     = [y    ,float(words[3])]
      endelse
      n=n+1
   endwhile

   free_lun,lun
   
end

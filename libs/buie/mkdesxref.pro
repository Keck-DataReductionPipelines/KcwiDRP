;+
; NAME:
;   mkdesxref
; PURPOSE:
;   Create data file to post into des.xref MySQL database from KBO search data
; DESCRIPTION:
; CATEGORY:
;   Database
; CALLING SEQUENCE:
;  mkdesxref,root
; INPUTS:
;  root - String, 6 digit date code for observation.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWWRITE - Flag to force overwrite of pre-existing output file.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/05/09
;-
pro mkdesxref,root,OVERWRITE=overwrite

   self='mkdesxref:'
   if badpar(overwrite,[0,1,2,3],0,caller=self+' (OVERWRITE) ',default=0) then return

   kred = '/net/frakir/raid/reduced/'
   info = kred+root+'/infox2.log'
   infoh = kred+root+'/Hide/infox2.log'
   outfile = root+'.xrft'

   ; Before starting, check to see if the output file already exists, if
   ;  it does, do not continue unless the override is set.
   if exists(outfile) and not overwrite then begin
      print,'Output file, ',outfile,' already exists.  Cannot continue.'
      print,'Use /OVERWRITE flag if you really want to proceed.'
      return
   endif

   ; First, look for the infox2.log file, don't do anything if this file
   ;  does not exist.
   if not exists(info) and not exists(infoh) then begin
      print,info,' not found.'
      print,'Unable to continue.'
      return
   endif

   ; Next, try to locate the xref file from this night
   xreffile = kred+root+'/lplast.xrft'
   xreffound = exists(xreffile)
   if not xreffound then begin
      print,'No lplast.xrft file found for ',root
      return
   endif

   ; get the length of the file
   nlines = file_lines(xreffile,/noexpand_path)

   if nlines eq 0 then begin
      print,'lplast.xrft file for ',root,' is empty.'
      return
   endif

   ; Load the information file
   lookerid = strarr(nlines)
   objectid = strarr(nlines)
   a=''
   b=''
   openr,lun,xreffile,/get_lun
   for i=0,nlines-1 do begin
      readf,lun,a,b,format='(a8,1x,a)'
      lookerid[i] = a
      b=strtrim(b,2)
      pos=strpos(b,' ')
      if pos ge 0 then begin
         objectid[i] = strmid(b,0,pos)
      endif else begin
         objectid[i] = b
      endelse
   endfor
   free_lun,lun

   ; Crack the root name and build the date string for this dataset
   if strlen(root) eq 6 then begin
      yy = fix(strmid(root,0,2))
      if yy lt 90 then yy=yy+2000 else yy=yy+1900
      yy = strn(yy)
      mm = strmid(root,2,2)
      dd = strmid(root,4,2)
      datestr = yy+'-'+mm+'-'+dd
   endif else begin
      print,'Root name format not recognized.  Unable to continue.'
      return
   endelse

   openw,lout,outfile,/get_lun
   for i=0,nlines-1 do begin
      str=string(lookerid[i],datestr,objectid[i], $
         format='(2(a,1x),a)')
      str=strcompress(str)
      printf,lout,str
   endfor
   free_lun,lout

   print,datestr,nlines,' cross reference entries, ',outfile

end

;+
; NAME:
;  rdplast
; PURPOSE:   (one line only)
;  Read ``plast'' output files (object lists for astrometry)
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdplast,plastfile,name,mag,num
; INPUTS:
;  plastfile - Name of file to read
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  name   - String array of names of objects
;  code   - String array of object codes (with c and e prefix)
;  mag    - Array of magnitudes for objects
;  num    - Number of objects read from file, if this is 0 then name and mag
;              will may be undefined.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/08/16
;  2002/10/23, MWB, fixed a LUN leak for empty plast file reading.
;  2005/03/11, MWB, fixed a problem with comet names in a pla file.
;  2007/06/19, MWB, added special case for P objects (must be in file with
;                    lower case p).  This triggers JPL ephemeris object.
;-
pro rdplast,plastfile,name,code,mag,num

   num = 0

   if badpar(plastfile,7,0,caller='RDPLAST (plastfile) ') then return

   if not exists(plastfile) then begin
      print,'RDPLAST: Error!  ',plastfile,' not found.'
      return
   endif

   ; Pre-declare strings for later use.
   version=''
   line=''
   astname0=''

   ; load the file, watch for different versions
   openr,lun,plastfile,/get_lun

   ; Count the number of lines in file.
   nlines=0L
   WHILE not eof(lun) DO BEGIN
      readf,lun,line,format='(a1)'
      nlines=nlines+1
   ENDWHILE
   point_lun,lun,0

   ; Read first line, this decides the file format
   readf,lun,version,format='(a)'
   if version eq 'PLAST v1.0' then begin
      nlines=nlines-1
      num=nlines
   endif else begin
      version = 'PLAST v0.0'
      num = (nlines-4)/2
   endelse

   ; trap trivial case
   if num eq 0 then begin
      free_lun,lun
      return
   endif

   ; Common init
   name  = strarr(num)
   code  = strarr(num)
   mag   = fltarr(num)

   if version eq 'PLAST v0.0' then begin

      ; Original plast format, skip next three lines
      for i=1,3 do readf,lun,line,format='(a1)'

      ; Now grab the rest of the data.
      for i=0,num-1 do begin
         readf,lun,astname0,mag0,format='(a23,f5.2)'
         readf,lun,line,format='(a1)'
         astname0=strtrim(astname0,2)
         name[i] = astname0
         mag[i]  = mag0
      endfor

   endif else if version eq 'PLAST v1.0' then begin
      for i=0,num-1 do begin
         readf,lun,astname0,mag0,format='(a23,f5.2)'
         astname0=strtrim(astname0,2)
         name[i] = astname0
         mag[i]  = mag0
      endfor

   endif else begin
      print,'FATAL ERROR!, this should never happen'
      stop
   endelse

   free_lun,lun

   ; Process the names to get back object codes
   for i=0,num-1 do begin
      tmpstr=strsplit(strtrim(name[i],2),' ',/extract)
      rawname=tmpstr[0]
      if strpos(rawname,'P/') ne -1 then begin
         code[i]='c'+strmid(rawname,2,99)
      endif else if strpos(rawname,'C/') ne -1 then begin
         code[i]='c'+strmid(rawname,2,99)
      endif else if strpos(rawname,'p') eq 0 then begin
         code[i]=rawname
      endif else begin
         code[i]='e'+rawname
      endelse
   endfor

end

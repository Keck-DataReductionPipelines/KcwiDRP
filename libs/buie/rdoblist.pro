;+
; NAME:
;  rdoblist
; PURPOSE:
;  Read a object list from a file.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  rdobjlist,file,nobj,filelist,dt,offset,pos,flags,idstr,nfiles
; INPUTS:
;  file - String of file name to be read.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  nobj     - number of objects in list.
;  filelist - string array of file names for this object list.
;  dt       - Array of time offsets with nfiles-1 elements.  The values are the
;                time offset from the first frame in hours.
;  offset   - 2*(nfiles-1) element vector [x,y offset (B-A), x,y offset (C-A), ...]
;  pos      - [2*nfiles,nobj] element vector, each row is set of positions [x1,y1,x2,y2,x3,y3]
;  flags    - nobj element vector of flags either ?, y, or n.
;  idstr    - String array of description info for each measurement.
;  nfiles   - Number of files in linked set of images for this field.
;
; KEYWORD OUTPUT PARAMETERS:
;  VERSION  - The version id string for the file that was read.
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
;  1998/10/6, Written by Marc W. Buie, Lowell Observatory
;  1998/11/2, MWB, generalized to handle other than frame triplets
;  1999/2/19, MWB, added dt and version arguments.
;  2000/11/9, MWB, removed Str_sep call.
;
;-
pro rdoblist,file,nobj,filelist,dt,offset,pos,flags,idstr,nfiles,VERSION=version

   if not exists(file) then begin
      print,'RDOBJLIST: File ',file,' not found.'
      nobj=0
      return
   endif

   ; Read the object list
   openr,lun,file,/get_lun

   ; Get the file version
   version=''
   readf,lun,version,format='(a)'
   if version ne 'OBJLIST v1.0' then version='OBJLIST v0'

   ; Count number of objects in the list.
   if version eq 'OBJLIST v1.0' then nobj=-3 else nobj=-1
   line=''
   WHILE not eof(lun) DO BEGIN
      readf,lun,line,format='(a1)'
      nobj=nobj+1
   ENDWHILE
   IF nobj lt -1 THEN BEGIN
      print,'RDOBJLIST: Invalid list file ',file
      free_lun,lun
      return
   ENDIF
   nobj = nobj > 0

   ; Rewind the file.
   point_lun,lun,0L

   ; Skip the version line
   if version eq 'OBJLIST v1.0' then readf,lun,line,format='(a1)'

   ; Get the triplet file names.
   readf,lun,line,format='(a)'
   filelist=strsplit(line,/extract)
   nfiles = n_elements(filelist)

   if eof(lun) or nfiles eq 1 then begin
      free_lun,lun
      return
   endif

   if version eq 'OBJLIST v1.0' then begin
      readf,lun,line,format='(a)'
      dt=double(strsplit(line,/extract))
   endif else begin
      dt=dblarr(nfiles-1)
   endelse

   ; Get the x,y offsets for frames
   readf,lun,line,format='(a)'
   offset=float(strsplit(line,/extract))

   if nobj eq 0 then begin
      free_lun,lun
      return
   endif

   pos=fltarr(2*nfiles,nobj)
   flags=replicate('?',nobj)
   idstr=replicate('',nobj)
   FOR i=0,nobj-1 DO BEGIN
      readf,lun,line,format='(a)'
      words = strsplit(line,/extract)
      pos[*,i]=float(words[0:2*nfiles-1])
      IF n_elements(words) gt 2*nfiles   THEN flags[i] = words[2*nfiles]
      IF n_elements(words) gt 2*nfiles+1 THEN idstr[i] = words[2*nfiles+1]
   ENDFOR

   free_lun,lun

end


;+
; NAME:
;  matchup
; PURPOSE:
;  Read a batch of FITS files and create a list of common object exposures.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  matchup,root
; INPUTS:
;  root - String, root of file name for the FITS files.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  OUTFILE - Name of output match file, default = root+'.match'
;  PATH - Path of where to find the source images, default=current directory.
;  PATTERN - File searching pattern, default='.???'
;  OBJECTKEY - Name of FITS keyword to use for the object name (default=OBJECT)
; OUTPUTS:
;
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
;  98/06/23, Written by Marc W. Buie, Lowell Observatory
;  99/04/25 - MWB, Removed spawn call to make it work under Windows
;  99/06/10, MWB, added pattern keyword
;  2000/02/05, MWB, Added support for '.fits' tag on file names.
;  2001/05/16, MWB, added OBJECTKEY keyword
;  2004/9/21, MWB, removed obsolete call to Findfile
;
;-
pro matchup,root,OUTFILE=outfile,PATH=path,PATTERN=pattern,OBJECTKEY=objectkey

   if n_params() eq 0 then begin
      doc_library,'matchup'
      return
   endif

   if badpar(root,7,0,CALLER='MATCHUP: (root) ') then return
   if badpar(outfile,[0,7],0,CALLER='MATCHUP: (OUTFILE) ', $
                default=root+'.match') then return
   if badpar(path,[0,7],0,CALLER='MATCHUP: (PATH) ',default='./') then return
   if badpar(pattern,[0,7],0,CALLER='MATCHUP: (PATTERN) ',default='*.???') then return
   if badpar(objectkey,[0,7],0,CALLER='MATCHUP: (OBJECTKEY) ',default='OBJECT') then return

   path=addslash(path)

   if exists(outfile) then begin
      print,'Warning: matchup file ',outfile,' already exists.'
      ans=''
      read,ans,prompt='Do you wish to overwrite? '
      if strlowcase(strmid(ans,0,1)) ne 'y' then return
   endif

   blanks='                '

   ; Get the list of files.
   file=file_search(path+pattern,count=nfiles)
   if nfiles eq 0 then begin
      print,'No files found with pattern [',path+pattern,']'
      return
   endif
   nfiles=n_elements(file)
   object=strarr(nfiles)
   for i=0,nfiles-1 do begin
      file[i] = strmid(file[i],strlen(path),999)
      if strmid(file[i],strlen(file[i])-5,5) eq '.fits' then begin
         file[i] = strmid(file[i],0,strlen(file[i])-5)
         ft='.fits'
      endif else ft=''
      hdr=headfits(path+file[i]+ft)
      object[i]=strupcase(strcompress(sxpar(hdr,objectkey),/remove_all))
   endfor

   ; Collate the list by unique object name.
   objlist=object[uniq(object,sort(object))]
   nobj=n_elements(objlist)
   ffn=strarr(nobj)
   for i=0,nobj-1 do begin
      z=where(objlist[i] eq object,count)
      ffn[i]=file[z[0]]
   endfor
   idx=sort(ffn)
   objlist=objlist[idx]

   ; Save results to output file.
   openw,lun,outfile,/get_lun
   for i=0,nobj-1 do begin
      z=where(objlist[i] eq object,count)
      printf,lun,objlist[i]+blanks,file[z],format='(a16,10(1x,a))'
      if count ne 3 then print,'Warning ',objlist[i],' has ',count,' entries.'
   endfor
   free_lun,lun

end

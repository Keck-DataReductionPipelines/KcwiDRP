;+
; NAME:
;  wroblist
; PURPOSE:
;  Write an object list to a file.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  wroblist,file,nobj,filelist,dt,offset,pos,flags,idstr,nf
; INPUTS:
;  file - String of file name to be written to.
;  nobj     - number of objects in list.
;  filelist - string array of file names for this object list.
;  dt       - nfiles-1 element vector with the time, in hours of each file
;               relative to the time of the first frame.  Since the first frame
;               is identically 0, this value isn't saved.
;  offset   - 2*(nfiles-1) element vector [x,y offset (B-A),
;                x,y offset (C-A), ...]
;  pos      - [2*nfiles,nobj] element vector, each row is set of
;                positions [x1,y1,x2,y2,x3,y3]
;  flags    - nobj element vector of flags either ?, y, or n.
;  idstr    - String that identifies the measurement in some way.
;  nf       - number of files in set.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
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
;  1998/11/3, Written by Marc W. Buie, Lowell Observatory
;  1999/1/29, MWB, added idstr
;  1999/2/29, MWB, added dt and version tag in file
;
;-
pro wroblist,file,nobj,filelist,dt,offset,pos,flags,idstr,nf

   openw,lun,file,/get_lun

   printf,lun,'OBJLIST v1.0'
   printf,lun,filelist,format='('+ $
      strcompress(nf,/remove_all)+'(a,1x))'

   if nf gt 1 then begin

      printf,lun,dt,format='(' + $
         strcompress(nf-1,/remove_all)+'(1x,f10.5))'

      printf,lun,offset,format='(' + $
         strcompress((nf-1)*2,/remove_all)+'(1x,f8.2))'

      fmt='(' + strcompress(nf*2,/remove_all) + '(1x,f8.2),1x,a1,1x,a)'

      for i=0,nobj-1 do begin
         printf,lun,pos[*,i],flags[i],idstr[i],format=fmt
      endfor
   endif

   free_lun,lun

end

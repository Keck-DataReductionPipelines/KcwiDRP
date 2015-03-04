;+
; NAME:
;  savestar
; PURPOSE: (one line)
;  Save the master star catalog file
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  loadstar
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     FILE    - Name of star catalog file.  Default is:
;                 '/net/frakir/raid/buie/starcat/starcat.dat'
;
; OUTPUTS:
;   file is overwritten with contents of common block
; COMMON BLOCKS:
;     MWB_STARCAT
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/21, Written by Marc W. Buie, Lowell Observatory
;  2004/02/09, MWB, changed path to starcat.dat file
;-
pro savestar,FILE=file

   common mwb_starcat, info

   if badpar(file,[0,7],0,CALLER='SAVESTAR: (file) ', $
         default='/net/frakir/raid/buie/starcat/starcat.dat') then return

   fmt='(a16,1x,a,1x,a,2(1x,f6.3),1x,a)'
   blanks='                   '

   sz_info=size(info)

   ; If no structure, can't save anything.
   if sz_info[n_elements(sz_info)-2] ne 8 then begin
      message,'Nothing is loaded, unable to save.',/info
      return
   endif

   ; Output the catalog
   openw,lun,file,/get_lun
   for i=0,info.nobj-1 do begin
      rastr,info.ra[i],1,str1
      decstr,info.dec[i],0,str2
      printf,lun,info.name[i]+blanks,str1,str2,info.rap[i]*3600.0*!radeg, $
            info.decp[i]*3600.0*!radeg,info.alias[i],format=fmt
   endfor
   free_lun,lun

   spawn,'ls -l '+file,result
   info.catdate = result[0]

end


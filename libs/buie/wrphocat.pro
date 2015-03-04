;+
; NAME:
;  wrphocat
;
; PURPOSE:
;  Write a photometry standard catalog file.
;
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
;
; CALLING SEQUENCE:
;  wrphocat,file,name,mags,codes,filname
;
; INPUTS:
;  file   - Name of catalog file to write.
;  name   - Name of the stars.
;  mags   - Stellar magnitudes  (nfil x npts)
;  codes  - Quality codes:      (nfil x npts)
;            0 - not enough measurements for use as a quality standard.
;            1 - good to use as standard
;            2 - Known or suspected variable, don't ever use.
;  filname- Filter names (string array)
;
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
;  97/03/04, Written by Marc W. Buie, Lowell Observatory
;
;-
pro wrphocat,file,name,mags,codes,filnam

   if badpar(file,7,0,CALLER='WRPHOCAT: (file) ') then return
   if badpar(name,7,1,CALLER='WRPHOCAT: (name) ') then return
   if badpar(mags,[4,5],[1,2],CALLER='WRPHOCAT: (mags) ') then return
   if badpar(codes,[1,2,3],[1,2],CALLER='WRPHOCAT: (codes) ') then return
   if badpar(filnam,7,1,CALLER='WRPHOCAT: (filnam) ') then return

   nfil = n_elements(filnam)
   blanks = '                '

   openw,lun,file,/get_lun
   printf,lun,'PHOT_CAT v1.0'
   printf,lun,strcompress(string(nfil),/remove_all)
   printf,lun,filnam

   fmt='(a16,'+string(nfil)+'(1x,f7.4),1x,'+string(nfil)+'(i1))'
   fmt=strcompress(fmt,/remove_all)
   FOR i=0,n_elements(name)-1 DO $
      printf,lun,name[i]+blanks,mags[*,i],codes[*,i],format=fmt

   free_lun,lun
   
end


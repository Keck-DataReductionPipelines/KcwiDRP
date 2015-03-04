;+
; NAME:
;  rdphocat
; PURPOSE:
;  Read a photometry standard catalog from a file.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdphocat,file,name,mags,codes,filname,nfil
; INPUTS:
;  file   - Name of catalog file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  name   - Name of the stars.
;  mags   - Stellar magnitudes  (nfil x npts)
;  codes  - Quality codes:      (nfil x npts)
;            0 - not enough measurements for use as a quality standard.
;            1 - good to use as standard
;            2 - Known or suspected variable, don't ever use.
;  filname- Filter names (string array)
;  nfil   - Number of filters
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/22, Written by Marc W. Buie, Lowell Observatory
;  2002/09/03, MWB, changed Str_sep call to strsplit
;-
pro rdphocat,file,name,mags,codes,filname,nfil

   if n_params() eq 0 then begin
      ;Show the calling sequence.
      print, 'rdphocat,file,name,mags,codes,filname,nfil'
      return
   endif

   on_ioerror,bad

   if badpar(file,7,0,CALLER='RDPHOCAT: (file) ') then return

   if not exists( file ) then begin
      message,'File '+file+' does not exist, cannot continue.'
      return
   endif

   version=''

   ; count the number of lines in the file.
   openr,lun,file,/get_lun
   nlines=0
   while not eof(lun) do begin
      readf,lun,version,format='(a)'
      nlines=nlines+1
   endwhile
   nrec=nlines-3
   point_lun,lun,0

   ; check for proper file version
   readf,lun,version,format='(a)'
   if version ne 'PHOT_CAT v1.0' then begin
      message,'Version of file '+file+' is incorrect.',/info
      return
   endif

   ; Get the number of filters in the file.
   nfil=0
   readf,lun,nfil,format='(i)'

   ; Get the filter names
   readf,lun,version,format='(a)'
   filname = strsplit(strtrim(strcompress(version),2),' ',/extract)
   if n_elements(filname) ne nfil then begin
      message,'Filter name list length is wrong length',/info
      return
   endif

   ; Format definition.
   fmt='(a16,'+string(nfil)+'(1x,f7.4),1x,'+string(nfil)+'(i1))'
   fmt=strcompress(fmt,/remove_all)

   in_name  = ''
   in_mags  = fltarr(nfil)
   in_codes = intarr(nfil)
   name     = strarr(nrec)
   mags     = fltarr(nfil,nrec)
   codes    = intarr(nfil,nrec)

   for i=0,nrec-1 do begin
      readf, lun, in_name, in_mags, in_codes, format=fmt

      name[i] = strtrim(in_name,2)
      mags[*,i]  = in_mags
      codes[*,i] = in_codes
   endfor

   free_lun, lun

   return

   bad:

   free_lun, lun
   message,'Error reading file.'

end

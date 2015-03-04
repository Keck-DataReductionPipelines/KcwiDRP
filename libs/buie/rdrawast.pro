;+
; NAME:
;  rdrawast
; PURPOSE:
;  Read raw astrometry data file.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdrawast,file,fn,jd,ra,dec,mag,nobs
; INPUTS:
;  file - Astrometry data file to be read.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  fn  - Original filename for measurement
;  jd  - Julian date of measurement
;  ra  - Right Ascension (radians)
;  dec - Declination (radians)
;  mag - measured magnitude
;  nobs - Number of measurements.  (zero if file not found or file empty).
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
;  98/01/13, Written by Marc W. Buie, Lowell Observatory
;  2002/08/10, MWB, added some error trapping on bad input files.
;  2002/09/03, MWB, changed Str_sep call to strsplit
;-
pro rdrawast,file,fn,jd,ra,dec,mag,nobs

   nobs=0

   if badpar(file,7,0,caller='rdrawast (file) ') then return

   if not exists(file) then begin
      print,'File: ',file,' does not exists.  Unable to continue.'
      return
   endif

   openr,lun,file,/get_lun
   nobs=0
   line=''
   while not eof(lun) do begin
      readf,lun,line,format='(a)'
      nobs=nobs+1
   endwhile
   point_lun,lun,0

   ; catch the trivial case of an empty file.
   if nobs eq 0 then begin
      free_lun,lun
      return
   endif

   fn=strarr(nobs)
   jd=dblarr(nobs)
   ra=dblarr(nobs)
   dec=dblarr(nobs)
   mag=fltarr(nobs)
   good=replicate(1B,nobs)

   for i=0,nobs-1 do begin
      readf,lun,line,format='(a)'
      words=strsplit(line,' ',/extract)
      if n_elements(words) eq 5 then begin
         fn[i]=words[0]
         jd[i]=double(words[1])
         ra[i]=raparse(words[2])
         dec[i]=decparse(words[3])
         mag[i]=float(words[4])
      endif else begin
         print,'Warning!  in file ',file,' an invalid line was seen'
         print,'[',line,']'
         print,'This line will be skipped.'
         good[i]=0B
      endelse
   endfor

   ; weed out bad lines that were skipped (if any)
   z=where(good eq 1B, count)
   if count ne 0 and nobs ne count then begin
      fn=fn[z]
      jd=jd[z]
      ra=ra[z]
      dec=dec[z]
      mag=mag[z]
   endif
   nobs=count

   free_lun,lun
   
end


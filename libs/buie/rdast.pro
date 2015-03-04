;+
; NAME:
;  rdast
; PURPOSE:
;  Read final astrometry data file.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdast,file,fn,jd,ra,dec,mag,obs,id
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
;  obs - Observatory code (string)
;  id  - Name of object
;  nobs- Total number of observations (0 if file is empty or missing).
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
;  98/03/26, Written by Marc W. Buie, Lowell Observatory
;  2002/09/09, MWB, added support for string obscode values
;-
pro rdast,file,fn,jd,ra,dec,mag,obs,id,nobs

   if not exists(file) then begin
      print,'File: ',file,' does not exists.  Unable to continue.'
      nobs=0
      return
   endif

   openr,lun,file,/get_lun
   nobs=0
   line=''
   WHILE not eof(lun) DO BEGIN
      readf,lun,line,format='(a)'
      nobs=nobs+1
   ENDWHILE
   point_lun,lun,0

   fn=strarr(nobs)
   jd=dblarr(nobs)
   ra=dblarr(nobs)
   dec=dblarr(nobs)
   mag=fltarr(nobs)
   obs=strarr(nobs)
   id=strarr(nobs)

   FOR i=0,nobs-1 DO BEGIN
      readf,lun,line,format='(a)'
      words=strsplit(line,' ',/extract)

      fn[i]=words[0]
      jd[i]=double(words[1])
      ra[i]=raparse(words[2])
      dec[i]=decparse(words[3])
      mag[i]=float(words[4])
      obs[i]=words[5]
      id[i]=words[6]
   ENDFOR

   free_lun,lun
   
end

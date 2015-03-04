;+
; NAME:
;  wrbophot
; PURPOSE: (one line)
;  Write a simple photometry data file.
; DESCRIPTION:
;
;   File format: (one blank separates each field)
;
;      JD-2450000 of observation (UT) F10.5   Julian date of the observation.
;
;      Observed Magnitude . . . . . . F7.4    Observed magnitude
;
;      Uncertainty (sigma)  . . . . . F6.4    Error bar for observation.
;
;      Source . . . . . . . . . . . . A45     Free format field to reference
;                                             the observer and the telescope
;                                             used for the observation.
;      This last field is left blank by this program.
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  wrbophot,jd,mag,err,file
; INPUTS:
;  jd     - Julian date of observation (double precision).
;  mag    - Observed magnitude.
;  err    - Uncertainty on the observed magnitude.
;  file   - String with name of file to save photometry to.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  APPEND - if set appends data to the named file.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, 1996 Aug 24
;-
pro wrbophot,jd,mag,err,file,APPEND=append

if badpar(jd,5,[0,1],caller='WRBOPHOT (jd) ',npts=njd) then return
if badpar(mag,[4,5],[0,1],caller='WRBOPHOT (mag) ',npts=nmag) then return
if badpar(err,[4,5],[0,1],caller='WRBOPHOT (err) ',npts=nerr) then return
if badpar(file,7,0,caller='WRBOPHOT (file) ') then return

if max([njd,nmag,nerr]) ne min([njd,nmag,nerr]) then begin
   print,'WRBOPHOT: All inputs must have the same length.  Aborting.'
   return
endif

if keyword_set(append) then $
   openw, lun, file, append=append, /get_lun $
else $
   openw, lun, file, /get_lun

for i=0,njd-1 do $
   printf,lun,jd[i]-2450000.0d0,mag[i],err[i], $
      format='(f10.5,1x,f7.4,1x,f6.4)'

free_lun,lun

end

;+
; NAME:
;  rdphot3
; PURPOSE: (one line)
;  Reads photometry from a reduced photometry file (see WRPHOT)
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdphot3,filename,jd,fil,mag,err,bad
; INPUTS:
;  filename - string containing the photometry log file to read from.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;  jd       - Julian date at mid-time of observation
;  fil      - Filter code for observation
;  mag      - Magnitude
;  err      - Uncertainty on the magnitude
;  bad      - Flag 0=good, 1=bad
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  11/3/96 - Written by Marc W. Buie
;-
pro rdphot3,filename,jd,fil,mag,err,bad

if not exists(filename) then begin
   print,'RDPHOT3: File ',filename,' not found.  Aborting.'
   return
endif

openr,unit,filename,/get_lun

;Read through and count the number of lines.
nobs=0
while(not eof(unit)) do begin
   readf,unit,line,format='(a1)'
   nobs=nobs+1
endwhile
nlines=nobs

;Rewind file.
point_lun,unit,0

;Create the output data vectors
jd       = dblarr(nobs)
fil      = strarr(nobs)
mag      = fltarr(nobs)
err      = fltarr(nobs)
bad      = intarr(nobs)

line=''

;form='(i1,1x,f13.5,1x,a2,1x,f7.4,1x,f6.4)'
;       0     2     16    19      27

for i=0,nobs-1 do begin
   readf,unit,line,format='(a)'
   jd[i]  = double(strmid(line,2,13))
   fil[i] = strmid(line,16,2)
   smag=strmid(line,19,7)
   if smag eq '*******' then begin
      mag[i] = 99.9999
      bad[i] = 1
   endif else begin
      mag[i] = float(strmid(line,19,7))
      bad[i] = fix(strmid(line,0,1))
   endelse
   err[i] = float(strmid(line,27,6))
endfor

free_lun,unit

end

;+
; NAME: 
;  rdctioph
; PURPOSE: 
;  Reads raw card image CTIO photometry data files.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This assumes that the data is single channel.
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/04/14 - Written by Marc W. Buie, Lowell Observatory.
;-
pro rdctioph,logname,jd,obsnum,id,obflag,filter,inttime,counts

on_ioerror,bad

if not exists(logname) then begin
   print,'RDCTIOPH: File ',logname,' not found.  Aborting.'
   return
endif

openr,unit,logname,/get_lun

line=''

;Read through and count the number of lines.
nobs=0
while(not eof(unit)) do begin
   readf,unit,line,format='(a1)'
   if strmid(line,0,1) eq '0' then nobs=nobs+1
endwhile

;Rewind file.
point_lun,unit,0

;Create the output data vectors
yr       = intarr(nobs)
mon      = intarr(nobs)
day      = intarr(nobs)
uth      = intarr(nobs)
utm      = intarr(nobs)
uts      = intarr(nobs)
obsnum   = intarr(nobs)
obflag   = bytarr(nobs)
filter   = strarr(nobs)
inttime  = intarr(nobs)
counts   = lonarr(nobs)
id       = strarr(nobs)

i=0
while(not eof(unit)) do begin

   readf,unit,line,format='(a80)'
   if strmid(line,0,1) ne '0' then goto,skipit

;  code0      =  fix(strmid(line, 0,1))   ; Code, must be zero to mean data.
   obsnum[i]  =  fix(strmid(line, 1,4))   ; Record number
   id[i]      =      strmid(line, 5,8)    ; Identification
;  stdflag    =  fix(strmid(line,13,1))   ; Standard flag
   obflag[i]  =  fix(strmid(line,14,1))   ; Object flag
;  diaph0     =  fix(strmid(line,15,1))   ; Diaphragm number
   filter[i]  =      strmid(line,16,1)    ; Channel 1 filter number
;  ch2fil     =      strmid(line,17,1)    ; Channel 2 filter number
;  ch3fil     =      strmid(line,18,1)    ; Channel 3 filter number
   uth[i]     =  fix(strmid(line,19,2))   ; Universal time hours
   utm[i]     =  fix(strmid(line,21,2))   ; Universal time minutes
   uts[i]     =  fix(strmid(line,23,2))   ; Universal time seconds
;  hah        =  fix(strmid(line,25,3))   ; Hour angle hours
;  ham        =  fix(strmid(line,28,2))   ; Hour angle minutes
;  has        =  fix(strmid(line,30,2))   ; Hour angle seconds
;  decd       =  fix(strmid(line,32,3))   ; Declination degrees
;  decm       =  fix(strmid(line,35,2))   ; Declination minutes
;  decs       =  fix(strmid(line,37,2))   ; Declination seconds
;  reject     =  fix(strmid(line,39,1))   ; Reject flag
;  nchan      =  fix(strmid(line,40,1))   ; Number of channels
   inttime[i] =  fix(strmid(line,41,4))   ; Integration time
   counts[i]  = long(strmid(line,45,7))   ; Counts channel 1
;  ch2cts     = long(strmid(line,52,7))   ; Counts channel 2
;  ch3cts     = long(strmid(line,59,7))   ; Counts channel 3
   mon[i]     =  fix(strmid(line,66,2))   ; Month
   day[i]     =  fix(strmid(line,68,2))   ; Day
   yr[i]      =  fix(strmid(line,70,2))   ; Year

   i=i+1

skipit:

endwhile

yr = yr + 1900
uttim = double(uth) + double(utm)/60.0d0 + double(uts)/3600.0d0
jdcnv,yr,mon,day,uttim,jd

bad:

free_lun,unit

end

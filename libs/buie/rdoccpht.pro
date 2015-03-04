;+
; NAME: 
;  rdoccpht
; PURPOSE: 
;  Read a Occultation Photometer data file.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdoccpht,file,time,counts,dt,comment
; INPUTS:
;  file - String containing filename to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  time    - vector of UT time (decimal hours)
;  counts  - vector of counts for each time
;  dt      - Time spacing between points (seconds)
;  comment - string that identifies data (read from header)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  Data is stored in 512 byte block "records".  The first record is a header
;  and contains the following:
;      0 -  79  Comment                 (ASCII string)
;     80 -  81  clock ticks/second      (integer)
;     82 -  83  clock ticks/integration (integer)
;     84 -  87  tick # of first point   (long)
;     88 -  91  tick # of last point    (long)
;     92 -  95  total # of clock ticks  (long)
;     96 -  99  total # of data points  (long)
;    100 - 101  UT Hours of first tick  (integer)
;    102 - 103  UT minutes              (integer)
;    104 - 107  UT seconds              (real)
;    108 - 511  Null padding 
; MODIFICATION HISTORY:
;  95/1/25 - Written by Marc W. Buie, Lowell Observatory
;-
pro rdoccpht,file,time,counts,dt,comment

if badpar(file,7,0,CALLER='RDOCCPHT: (file) ') then return

on_ioerror,bad

if not exists(file) then $
   message,'File '+file+' does not exist.'

openr, lun, file, /get_lun

; Get the data label comment from the header.
comment=string(0,format='(i80)')
readu,lun,comment
comment=strtrim(comment)

;Get the rate information
tps=0
tpi=0  ; make integer (2 byte) values to be read into
readu,lun,tps,tpi

; Get time tick information
tick_first = 0L
tick_last  = 0L
readu,lun,tick_first,tick_last

; Get size of data file
total_ticks  = 0L
total_points = 0L
readu,lun,total_ticks,total_points

; Get starting UT time
h = 0
m = 0
s = 0.0
readu,lun,h,m,s
t = (float(h) + float(m)/60.0 + float(s)/3600.0 ) / 12.0 * !pi
rastr,t,3,tstr

; Skip the rest of the null padding in the header
counts = bytarr(404,/nozero)
readu,lun,counts

; Read the data
counts = intarr(total_points,/nozero)
readu,lun,counts

; Compute derived values
dt = float(tpi)/float(tps)   ; seconds per integration
time = findgen(total_points) * dt + float(tick_first)/float(tps) + dt/2.0
time = time / 3600.0 + t / !pi * 12.0

print,comment
print,tps,' clock ticks per second'
print,tpi,' clock ticks per integration'
print,tick_first,' tick number of first data point'
print,tick_last,' tick number of last data point'
print,total_ticks,' total number of ticks'
print,total_points,' total number of points'
print,'UT time of first tick   ',tstr

free_lun, lun
return

bad:

free_lun, lun
message,'Error reading file.'

end

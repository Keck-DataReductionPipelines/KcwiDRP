;+
; NAME:
;  rdtbl
; PURPOSE:
;  Read the data from the table file from one night of OSIRIS observations
; DESCRIPTION:
;  The table file is read one line at a time and then disected into the various
;  pieces of information in the form of arrays.  These arrays are then saved in
;  a structure.
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdtbl,tablname,tbl
; INPUTS:
;  tablname - The name of the table file to be read
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  tbl - Structure containing the information from the table file
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  98/06/10 - Written by Chris Dalla Piazza, Lycoming College; extracted from
;             xdspec
;  2002/09/03, MWB, changed Str_sep call to strsplit
;-

pro rdtbl,tablname,tbl

if n_params() eq 0 then begin
   print,'rdtbl,tablname,tbl'
   return
endif

if badpar(tablname,7,0,caller='RDTBL: (tablname) ') then return

print,'   ---> Loading information table file.'
openr,lun,tablname,/get_lun
nobs=0
line=''
while(not eof(lun)) do begin
  readf,lun,line,format='(a1)'
  nobs=nobs+1
endwhile
imtype=strarr(nobs)
imflag=strarr(nobs)
imnum =intarr(nobs)
fn    =strarr(nobs)
objnam=strarr(nobs)
jd    =dblarr(nobs)
airmas=fltarr(nobs)
exptim=fltarr(nobs)
avego =intarr(nobs)
mate  =intarr(nobs)
relsig=fltarr(nobs)
point_lun,lun,0
FOR i=0,nobs-1 DO BEGIN
  readf,lun,line,format='(a)'
  words     = strsplit(strcompress(line),' ',/extract)
  parts     = strsplit(words[2],'.',/extract)
  imtype[i] = words[0]
  imflag[i] = words[1]
  imnum[i]  = fix(parts[1])
  fn[i]     = words[2]
  objnam[i] = words[3]
  jd[i]     = double(words[4])
  airmas[i] = float(words[5])
  exptim[i] = float(words[6])
  avego[i]  = fix(words[7])
  mate[i]   = fix(words[8])
  relsig[i] = float(words[9])
ENDFOR
free_lun,lun
tbl = { $
  name:   tablname, $
  nobs:   nobs, $
  imtype: imtype, $
  imflag: imflag, $
  imnum:  imnum, $
  fn:     fn, $
  objnam: objnam, $
  jd:     jd, $
  airmas: airmas, $
  exptim: exptim, $
  avego:  avego, $
  mate:   mate, $
  relsig: relsig, $
  dirty:  0 $
  }

end


;+
; NAME:
;  wrtbl
; PURPOSE:
;  Update the table file of a night's OSIRIS data
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  wrtbl,tablname,tbl
; INPUTS:
;  tablname - Table file to be updated
;  tbl      - Structure containing the updated table information
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  98/06/10 - Written by Chris Dalla Piazza, Lycoming College; extracted from
;             xdspec
;-

pro wrtbl,tablname,tbl

if n_params() eq 0 then begin
   print,'wrtbl,tablname,tbl'
   return
endif

if badpar(tablname,7,0,caller='WRTBL: (tablname) ') then return
if badpar(tbl,8,1,caller='WRTBL: (tbl) ') then return

blanks='                '
openw,lun,tablname,/get_lun
FOR i=0,tbl.nobs-1 DO BEGIN
   IF tbl.imflag[i] eq 't' THEN tbl.imflag[i]='-'
   printf,lun,tbl.imtype[i],tbl.imflag[i],tbl.fn[i], $
   tbl.objnam[i]+blanks,tbl.jd[i],tbl.airmas[i],tbl.exptim[i], $
   tbl.avego[i],tbl.mate[i],tbl.relsig[i], $
   format='(2(a1,1x),a,1x,a16,1x,f13.5,1x,f5.3,1x,f8.3,1x,i3,1x,i3,1x,f7.4)'
ENDFOR
free_lun,lun

end

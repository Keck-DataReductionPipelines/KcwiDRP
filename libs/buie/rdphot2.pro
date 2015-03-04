pro rdphot2,filename,obj,name,xpos,ypos,mag,err
;+
; NAME:
;	rdphot2
; PURPOSE: (one line)
;	Reads the original BASPHOTC photometry log file.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-

openr,unit,filename,/get_lun

first=1
name0=''
junk=''

while(not eof(unit)) do begin

   readf,unit,name0,obj0,xpos0,ypos0,mag0,err0,format='(a8,1x,i4,2x,f7.3,1x,f7.3,23x,f8.4,1x,f6.4)'
   readf,unit,junk,format='(a1)'

   if obj0 eq obj then begin
      if (first eq 1) then begin
         name = name0
         xpos = xpos0
         ypos = ypos0
         mag  = mag0
         err  = err0
         first = 0
      endif else begin
         name = [name,name0]
         xpos = [xpos,xpos0]
         ypos = [ypos,ypos0]
         mag  = [mag,mag0]
         err  = [err,err0]
      end
   endif

end

free_lun,unit

end

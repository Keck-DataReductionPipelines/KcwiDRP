pro rdphot,filename,obj,name,xpos,ypos,mag,err
;+
; NAME:
;	rdphot
; PURPOSE: (one line)
;	Reads photometry from a basphotc log file.
; DESCRIPTION:
;	All photometry for the given object serial number is read from the file
;	and returned to the caller.
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;	rdphot,filename,obj,name,xpos,ypos,mag,err
; INPUTS:
;	filename - string containing the photometry log file to read from.
;	obj      - object serial number to extract from the file.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	name     - Original image file name for each point.
;	xpos     - vector containing the x position for each point.
;	ypos     - vector containing the y position for each point.
;	mag      - instrumental magnitudes
;	err      - uncertainties on the magnitudes
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Revised for new version of basphotc
;	  8/31/92 - Marc W. Buie, Lowell Observatory.
;-

openr,unit,filename,/get_lun

first=1
name0=''
junk=''

while(not eof(unit)) do begin

   form='(a8,1x,i4,1x,f8.3,1x,f8.3,33x,f8.4,1x,f6.4)'
   readf,unit,name0,obj0,xpos0,ypos0,mag0,err0,format=form
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

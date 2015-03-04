;+
; NAME:
;	rdgrab
; PURPOSE: (one line)
;	Read raw binary IRTF-grabber1 format data files.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;	vec=rdgrab(file)
; INPUTS:
;	file - string containing the name of a file to read from
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	Returned is a 1-d array of values from the file.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, Oct 1992
;-
function rdgrab,filenam
   a=intarr(4096)
   openr,lun,filenam,/get_lun
   readu,lun,a
   byteorder,a
   a=a[96:4095]
   data=a
   while not eof(lun) do begin
      a=intarr(4096,/nozero)
      readu,lun,a
      byteorder,a
      a=a[96:4095]
      data=[data,a]
   end
   close,lun
   return,data
end

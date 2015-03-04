function rdbyt,filename
;+
;NAME:
;	rdbyt
;PURPOSE:
;	Read and return a two dimensional byte array from an animation file.
;CATEGORY:
;  File I/O
;CALLING SEQUENCE:
;	vec=rdbyt('myfile.dat')
;INPUTS:
;	filename = string containing the name of the file to read.
;OUTPUTS:
;	returns the vector
;REVISION HISTORY:
;	Written by Marc Buie on 12/17/91
;COMMENTS:
;-

on_error,2
on_ioerror,bad

openr,unit,filename,/get_lun

ncols=0
nrows=0
readu,unit,ncols,nrows
vec=bytarr(ncols,nrows)

readu,unit,vec
goto,done

bad: print,!error_state.msg

done:
free_lun,unit

return,vec
end

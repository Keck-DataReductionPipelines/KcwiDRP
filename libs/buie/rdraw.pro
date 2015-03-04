function rdraw,filename,ncols,nrows
;+
;NAME:
;	rdraw
;PURPOSE:
;	Read and return a two dimensional byte array from a raw animation file.
;CATEGORY:
;  File I/O
;CALLING SEQUENCE:
;	vec=rdraw('myfile.dat',640,480)
;INPUTS:
;	filename = string containing the name of the file to read.
;	ncols    = number of columns in image
;	nrows    = number of rows in image
;OUTPUTS:
;	returns the vector
;REVISION HISTORY:
;	Written by Marc Buie on 4/24/91
;COMMENTS:
;-

on_error,2
on_ioerror,bad

openr,unit,filename,/get_lun
vec=bytarr(ncols,nrows)

readu,unit,vec
goto,done

bad: print,!error_state.msg

done:
free_lun,unit

return,vec
end

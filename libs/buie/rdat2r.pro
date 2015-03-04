function rdat2r,nrows,ncols,filename
;+
;NAME:
;	rdat2r
;PURPOSE:
;      Read and return a two dimensional single precision floating point vector.
;DESCRIPTION:
;	The file should contain at least nrows x ncols points.
;CATEGORY:
;  File I/O
;CALLING SEQUENCE:
;	vec=rdat2r(300,100,'myfile.dat')
;INPUTS:
;	nrows    = number of values to read from the file.
;       ncols    = number of columns to read from the file.
;	filename = string containing the name of the file to read.
;OUTPUTS:
;	returns the array
;REVISION HISTORY:
;	Written by Marc Buie on 5/2/91
;COMMENTS:
;	A bit of a kludge.
;-

on_error,2
on_ioerror,bad

openr,unit,filename,/get_lun
vec=fltarr(ncols,nrows)

readu,unit,vec
goto,done

bad: print,!error_state.msg

done:
free_lun,unit

return,vec
end

function rdat2u,nrows,ncols,filename
;+
;NAME:
;	rdat2u
;PURPOSE:
;	Read and return a two dimensional two byte unsigned integer vector.
;DESCRIPTION:
;	The file should contain at least nrows x ncols points.
;CATEGORY:
;  File I/O
;CALLING SEQUENCE:
;	vec=rdat2u(300,100,'myfile.dat')
;INPUTS:
;	nrows    = number of values to read from the file.
;       ncols    = number of columns to read from the file.
;	filename = string containing the name of the file to read.
;OUTPUTS:
;	returns the array
;REVISION HISTORY:
;	Written by Marc Buie on 6/3/91
;COMMENTS:
;	A bit of a kludge.
;-

on_error,2
on_ioerror,bad

openr,unit,filename,/get_lun
vec=intarr(ncols,nrows)

readu,unit,vec
vecuns = long(vec)
high = where(vecuns < 0, count)
if count ne 0  then vecuns[high] = vecuns[high]+65536

goto,done

bad: print,!error_state.msg

done:
free_lun,unit

return,vecuns
end

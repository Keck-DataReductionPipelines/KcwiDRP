function rdat1r,num,filename
;+
;NAME:
;	rdat1r
;PURPOSE:
;      Read and return a one dimensional single precision floating point vector.
;DESCRIPTION:
;      The file should contain at least num points.
;CATEGORY:
;  File I/O
;CALLING SEQUENCE:
;	vec=rdat1r(100,'myfile.dat')
;INPUTS:
;	num      = number of values to read from the file.
;	filename = string containing the name of the file to read.
;OUTPUTS:
;	returns the vector
;REVISION HISTORY:
;	Written by Marc Buie on 2/19/91
;COMMENTS:
;	A bit of a kludge.
;-

on_error,2
on_ioerror,bad

openr,unit,filename,/get_lun
vec=fltarr(num)

readu,unit,vec
goto,done

bad: print,!error_state.msg

done:
free_lun,unit

return,vec
end

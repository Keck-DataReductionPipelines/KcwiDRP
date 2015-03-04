function rdat1i,num,filename
;+
;NAME:
;	rdat1i
;PURPOSE:
;      Read and return a one dimensional 2-byte integer vector.
;DESCRIPTION:
;      The file should contain at least num points.
;CATEGORY:
;	File I/O
;CALLING SEQUENCE:
;	vec=rdat1i(100,'myfile.dat')
;INPUTS:
;	num      = number of values to read from the file.
;	filename = string containing the name of the file to read.
;OUTPUTS:
;	returns the vector
;REVISION HISTORY:
;	Written by Marc Buie on 12/28/92
;COMMENTS:
;	A bit of a kludge.
;-

on_error,2
on_ioerror,bad

openr,unit,filename,/get_lun
vec=intarr(num)

readu,unit,vec
goto,done

bad: print,!error_state.msg

done:
free_lun,unit

return,vec
end

pro rdimg,filename,im1,im2

;+
;NAME:
;       rdimg
;PURPOSE:
;       Read and return the two images in an old style MaxEnt map.
;CATEGORY:
;  File I/O
;CALLING SEQUENCE:
;       rdimg('myfile.dat',pluto,charon)
;INPUTS:
;       filename = string containing the name of the file to read.
;OUTPUTS:
;       pluto    = First image in file, usually Pluto.
;       charon   = Second image in file, usually Charon.
;REVISION HISTORY:
;       Written by Marc Buie on 10/21/91
;COMMENTS:
;-

on_error,2
on_ioerror,bad

openr,unit,filename,/get_lun
readf,unit,ncols,nrows,nbody

im1=fltarr(ncols,nrows)
im2=fltarr(ncols,nrows)

readf,unit,im1,im2
goto,done

bad: print,!error_state.msg

done:
free_lun,unit

end

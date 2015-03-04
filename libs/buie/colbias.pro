;+
; NAME: 
;  colbias
; PURPOSE:
;  Determine and subtract column-wise overscan correction with cropping.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  result=colbias(array,x1,x2,xc1,xc2,yc1,yc2)
; INPUTS:
;  array - Input array to perform operation on (2-D)
;  x1    - First column for overscan region
;  x2    - Last column for overscan region
;  xc1   - X coordinate of LLHC of sub-array to save (default=0)
;  xc2   - X coordinate of URHC of sub-array to save (default=last pixel)
;  yc1   - Y coordinate of LLHC of sub-array to save (default=0)
;  yc2   - Y coordinate of URHC of sub-array to save (default=last pixel)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the overscan corrected, cropped image.
; KEYWORD OUTPUT PARAMETERS:
;  BIASVAL - the mean of the overscan area is returned.
;  NOISEVAL -the standard deviation of the overscan area is returned.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/06/13 - Initial version written, Marc W. Buie, Lowell Observatory
;  2000/02/05, MWB, minor bug fixed when overscan region started at 0.
;  2000/11/19, MWB, added support for new data types
;  2001/01/22, MWB, changed so bias region is clipped by crop region.
;  2006/06/30, Peter L. Collins, Lowell Observatory
;              add NOISEVAL output keyword.
;-
function colbias,arr,x1,x2,xc1,xc2,yc1,yc2,BIASVAL=biasval,NOISEVAL=noiseval

on_error,2

if badpar(arr,[1,2,3,4,5,12,13,14,15],2,caller='COLBIAS: (array) ') then return,arr
sz=size(arr)
nx=sz[1]
ny=sz[2]
if badpar(x1,[2,3],0,caller='COLBIAS: (x1) ') then return,arr
if badpar(x2,[2,3],0,caller='COLBIAS: (x2) ') then return,arr
if badpar(xc1,[0,2,3,4,5],0,caller='COLBIAS: (xc1) ',DEFAULT=0) then return,arr
if badpar(yc1,[0,2,3,4,5],0,caller='COLBIAS: (yc1) ',DEFAULT=0) then return,arr
if badpar(xc2,[0,2,3,4,5],0,caller='COLBIAS: (xc2) ',DEFAULT=nx-1) then return,arr
if badpar(yc2,[0,2,3,4,5],0,caller='COLBIAS: (yc2) ',DEFAULT=ny-1) then return,arr

if xc1 eq -1 then xc1 = 0
if yc1 eq -1 then yc1 = 0
if xc2 eq -1 then xc2 = nx-1
if yc2 eq -1 then yc2 = ny-1

if x1 gt x2 then message,'x1 must be <= x2'
;if x1 lt 0  then message,'x1 must be >= 0'
if x1 ge nx then message,'x1 must be <'+strcompress(string(nx))
;if x2 lt 0  then message,'x2 must be >= 0'
if x2 ge nx then message,'x2 must be <'+strcompress(string(nx))

if xc1 gt xc2 then message,'xc1 must be <= xc2'
if yc1 gt yc2 then message,'yc1 must be <= yc2'
if xc1 lt 0   then message,'xc1 must be >= 0'
if xc1 ge nx  then message,'xc1 must be <'+strcompress(string(nx))
if yc1 lt 0   then message,'yc1 must be >= 0'
if yc1 ge ny  then message,'yc1 must be <'+strcompress(string(ny))
if xc2 lt 0   then message,'xc1 must be >= 0'
if xc2 ge nx  then message,'xc2 must be <'+strcompress(string(nx))
if yc2 lt 0   then message,'yc2 must be >= 0'
if yc2 ge ny  then message,'yc2 must be <'+strcompress(string(ny))

if (x1 ge 0 and x2 ge 0) then begin
   vals=moment(arr[x1:x2,yc1:yc2],maxmoment=2) 
   biasval = vals[0]
   noiseval = sqrt(vals[1])
endif else begin
   biasval=0.0
   noiseval=0.0
endelse

return,arr[xc1:xc2,yc1:yc2]-biasval

end

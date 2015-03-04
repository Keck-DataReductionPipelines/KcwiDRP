;+
; NAME:
;     maxloc
; PURPOSE: (one line)
;     Find the column-wise, row-wise, or point location of the image maximum.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;     maxloc,array,xpos,ypos,start,stop
; INPUTS:
;     array - Input array to be scanned.
; OPTIONAL INPUT PARAMETERS:
;     start - starting location in array to begin scanning (def=0)
;     stop  - ending location in array for scan (def=end of array)
; KEYWORD INPUT PARAMETERS:
;     X     - FLAG, find maxima as a function of x in the image.
;     Y     - FLAG, find maxima as a function of y in the image.
;     POINT - FLAG, find location of absolute maximum in the image (default).
; OUTPUTS:
;     xpos  - x index for maximum location.
;     ypos  - y index for maximum location.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     93/04/27 - Written by Marc W. Buie, Lowell Observatory
;     95/03/24, MWB, added start and stop optional inputs.
;     99/07/12, MWB, fixed bug with start
;-

pro maxloc,array,xpos,ypos,start,stop, $
       X=x_dir, Y=y_dir, POINT=point

   if badpar(array,[1,2,3,4,5,12,13,14,15],2,CALLER='MAXLOC (array): ') then return
   if badpar(x_dir,[0,1,2,3],0,CALLER='MAXLOC (X): ',DEFAULT=0) then return
   if badpar(y_dir,[0,1,2,3],0,CALLER='MAXLOC (Y): ',DEFAULT=0) then return
   if x_dir or y_dir then begin
      if badpar(point,[0,1,2,3],0,CALLER='MAXLOC (POINT): ',DEFAULT=0) then return
   endif else begin
      if badpar(point,[0,1,2,3],0,CALLER='MAXLOC (POINT): ',DEFAULT=1) then return
   endelse

   if x_dir+y_dir+point ne 1 then begin
      message,'Only one of X, Y, or POINT keywords can be set at once.'
   endif

   sz=size(array)
   xlen = sz[1]
   ylen = sz[2]

   if x_dir then def_stop = xlen-1
   if y_dir then def_stop = ylen-1
   if point then def_stop = 0

   if badpar(start,[0,1,2,3,4,5],0,CALLER='MAXLOC (start) ',DEFAULT=0) then return
   if badpar(stop,[0,1,2,3,4,5],0,CALLER='MAXLOC (stop) ',DEFAULT=def_stop) then return

   npts  = stop-start+1

   if x_dir then begin

      xpos=indgen(npts)+start
      ypos=intarr(npts)
      for i=0,npts-1 do begin
         vec = array[xpos[i],*]
         loc = where(vec eq max(vec))
         ypos[i] = loc[0]
      endfor

   endif else if point then begin

      idx = where(array eq max(array))
      sz=size(array)
      ypos = idx[0] / sz[1]
      xpos = idx[0] - ypos*sz[1]

   endif else begin

      xpos=intarr(npts)
      ypos=indgen(npts)+start
      for i=0,npts-1 do begin
         vec = array[*,ypos[i]]
         loc = where(vec eq max(vec))
         xpos[i] = loc[0]
      endfor

   endelse

end

;+
; NAME: 
;  cleandat
; PURPOSE: 
;  Interactive program to eliminate and smooth over bad data points.
; DESCRIPTION:
;
;  The data are plotted to window 6, then the cursor becomes active.
;  Clicking right will exit the routine.  Clicking left will select a
;  point for change.  After a left click, you must click again at the
;  y value to give to the corrected point.  A running display helps
;  identify the current point that the cursor is nearest to in X.
;
;  Clicking the middle button will cause the point nearest the cursor
;  (in X) to be replaced by the average of its nearest neighbors.
;
; CATEGORY:
;  2-D plotting
; CALLING SEQUENCE:
;  cleandat,xval,yval
; INPUTS:
;  xval - Input vector of x values.
;  yval - Input vector of y values (modified).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  TITLE - Optional title for plot
; OUTPUTS:
;  yval - (potentially) cleaned version of y values.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  Uses graphics window 6 and forces its size.  !p.multi is also set to 0.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/03/24, Written by Marc W. Buie, Lowell Observatory
;-
pro cleandat,xval,yval,TITLE=title

   if badpar(xval,[1,2,3,4,5],1,CALLER='CLEANDAT (xval) ',NPTS=xlen) then return
   if badpar(yval,[1,2,3,4,5],1,CALLER='CLEANDAT (xval) ',NPTS=ylen) then return
   
   if not keyword_set(title) then title=' '

   if xlen ne ylen then begin
      print,'CLEANDAT:  Error, x and y vectors must be the same length'
      return
   endif

   in_yval=yval
   setwin,6,xsize=1000,ysize=500,/show
   !p.multi=0
   plot,xval,yval,psym=-8,symsize=0.5,title=title
   
   !mouse.button=0
   cr = string("15b)  ;"
   form="($,a,'pix=',i3,3x,f5.0,4x,i2,2x,'(',f5.1,',',f5.0,')')"
   while !mouse.button eq 4 do begin
      cursor,x,y,2,/data
      dist=abs(xval-x)
      loc=where(dist eq min(dist))
      loc=loc[0]
      if !mouse.button eq 1 then begin
         print,' click left on new y',form='($,a)'
         cursor,newx,newy,3,/data
         if !mouse.button eq 1 then begin
            print,'.   new y is ',newy
            yval[loc]=newy
            plot,xval,in_yval,yr=minmax(yval),title=title
            oplot,xval,yval,color=100,psym=-8,symsize=0.5
         endif
      endif
      if !mouse.button eq 2 then begin
         yval[loc]=(yval[loc-1]+yval[loc+1])/2.0
         print,' auto interpolate  ',yval[loc]
         plot,xval,in_yval,yr=minmax(yval),title=title
         oplot,xval,yval,color=100,psym=-8,symsize=0.5
      endif
      print,form=form, cr,xval[loc],yval[loc],fix(abs(y-yval[loc]+0.5)),x,y
   endwhile
   print,form="(/)"
end

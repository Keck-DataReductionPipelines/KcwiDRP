;+
; NAME:
;  edgefit
; PURPOSE:
;  Fit an edge between two signal levels
; DESCRIPTION:
; CATEGORY:
;  Function fitting
; CALLING SEQUENCE:
;  edgefit,x,y
; INPUTS:
;  x - independent variable (vector)
;  y - dependent variable (vector)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  Two plots are generated and the fitted values are printed to the screen.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/12/23, Written by Marc W. Buie, Lowell Observatory
;-
function edgefit_func, vec
   common edgefit_common,x,y,dx,len,model
   left = vec[0]
   right = vec[1]
   midpt = vec[2]
   model = fltarr(len)
   model[where(x lt midpt-dx/2.0)]=left
   model[where(x gt midpt+dx/2.0)]=right
   z=where(midpt-dx/2.0 le x and midpt+dx/2.0 ge x,count)
   if count ne 0 then begin
      if left lt right then $
         model[z] = (right*(midpt-(x[z]-dx/2.0)) + left*((x[z]-dx/2.0)-midpt))/dx $
      else $
         model[z] = (left*(midpt-(x[z]-dx/2.0)) + right*((x[z]-dx/2.0)-midpt))/dx
   endif
;   print,'aaaa ',model(z)
   return,total((y-model)^2)
end

pro edgefit,x,y

common edgefit_common,xdat,ydat,dx,len,model

   len=n_elements(x)
   xdat=x
   ydat=y

   ; Get the mean level at the ends (first and last 10%)
   left  = mean(y[0:0.1*len])
   right = mean(y[0.9*len:len-1])
   print,left,right
   midpt = x[len/2]
   dx=x[1]-x[0]

   vals=[left,right,midpt]
   dirs=[[0.1,0,0],[0,0.1,0],[0,0,0.1]]
   powell,vals,dirs,0.0001,chibest,'edgefit_func'
   print,dirs

;   chibest=edgefit_func([left,right,midpt])

   setwin,0
   plot,x,y
   oplot,x,model,psym=-5

   setwin,1
   plot,x,y-model

   print,left,right,midpt
   print,chibest

end


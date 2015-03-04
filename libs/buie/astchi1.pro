;+
; NAME:
;  astchi1
; PURPOSE:   (one line only)
;  Astrometric goodness-of-fit for one image based on rotation and offset
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  chisq = astchi1(x,y,xpred,ypred,xoffset,yoffset,rotang,alpha)
; INPUTS:
;  x       - Measured X position of an object, this is in raw chip coordinates
;  y       - Measured Y position of an object, this is in raw chip coordinates
;  xpred   - Predicted X location of the object
;  ypred   - Predicted Y location of the object
;  xoffset - Offset to apply to X positions
;  yoffset - Offset to apply to Y positions
;  rotang  - Rotation to apply to X coordinates (applied after rotation)
;  alpha   - 2-element vector that gives the relative weighting between
;              the chisq values for the X and Y axes.  [1.,1.] would apply
;              equal weights.  [1.,0.] would return just the X chisq, and
;              so on.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is a scalar number giving the goodness-of-fit statistic
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2010/01/13
;-
function astchi1,x,y,xpred,ypred,xoffset,yoffset,rotang,alpha

   crang = cos(rotang)
   srang = sin(rotang)

   cd1_1= crang
   cd1_2= srang
   cd2_1= -1.0*srang
   cd2_2= crang

   ncheck = float(n_elements(x))
   
   xp = cd1_1*(x-xoffset) + cd1_2*(y-yoffset)
   yp = cd2_1*(x-xoffset) + cd2_2*(y-yoffset)

   xchi = total((xpred-xp)^2)/ncheck
   ychi = total((ypred-yp)^2)/ncheck

   chi = total(alpha*[xchi,ychi])
   return,chi

end

;+
; NAME:
;  wfpc2_distorted
; PURPOSE:   (one line only)
;  Compute inverse of the wfpc2\_metric function.
; DESCRIPTION:
;  Iterative reverse solution of wfpc2_metric (HST WFPC2 camera distortion
;    model).  This depends entirely on the solutions in the wfpc2_metric
;    program.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  wfpc2_distorted,xundist,yundist,xdist,ydist
; INPUTS:
;  xundist - X coordinate in the undistorted (sky) plane
;  yundist - Y coordinate in the undistorted (sky) plane
; OPTIONAL INPUT PARAMETERS:
;  ITER - Number of iteration to run to get the answer (default=1)
;  FILTER - String with the name of the filter used (this defines the
;             distortion coefficients used.
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  xdist   - X coordinate in the native chips coordinates
;  ydist   - Y coordinate in the native chips coordinates
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   Written by Peter Collins, Lowell Observatory, circa 2008
;   2008/12/15, MWB, promoted internal calculations to double precision
;   2010/03/08, MWB, placed into library with documentation
;-
pro wfpc2_distorted, xundist, yundist, xdist, ydist, ITER=iter, FILTER=filter
   wfpc2_metric,double(xundist),double(yundist),xtemp,ytemp,1
   deltadistx= xtemp - xundist
   deltadisty= ytemp - yundist
   xdist = xundist - deltadistx
   ydist = yundist - deltadisty
   if n_elements(iter) eq 0 then begin
      wfpc2_metric,xdist, ydist, xtemp,ytemp,1, FILTER=filter
      xdist += (xundist - xtemp)
      ydist += (yundist - ytemp)
   endif else $
      for i=0,iter-1 do begin
         wfpc2_metric,xdist, ydist, xtemp,ytemp,1, FILTER=filter
         ddeltax = (xtemp - xundist)
         ddeltay = (ytemp - yundist)
         deltadistx += ddeltax
         deltadisty += ddeltay
         ;print, ' ddeltas ', ddeltax, ddeltay
         xdist = xundist - deltadistx
         ydist = yundist - deltadisty
      endfor
end


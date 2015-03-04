;+
; NAME:
;  blurmap
; PURPOSE:   (one line only)
;  Apply spherical blurring to a map
; DESCRIPTION:
; CATEGORY:
;  Numerical
; CALLING SEQUENCE:
;  blurmap,inmap,blurradius,outmap
; INPUTS:
;  inmap - Rectangular array which contains a map of a spherical body.  The
;            coordinate scheme used is documented in RENDER.PRO
;  blurradius - radius of the blurring function in degrees (<90)
;                 a gaussian filter is used and this is the FWHM of the filter
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OUTSIZE - two element vector that gives the size of the output map.
;              Default is the same as the input map.
; OUTPUTS:
;  outmap - Output map smoothed to desired level
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/09/18
;-
pro blurmap,inmap,blurradius,outmap,OUTSIZE=outsize

   self='BLURMAP '
   if badpar(inmap,[4,5],2,caller=self+'inmap') then return
   if badpar(blurradius,[2,3,4,5],0,caller=self+'blurradius') then return

   blur_r = (blurradius<90.0) / !radeg

   sz = size(inmap,/dimen)

   if badpar(outsize,[0,1,2,3],1,caller=self+'OUTSIZE',default=sz) then return

   outmap = fltarr(outsize[0],outsize[1])

   ; compute the unit vectors for each pixel in the input map
   initvc,sz[0],sz[1],xn1,yn1,zn1,area1

   ; compute the unit vectors for each pixel in the output map
   initvc,outsize[0],outsize[1],xn2,yn2,zn2,area2

   ; loop over all tiles in the output map
   for i=0,outsize[0]-1 do begin  ; x/lon
      for j=0,outsize[1]-1 do begin ; y/lat
         arg = ( (xn2[i,j]*xn1 + yn2[i,j]*yn1 + zn2[i,j]*zn1) < 1.0 ) > (-1.0)
         ang = acos(arg)
         arg = -(ang/blur_r)^2
         z=where(arg gt -20 and ang lt !pi/2.0,count)
         w = exp(arg[z])*area1[z]
         outmap[i,j] = total(inmap[z]*w)/total(w)
      endfor
   endfor


end

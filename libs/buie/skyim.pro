;+
; NAME:
;  skyim
; PURPOSE:   (one line only)
;  Calculate a smooth sky image from 2-d polynomial fit coefficients
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  skyim,dimen,coeff,skyimage
; INPUTS:
;  dimen   - Size of array to compute [nx,ny]
;  coeff   - Coefficients of fit
;  xorder  - order of fit to X direction (default=0, constant)
;  yorder  - order of fit to Y direction (default=0, constant)
;              Length of coeff must be (xorder+1)*(yorder+1)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  skyimage - Smooth image of sky in image.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2004/07/06
;  2010/11/30, MWB, fixed problem with constant sky value option
;-
pro skyim,dimen,coeff,xorder,yorder,skyimage

   self='SKYIM: '
   if badpar(dimen,[2,3],1,caller=self+'(dimen): ') then return
   if badpar(coeff,[4,5],[0,1],caller=self+'(coeff): ',npts=ncoeff) then return
   if badpar(xorder,[2,3],0,caller=self+'(XORDER): ') then return
   if badpar(yorder,[2,3],0,caller=self+'(YORDER): ') then return

   ; error check
   if ncoeff ne (xorder+1)*(yorder+1) then begin
      print,self,' coeff not consistent with xorder and yorder. Aborting.'
      return
   endif

   xidx = indgen(dimen[0])
   yone = replicate(1,dimen[1])
   xarr = xidx#yone

   yidx = indgen(dimen[1])
   xone = replicate(1,dimen[0])
   yarr = xone#yidx

   ; Build final sky image.
   skyimage = replicate(float(coeff[0]),dimen[0],dimen[1])
   if xorder ne 0 or yorder ne 0 then begin
      k=0
      for j=0,xorder do begin
         for i=0,yorder do begin
            if i eq 0 and j eq 0 then begin
               ; no-op
            endif else if i eq 0 then begin
               skyimage += float(coeff[k]*yarr^j)
            endif else if j eq 0 then begin
               skyimage += float(coeff[k]*xarr^i)
            endif else begin
               skyimage += float(coeff[k]*xarr^i*yarr^j)
            endelse
            k++
         endfor
      endfor
   endif

end

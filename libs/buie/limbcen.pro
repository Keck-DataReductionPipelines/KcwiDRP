;+
; NAME: 
;  limbcen
; PURPOSE: 
;  Find center of body from centroid of limb points (Designed for Jupiter).
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  limbcen,image,xcen,ycen,DISPLAY=display
; INPUTS:
;  image   - Image (or cube) to find limb-centers for.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DISPLAY - window number for optional display (not 0), default no display.
; OUTPUTS:
;  xcen    - Centroided position in X
;  ycen    - Centroided position in Y
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/08/16 - Written by Marc W. Buie, Lowell Observatory.
;-
pro limbcen,image,xcen,ycen,DISPLAY=display

if badpar(image,[1,2,3,4,5],[2,3],CALLER='LIMBCEN: (image) ',DIMEN=dimen,RANK=rank) then return
if badpar(display,[0,1,2,3,4,5],0,CALLER='LIMBCEN: (display) ',DEFAULT=0) then return

if rank eq 2 then begin
   nim = 1
   xcen = 0
   ycen = 0
endif else begin
   nim=dimen[2]
   xcen = fltarr(dimen[2])
   ycen = fltarr(dimen[2])
endelse

xarr = indgen(dimen[0])#replicate(1,dimen[1])
yarr = replicate(1,dimen[0])#indgen(dimen[1])

for i=0,nim-1 do begin
   xn = minmax(image[*,*,i])
   mid = mean(xn)
   hrange = (xn[1] - xn[0])/2.0
   z = where( image[*,*,i] gt mid - hrange*0.05 and $
              image[*,*,i] lt mid + hrange*0.05,       count)
   if count eq 0 then begin
      print,'LIMBCEN: abort, no limb points found',i
      return
   endif

   test=image[*,*,i]

   xcen[i]=total(xarr[z]*test[z])/total(test[z])
   ycen[i]=total(yarr[z]*test[z])/total(test[z])

   if display ne 0 then begin
      print,i,count,xcen[i],ycen[i]
      setwin,display
      test[z]=test[z]*2
      tvscl,test,i
   endif
endfor

end

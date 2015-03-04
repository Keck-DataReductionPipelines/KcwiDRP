;+
; NAME: 
;  getstrip
; PURPOSE: 
;  Extract a 2-d rectangular strip image from a OSIRIS XD image.
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  getstrip,calib,root,frno,strip,hdr
; INPUTS:
;  calib- Anonymous structure containing all pertinent calibration
;           information.  This structure is usually loaded beforehand using
;           the routine, "ldcalir"
;  root - string containing the root of the file name (with leading path
;         if desired).
;  frno - Frame id (integer) of image to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OFF_CHIP - Value to assign to output array for pixels that do not map
;                to the original array (default=0.0)
; OUTPUTS:
;  strip - 2-d rectangular strip image pulled from raw data (see getstrip).
;  hdr   - FITS header for image.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/03/24, Written by Marc W. Buie, Lowell Observatory
;  95/09/15, MWB, added usage of calib structure and return of header.
;  98/06/08, MWB, added OFF_CHIP keyword
;  98/08/17, MWB, added fix to get actual Exposure time from image.
;-
pro getstrip,calib,root,frno,strip,hdr,OFF_CHIP=off_chip

   if badpar(calib,8,1,CALLER='getstrip (calib) ') then return
   if badpar(root,7,0,CALLER='getstrip (root) ') then return
   if badpar(frno,[2,3],0,CALLER='getstrip (frno) ') then return
   if badpar(off_chip,[0,2,3,4,5],0,CALLER='getstrip (OFF_CHIP) ', $
                                  default=0.0) then return

   filename = root+'.'+string(frno,form='(i3.3)')
   if not exists(filename) then begin
      print,'GETSTRIP: file ',filename,' does not exist. Quitting.'
      return
   endif
   image=readfits(filename,hdr,/silent)
   sz=size(image)
   nx=sz[1]
   ny=sz[2]

   ; Adjust the exposure time based on the goofy number stored in the image data.
   exptim = float(sxpar(hdr,'EXPTIME'))
   if nx gt 128 then begin ; protection just in case data format not right.
      if exptim le 60.0 then begin
         exptim = image[128,0] / 1000.0  ; convert from milliseconds to seconds.
      endif else begin
         exptim = exptim+0.9   ; This is just a guess
      endelse
   endif
   sxaddpar,hdr,'EXPTIME',exptim,' (GETSTRIP) Exposure time in seconds (corrected)', $
      format='f8.3'

   strip=replicate(float(off_chip),calib.npts,calib.height)
   j=0
   for i=0,calib.nor-1 do begin
     for x=calib.x1[i],calib.x2[i] do begin
      y = fix(x*calib.slope[i] + calib.y0[i] + 0.5)
      y1 = y
      y2 = y+calib.height-1
      if y1 lt  0 then y1 = 0
      if y1 ge ny then y1 = -1
      if y2 lt  0 then y2 = -1
      if y2 ge ny then y2 = ny-1
      if y1 ne -1 and y2 ne -1 then strip[j,y1-y:y2-y]=image[x,y1:y2]
      j=j+1
     endfor
   endfor

end

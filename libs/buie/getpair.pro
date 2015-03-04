;+
; NAME:
;  getpair
; PURPOSE:
;  Read two OSIRIS XD data files and return the difference strip image.
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  getpair,calib,root,i1,i2,diff,hdr,RAW=raw
; INPUTS:
;  calib- Anonymous structure containing all pertinent calibration
;           information.  This structure is usually loaded beforehand using
;           the routine, "ldcalir"
;  root - string containing the root of the file name (with leading path
;         if desired).
;  i1   - Frame id (integer) of first image.
;  i2   - Frame id (integer) of second image.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  RAW  - Flag, if true will inhibit the column-wise background subtraction.
;           The default is to fit the background in each column (along the
;           slit) and subtract the fitted background.  Use a linear function
;           for the background.
; OUTPUTS:
;  diff - i1-i2 after XD spectra extracted to strip image. (see getstrip)
;  hdr   - FITS header for image.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Specifically written for OSIRIS cross-dispersed spectral data.
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/03/24, Written by Marc W. Buie, Lowell Observatory
;  95/09/15, MWB, added usage of calib structure and return of header.
;-
pro getpair,calib,root,i1,i2,diff,hdr,RAW=raw

   if badpar(calib,8,1,CALLER='getpair (calib) ') then return
   if badpar(root,7,0,CALLER='getpair (root) ') then return
   if badpar(i1,[2,3],0,CALLER='getpair (i1) ') then return
   if badpar(i2,[2,3],0,CALLER='getpair (i2) ') then return
   if badpar(raw,[0,1,2,3],0,CALLER='getpair (RAW) ',DEFAULT=0) then return

   getstrip,calib,root,i1,f1,hdr,off_chip=-1000.0
   getstrip,calib,root,i2,f2,off_chip=-1000.0

   z=where(f1 le -1000.0 or f2 le -1000.0,count)

   diff= ( f1 > (-1000.0) ) - ( f2 > (-1000.0) )

   diff = diff / calib.flat

   IF count ne 0 THEN BEGIN
      diff[z] = -1.0e6
   ENDIF

   if not raw then begin
      backsub,diff,/col,order=1,min_value=-1.0e6
   endif

end

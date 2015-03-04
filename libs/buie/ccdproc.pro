;+
; NAME:
;  ccdproc
; PURPOSE:
;  Apply standard CCD image correction steps to a raw image.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  ccdproc,raw,hdrinfo,calib,corr
; INPUTS:
;  raw     - Raw CCD image (or cube)
;  hdrinfo - Structure containing relevant header information (see PARSEKEY).
;  calib   - Image calibration information structure.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  corr    - Corrected image.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1999 Jun 5
;  99/10/28, MWB, minor bug fix.
;  2000/01/22, MWB, fixed overscan correction to be limited by crop region
;  2001/09/19, MWB, fixed minor bug in error messages
;-
pro ccdproc,raw,hdrinfo,calib,corr

   if badpar(raw,[2,3,4,5,12,13,14,15],2,caller='CCDPROC: (raw) ') then return
   if badpar(hdrinfo,8,1,caller='CCDPROC: (hdrinfo) ') then return
   if badpar(calib,8,1,caller='CCDPROC: (calib) ') then return

   sz = size(raw,/dimen)

   if ( calib.xl ge 0 and calib.xl ge sz[0] ) or $
      ( calib.xr ge 0 and calib.xr ge sz[0] )      then begin
      print,hdrinfo.imfile,' image size inconsistent with overscan area.'
      corr=raw
      return
   endif

   if calib.x2 ge sz[0] or calib.y2 ge sz[1] then begin
      print,hdrinfo.imfile,' image size inconsistent with cropping area.'
      corr=raw
      return
   endif

   x1 = calib.x1
   x2 = calib.x2
   y1 = calib.y1
   y2 = calib.y2

   if x1 lt 0 or x2 lt 0 then begin
      x1 = 0
      x2 = sz[0]-1
   endif

   if y1 lt 0 or y2 lt 0 then begin
      y1 = 0
      y2 = sz[1]-1
   endif

   flatcode = where(hdrinfo.filter eq calib.filter,count)
   flatcode=flatcode[0]
   if flatcode eq -1 then $
      print,hdrinfo.imfile,': filter ',hdrinfo.filter,' not found in calib information.'

   ; Overscan and cropping correction.
   if calib.xl ge 0 and calib.xr ge 0 then begin
      os = mean( raw[calib.xl:calib.xr,y1:y2] )
      corr = raw[x1:x2,y1:y2] - os
   endif else begin
      corr = raw[x1:x2,y1:y2]
   endelse

   if n_elements(calib.bias) ne 1 then $
      corr = temporary(corr) - calib.bias

   if n_elements(calib.dark) ne 1 then $
      corr = temporary(corr) - calib.dark*hdrinfo.exptime

   if flatcode ne -1 then $
      if calib.flatptr[flatcode] ne -1 then $
         corr = temporary(corr) / calib.flat[*,*,calib.flatptr[flatcode]]

   if flatcode ne -1 then $
      if calib.frngptr[flatcode] ne -1 then begin
         idx=long(randomu(seed,min([6001,n_elements(corr)]))*(n_elements(corr)-1))
         robomean,corr[idx],2.0,0.5,meanval
         corr = temporary(corr) - calib.frngarr[*,*,calib.frngptr[flatcode]]*meanval         
      endif

end

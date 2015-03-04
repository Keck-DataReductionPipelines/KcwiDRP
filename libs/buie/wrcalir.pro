;+
; NAME:
;  wrcalir
; PURPOSE:
;  Write calibration information for OSIRIS XD data to a calib file.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  wrcalir,calib,calibfile
; INPUTS:
;  calib - anonymous structure with calibration information (see LDCALIR)
;  calibfile - Name of file to write calibration information
;                 (default=files.cal)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
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
;  97/12/4, Written by Marc W. Buie, Lowell Observatory
;-
PRO wrcalir,calib,calibfile

   IF badpar(calib,8,1,caller='WRCALIR: (calib) ') THEN return
   IF badpar(calibfile,7,0,caller='WRCALIR: (calibfile)', $
                default='files.cal') THEN return

   openw,lun,calibfile,/get_lun
   printf,lun,'OSIRIS XD v2.0'
   printf,lun,calib.height
   printf,lun,calib.nor
   printf,lun,calib.por
   FOR i=0,calib.nor-1 DO BEGIN
      printf,lun,calib.x1[i],calib.x2[i],calib.y0[i],calib.slope[i], $
                 calib.cof[i,*],format='(i3,1x,i3,1x,f6.2,1x,f6.3,99(1x,e))'
   ENDFOR
   printf,lun,calib.flatname
   free_lun,lun

END

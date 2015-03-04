;+
; NAME:
;  bildmask
; PURPOSE:
;  Stack a set of bad pixel mask images into one master mask.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  bildmask,outfile
; INPUTS:
;
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
;
;-
PRO bildmask,outfile,mask
   
   spawn,'ls mask.*',filelist

   IF filelist[0] eq '' THEN BEGIN
      print,'No raw mask files found, unable to continue.'
      return
   ENDIF

   nmask=n_elements(filelist)

   print,'   ---> Scanning for image mates.'
   print,'   ---> Combining ',strcompress(string(nmask),/remove_all),' mask files.'

   mask = 0.0

   FOR i=0,nmask-1 DO BEGIN
      mask = mask + float(readfits(filelist[i],/silent))
   ENDFOR

   mask = mask/nmask

   writefits,outfile,mask

END

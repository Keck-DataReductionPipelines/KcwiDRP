;+
; NAME:
;  wrcalb
; PURPOSE:
;  Save contents of a calibration structure to a file.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  wrcalib,calib,calibfile
; INPUTS:
;  calib     - anonymous structure with calibration information
;                 see ldcalib.pro for description of the structure.
; OPTIONAL INPUT PARAMETERS:
;  calibfile - Name of calibration file to read (default=files.cal).
;
; KEYWORD INPUT PARAMETERS:
;  CALIBPATH - directory that calibration file should be written to
;                 (default=current directory).
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
;  98/05/16, Written by Marc W. Buie, Lowell Observatory
;-
pro wrcalib,calib,calibfile,CALIBPATH=in_calibpath

   if badpar(calibfile,[0,7],0,caller='WRCALIB: (calibfile) ', $
                               default='files.cal') then return
   if badpar(in_calibpath,[0,7],0,caller='WRCALIB: (path) ', $
                                  default='') then return

   calibpath = addslash(in_calibpath)
   openw,lun,calibpath+calibfile,/get_lun
   printf,lun,'calib_file_v03'
   printf,lun,calib.xl,calib.xr,format='(2i5)'
   printf,lun,calib.x1,calib.x2,calib.y1,calib.y2,format='(4i5)'
   printf,lun,calib.bname
   printf,lun,calib.dname
   for i=0,calib.nfilters-1 do begin
      str=calib.filter[i]+' '+calib.flname[i]+' '+calib.frngname[i]
      str=strtrim(strcompress(str),2)
      printf,lun,str
   endfor
   free_lun,lun

end

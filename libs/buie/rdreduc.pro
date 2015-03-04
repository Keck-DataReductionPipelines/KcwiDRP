;+
; NAME:
;  rdreduc
; PURPOSE: 
;  Reader for a reductor info file (reduc.inf)
; DESCRIPTION:
;  Reads all fields of the reduc.inf file, including the rules. If the
;  info file is not the latest version, it is promoted with reducprmt.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  rdreduc,infofile,inst,ddir,rundate,rad,sky1,sky2,gain,rdnoise,oplines
; INPUTS:
;  infofile-   filename for the info file.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  GUI-     Flag, if set use widgets to prompt for any additional inputs.
;              This flag is also passed to reducprmt.
;  RDNOISE- default read noise, passed to reducprmt.
; OUTPUTS:
;  inst  -  name of instrument for the run. Will be a NULL string
;              if the info file cannot be found.
;  ddir  -  path to image data for the run.
;  rundate- date code for the run, six digit string.
;  rad-     float, object aperture radius in pixels.
;  sky1-    float, inner sky annulus radius in pixels.
;  sky2-    float, outer sky annulus radius in pixels.
;  gain-    float, instrument gain in e-/DN
;  rdnoise- float, instrument read noise in e-/pixel.
;  oplines- string array, rule lines ( line 6 and beyond of info file)
;             Will be a single null string if there are no rule lines
;             or the infofile cannot be found.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  Will rewrite infofile if it is promoted.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2006/08/25   Written, Peter L. Collins, Lowell Observatory
;  2009/12/31, MWB, minor tweak to protect against output variables having
;                 values (and rank) upon input.
;-
pro rdreduc,infofile,inst,ddir,rundate,rad,sky1,sky2,gain,rdnoise,oplines, $
            GUI=gui,RDNOISE=dfltrdnoise

   self='rdreduc: '
   rundate=''
   inst=''
   ddir=''

   if badpar(infofile,7,0,caller=self +  '(infofile) ') then return

   if not exists(infofile) then begin
      print, self + " can't read ", infofile
      return
   endif

   ;  Load the first line from the information file to get version and 
   ;  promote if necessary
   openr,lun,infofile,/get_lun

   version=''
   v1pt1='REDUCTOR v1.1'

   readf,lun,version
   if version ne v1pt1  then begin
      free_lun, lun
      
      reducprmt, infofile, RDNOISE=dfltrdnoise,GUI=gui

      openr,lun,infofile,/get_lun
      readf,lun,version

      if version ne v1pt1  then begin
         print,'ERROR!  bad version id [',version,'] in file ',infofile
         print, 'file could not be promoted'
         free_lun, lun
         return
      endif
   endif

   inst=''
   readf,lun,inst
   ddir=''
   readf,lun,ddir
   rundate=''
   readf,lun,rundate

   rad=0.
   sky1=0.
   sky2=0.
   gain=0.
   rdnoise=0.
   readf, lun,rad,sky1,sky2,gain,rdnoise

   nextop=''
   oplines =['' ]
   while not eof(lun) do begin
      readf,lun,nextop,format='(a)'
      oplines = [oplines, nextop]
   endwhile
   
   if n_elements(oplines) gt 1 then oplines = oplines[1:*]
   free_lun, lun
end

;+
; NAME:
;  wrreduc
; PURPOSE: 
;  Write a reductor info file (reduc.inf)
; DESCRIPTION:
;  Writes all fields of the reduc.inf file, including the rules. 
; The current version (V1.1) of the file is generated.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  wrreduc,infofile,inst,ddir,rundate,rad,sky1,sky2,gain,rdnoise,oplines
; INPUTS:
;  infofile-   filename for the info file.
;  inst  -  name of instrument for the run. Will be a NULL string
;              if the info file cannot be found.
;  ddir  -  path to image data for the run.
;  rundate- date code for the run, six digit string.
;  rad-     float, object aperture radius in pixels.
;  sky1-    float, inner sky annulus radius in pixels.
;  sky2-    float, outer sky annulus radius in pixels.
;  gain-    float, instrument gain in e-/DN
;  rdnoise- float, instrument read noise in e-/pixel.
; OPTIONAL INPUT PARAMETERS:
;  oplines- string array, rule lines ( line 6 and beyond of info file)
;           No rule lines generated if vector unknown ( or starts with '').
; KEYWORD INPUT PARAMETERS:
;  GUI-     Flag, if set use modal widgets to prompt for any additional inputs.
;              This flag is also passed to reducprmt.
; NOCLOBBER- Flag, if set, prompts if the infofile already exists.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  Will write or rewrite infofile.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2006/08/27   Written, Peter L. Collins, Lowell Observatory
;-
pro wrreduc,infofile,inst,ddir,rundate,rad,sky1,sky2,gain,rdnoise,oplines, $
            GUI=gui,NOCLOBBER=noclobber

   self='wrreduc: '

   if badpar(infofile,7,         0,caller=self +  '(INFOFILE) ') then return
   if badpar(inst,7,             0,caller=self +  '(INST) ') then return
   if badpar(ddir,7,             0,caller=self +  '(DDIR) ') then return
   if badpar(rundate,7,          0,caller=self +  '(RUNDATE) ') then return
   if badpar(rad,[4,5],          0,caller=self +  '(RAD) ') then return
   if badpar(sky1,[4,5],         0,caller=self +  '(SKY1) ') then return
   if badpar(sky2,[4,5],         0,caller=self +  '(SKY2) ') then return
   if badpar(gain,[4,5],         0,caller=self +  '(GAIN) ') then return
   if badpar(rdnoise,[4,5],      0,caller=self +  '(RDNOISE) ') then return
   if badpar(oplines,[0,7],      1,caller=self +  '(OPLINES) ', $
             default=['']) then return
   if badpar(gui,[0,1,2,3],      0,caller=self +  '(GUI) ', $
             default=0) then return
   if badpar(noclobber,[0,1,2,3],0,caller=self +  '(NOCLOBBER) ', $
             default=0) then return

   if exists(infofile) and noclobber then begin
      clobber=''
      if gui then clobber = qinput(/STRING, $
                                   PROMPT= strn(infofile) + $
                                 ' already exists. Want to overwrite? [y,n]') $
      else read, PROMPT= strn(infofile) + $
                 ' already exists. Want to overwrite?' ,clobber
      ans = strmid(strtrim(clobber,1),0,1)
      if ans ne 'y' and ans ne 'Y' then begin
         print, 'Operation cancelled.'
         return
      endif
   endif

   openw,lun,infofile,/get_lun

   version='REDUCTOR v1.1'

   printf,lun,version

   printf,lun,inst
   printf,lun,ddir
   printf,lun,rundate

   printf, lun,rad,sky1,sky2,gain,rdnoise

   if oplines[0] ne '' then printf, lun, oplines, FORMAT="(a)"

   free_lun,lun
end

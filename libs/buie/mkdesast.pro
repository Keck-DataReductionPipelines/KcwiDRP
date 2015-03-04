;+
; NAME:
;   mkdesast
; PURPOSE:
;   Create data file to post into des.ast MySQL database from KBO search data
; DESCRIPTION:
; CATEGORY:
;   Database
; CALLING SEQUENCE:
;  mkdesast,root
; INPUTS:
;  root - String, 6 digit date code for observation.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWWRITE - Flag to force overwrite of pre-existing output file.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/04/30
;-
pro mkdesast,root,OVERWRITE=overwrite

   self='mkdesast:'
   if badpar(overwrite,[0,1,2,3],0,caller=self+' (OVERWRITE) ',default=0) then return

   kred = '/net/frakir/raid/reduced/'
   astdir = kred+'/db/'
   info = kred+root+'/infox2.log'
   infoh = kred+root+'/Hide/infox2.log'
   infof = kred+'Followup/'+root+'/infox2.log'
   outfile = root+'.des_ast'

   ; Before starting, check to see if the output file already exists, if
   ;  it does, do not continue unless the override is set.
   if exists(outfile) and not overwrite then begin
      print,'Output file, ',outfile,' already exists.  Cannot continue.'
      print,'Use /OVERWRITE flag if you really want to proceed.'
      return
   endif

   ; First, look for the infox2.log file, don't do anything if this file
   ;  does not exist.
   if not exists(info) and not exists(infoh) and not exists(infof) then begin
      print,info,' not found.'
      print,'Unable to continue.'
      return
   endif

   ; Next, try to locate the astrometry data from this night, look in astdir
   ;   and in astdir/Submitted, there should be only one copy.  If the one
   ;   copy is NOT in astdir/Submitted, generate a warning message.
   ast1file = astdir+root+'.ast'
   ast2file = astdir+'Submitted/'+root+'.ast'
   ast1found = exists(ast1file)
   ast2found = exists(ast2file)
   if not ast1found and not ast2found then begin
      print,'No astrometry for ',root,' found in ',astdir
      return
   endif
   if ast1found and ast2found then begin
      print,'Multiple copies of ',root,'.ast found in ',astdir
      print,'Unable to process this night until data properly filed.'
      return
   endif
   if ast1found then begin
;      print,'Warning! data have not yet been filed as Submitted.'
      astfile = ast1file
   endif else begin
      astfile = ast2file
   endelse

   ; Crack the root name and build the date string for this dataset
   if strlen(root) eq 6 then begin
      yy = fix(strmid(root,0,2))
      if yy lt 90 then yy=yy+2000 else yy=yy+1900
      yy = strn(yy)
      mm = strmid(root,2,2)
      dd = strmid(root,4,2)
      datestr = yy+'-'+mm+'-'+dd
   endif else begin
      print,'Root name format not recognized.  Unable to continue.'
      return
   endelse

   ; Load the astrometry data file
   rdast,astfile,fn,jd,ra,dec,mag,obs,id
   npoints = n_elements(jd)

   jdstr,jd,300,jds
;   jds = '"'+jds+'"'

   print,datestr,npoints,' measurements, ',outfile

   openw,lout,outfile,/get_lun
   for i=0,npoints-1 do begin
      str=string(id[i],datestr,fn[i],jds[i],ra[i],dec[i],mag[i],obs[i], $
         format="(4(a,1x),f12.10,1x,f13.10,1x,f4.1,1x,a)")
      str=strcompress(str)
      printf,lout,str
   endfor
   free_lun,lout

end

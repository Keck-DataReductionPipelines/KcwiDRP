;+
; NAME:
;   mkdesobj
; PURPOSE:
;   Create data file to post into des.obj MySQL database from KBO search data
; DESCRIPTION:
; CATEGORY:
;   Database
; CALLING SEQUENCE:
;  mkdesobj,root
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
pro mkdesobj,root,OVERWRITE=overwrite

   self='mkdesobj:'
   if badpar(overwrite,[0,1,2,3],0,caller=self+' (OVERWRITE) ',default=0) then return

   kred = '/net/frakir/raid/reduced/'
   astdir = kred+'/db/'
   info = kred+root+'/infox2.log'
   infoh = kred+root+'/Hide/infox2.log'
   infof = kred+'Followup/'+root+'/infox2.log'
   outfile = root+'.des_obj'

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

   ; Next, try to locate the info from this night, look in astdir
   ;   and in astdir/Submitted, there should be only one copy.  If the one
   ;   copy is NOT in astdir/Submitted, generate a warning message.
   obj1file = astdir+root+'.info'
   obj2file = astdir+'Submitted/'+root+'.info'
   obj1found = exists(obj1file)
   obj2found = exists(obj2file)
   if not obj1found and not obj2found then begin
      print,'No information for ',root,' found in ',astdir
      return
   endif
   if obj1found and obj2found then begin
      print,'Multiple copies of ',root,'.info found in ',astdir
      print,'Unable to process this night until data properly filed.'
      return
   endif
   if obj1found then begin
;      print,'Warning! data have not yet been filed as Submitted.'
      objfile = obj1file
   endif else begin
      objfile = obj2file
   endelse

   ; Load the information file
   rdainfo,objfile,object,firstfile,nobs,rmag,rmagerr,rate,angle,arc,dt, $
                   vsig,sel,dist,nobj

   openw,lout,outfile,/get_lun
   for i=0,nobj-1 do begin
      words=strsplit(firstfile[i],'.',/extract)
      root = words[0]
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
         free_lun,lout
         return
      endelse
      str=string(object[i],datestr,nobs[i],rmag[i],rmagerr[i], $
                 rate[i],angle[i],arc[i],dt[i],vsig[i],sel[i], $
         format='(2(a,1x),i2,2(1x,f10.1),1x,f10.2,3(1x,f10.1),1x,f10.2,1x,i3)')
      str=strcompress(str)
      printf,lout,str
   endfor
   free_lun,lout

   print,datestr,nobj,' objects, ',outfile

end

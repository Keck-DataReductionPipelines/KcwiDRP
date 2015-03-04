;+
; NAME:
;  objprmt
; PURPOSE:
;  Promote version of an object list file to highest version.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  objprmt,objfile,[path=]
; INPUTS:
;  objfile - Object list file to promote
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;    KEYLIST=      : Name of a file containing a correspondence list. This list
;                    associates a set of standard names with the actual keyword
;                    names found in a FITS file header. If this keyword is
;                    omitted, a default list is used, as if a file with the
;                    following contents had been supplied:
;                     AIRMASS   K  AIRMASS
;                     DATE      K  DATE-OBS
;                     DATETMPL  T  DD-MM-YYYY
;                     EXPDELTA  V  0.0
;                     EXPTIME   K  EXPTIME
;                     FILTER    K  FILTERS
;                     FILENAME  K  CCDFNAME
;                     OBJECT    K  OBJECT
;                     UT        K  UT 
;                    The middle column is a flag. It may be K, for Keyword,
;                    T, for Template, or V, for Value. If it is V, the contents
;                    of the third field on that line should make sense for the
;                    name in the first field.
;
;  PATH - Directory where image is stored that corresponds to the obj file.
;          This will be required when going from version 0 to version 1.
;
; OUTPUTS:
;
;  The object file is updated to the most recent version.  Not changed if
;    already current.
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
;  99/02/19, Written by Marc W. Buie, Lowell Observatory
;-
pro objprmt,objfile,PATH=in_path,KEYLIST=in_keylist

   if badpar(objfile,7,0,caller='OBJPRMT: (objfile) ') then return
   if badpar(in_path,[0,7],0,caller='OBJPRMT: (PATH) ',default='') then return

   path=in_path
   if path ne '' then path=addslash(path)

   ; If not present, don't do anything.
   if not exists(objfile) then return

   ; Check the file version.
   version=''
   openr,lun,objfile,/get_lun
   readf,lun,version,format='(a)'
   free_lun,lun

   ; It's current, do nothing.
   latest='OBJLIST v1.0'
   if version eq latest then return

   ; Read in the current contents.
   rdoblist,objfile,nobj,filelist,dt,offset,pos,flags,idstr,nfiles, $
      version=oldver

   print,'OBJPRMT: Upgrading ',objfile,' from ',oldver,' to ',latest

   ; If there's more than one file, get the dt vector
   if nfiles gt 1 then begin

      ; Load the keyword list
      if keyword_set( in_keylist ) then $
         loadkeys,in_keylist,hdrlist $
      else $
         loadkeys,'[[DEFAULT]]',hdrlist

      ; Now, we must locate the original images and fetch the start time of the
      ;   exposure.
      jd=dblarr(nfiles)
      for i=0,nfiles-1 do begin
         ; check to see if image file exists
         if not exists(path+filelist[i]) then begin
            print,'OBJPRMT: Image file ',path+filelist[i],' not found.'
            print,'Unable to upgrade file.'
            return
         endif
         hdr=headfits(path+filelist[i])
         parsekey,hdr,hdrlist,info
         jd[i]=info.jd
      endfor
      dt = (jd[1:nfiles-1]-jd[0])*24.0d0
   endif

   ; Write the updated file.
   wroblist,objfile,nobj,filelist,dt,offset,pos,flags,idstr,nfiles

end

;+
; NAME:
;  redfits
; PURPOSE:
;  Apply standard CCD processing steps to a raw CCD image.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;
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
PRO redfits, root, outdir, tag, $
       PATH=path, CALIBPATH=calibpath, KEYLIST=keylist, CALIBFILE=calibfile, $
       CLEAN=cleanimage

   if badpar(root,7,0,caller="REDFITS: (root) ") then return
   if badpar(outdir,[0,7],0,caller="REDFITS: (outdir) ", $
                                   default="./") then return
   outdir=addslash(outdir)
   if badpar(tag,[0,7],0,caller="REDFITS: (tag) ", $
                                   default="c") then return

   if badpar(path,[0,7],0,caller="REDFITS: (PATH) ", $
                                   default="./") then return
   path=addslash(path)
   if badpar(calibpath,[0,7],0,caller="REDFITS: (CALIBPATH) ", $
                                   default=path+"calib/") then return
   calibpath=addslash(calibpath)
   if badpar(calibfile,[0,7],0,caller="REDFITS: (CALIBFILE) ", $
                                   default="files.cal") then return
   if badpar(keylist,[0,7],0,caller="REDFITS: (KEYLIST) ", $
                                   default="[[DEFAULT]]") then return
   if badpar(cleanimage,[0,1,2,3],0,CALLER='REDFITS: (CLEAN) ',default=0) then return

   ; Get the list of images to reduce.
   spawn,'cd '+path+' ; ls '+root+'.*',filename
   IF filename[0] eq '' THEN BEGIN
      print,'No files to reduce.  Check directory and root of filename.'
      print,'Dir=[',path,']  root=[',root,']'
      return
   ENDIF
   nfiles=n_elements(filename)
   print,nfiles,' files to be reduced.'

   ; Get the header decoding keys
   loadkeys,keylist,hdrlist

   ; Load the calibration information
   ldcalib,calibfile,calib,valid,CALIBPATH=calibpath
   IF NOT valid THEN BEGIN
      print,'Calibration file not valid.'
      return
   ENDIF

   ; Process all the images
   FOR i=0,nfiles-1 DO BEGIN
      fnf=strsplit(filename[i],'.',/extract)
      outname=fnf[0]+tag+'.'+fnf[1]
      image = readfits(path+filename[i],hdr,/silent)
      parsekey,hdr,hdrlist,info
      flatcode = where(info.filter eq calib.filter,count)
      flatcode=flatcode[0]
      szi = size(image)

      IF flatcode eq -1 THEN BEGIN
         warn = ' missing flat '+info.filter
      ENDIF ELSE BEGIN
         warn = ''
         szf = size(calib.flat[*,*,flatcode])
         if szf[1] ne szi[1] or szf[2] ne szi[2] then begin
            print,filename[i],' image size does not match flat, skipping.'
            goto,skip_proc
         endif
      ENDELSE

      if ( calib.xl ge 0 and calib.xl ge szi[1] ) or $
         ( calib.xr ge 0 and calib.xr ge szi[1] )      then begin
         print,filename[i],' image size inconsistent with overscan area, skipping.'
         goto,skip_proc
      endif

      if calib.x2 ge szi[1] or calib.y2 ge szi[2] then begin
         print,filename[i],' image size inconsistent with cropping area, skipping.'
         goto,skip_proc
      endif

      print,filename[i],' ',outname,' ',warn,'  ',systime()

      ; Overscan correction.
      if calib.xl ge 0 and calib.xr ge 0 then $
         os = mean( image[calib.xl:calib.xr,*] ) $
      else $
         os = 0.0

      if n_elements(calib.dark) eq 1 then begin

         if flatcode eq -1 then $
            image = (image[calib.x1:calib.x2,calib.y1:calib.y2] - calib.bias - os) $
         else $
            image = (image[calib.x1:calib.x2,calib.y1:calib.y2] - calib.bias - os) / $
                     calib.flat[*,*,flatcode]

      endif else begin

         if flatcode eq -1 then $
            image = (image[calib.x1:calib.x2,calib.y1:calib.y2] - calib.bias - $
                                                      calib.dark*info.exptime - os ) $
         else $
            image = (image[calib.x1:calib.x2,calib.y1:calib.y2] - calib.bias - $
                                                      calib.dark*info.exptime - os ) / $
                                 calib.flat[*,*,flatcode]

      endelse

      sxaddpar,hdr,'BITPIX',-32
      IF cleanimage THEN BEGIN
         acre,image,new,5,4
         sxaddpar,hdr,'HISTORY','REDFITS calibrated image, with cosmic ray cleaning'
         writefits,outdir+outname,new,hdr
      ENDIF ELSE BEGIN
         sxaddpar,hdr,'HISTORY','REDFITS calibrated image'
         writefits,outdir+outname,image,hdr
      ENDELSE

skip_proc:

   ENDFOR

   print,'Processing complete.  ',systime()

END

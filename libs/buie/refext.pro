;+
; NAME:
;  refext
; PURPOSE:
;  Batch mode extraction of REFNET star catalog fields in support of ASTROM
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  refext,root,startnum,finnum
; INPUTS:
;  startnum - First frame number to process
;  finnum   - Last frame number to process
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;
;  CATPAD   - Amount of extra padding for catalog extraction in arcsec.  The
;                default is zero.
;
;  EXTLIST  - If image is a multi-extension FITS image, this list will
;                force the reduction of only the extension numbers listed.
;                The default is to do all the extensions, one at a time.
;
;  KEYLIST  - Name of a file containing a correspondence list. This list
;                associates a set of standard names with the actual keyword
;                names found in a FITS file header. If this keyword is
;                omitted, a default list is used, as if a file with the
;                following contents had been supplied:
;                   AIRMASS   K  AIRMASS
;                   DATE      K  DATE-OBS
;                   DATETMPL  T  DD-MM-YYYY
;                   EXPDELTA  V  0.0
;                   EXPTIME   K  EXPTIME
;                   FILTER    K  FILTERS
;                   FILENAME  K  CCDFNAME
;                *  OBJECT    K  OBJECT
;                   UT        K  UT 
;                *  RA        K  RA
;                *  DEC       K  DEC
;                *  EPOCH     K  EPOCH
;                The middle column is a flag. It may be K, for Keyword,
;                T, for Template, or V, for Value. If it is V, the contents
;                of the third field on that line should make sense for the
;                name in the first field.  Only those fields marked with '*'
;                are actually used by this program.
;
;  MAGLIM   - Limiting (faint) magnitude for catalog extraction (default=30.0)
;
;  NODOT    - Flag, if set suppresses inserting a . in the file name
;
;  PATH     - String, this is the name of the directory where the data are
;                stored.  The actual data directory used is PATH+'/'+root.
;                The default is '' (blank) and the file would be root.NNN
;                which would permit putting a leading path on the root.
;
;  PSCALE   - Scalar number, this is the scale of the image in arcsec/pixel.
;                If not specified, then this program will look for the
;                ASTROM information file, astrom.inf, in the current directory
;                for the plate scale.  If the file does not exist, then you
;                will be prompted to provide the plate scale directly.
;
;  SCRIPT   - String, if provided is the file name to save a script to.
;                If set will suppress the actual call to refnet to
;                generate the star sub-catalogs.  Instead, all the calls
;                to refnet will be saved to this file.
;
;  NOBINARY - Flag, if set will suppress automatically promoting the ASCII
;                catalog files to the binary format.
;
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  97/11/13 - Original version written by Gerbs Bauer, Institute for Astronomy
;  97/11/14, MWB, stylistic rewrite
;  98/03/25, MWB, add EXTLIST keyword and group extension support
;  98/04,    MWB, changed readfits to fits_read.
;  99/06/21, MWB, added NODOT keyword
;  2000/02/05, MWB, minor mod to add .fits tag support.
;  2000/08/25, MWB, added CATPATH pass through for refnet.
;  2001/12/07, MWB, added CATPAD (same as in astrom.pro).
;  2002/02/06, MWB, removed CATPATH keyword
;  2003/05/31, MWB, changed to prevent overwriting pre-existing cat files
;  2007/11/30, MWB, changed to support the new binary file formats.
;-
pro refext,root,startnum,finnum, $
           PATH=in_path,KEYLIST=keylist,SCRIPT=script, $
           PSCALE=pscale,MAGLIM=maglim,EXTLIST=extlist,NODOT=nodot, $
           CATPAD=catpad,NOBINARY=nobinary

   if badpar(root,7,0,CALLER='REFEXT: (root) ') then return
   if badpar(startnum,[2,3],0,CALLER='REFEXT: (startnum) ') then return
   if badpar(finnum,[0,2,3],0,CALLER='REFEXT: (finnum) ', $
                              default=startnum) then return

   if badpar(in_path,[0,7],0,CALLER='REFEXT: (PATH) ', $
                             default='[[DEFAULT]]') then return
   if badpar(keylist,[0,7],0,CALLER='REFEXT: (KEYLIST) ', $
                              default='[[DEFAULT]]') then return
   if badpar(script,[0,7],0,CALLER='REFEXT: (SCRIPT) ', $
                              default='[[DEFAULT]]') then return
   if badpar(pscale,[0,3,4,5,6],0,CALLER='REFEXT: (PSCALE) ', $
                              default=-1.0) then return
   if badpar(maglim,[0,2,3,4,5],0,CALLER='REFEXT: (MAGLIM) ', $
                              default=30.0) then return
   if badpar(extlist,[0,1,2,3],[0,1],CALLER='REFEXT: (EXTLIST) ', $
                              default=-1) then return
   if badpar(nodot,[0,1,2,3],[0,1],CALLER='REFEXT: (NODOT) ', $
                              default=0) then return
   if badpar(catpad,[0,2,3,4,5],0,CALLER='REFEXT: (CATPAD) ', $
                              default=0.0) then return
   if badpar(nobinary,[0,1,2,3],[0,1],CALLER='REFEXT: (NOBINARY) ', $
                              default=0) then return

   ; Set the path to where the data reside.
   IF in_path eq '[[DEFAULT]]' THEN BEGIN
      path = ''
   ENDIF ELSE BEGIN
      path = addslash(in_path)+root+'/'
   ENDELSE

   ; Get header correspondence list.
   loadkeys,keylist,hdrlist

   ; Setup output script file (if needed).
   dorefnet = script eq '[[DEFAULT]]'
   if not dorefnet then begin
      print,'Saving refnet calls to script file: ',script
      openw,lref,script,/get_lun
   endif

   donecat=''

   if nodot then dot = '' else dot = '.'

   FOR fileno=startnum,finnum DO BEGIN

      suffix=string(fileno,format='(i3.3)')
      imfile=root+dot+suffix
      if exists(path+imfile+'.fits') then ft='.fits' else ft=''

      IF exists(path+imfile+ft) THEN BEGIN

         fits_read,path+imfile+ft,dd,hdr,/header_only
;         hdr=headfits(path+imfile+ft)
         numext=sxpar(hdr,'NEXTEND')

         IF numext eq 0 THEN BEGIN
            extlist=0
         ENDIF ELSE BEGIN
            IF extlist[0] eq -1 THEN BEGIN
               extlist=indgen(numext)+1
            ENDIF ELSE BEGIN
               IF max(extlist) gt numext THEN BEGIN
                  print,'ASTROM: Input extension list is incompatible with ', $
                        'the number of extensions'
                  print,'in the file.  This file had ',numext,' extensions ', $
                        'and the largest item in'
                  print,'your list is ',max(extlist)
                  print,'Aborting.'
                  return
               ENDIF ELSE IF min(extlist) le 0 THEN BEGIN
                  print,'ASTROM: Input extension list is invalid.  You have', $
                        ' one or more values less'
                  print,'than or equal to zero.'
                  print,'Aborting.'
                  return
               ENDIF
            ENDELSE
         ENDELSE
         numext=n_elements(extlist)

         FOR ext=0,numext-1 DO BEGIN

            IF extlist[ext] eq 0 THEN BEGIN
               astinf = 'astrom.inf'
               exttag = ''
               extstr = ''
            ENDIF ELSE BEGIN
               extstr = strb36(extlist[ext])
               astinf = 'astrom'+extstr+'.inf'
               exttag = 'x'+extstr
            ENDELSE

            ; Get information from header
            fits_read,path+imfile+ft,dd,hdr,/header_only,exten=extlist[ext]
;            hdr=headfits(path+imfile+ft,exten=extlist[ext])
            parsekey,hdr,hdrlist,info
            starfile=strlowcase(nobname(strcompress(strtrim(info.object,2))))+ $
                       exttag + '.cat'
            naxis1=sxpar(hdr,'NAXIS1')
            naxis2=sxpar(hdr,'NAXIS2')
            IF info.ra lt 0.0 THEN BEGIN
               print,'There are no valid coordinates in the header, you must enter them by hand.'
               ans=''
               read,ans,prompt='RA of image center  (J2000) ? ',format='(a)'
               info.ra=raparse(ans)
               read,ans,prompt='Dec of image center (J2000) ? ',format='(a)'
               info.dec=decparse(ans)
               info.epoch=2000.0
               raoff  = 0.0
               decoff = 0.0
            ENDIF
            IF info.epoch NE 2000.0 THEN BEGIN
               precess,info.ra,info.dec,info.epoch,2000.0,/radian
               info.epoch=2000.0
            ENDIF

         ; If plate scale not set with keyword, try to get value from astrom.inf
            IF exists(astinf) THEN BEGIN
               version=''
               openr,lun,astinf,/get_lun
               readf,lun,version,format='(a)'
               if version ne 'ASTROM v1.0' then begin
                  print,'Illegal astrom.inf file, version tag is wrong.'
               endif else begin
                  readf,lun,raoff,decoff
                  readf,lun,pscale
               endelse
               free_lun,lun
            ENDIF ELSE BEGIN
               raoff=0.
               decoff=0.
            ENDELSE

            ; If plate scale still not good, get value from user
            WHILE pscale le 0.0 DO BEGIN
               read,pscale,prompt='Image scale? (arcsec/pixel)  '
            ENDWHILE

            ; Compute size of region to extract
            psize=fix(sqrt(naxis1^2+naxis2^2)*pscale*1.05+0.5+catpad)

            ; Shift coordinate by astrom.inf offset.
            info.ra  = info.ra  + raoff*cos(info.dec)
            info.dec = info.dec + decoff

            z=where(starfile eq donecat,count)
            IF count eq 0 THEN BEGIN
               IF dorefnet THEN BEGIN
                  if not exists(starfile) then begin
                     print,'Generating ',starfile,' with refnet.'
                     refnet,info.ra,info.dec,psize,psize,maglim,maglim,starfile
                     if not nobinary then begin
                        print,'Promote ',starfile,' to binary format'
                        starcprmt,starfile
                     endif
                  endif else begin
                     print,starfile,' already exists, skipping.'
                  endelse
               ENDIF ELSE BEGIN
                  rastr,info.ra,1,ras
                  decstr,info.dec,0,decs
                  ras="'"+ras+"'"
                  decs="'"+decs+"'"
                  file="'"+starfile+"'"
                  printf,lref,'print,"Generating ',starfile,' with refnet."'
                  printf,lref,ras,decs,psize,psize,maglim,maglim,file, $
                              format='("refnet,",a,",",a,",",f10.1,","' + $
                                     ',f10.1,",",f5.1,",",f5.1,",",a)'
                  if not nobinary then begin
                     printf,'print,"Promote ',starfile,' to binary format"'
                     printf,lref,'starcprmt,'+starfile
                  endif
               ENDELSE
               donecat = [donecat,starfile]
            ENDIF

         ENDFOR

      ENDIF ELSE BEGIN
         print,'File: ',path+imfile,' not found.'
      ENDELSE

   ENDFOR

   IF not dorefnet THEN free_lun,lref

END

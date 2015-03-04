;+
; NAME:
;  plastext
; PURPOSE:
;  Batch mode extraction of PLAST asteroid lists.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  plastext,root,startnum,finnum
; INPUTS:
;  startnum - First frame number to process
;  finnum   - Last frame number to process
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;
;  EXTLIST  - If image is a multi-extension FITS image, this list will
;                force the reduction of only the extension numbers listed.
;                The default is to do all the extensions, one at a time.
;
;  FULL     - Flag, if set, causes this program to call the original 'plast'
;                program.  The original is much slower but can be more accurate
;                since it does full formal orbit calculations with integration
;                for every step.  Warning, the speed difference is more than
;                a factor of 100 and the differential gain is likely to be
;                imperceptibly small.
;
;  KEYLIST  - Name of a file containing a correspondence list. This list
;                associates a set of standard names with the actual keyword
;                names found in a FITS file header. If this keyword is
;                omitted, a default list is used, as if a file with the
;                following contents had been supplied:
;                   AIRMASS   K  AIRMASS
;                *  DATE      K  DATE-OBS
;                *  DATETMPL  T  DD-MM-YYYY
;                   EXPDELTA  V  0.0
;                   EXPTIME   K  EXPTIME
;                   FILTER    K  FILTERS
;                   FILENAME  K  CCDFNAME
;                *  OBJECT    K  OBJECT
;                *  UT        K  UT 
;                *  RA        K  RA
;                *  DEC       K  DEC
;                *  EPOCH     K  EPOCH
;                The middle column is a flag. It may be K, for Keyword,
;                T, for Template, or V, for Value. If it is V, the contents
;                of the third field on that line should make sense for the
;                name in the first field.  Only those fields marked with '*'
;                are actually used by this program.
;
;  MAGLIM   - Limiting (faint) magnitude for asteroid extraction (default=30.0)
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
;                If set will suppress the actual call to plast to
;                generate the asteroid lists.  Instead, all the calls
;                to plast will be saved to this file.
;
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  97/11/24, MWB, cloned from REFEXT.PRO
;  98/03/25, MWB, add EXTLIST keyword and group extension support
;  2000/02/05, MWB, minor mod to add .fits tag support.
;  2002/08/23, MWB, added support for new plast program.
;  2002/10/23, MWB, changed headfits call to fits_read for group data.
;-
pro plastext,root,startnum,finnum, $
           PATH=in_path,KEYLIST=keylist,SCRIPT=script, $
           PSCALE=pscale,MAGLIM=maglim,EXTLIST=extlist,FULL=full

   if n_params() eq 0 then begin
      print,'plastext,root,num1, [num2]'
      return
   endif

   if badpar(root,7,0,CALLER='PLASTEXT: (root) ') then return
   if badpar(startnum,[2,3],0,CALLER='PLASTEXT: (startnum) ') then return
   if badpar(finnum,[0,2,3],0,CALLER='PLASTEXT: (finnum) ', $
                              default=startnum) then return

   if badpar(in_path,[0,7],0,CALLER='PLASTEXT: (PATH) ', $
                             default='[[DEFAULT]]') then return
   if badpar(keylist,[0,7],0,CALLER='PLASTEXT: (KEYLIST) ', $
                              default='[[DEFAULT]]') then return
   if badpar(script,[0,7],0,CALLER='PLASTEXT: (SCRIPT) ', $
                              default='[[DEFAULT]]') then return
   if badpar(pscale,[0,3,4,5,6],0,CALLER='PLASTEXT: (PSCALE) ', $
                              default=-1.0) then return
   if badpar(maglim,[0,2,3,4,5],0,CALLER='PLASTEXT: (MAGLIM) ', $
                              default=30.0) then return
   if badpar(extlist,[0,1,2,3],[0,1],CALLER='EXTLIST: (EXTLIST) ', $
                              default=-1) then return
   if badpar(full,[0,1,2,3],0,CALLER='EXTLIST: (FILE) ', $
                              default=0) then return

   ; Set the path to where the data reside.
   if in_path eq '[[DEFAULT]]' then begin
      path = ''
   endif else begin
      path = addslash(in_path)+root+'/'
   endelse

   ; Get header correspondence list.
   loadkeys,keylist,hdrlist

   ; Setup output script file (if needed).
   doplast = script eq '[[DEFAULT]]'
   if not doplast then begin
      print,'Saving plast calls to script file: ',script
      openw,lpla,script,/get_lun
   endif

   doneplast=''

   if not full and doplast then begin
      if not exists('allsky.plast') then begin
         code=807
         y = fix(strmid(root,0,2))
         if y lt 90 then y=y+2000 else y=y+1900
         m = fix(strmid(root,2,2))
         d = fix(strmid(root,4,2))
         jds = strn(y)+'/'+strn(m)+'/'+strn(d)
         jdy=jdparse(jds)
         if code eq 807 then jdy=jdy+1
         jdstr,jdy,0,jds
         openw,luntmp,'plast.in',/get_lun
         printf,luntmp,strmid(jds,0,4),',',strmid(jds,5,2),',',strmid(jds,8,2)
         printf,luntmp,code
         printf,luntmp,'y'
         free_lun,luntmp
         print,'Generating allsky.plast accelerator file.'
         spawn,'plastset < plast.in > /dev/null ; ' + $
               'rm plast.in'
      endif
   endif

   for fileno=startnum,finnum do begin

      suffix=string(fileno,format='(i3.3)')
      imfile=root+'.'+suffix
      if exists(path+imfile+'.fits') then ft='.fits' else ft=''

      if exists(path+imfile+ft) then begin

         hdr=headfits(path+imfile+ft)
         numext=sxpar(hdr,'NEXTEND')

         if numext eq 0 then begin
            extlist=0
         endif else begin
            if extlist[0] eq -1 then begin
               extlist=indgen(numext)+1
            endif else begin
               if max(extlist) gt numext then begin
                  print,'ASTROM: Input extension list is incompatible with ', $
                        'the number of extensions'
                  print,'in the file.  This file had ',numext,' extensions ', $
                        'and the largest item in'
                  print,'your list is ',max(extlist)
                  print,'Aborting.'
                  return
               endif else if min(extlist) le 0 then begin
                  print,'ASTROM: Input extension list is invalid.  You have', $
                        ' one or more values less'
                  print,'than or equal to zero.'
                  print,'Aborting.'
                  return
               endif
            endelse
         endelse
         numext=n_elements(extlist)

         for ext=0,numext-1 do begin

            if extlist[ext] eq 0 then begin
               astinf = 'astrom.inf'
               exttag = ''
               extstr = ''
            endif else begin
               extstr = strb36(extlist[ext])
               astinf = 'astrom'+extstr+'.inf'
               exttag = 'x'+extstr
            endelse

            ; Get information from header
;            hdr=headfits(path+imfile+ft,exten=extlist[ext])
            fits_read,path+imfile+ft,data,hdr,exten=extlist[ext],/header_only
            parsekey,hdr,hdrlist,info
            plastfile=strlowcase(nobname(strcompress(strtrim(info.object,2))))+ $
                        exttag + '.pla' 
            naxis1=sxpar(hdr,'NAXIS1')
            naxis2=sxpar(hdr,'NAXIS2')
            if info.ra lt 0.0 then begin
               print,'There are no valid coordinates in the header, you must enter them by hand.'
               ans=''
               read,ans,prompt='RA of image center  (J2000) ? ',format='(a)'
               info.ra=raparse(ans)
               read,ans,prompt='Dec of image center (J2000) ? ',format='(a)'
               info.dec=decparse(ans)
               info.epoch=2000.0
               raoff  = 0.0
               decoff = 0.0
            endif
            if info.epoch NE 2000.0 then begin
               precess,info.ra,info.dec,info.epoch,2000.0,/radian
               info.epoch=2000.0
            endif

         ; If plate scale not set with keyword, try to get value from astrom.inf
            if exists(astinf) then begin
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
            endif

            ; If plate scale still not good, get value from user
            while pscale le 0.0 do begin
               read,pscale,prompt='Image scale? (arcsec/pixel)  '
            endwhile

            ; Compute size of region to extract
            psize=fix(sqrt(naxis1^2+naxis2^2)*pscale*1.05+0.5)

            ; Shift coordinate by astrom.inf offset.
            info.ra  = info.ra  + raoff*cos(info.dec)
            info.dec = info.dec + decoff

            z=where(plastfile eq doneplast,count)
            if count eq 0 then begin
               if doplast then begin
                  print,'Generating "plast" extraction for ',plastfile
                  plast,info.jd,info.ra,info.dec,psize,psize,plastfile, $
                     title=imfile,maglim=maglim,oldplast=(full ne 0)
               endif else begin
                  rastr,info.ra,1,ras
                  decstr,info.dec,0,decs
                  ras="'"+ras+"'"
                  decs="'"+decs+"'"
                  file="'"+plastfile+"'"
                  ifile="'"+imfile+"'"
                  if full then oldpla=',/oldplast' else oldpla=''
                  printf,lpla,'print,"Generating plast extraction for ',plastfile,'"'
                  printf,lpla,info.jd,ras,decs,psize,psize,file,ifile,maglim,oldpla, $
                              format='("plast,",f13.5,"d0,",a,",",a,",",f10.1,","' + $
                                     ',f10.1,",",a,",","title=",a,",maglim=",f4.1,a)'
               endelse
               doneplast = [doneplast,plastfile]
            endif

         endfor

      endif else begin
         print,'File: ',path+imfile,' not found.'
      endelse

   endfor

   if not doplast then free_lun,lpla

END

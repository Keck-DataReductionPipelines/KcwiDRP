;+
; NAME: 
;  saltdes2
; PURPOSE:   (one line only)
;  Implant fake objects into DES data, second stage addition of objects.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  saltdes2
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  KEYFILE   - Name of a file containing a correspondence list. This list
;                 associates a set of standard names with the actual keyword
;                 names found in a FITS file header. If this keyword is
;                 omitted, a default list is used, as if a file with the
;                 following contents had been supplied:
;                  AIRMASS   K  AIRMASS
;                  DATE      K  DATE-OBS
;                  DATETMPL  T  DD-MM-YYYY
;                  EXPDELTA  V  0.0
;                  EXPTIME   K  EXPTIME
;                  FILTER    K  FILTERS
;                  FILENAME  K  CCDFNAME
;                  OBJECT    K  OBJECT
;                  UT        K  UT 
;                 The middle column is a flag. It may be K, for Keyword,
;                 T, for Template, or V, for Value. If it is V, the contents
;                 of the third field on that line should make sense for the
;                 name in the first field.
;
;  IMDIR      :String, the path to the original images- DEFAULT:current dir
;  PSFDIR     :String, the path to the psfs- DEFAULT:SRCDIR+'/psf'
;  SRCDIR     :String, the path to the src and srd files- DEFAULT:current dir
;  OUTDIR     :String, set the path for the output data file. IMSALT
;              will not run if imdir=outdir - DEFAULT:'salted/'
;  QUIET      :Flag, if set will suppress chatty printed output.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  For use with the DES.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/20, Written by Marc W. Buie, Lowell Observatory
;  2010/07/26, MWB, minor mod for new frmdxdy error codes
;-
pro saltdes2,IMDIR=imdir,SRCDIR=srcdir,OUTDIR=outdir, $
            QUIET=quiet,PSFDIR=psfdir,KEYFILE=keyfile

   self='SALTDES2: '
   if badpar(imdir,[0,7],0,CALLER=self+'(IMDIR) ',DEF='') then return
   if imdir eq '' then begin
      cd,current=imdir
   endif
   imdir=addslash(imdir)

   if badpar(srcdir,[0,7],0,CALLER=self+'(SRCDIR) ',DEF='') then return
   srcdir=addslash(srcdir)

   if badpar(outdir,[0,7],0,CALLER=self+'(OUTDIR) ', $
                             default=imdir+'salted') then return
   outdir=addslash(outdir)

   if badpar(psfdir,[0,7],0,CALLER=self+'(PSFDIR) ', $
                           default=srcdir+'psf') then return
   psfdir=addslash(psfdir)

   if badpar(quiet,[0,1,2,3],0,CALLER=self+'(QUIET) ',default=0) then return 
   if badpar(keyfile,[0,7],0,caller=self+'(KEYFILE) ', $
                default='nodefault') then return

   loadkeys,keyfile,hdrlist,foundit=foundkey
   if not foundkey then begin
      print,self,'Keyfile ',keyfile,' could not be loaded.  Aborting.'
      return
   endif
 
   ; figure out the root of the reduction area from the image directory
   root = strmid(imdir,strlen(imdir)-7,6)

   ; read the "master" matchfile (root+'.match'), this will provide a list
   ;   of related images.
   fnmatch = root+'.match'
   if not exists(srcdir+fnmatch) then begin
      print,self,'The match file, ',fnmatch,' cannot be found in the reduction'
      print,self,'directory ',srcdir
      print,self,'This file must exist and must contain all the file sets to be'
      print,self,'scanned to add salted objects.  Aborting.'
      return
   endif
   openr,lun,srcdir+fnmatch,/get_lun
   line=''

   ndone = 0
   ; Loop over the fields found in the matchfile
   while not eof(lun) do begin

      ; Get the next line from the file and parse
      readf,lun,line,format='(a)'
      line=strcompress(line)
      if line eq '' then continue

      words=strsplit(line,' ',/extract)
      obj=strlowcase(words[0])
      nfiles = n_elements(words)-1
      if nfiles eq 2 or $
         (strmid(obj,0,1) ne 'f' and strmid(obj,0,1) ne 's') then continue
      fnlist = words[1:*]
      if not quiet then print,line

      ; Check in the input files and figure out if any (or all) need the
      ;  .fits suffix tag.
      ft   = strarr(nfiles)
      for i=0,nfiles-1 do begin
         if exists(imdir+fnlist[i]+'.fits') then begin
            ft[i] = '.fits'
         endif else if not exists(imdir+fnlist[i]) then begin
            print,self,'Image ',imdir+fnlist[i],' does not exist.  Quitting.'
            free_lun,lun
            return
         endif
      endfor

      ; Check the list of images and find out if any have not been salted.  It
      ;  is an error to find the first two NOT salted already if the field name
      ;  starts with f or s.
      todo = replicate(1B,nfiles)
      for i=0,nfiles-1 do if exists(outdir+fnlist[i]+ft[i]) then todo[i] = 0B
      znew=where(todo eq 1,numnew)
      if numnew eq 0 then continue

      ; Load the obj file for this field (just need chip 1 right now)
      rdoblist,obj+'x1.obj',nobj,filelist,dt,offset,pos,flags,idstr,nfiles
      if not quiet then begin
         print,'Field ',obj,', files in x1.obj ',strn(nfiles), $
               ' and there are ',strn(numnew),' new files.'
         print,'Need to add',fnlist[znew]
         print,'Anchor image ',imdir+fnlist[0]+ft[0]
      endif

      ; Collect information from the anchor image, need just the mid-time
      ;   from the header.
      hdr0 = headfits(imdir+fnlist[0]+ft[0])
      parsekey,hdr0,hdrlist,info
      jd0 = info.jd

      ; Get the information about fake objects on this field from the list
      ;  for the anchor image.
      saltfn0 = fnlist[0]+'.slt'
      if not exists(psfdir+saltfn0) then begin
         print,self,'Fatal error!  The salt file ',saltfn0, $
                    ' for the anchor frame could not be found.'
         return
      endif
      readcol,psfdir+saltfn0,ext,x0,y0,rmag,theta,angle,totangle,rate, $
         format='x,a,f,f,f,x,x,f,f,f,f'
      totangle /= !radeg
      ntotal=n_elements(ext)
      ext = fix(strmid(ext,1,99))
      x = fltarr(ntotal)
      y = fltarr(ntotal)

      ; make sure all the source files are present before starting to work
      ;   on this field
      error=0
      for j=1,8 do begin
         fnsrc = fnlist[0]+'.srdx'+strn(j)
         if not exists(srcdir+fnsrc) then begin
            print,self,'The source list file',srcdir+fnsrc,' does not exist.'
            error=1
         endif
      endfor
      for i=0,numnew-1 do begin
         for j=1,8 do begin
            fnsrc = fnlist[znew[i]]+'.srdx'+strn(j)
            if not exists(srcdir+fnsrc) then begin
               print,self,'The source list file',srcdir+fnsrc,' does not exist.'
               error=1
            endif
         endfor
      endfor
      if error then begin
         print,'Unable to process field ',obj,' due to missing src files.'
         continue
      endif

      ; Loop over the list of files to salt
      for i=0,numnew-1 do begin

         if not quiet then print,'Add objects to ',fnlist[znew[i]]

         ; snag the header information for the image to be salted.
         hdr = headfits(imdir+fnlist[znew[i]]+ft[znew[i]])
         parsekey,hdr,hdrlist,info
         jd = info.jd
         dtnew = (jd-jd0)*24.0

         xoff=fltarr(8)
         yoff=fltarr(8)

         ; Loop over the extensions
         for j=1,8 do begin

            z=where(j eq ext,nobj)
            if nobj eq 0 then continue

            ; Load the source list for the anchor frame
            fnsrc = fnlist[0]+'.srdx'+strn(j)
            data = readfits(fnsrc,hdrsrc,/silent)
            idx0=sort(data[*,3])
            nsel0 = n_elements(idx0)/9 < 1000
            srcx0 = data[idx0[0:nsel0-1],0]
            srcy0 = data[idx0[0:nsel0-1],1]

            ; Load the source list for the new frame
            fnsrc = fnlist[znew[i]]+'.srdx'+strn(j)
            data = readfits(fnsrc,hdrsrc,/silent)
            idx1=sort(data[*,3])
            nsel1 = n_elements(idx1)/9 < 1000
            srcx1 = data[idx1[0:nsel1-1],0]
            srcy1 = data[idx1[0:nsel1-1],1]

            ; need the dx,dy frame offset between new frame and anchor
            frmdxdy,srcx0,srcy0,srcx1,srcy1,xoff0,yoff0,error
            if error gt 0 then begin
               print,self,'Error in finding frame offset=',error
            endif
            print,'offset',xoff0,yoff0
            xoff[j-1] = xoff0
            yoff[j-1] = yoff0

            ; compute the positions of the fake objects on this chip
            x[z]=round(x0[z]+rate[z]/0.52*dtnew*cos(totangle[z])+xoff0)
            y[z]=round(y0[z]+rate[z]/0.52*dtnew*sin(totangle[z])+yoff0)

         endfor

         ; salt the image
         if not quiet then print,'Salting ',imdir+fnlist[znew[i]]+ft[znew[i]]
         imsalt,fnlist[znew[i]]+ft[znew[i]],x,y,rmag,ext, $
                imdir=imdir,srcdir=srcdir,keyfile=keyfile, $
                outdir=outdir,/noplots,/saltflag,quiet=quiet      

         ; add information to the obj file information, filename, offset, dt
         ;   and positions
         for j=1,8 do begin
            fnobj = srcdir+'psf/' + obj + 'x' + strn(j) + '.obj'
            print,'Reading  ',fnobj
            rdoblist,fnobj,nobj_obj,filelist,dt,offset,pos,flags,idstr,nfiles
            nfiles++
            filelist = [filelist,fnlist[znew[i]]]
            dt = [dt,dtnew]
            offset = [offset,xoff[j-1],yoff[j-1]]

            z=where(j eq ext,nobj)
            if nobj gt 0 then begin
               xnew = reform(x[z],1,nobj)
               ynew = reform(y[z],1,nobj)
               pos = [pos,xnew,ynew]
            endif
            print,'Writing ',fnobj
            wroblist,fnobj,nobj_obj,filelist,dt,offset,pos,flags,idstr,nfiles
         endfor

         ;updating the .SLT file-------------------
         ;open the .slt file created by imsalt.pro
         saltfn = fnlist[znew[i]]+'.slt'
         openr,lunslt,psfdir+saltfn,/get_lun
         ;open a new .slt file
         openw,olun,psfdir+saltfn+'temp',/get_lun
         tmpline=''
         ; write out the slt file
         for j=0,ntotal-1 do begin
            readf,lunslt,tmpline,format='(a)'
            ;printing into the new .slt file with all the added info.
            printf,olun,tmpline,theta[j],angle[j], $
                   totangle[j]*!radeg,rate[j],rate[j]/0.52*dtnew, $
                   format='(a,3(1x,f7.2),1x,f5.3,1x,f6.1)'
         endfor
         free_lun,lunslt,olun
         ;overwriting the old .slt file with the new .slt file that has the
         ;additional information
         file_move,psfdir+saltfn+'temp',psfdir+saltfn,/overwrite

      endfor

      ndone++

   endwhile

   free_lun,lun

   if not quiet and ndone eq 0 then begin
      print,self,'No files were processed.'
   endif

end

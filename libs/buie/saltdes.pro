;+
; NAME: 
;  saltdes
; PURPOSE:   (one line only)
;  Implant fake objects into all relevant images on one night of DES data.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  saltdes
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  KEYFILE   - Name of a file containing a header keyword correspondence list.
;  IMDIR      :String, the path to the original images- DEFAULT:current dir
;  PSFDIR     :String, the path to the psfs- DEFAULT:SRCDIR+'/psf'
;  SRCDIR     :String, the path to the src and srd files- DEFAULT:current dir
;  OUTDIR     :String, set the path for the output data file. IMSALT
;              will not run if imdir=outdir - DEFAULT:'salted/'
;  OVERWRITE  :Flag, if set will force program to re-salt previously salted
;                data.  Any prior salting products are removed and the fake
;                data is completely re-generated (and will be different).
;  QUIET      :Flag, if set will suppress chatty printed output.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  For use with the DES.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/14, Written by Maureen Teyssier, REU Lowell Observatory
;  2004/07/16, MWB, incorporated into library.
;  2004/07/17, MWB, added OVERWRITE keyword
;  2004/08/13, MWB, fixed bug with imsalt call on x2+ extensions
;  2004/10/15, MWB, fixed bug with ecliptic angle at RA=0
;  2004/11/10, MWB, fixed bug when no F or S fields.
;-
pro saltdes,IMDIR=imdir,SRCDIR=srcdir,OUTDIR=outdir,KEYFILE=keyfile, $
            QUIET=quiet,PSFDIR=psfdir,OVERWRITE=overwrite

   self='SALTDES: '
   if badpar(imdir,[0,7],0,CALLER=self+'(IMDIR) ',DEF='') then return
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
   if badpar(overwrite,[0,1,2,3],0,CALLER=self+'(OVERWRITE) ', $
                                   default=0) then return 
   if badpar(keyfile,[0,7],0,caller=self+'(KEYFILE) ', $
                default='nodefault') then return

   if not exists(keyfile) then begin
      print,self,'Header keyword correspondence list file ',keyfile, $
                 ' not found.  Aborting.'
      return
   endif

   ;-----------------------
   ;probabalistic sampling to find R magnitude, rate of motion, and direction
   bign=1000000L   
   prbh_rate=smplprb('num_mag',19,23.8,bign)
   prbn_rate=smplprb('num_rate',2,5,bign)
   prbangle=randomn(seed,bign)*(20./3.)

   ;----------------------- 
   ;choose a telescope:CTIO and KPNO
   whichT=fileline(srcdir+'astrom1.inf',3)

   ;-----------------------    
   ;obtain the unsalted .obj files
   objf=file_search(srcdir+'/f*.obj',count=nobjf)
   objs=file_search(srcdir+'/s*.obj',count=nobjs)

   ;run a general check for .obj files
   if (nobjf/8*8 ne nobjf and nobjf ne 0) or $
      (nobjs/8*8 ne nobjs and nobjs ne 0) then begin
      print,self,'ERROR: There are .obj files missing.'
      print,self,' There are ',strn(nobjf),' F field obj files and ', $
                 strn(nobjs),' S field obj files'
      return
   endif

   nobjfiles = 0
   if nobjf gt 0 then begin
      fvar   = strmid(objf,11,6,/reverse_offset)
      flist  = fvar[uniq(fvar,sort(fvar))]
      fslist = flist
      nobjfiles = n_elements(fslist)
   endif
   if nobjs gt 0 then begin
      svar= strmid(objs,13,8,/reverse_offset)
      slist=svar[uniq(svar,sort(svar))]
      if nobjfiles eq 0 then begin
         fslist = slist
      endif else begin
         fslist = [fslist,slist]
      endelse
      nobjfiles += n_elements(slist)
   endif

   if nobjfiles eq 0 then begin
      print,self,'There are no obj files to process for these data.  Quitting.'
      return
   endif

   ;-----------------------
   for ifield=0,nobjfiles-1 do begin;field loop

      objnames=strarr(8)

      ;read an unsalted .obj file
      rdoblist,srcdir+fslist[ifield]+'x2.obj', $     
               nobj_obj,filelist,dt,offset,pos,flags,idstr,nfiles

      if nfiles eq 0 then begin
         print,self,'Warning! ',fslist[ifield],'x2.obj has no files.  Skipping.'
         continue
      endif

      done=0
      for iext=1,8 do begin ;.obj loop- loops over extensions

         stext=strb36(iext)

         ;the unsalted .obj name
         oldobj=srcdir+fslist[ifield]+'x'+stext+'.obj'

         ;check for a specific unsalted .obj file
         if not exists(oldobj) then begin
            print,self,oldobj,'<- FILE MISSING!!'
            return
         endif

         ;the new salted .obj files
         objnames[iext-1]=psfdir+fslist[ifield]+'x'+stext+'.obj'

         ;check for salted .obj files
         if exists(objnames[iext-1]) then done++

      endfor

      if done gt 0 and done lt 8 then begin
         print,'The salted .OBJ files are incomplete.'
         print,'There were supposed to be 8, and',done,'were found.'
         return
      endif

      if not overwrite and done eq 8 then begin 
         if not quiet then $
            print,'The field: ',fslist[ifield], $
                  ' has already been processed, skipping.'
         continue
      endif 

      ;defining the parameters for the salt objects  
      nobj=20+round(randomu(seed,1)*10)
      exten=1+round(randomu(seed,nobj)*7)
      exten=exten[sort(exten)]
      x1=round(randomu(seed,nobj)*1024.)*1.
      y1=round(randomu(seed,nobj)*2048.)*1.
      rmag=prbh_rate[round(randomu(seed,nobj)*bign)]
      angle=prbangle[round(randomu(seed,nobj)*bign)]/!radeg
      rate=prbn_rate[round(randomu(seed,nobj)*bign)]

      if not quiet then begin
         print,self,'FSlist:',fslist[ifield]
         print,self,'FILELIST:',filelist
         print,self,'NUMBER OBJECTS/FILE:',nobj
      endif

      ;flags for the salted .obj files
      xyvals=fltarr(2*n_elements(filelist),nobj)
      sflag=replicate('s',nobj)
      saltflag=replicate('SALT',nobj)
      tdt=[0.,dt]

      for imfl=0,nfiles-1 do begin;matching file loop 

         ;check to see if the image has a .fits-the .OBJ files leave off .fits
         im=filelist[imfl]
         if exists(imdir+im+'.fits') then imname=im+'.fits'else imname=im

         if not quiet then print,self,'FILE:',imname

         if exists(psfdir+im+'.slt') eq 1 then file_delete,psfdir+im+'.slt'

         ; first frame is special since it is the anchor and rates and angles
         ;   don't apply.  This gets us started and we also learn RA,DEC here.
         if imfl eq 0 then begin

            ;salt the image!!!!!
            imsalt,imname,x1,y1,rmag,exten, $
                   imdir=imdir,srcdir=srcdir,keyfile=keyfile, $
                   outdir=outdir,ra=ra,dec=dec,/noplots,/saltflag,quiet=quiet      
            ;using ra and dec to find the angle of the ecliptic on the image
            ;and taking that angle to calculate the new x,y for the next
            ;matching file

            ;To find the angle of the ecliptic on the frame:    
            ;convert from ra,dec and jog the longitiude over, then
            ;convert back to ra,dec and find the angle of the vector
            euler,ra*!radeg,dec*!radeg,ao,bo,3 ; ra,dec->ecliptic
            a1=ao-1 ; negative to get retrograde motion
            b1=bo
            euler,a1,b1,ra1,dec1,4 ; ecliptic->ra&dec
            ra1  /= !radeg
            if ra1 gt !pi then ra1 -= 2.0*!pi
            dec1 /= !radeg
            ;choose a telescope:CTIO or KPNO
            if whichT gt 0 then begin ; KPNO
               thet=atan(-1.0*(ra1-ra),-1.0*(dec1-dec))
            endif else begin ; CTIO
               thet=atan((ra1-ra),(dec1-dec))
            endelse
            totangle=angle+thet
            x=x1
            y=y1
         endif else begin
     
            ;putting the corrected xy values from IMSALT into an array
            ;shifting the xy position for the next matching file
            x=round(x1+rate/0.52*tdt[imfl]*cos(totangle)+ $
                 offset[2*(imfl-1)])
            y=round(y1+rate/0.52*tdt[imfl]*sin(totangle)+ $
                 offset[2*(imfl-1)+1])

            ;salt the image!!!!!
            imsalt,imname,x,y,rmag,exten, $
                    imdir=imdir,srcdir=srcdir,keyfile=keyfile, $
                    outdir=outdir,/noplots,/saltflag,quiet=quiet

         endelse

         xyvals[(2*imfl),*]   = x
         xyvals[(2*imfl+1),*] = y 

         ;updating the .SLT file-------------------
         ;open the .slt file created by imsalt.pro
         openr,slt,psfdir+im+'.slt',/get_lun

         ;open a new .slt file
         openw,temp,psfdir+im+'.slttemp',/get_lun

         line=''
         ct=0
         while not eof(slt) do begin
            readf,slt,line,format='(a)'
            ;printing into the new .slt file with all the added info.
            printf,temp,line,thet*!radeg,angle[ct]*!radeg, $
                   totangle[ct]*!radeg,rate[ct],rate[ct]/0.52*tdt[imfl], $
                   format='(a,3(1x,f7.2),1x,f5.3,1x,f6.1)'
            ct+=1
         endwhile

         free_lun,slt,temp

         ;overwriting the old .slt file with the new .slt file that has the
         ;additional information
         file_move,psfdir+im+'.slttemp',psfdir+im+'.slt',/overwrite

      endfor;matching file loop

      ;writing the .obj files
      for next=1,8 do begin ;extension loop
         z=where(exten eq next,countit)
         if countit eq 0 then begin
            ;to create an empty file
            wroblist,objnames[next-1],countit,filelist,dt,offset,xyvals, $ 
                     sflag,saltflag,nfiles
         endif else begin
            ;to create a file with a list of salty objects
            wroblist,objnames[next-1],countit,filelist,dt,offset, $ 
                     xyvals[*,z],sflag[z],saltflag[z],nfiles  
         endelse
      endfor;extension loop

   endfor;field loop

   if not quiet then print,self,'COMPLETED'

end

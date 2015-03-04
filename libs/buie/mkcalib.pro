;+
; NAME:
;  mkcalib
; PURPOSE:
;  Interactive program for creating CCD calibration files.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  mkcalib
; INPUTS:
;  Everything is asked for interactively
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;   FILKEY = String - FITS keyword to read to get filter code, default = FILPOS
;     Note: If you have multiple filter wheels AND you have a set of keywords
;           in the form KEYn where n is a single digit number and KEY is a
;           static string pattern (eg., FILT_0, FILT_1, etc.), then give this
;           keyword as KEY* (ex: FILT_*).  When processed, you will get a
;           string back from the concatenation of all the filter strings,
;           separated by '+'.
;   EXPKEY = String - FITS keyword to read to get exposure time, default = EXPTIME
;   JUSTMEDIAN - Flag, if set will stop processing after the initial median
;                  combination of the image cube and this will be the final
;                  answer.  However, this is only honored if the NORMALIZE and
;                  PRESCALE flags are both set.  This should only be used if
;                  the intrinsic noise level of the result can never drop
;                  below 1 DN.  This flag only affects the flat field
;                  processing step.  The default is sigma clipping.
;
;   OVERSCAN = column overscan region to use for frame bias level,
;                 default=[515,531]
;   CROP     = region of original image to save, default=[0,511,0,511]
;   DDIR     = String, name of directory where data exists.  If not provided,
;                 the program will ask for it interactively.
;   RDNOISE  - Read noise of CCD, e-/DN, default=10
;   ROOT     = String, root of file name for data, usually YYMMDD (ut date).
;                 Program will ask for this if not provided.
;   SCALE - 4 element vector which, if provide, defines the region of the
;           array dimensions that are used to scale the mean
;           of the arrays before combining (.  If combined in this
;           manner, the arrays are combined weighted by the means.
;                 [x1,x2,y1,y2]  (Used for flats only.)
;           These coordinates apply to the pixel locations AFTER cropping.
; OUTPUTS:
;  All output is confined to files.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/01/07 - Marc W. Buie, Lowell Observatory
;  96/05/24 - MWB, added PICKFILE support, various bug fixes, improvements
;               to the files.cal file creation.
;  96/06/08 - MWB, added DDIR and ROOT
;  2000/02/06, MWB, added .fits optional tag on file name
;  2000/03/14, MWB, replaced spawn,copy with new Filecopy routine.
;  2000/03/24, MWB, substantial changes to make program platform independent.
;  2003/10/01, MWB, converted my Filecopy call to system file_copy routine
;                   converted my Filemove call to system file_move routine
;                   converted my Mkdir calls to IDL file_mkdir calls
;  2006/07/14, MWB, added RDNOISE keyword
;  2006/07/25, MWB, slight mod to support multiple filter wheels, plus changed
;                      PICKFILE calls to DIALOG_PICKFILE
;  2006/07/27, MWB, added JUSTMEDIAN keyword
;-
pro mkcalib,FILKEY=filkey,EXPKEY=expkey,OVERSCAN=in_overscan,CROP=in_crop, $
            DDIR=ddir,ROOT=root,SCALE=scale,RDNOISE=rdnoise, $
            JUSTMEDIAN=justmedian

   self='MKCALIB: '
   if badpar(filkey,[0,7],0,caller=self+"(FILKEY) ",default="FILPOS") then return
   if badpar(expkey,[0,7],0,caller=self+"(EXPKEY) ",default="EXPTIME") then return
   if badpar(in_overscan,[0,2,3],[0,1],caller=self+"(OVERSCAN) ",default=[515,531]) then return
   if badpar(in_crop,[0,2,3],[0,1],caller=self+"(CROP) ",default=[0,511,0,511]) then return
   if badpar(ddir,[0,7],0,caller=self+"(DDIR) ",default="[[ASK]]") then return
   if badpar(root,[0,7],0,caller=self+"(ROOT) ",default="[[ASK]]") then return
   if badpar(rdnoise,[0,2,3,4,5],0,caller=self+'(RDNOISE) ',default=10.) then return
   if badpar(justmedian,[0,1,2,3],0,caller=self+'(JUSTMEDIAN) ', $
                    default=0) then return

; Get name of directory where raw data resides.
   ddirok=0
   while not ddirok do begin
      if ddir eq '[[ASK]]' then begin
         ddir='.'
         print,'Enter directory path to where the data resides'
         read,ddir
      endif

      ; Make sure the directory exists
      if not exists(ddir) then begin
         print,'The directory [',ddir,']'
         print,'does not exist.  Unable to continue'
      endif else begin
         ddirok=1
      endelse
   endwhile

   ddir=addslash(ddir)

   ; Get the date of data to reduce
   if root eq '[[ASK]]' then begin
      root=' '
      read,prompt='Enter UT date of data (YYMMDD) ',root
   endif
   root=root+'.'

   if in_overscan[0] ne -1 then overscan=in_overscan else overscan=[-1,-1]
   if in_crop[0] ne -1 then crop=in_crop else crop=[-1,-1,-1,-1]

   ; Setup calibration directory with the data.
   if not exists(ddir+'calib') then begin
      print,'Creating calibration directory.'
      file_mkdir,ddir+'calib'
   endif
   calibdir = addslash(ddir+'calib')

   ; Print some info to the screen
   print,' '
   print,'The file names for the calbration files you will create with this'
   print,'program are all of the form YYMMDD.ext, where YYMMDD is the UT date'
   print,'of the data that you just entered and ext is the file extension that'
   print,'identifies the file.  The identity of the calibration file is solely'
   print,'determined from the extension so you need to choose this carefully.'
   print,'It is recommended that you only use 1-3 characters to identify the'
   print,'different types of files, such as d for dark or r for an R-band flat'
   print,'field.  If you use b for bias frame it may conflict with using b for'
   print,'a B-band flat field.  You might consider using b1 for bias to'
   print,'distinguish it from a b flat.  The most important thing to remember'
   print,'is that ALL of the extensions you use within a set of calibration'
   print,'files must be unique.'
   print,' '

   ; Bias frame range (or load existing bias)
   biasok=0
   bn0=0
   bn1=0
   while not biasok do begin

      read,prompt='Enter frame number of first bias frame (-1 to load a file) ',bn0

   ; Bias frame already exists.
      if bn0 lt 0 then begin
         print,'Select file for bias frame and click ok to continue.'
         junk=ddir
         bname=dialog_pickfile(title='Choose bias frame',path=junk)
         if exists(bname) then begin
            bias = readfits(bname,/silent)
            fdecomp,bname,disk,dir,name,qual,version
            file_copy,bname,calibdir+name+'.'+qual,/verbose,/noexpand_path,/overwrite
            bname = '+' + name + '.' + qual
            biasok=1
         endif else begin
            print,'Bias file ',bname,' does not exist'
         endelse

   ; Must create a bias frame.
      endif else begin
         read,prompt='Enter last frame number for bias ',bn1
         outsuff=' '
         read,prompt='Enter extension for the bias frame (ie., b1) ',outsuff
         ; Check to see that all the bias frames exist.
         biasok=1
         for i=bn0,bn1 do begin
            fname=root+string(i,format='(i3.3)')
            if exists(ddir+fname+'.fits') then fname=fname+'.fits'
            if not exists(ddir+fname) then begin
               print,'Bias frame ',ddir+fname,' does not exist.'
               i=bn1+1
               biasok=0
            endif
         endfor
         if biasok then begin
            exclude=[-1]
            oldexclude=[-1,-1]
            while n_elements(exclude) ne n_elements(oldexclude) do begin
               oldexclude=exclude
               mkbias,ddir+root,outsuff,bn0,bn1-bn0+1,bias, $
                  overscan=overscan,crop=crop,exclude=exclude,rdnoise=rdnoise
               setwin,1,xsize=crop[1]-crop[0],ysize=crop[3]-crop[2]
               skysclim,bias,v1,v2,mval,sig
               tv,bytscl(bias,min=v1,max=v2,top=!d.n_colors-1)
               setwin,0,xsize=crop[1]-crop[0],ysize=crop[3]-crop[2]
               ask=1
               for i=bn0,bn1 do begin
                  z=where(i eq exclude,count)
                  if count eq 0 then begin
                     fname=root+string(i,format='(i3.3)')
                     if exists(ddir+fname+'.fits') then fname=fname+'.fits'
                     f=float(readfits(ddir+fname,/silent))
                     f=colbias(f,overscan[0],overscan[1],crop[0],crop[1],crop[2],crop[3])
                     f=f-bias
                     skysclim,f,v1,v2,mval,sig
                     print,fname,' ',mval,' +/- ',sig
                     if ask then tv,bytscl(f,min=v1,max=v2,top=!d.n_colors-1)
                     ans='y'
                     if ask then read,prompt='Does this bias frame look okay?',ans
                     if strmid(ans,0,1) eq 'n' then exclude=[exclude,i]
                     if strmid(ans,0,1) eq 'Y' then ask=0
                  endif
               endfor
            endwhile
            ans='y'
            read,prompt='Would you like to save this super bias frame to the calib directory ',ans
            if strmid(ans,0,1) eq 'y' then begin
               fullbname = calibdir+root+outsuff
               file_move,ddir+root+outsuff,fullbname,/verbose,/noexpand_path,/overwrite
               bname='+'+root+outsuff
            endif else begin
               fullbname = ddir+root+outsuff
            endelse
         endif
      endelse ; end of bias frame creation block

   endwhile ; end of bias block

   print,'Bias: ',bname

   ; Process dark frames (if wanted)
   darkok=0
   bn0=0
   bn1=0
   while not darkok do begin

      dname=' '
      read,prompt='Do you require dark frame calibration? (y/n) ',dname
      if dname eq 'n' then begin
         dname='[none]'
         dark = 0.
         darkok = 1
      endif else begin

         read,prompt='Enter frame number of first dark frame (-1 to load a file) ',bn0
         if bn0 lt 0 then begin
            print,'Select file for dark frame and click ok to continue.'
            junk=ddir
            dname=dialog_pickfile(title='Choose dark frame',path=junk)
            if exists(dname) then begin
               dark = readfits(dname,/silent)
               fdecomp,dname,disk,dir,name,qual,version
               file_copy,dname,calibdir+name+'.'+qual,/verbose,/noexpand_path,/overwrite
               dname = '+' + name + '.' + qual
               darkok=1
            endif else begin
               print,'Dark file ',dname,' does not exist'
            endelse

         endif else begin
            read,prompt='Enter last frame number for dark ',bn1
            outsuff=' '
            read,prompt='Enter extension for the dark frame (ie., d) ',outsuff
            ; Check to see that all the dark frames exist.
            darkok=1
            for i=bn0,bn1 do begin
               fname=root+string(i,format='(i3.3)')
               if exists(ddir+fname+'.fits') then fname=fname+'.fits'
               if not exists(ddir+fname) then begin
                  print,'Dark frame ',ddir+fname,' does not exist.'
                  i=bn1+1
                  darkok=0
               endif
            endfor
            if darkok then begin
               exclude=[-1]
               oldexclude=[-1,-1]
               while n_elements(exclude) ne n_elements(oldexclude) do begin
                  oldexclude=exclude
                  mkdark,ddir+root,outsuff,bn0,bn1-bn0+1,fullbname,dark, $
                     overscan=overscan,crop=crop,exclude=exclude,rdnoise=rdnoise
                  setwin,1,xsize=crop[1]-crop[0],ysize=crop[3]-crop[2]
                  skysclim,dark,v1,v2,mval,sig
                  tv,bytscl(dark,min=v1,max=v2,top=!d.n_colors-1)
                  setwin,0,xsize=crop[1]-crop[0],ysize=crop[3]-crop[2]
                  ask=1
                  for i=bn0,bn1 do begin
                     z=where(i eq exclude,count)
                     if count eq 0 then begin
                        fname=root+string(i,format='(i3.3)')
                        if exists(ddir+fname+'.fits') then fname=fname+'.fits'
                        f=float(readfits(ddir+fname,hdr,/silent))
                        exptime=sxpar(hdr,expkey,ddir+fname)
                        f=colbias(f,overscan[0],overscan[1],crop[0],crop[1],crop[2],crop[3])
                        f=f-bias-dark*exptime
                        skysclim,f,v1,v2,mval,sig
                        print,fname,' ',mval,' +/- ',sig
                        if ask then tv,bytscl(f,min=v1,max=v2,top=!d.n_colors-1)
                        ans='y'
                        if ask then read,prompt='Does this dark frame look okay?',ans
                        if strmid(ans,0,1) eq 'n' then exclude=[exclude,i]
                        if strmid(ans,0,1) eq 'Y' then ask=0
                     endif
                  endfor
               endwhile
               ans='y'
               read,prompt='Would you like to save this super dark frame to the calib directory ',ans
               if strmid(ans,0,1) eq 'y' then begin
                  fulldname = calibdir+root+outsuff
                  file_move,ddir+root+outsuff,fulldname,/verbose,/noexpand_path,/overwrite
                  dname='+'+root+outsuff
               endif else begin
                  fulldname = ddir+root+outsuff
               endelse
            endif
         endelse
      endelse
   endwhile
   print,'Dark: ',dname

   ; Process flat(s)
   ans='y'
   read,prompt='Do you need to process or specify any flats? ',ans
   if strmid(ans,0,1) eq 'n' then flatdone=1 else flatdone=0
   fn=0
   while not flatdone do begin
      flatok=0
      bn0=0
      bn1=0
      while not flatok do begin
         filcode='a'
         read,prompt='Enter filter code for this flat ',filcode
         if fn eq 0 then begin
            filter = [filcode]
            flname  = [' ']
         endif else begin
            filter = [filter,filcode]
            flname  = [flname,' ']
         endelse
         read,prompt='Enter frame number of first flat frame (-1 to load a file) ',bn0
         if bn0 lt 0 then begin
            print,'Select file for flat frame and click ok to continue.'
            junk=ddir
            str=dialog_pickfile(title='Choose flat frame for filter '+filcode,path=junk)
            flname[fn]=str
            if exists(flname[fn]) then begin
               flat = readfits(flname[fn],/silent)
               fdecomp,flname[fn],disk,dir,name,qual,version
               file_copy,flname[fn],calibdir+name+'.'+qual,/verbose,/noexpand_path,/overwrite
               flname[fn] = '+' + name + '.' + qual
               flatok=1
            endif else begin
               print,'Flat file ',flname[fn],' does not exist'
            endelse
         endif else begin
            read,prompt='Enter last frame number for flat ',bn1
            outsuff=' '
            read,prompt='Enter extension for the flat frame (ie., b, v, or r) ',outsuff
            ; Check to see that all the flat frames exist.
            flatok=1
            for i=bn0,bn1 do begin
               fname=root+string(i,format='(i3.3)')
               if exists(ddir+fname+'.fits') then fname=fname+'.fits'
               if not exists(ddir+fname) then begin
                  print,'Flat frame ',ddir+fname,' does not exist.'
                  i=bn1+1
                  flatok=0
               endif else begin
                  hdr=headfits(ddir+fname)
                  fil=strtrim(sxpar(hdr,filkey,ddir+fname),2)
                  fil = strjoin(fil,'+')
                  if fil ne filter[fn] then begin
                     print,'Filter code for file ',fname
                     print,'does not match.  value is ',fil,' should be ',filter[fn]
                     ans='n'
                     read,prompt='Is this okay? ',ans
                     if strmid(ans,0,1) ne 'y' then begin
                        i=bn1+1
                        flatok=0
                     endif
                  endif
               endelse
            endfor
            if flatok then begin
               exclude=[-1]
               oldexclude=[-1,-1]
               while n_elements(exclude) ne n_elements(oldexclude) do begin
                  oldexclude=exclude
                  mkflat,ddir+root,outsuff,bn0,bn1-bn0+1,fullbname,fulldname, $
                     flat,overscan=overscan,crop=crop,exclude=exclude, $
                     scale=scale,rdnoise=rdnoise,justmedian=justmedian
                  setwin,1,xsize=crop[1]-crop[0],ysize=crop[3]-crop[2]
                  skysclim,flat,v1,v2,mval,sig
                  tv,bytscl(flat,min=v1,max=v2,top=!d.n_colors-1)
                  setwin,0,xsize=crop[1]-crop[0],ysize=crop[3]-crop[2]
                  ask=1
                  for i=bn0,bn1 do begin
                     z=where(i eq exclude,count)
                     if count eq 0 then begin
                        fname=root+string(i,format='(i3.3)')
                        if exists(ddir+fname+'.fits') then fname=fname+'.fits'
                        f=float(readfits(ddir+fname,hdr,/silent))
                        exptime=sxpar(hdr,expkey,ddir+fname)
                        f=colbias(f,overscan[0],overscan[1],crop[0],crop[1],crop[2],crop[3])
                        f=(f-bias-dark*exptime)/flat
                        skysclim,f,v1,v2,mval,sig
                        print,fname,' ',mval,' +/- ',sig
                        if ask then tv,bytscl(f,min=v1,max=v2,top=!d.n_colors-1)
                        ans='y'
                        if ask then read,prompt='Does this flat frame look okay?',ans
                        if strmid(ans,0,1) eq 'n' then exclude=[exclude,i]
                        if strmid(ans,0,1) eq 'Y' then ask=0
                     endif
                  endfor
               endwhile
               ans='y'
               read,prompt='Would you like to save this super flat frame to the calib directory ',ans
               if strmid(ans,0,1) eq 'y' then begin
                  file_move,ddir+root+outsuff,calibdir+root+outsuff, $
                     /verbose,/noexpand_path,/overwrite
                  flname[fn]='+'+root+outsuff
               endif
            endif
         endelse
      endwhile
      print,'Flat: ',filter[fn],' ',flname[fn]
      read,prompt='Do you need to process another set of flats? ',ans
      if strmid(ans,0,1) eq 'n' then flatdone=1
      fn=fn+1
   endwhile

   ; Save names to a calib file
   print,'Saving files.cal to calib directory in data area'
   openw,lun,calibdir+'files.cal',/get_lun
   printf,lun,'calib_file_v03'
   printf,lun,overscan,format='(2i5)'
   print,overscan,format='(2i5)'
   printf,lun,crop,format='(4i5)'
   print,crop,format='(4i5)'
   printf,lun,bname
   print,bname
   printf,lun,dname
   print,dname
   if fn gt 0 then begin
      for i=0,fn-1 do begin
         printf,lun,filter[i],' ',flname[i]
         print,filter[i],' ',flname[i]
      endfor
   endif
   free_lun,lun

end

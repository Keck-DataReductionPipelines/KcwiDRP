;+
; NAME:
;  autocal
; PURPOSE:
;  Automatic program for creating CCD calibration files.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  autocal
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;   ASK    = flag, if set will ask if you want to delete each image in turn
;               once you don't delete any images in a pass it will then
;               proceed to finish.  If you delete anything, that calibration
;               file will be regenerated using the modified set you have chosen.
;   FILKEY = String - FITS keyword to read to get filter code, default = FILPOS
;   EXPKEY = String - FITS keyword to read to get exposure time, default = EXPTIME
;   TYPEKEY= String - FITS keyword to read to get exposure type, default = IMAGETYP
;   OVERSCAN = column overscan region to use for frame bias level,
;                 default=[-1,-1]
;   CROP     = region of original image to save, default=[-1,-1,-1,-1]=no crop
;                                                        [x1,x2,y1,y2]
;   DDIR     = String, name of directory where data exists.  If not provided,
;                 the program assumes the current directory.
;   ROOT     = String, root of file name for data, usually YYMMDD (ut date).
;                 Default is the last part of DDIR.
;   SCALE - 4 element vector which, if provide, defines the region of the
;           array dimensions that are used to scale the mean
;           of the arrays before combining (.  If combined in this
;           manner, the arrays are combined weighted by the means.
;                 [x1,x2,y1,y2]  (Used for flats only.)
;           These coordinates apply to the pixel locations AFTER cropping.
;   BINFAC   - binning factor for display of final products (default=1)
;  PATTERN - File searching pattern, default=root+'*.???'
;   RDNOISE  - Read noise of CCD [in DN], default=10
;
; OUTPUTS:
;  All output is confined to files.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2001/02/15 - Marc W. Buie, Lowell Observatory
;  2001/03/19, MWB, fixed file name bug.
;  2001/04/24, MWB, fixed problem with incomplete flat sets on a night.
;  2001/09/25, MWB, forced imagetyp field to be lower case.
;  2003/10/01, MWB, converted my Filemove call to system file_move routine
;                   converted my Mkdir calls to IDL file_mkdir calls
;  2003/11/05, MWB, changed file search pattern to get YYMMDD*.???
;                       (the * is new)
;  2004/9/21, MWB, removed obsolete call to Findfile
;  2005/4/1, MWB, modified for new version of MKDARK
;  2005/10/26, MWB, added PATTERN keyword
;  2006/03/15, MWB, added BINFAC keyword
;  2007/05/08, MWB, removed unused FILNAMEKEY keyword, modified FILKEY to use
;                     multiple filters (ie., FILKEY='FILTER*')
;  2008/01/22, MWB, added RDNOISE keyword.
;-
pro autocal,FILKEY=filkey,EXPKEY=expkey, $
            TYPEKEY=typekey,OVERSCAN=in_overscan,CROP=in_crop, $
            DDIR=ddir,ROOT=root,SCALE=scale,ASK=in_ask,PATTERN=pattern, $
            BINFAC=binfac,RDNOISE=rdnoise

   self='AUTOCAL: '
   if badpar(filkey,[0,7],0,caller=self+"(FILKEY) ",default="FILPOS") then return
   if badpar(expkey,[0,7],0,caller=self+"(EXPKEY) ",default="EXPTIME") then return
   if badpar(typekey,[0,7],0,caller=self+"(TYPEKEY) ",default="IMAGETYP") then return
   if badpar(in_overscan,[0,2,3],[0,1],caller=self+"(OVERSCAN) ",default=[-1,-1]) then return
   if badpar(in_crop,[0,2,3],[0,1],caller=self+"(CROP) ",default=[-1,-1,-1,-1]) then return
   if badpar(ddir,[0,7],0,caller=self+"(DDIR) ",default="") then return
   if badpar(in_ask,[0,1,2,3],0,caller=self+"(ASK) ",default=0) then return
   if badpar(binfac,[0,1,2,3],0,caller=self+"(BINFAC) ",default=1) then return
   if badpar(rdnoise,[0,2,3,4,5],0,caller=self+'(RDNOISE) ',default=10.) then return

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

   if strmid(ddir,0,strlen(ddir)-1) eq '.' then begin
      cd,'.',current=ddir
      ddir=addslash(ddir)
   endif
   tmproot=strmid(ddir,strlen(ddir)-7,6)
   if badpar(root,[0,7],0,caller="AUTOCAL: (ROOT) ",default=tmproot) then return
   if badpar(pattern,[0,7],0,caller=self+"(PATTERN) ",default=root+'*.???') then return

   ; Get the date of data to reduce
   if root eq '[[ASK]]' then begin
      root=' '
      read,prompt='Enter UT date of data (YYMMDD) ',root
   endif

   if in_overscan[0] ne -1 then overscan=in_overscan else overscan=[-1,-1]
   if in_crop[0] ne -1 then crop=in_crop else crop=[-1,-1,-1,-1]

   ; Setup calibration directory with the data.
   if not exists(ddir+'calib') then begin
      print,'Creating calibration directory.'
      file_mkdir,ddir+'calib'
   endif
   calibdir = addslash(ddir+'calib')

   ; Scan the headers of all the images in the data directory.  Remember the
   ;  image type and the filter.
   fnlist=file_search(ddir+pattern,count=nfiles)
   if nfiles eq 0 then begin
      print,'No data files found, cannot continue.'
      print,'Search pattern was ',ddir+pattern
      return
   endif
   imagetype=strarr(nfiles)
   filternum=strarr(nfiles)
   for i=0,nfiles-1 do begin
      hdr=headfits(fnlist[i])
      imagetype[i] = strlowcase(sxpar(hdr,typekey))
      fnum = strtrim(sxpar(hdr,filkey),2)
      z=where(fnum ne 'Open',count)
      if count eq 0 then filternum[i]='Open' $
      else if count eq 1 then filternum[i]=fnum[z[0]] $
      else filternum[i] = strjoin(fnum[z],'+')
   endfor

   imagetype=strtrim(imagetype,2)

   ; Get a list of unique filter and image types
   filterlist = filternum[uniq(filternum,sort(filternum))]
   typelist   = imagetype[uniq(imagetype,sort(imagetype))]
   z=where(typelist ne '')
   typelist=typelist[z]

   z=where(imagetype eq 'bias',nbias)
   if nbias eq 0 then begin
      print,'No bias frames in this dataset, cannot continue'
      return
   endif
   fname = fnlist[z]
   exclude=intarr(nbias)
   biasok=0
   while not biasok and nbias gt 0 do begin

      outsuff='b1'
      biasok=1
      oldexclude=-1
      while total(exclude) ne total(oldexclude) do begin
         oldexclude=exclude
         mkbias,ddir+root+'.',outsuff,fname,0,bias, $
            overscan=overscan,crop=crop,exclude=exclude,rdnoise=rdnoise
         sz=size(bias,/dimen)
         xsz = sz[0]/binfac
         ysz = sz[1]/binfac
         setwin,1,xsize=xsz,ysize=ysz
         skysclim,bias,v1,v2,mval,sig
         if binfac eq 1 then begin
            tv,bytscl(bias,min=v1,max=v2,top=!d.n_colors-1)
         endif else begin
            xend = xsz*binfac-1
            yend = ysz*binfac-1
            tv,bytscl(rebin(bias[0:xend,0:yend],xsz,ysz), $
                      min=v1,max=v2,top=!d.n_colors-1)
         endelse
         setwin,0,xsize=xsz,ysize=ysz
         ask=in_ask
         z=where(exclude eq 0,count)
         for i=0,count-1 do begin
            f=float(readfits(fname[z[i]],/silent))
            f=colbias(f,overscan[0],overscan[1],crop[0],crop[1],crop[2],crop[3])
            f=f-bias
            skysclim,f,v1,v2,mval,sig
            print,fname[z[i]],' ',mval,' +/- ',sig
            if binfac eq 1 then begin
               tv,bytscl(f,min=v1,max=v2,top=!d.n_colors-1)
            endif else begin
               xend = xsz*binfac-1
               yend = ysz*binfac-1
               tv,bytscl(rebin(f[0:xend,0:yend],xsz,ysz), $
                         min=v1,max=v2,top=!d.n_colors-1)
            endelse
            ans='y'
            if ask then read,prompt='Does this bias frame look okay?',ans
            if strmid(ans,0,1) eq 'n' then exclude[z[i]]=1
            if strmid(ans,0,1) eq 'Y' then ask=0
         endfor
      endwhile
      fullbname = calibdir+root+'.'+outsuff
      file_move,ddir+root+'.'+outsuff,fullbname, $
         /verbose,/noexpand_path,/overwrite
      bname='+'+root+'.'+outsuff

   endwhile ; end of bias block

   print,'Bias: ',bname

   ; Process dark frames (if present)
   dname='[none]'
   dark = 0.
   z=where(imagetype eq 'dark',ndark)
   if ndark eq 0 then $
      darkok=1 $
   else begin
      fname = fnlist[z]
      exclude=intarr(ndark)
      darkok=0
   endelse

   while not darkok do begin

      outsuff='d'
      darkok=1
      oldexclude=-1

      while total(exclude) ne total(oldexclude) do begin
         oldexclude=exclude
         mkdark,ddir+root+'.',outsuff,fname,0,fullbname,dark, $
            overscan=overscan,crop=crop,exclude=exclude,rdnoise=rdnoise
         setwin,1
         skysclim,dark,v1,v2,mval,sig
         if binfac eq 1 then begin
            tv,bytscl(dark,min=v1,max=v2,top=!d.n_colors-1)
         endif else begin
            xend = xsz*binfac-1
            yend = ysz*binfac-1
            tv,bytscl(rebin(dark[0:xend,0:yend],xsz,ysz), $
                      min=v1,max=v2,top=!d.n_colors-1)
         endelse
         setwin,0
         ask=in_ask
         z=where(exclude eq 0,count)
         for i=0,count-1 do begin
            f=float(readfits(fname[z[i]],hdr,/silent))
            exptime=sxpar(hdr,expkey,ddir+fname)
            f=colbias(f,overscan[0],overscan[1],crop[0],crop[1],crop[2],crop[3])
            f=f-bias-dark*exptime
            skysclim,f,v1,v2,mval,sig
            print,fname[z[i]],' ',mval,' +/- ',sig
            if binfac eq 1 then begin
               tv,bytscl(f,min=v1,max=v2,top=!d.n_colors-1)
            endif else begin
               xend = xsz*binfac-1
               yend = ysz*binfac-1
               tv,bytscl(rebin(f[0:xend,0:yend],xsz,ysz), $
                         min=v1,max=v2,top=!d.n_colors-1)
            endelse
            ans='y'
            if ask then read,prompt='Does this dark frame look okay?',ans
            if strmid(ans,0,1) eq 'n' then exclude[z[i]]=1
            if strmid(ans,0,1) eq 'Y' then ask=0
         endfor
      endwhile
      fulldname = calibdir+root+'.'+outsuff
      file_move,ddir+root+'.'+outsuff,fulldname, $
         /verbose,/noexpand_path,/overwrite
      dname='+'+root+'.'+outsuff

   endwhile

   print,'Dark: ',dname

   ; Process flat(s)
   nflats=n_elements(filterlist)
   filter = intarr(nflats)
   flname = strarr(nflats)
   for i=0,nflats-1 do begin
      z=where(filternum eq filterlist[i] and imagetype eq 'flat',nflat)
      if nflat gt 0 then begin
         fname = fnlist[z]
         exclude=intarr(nflat)
         flatok=0
         ; TBD
         flname[i] = strn(filterlist[i])
         ; TBD
         outsuff=flname[i]
         flatok=1
         oldexclude=-1
         while total(exclude) ne total(oldexclude) do begin
            oldexclude=exclude
            mkflat,ddir+root+'.',outsuff,fname,0,fullbname,fulldname,flat, $
               overscan=overscan,crop=crop,exclude=exclude,scale=scale, $
               rdnoise=rdnoise
            setwin,1
            skysclim,flat,v1,v2,mval,sig
            if binfac eq 1 then begin
               tv,bytscl(flat,min=v1,max=v2,top=!d.n_colors-1)
            endif else begin
               xend = xsz*binfac-1
               yend = ysz*binfac-1
               tv,bytscl(rebin(flat[0:xend,0:yend],xsz,ysz), $
                         min=v1,max=v2,top=!d.n_colors-1)
            endelse
            setwin,0
            ask=in_ask
            z=where(exclude eq 0,count)
            for j=0,count-1 do begin
               f=float(readfits(fname[z[j]],hdr,/silent))
               exptime=sxpar(hdr,expkey,ddir+fname)
               f=colbias(f,overscan[0],overscan[1],crop[0],crop[1],crop[2],crop[3])
               f=(f-bias-dark*exptime)/flat
               skysclim,f,v1,v2,mval,sig
               print,fname[z[j]],' ',mval,' +/- ',sig
               if binfac eq 1 then begin
                  tv,bytscl(f,min=v1,max=v2,top=!d.n_colors-1)
               endif else begin
                  xend = xsz*binfac-1
                  yend = ysz*binfac-1
                  tv,bytscl(rebin(f[0:xend,0:yend],xsz,ysz), $
                            min=v1,max=v2,top=!d.n_colors-1)
               endelse
               ans='y'
               if ask then read,prompt='Does this flat frame look okay?',ans
               if strmid(ans,0,1) eq 'n' then exclude[z[j]]=1
               if strmid(ans,0,1) eq 'Y' then ask=0
            endfor
         endwhile
         file_move,ddir+root+'.'+outsuff,calibdir+root+'.'+outsuff, $
            /verbose,/noexpand_path,/overwrite
         flname[i]='+'+root+'.'+outsuff
         print,'Flat: ',filterlist[i],' ',flname[i]
      endif
   endfor

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
   for i=0,nflats-1 do begin
      if flname[i] ne '' then begin
         printf,lun,filterlist[i],' ',flname[i]
         print,filterlist[i],' ',flname[i]
      endif
   endfor
   free_lun,lun

end

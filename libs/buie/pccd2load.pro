;+
; NAME:
;  pccd2load
; PURPOSE:   (one line only)
;  Load information from PCCD (automated version) data into mysql database
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  pccd2load,dir
; INPUTS:
;  dir - directory in which to look for data (default=current directory)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWRITE - flag, if set will force overwrite existing records
;  DBNAME    - database name (default='pccd2obs')
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/05/21
;  2003/07/09, MWB, added protection against incorrect file names in header.
;  2004/9/21, MWB, removed obsolete call to Findfile
;-
pro pccd2load,dir,OVERWRITE=overwrite,DBNAME=dbname,QUICKSKIP=quickskip

   self='pccd2load '
   if badpar(dir,[0,7],0,caller=self+'(dir) ',default='') then return
   if badpar(overwrite,[0,1,2,3],0,caller=self+'(OVERWRITE) ', $
                                   default=0) then return
   if badpar(dbname,[0,7],       0,caller=self+'(DBNAME) ', $
                                   default='pccd2obs') then return
   if badpar(quickskip,[0,1,2,3],0,caller=self+'(QUICKSKIP) ', $
                                   default=0) then return

   c=','
   t="'"

   ; Get the list of images to process
   if dir ne '' then ldir=addslash(dir) else ldir=dir
   fnlist = file_search(ldir+'[0-9][0-9]*.[0-9][0-9][0-9]',count=count)

   print,'Found ',strn(count),' files to process.'
   if count eq 0 then return

   openmysql,lun,dbname

   first=1
   bias=0
   dark=0
   flat=0
   obj=0
   flatfilt=''
   datafilt=''

   for i=0,count-1 do begin
      words=strsplit(fnlist[i],'.',/extract)
      print,fnlist[i]

      ; get header and the filename
      hdr=headfits(fnlist[i])
      if first then begin
         programv=sxpar(hdr,'PROGRAMV')
         pos=strpos(programv,'PCCD')
         if pos lt 0 then begin
            print,self,'data are not from PCCD generation 2.'
            free_lun,lun
            return
         endif
         first=0
      endif
      filename = strtrim(sxpar(hdr,'CCDFNAME'),2)

      ; check for file name consistency
      pos = strpos(fnlist[i],filename)
      if pos lt 0 then begin
         fdecomp,fnlist[i],disk,dir,nfilename,qual
         nfilename = nfilename+'.'+qual
         print,'Warning, filename discrepancy: hdr=',filename, $
            ', using ',nfilename,' instead.'
         filename = nfilename
      endif

      dateobs  = sxpar(hdr,'DATE-OBS')
      ut       = sxpar(hdr,'UT')
      jd = jdparse(dateobs+' '+ut)

      ; data for image table
      object   = strtrim(sxpar(hdr,'OBJECT'),2)
      imagetype= strtrim(sxpar(hdr,'IMAGETYP'),2)
      if imagetype eq 'bias' then bias=bias+1
      if imagetype eq 'dark' then dark=dark+1
      if imagetype eq 'flat' then flat=flat+1
      if imagetype eq 'object' then obj=obj+1
      exptime  = sxpar(hdr,'EXPTIME')
      jdmid = jd + exptime/86400.0d0
      jdstr,jdmid,300,jds
      jdmid = string(jdmid,format='(f13.5)')
      fwhm = sxpar(hdr,'SEEING')
      scale = sxpar(hdr,'SCALE')
      seeing = ((fwhm*scale) > (-90)) < 90
      seeing = string(seeing,format='(f7.1)')
      peak = sxpar(hdr,'IMAGEMAX')
      xpk = sxpar(hdr,'XPEAK')
      ypk = sxpar(hdr,'YPEAK')
      sky = sxpar(hdr,'BACKGRND')
      skysig = sxpar(hdr,'BACKSIG')
      sky = string(sky,format='(f7.1)')
      skysig = string(skysig,format='(f7.1)')
      cmd = 'insert into image values ('
      cmd = [cmd,quote(filename)+c+quote(object)+c+quote(imagetype)+c]
      cmd = [cmd,jdmid+c+jds+c+seeing+c+strn(peak)+c+strn(xpk)+c+strn(ypk)+c]
      cmd = [cmd,sky+c+skysig+');']

      ; check to see if it's already there
      chk='select filename from image where filename='+quote(filename)+';'
      mysqlcmd,lun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from image where filename='+quote(filename)+';'
            mysqlcmd,lun,chk,answer
         endif
         print,cmd
         mysqlcmd,lun,cmd,answer,nlines
         if nlines gt 0 then print,answer,nlines
      endif else if nlines gt 1 and quickskip then begin
         print,'QUICKSKIP enabled, entry found for this directory, quitting.'
         free_lun,lun
         return
      endif

      ; data for instrument table
      exptime=string(exptime,format='(f8.1)')
      filnum = strn(sxpar(hdr,'FILPOS'))
      filtname = strtrim(sxpar(hdr,'FILTER'),2)
      if imagetype eq 'object' then begin
         newf = strmid(filtname,0,1)
         pos=strpos(datafilt,newf)
         if pos lt 0 then datafilt=datafilt+newf
      endif
      if imagetype eq 'flat' then begin
         newf = strmid(filtname,0,1)
         pos=strpos(flatfilt,newf)
         if pos lt 0 then flatfilt=flatfilt+newf
      endif
      jdstr,jd,300,jds
      nx = strn(sxpar(hdr,'NAXIS1'))
      ny = strn(sxpar(hdr,'NAXIS2'))
      xbin = strn(sxpar(hdr,'CDELT1'))
      ybin = strn(sxpar(hdr,'CDELT2'))
      gain = string(sxpar(hdr,'GAIN'),format='(f8.3)')
      cmd = 'insert into instrument values ('
      cmd = [cmd,quote(filename)+c+exptime+c+filnum+c+quote(filtname)+c]
      cmd = [cmd,jds+c+nx+c+ny+c+xbin+c+ybin+c+gain+');']

      ; check to see if it's already there
      chk='select filename from instrument where filename='+quote(filename)+';'
      mysqlcmd,lun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from instrument where filename='+quote(filename)+';'
            mysqlcmd,lun,chk,answer
         endif
         print,cmd
         mysqlcmd,lun,cmd,answer,nlines
         if nlines gt 0 then print,answer
      endif

      ; data for telescope table
      ras = sxpar(hdr,'TELRA')
      ra = string(raparse(ras),format='(f16.13)')
      decs = sxpar(hdr,'TELDEC')
      dec = string(decparse(decs),format='(f16.13)')
      equinox = sxpar(hdr,'EPOCH')
      equinox = string(equinox,format='(f7.2)')
      jd2year,jd,epoch
      epoch = string(epoch,format='(f7.2)')
      hangle = sxpar(hdr,'TELHA')
      hangle = string(haparse(hangle),format='(f16.13)')
      airmass = sxpar(hdr,'AIRMASS')
      airmass = string(airmass,format='(f6.3)')
      focus = sxpar(hdr,'FOCUS')
      focus = strn(focus)
      domestat = strtrim(sxpar(hdr,'DOMESTAT'),2)
      cmd = 'insert into telescope values ('
      cmd = [cmd,quote(filename)+c+ra+c+dec+c+epoch+c]
      cmd = [cmd,equinox+c+hangle+c+airmass+c+focus+c+quote(domestat)+');']

      ; check to see if it's already there
      chk='select filename from telescope where filename='+quote(filename)+';'
      mysqlcmd,lun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from telescope where filename='+quote(filename)+';'
            mysqlcmd,lun,chk,answer
         endif
         print,cmd
         mysqlcmd,lun,cmd,answer,nlines
         if nlines gt 0 then print,answer
      endif

      ; data for weather table
      temp = sxpar(hdr,'OAIRTEMP')
      temp = string(temp,format='(f6.1)')
      wspeed = sxpar(hdr,'WINDSPEE')
      wspeed = strn(fix(wspeed))
      wdirect = sxpar(hdr,'WDIRECT')
      wdirect = strn(fix(wdirect))
      wdir = strtrim(sxpar(hdr,'WDIR'),2)
      barom = sxpar(hdr,'BAROM')
      barom = string(barom,format='(f6.2)')
      relhum = sxpar(hdr,'RELHUM')
      relhum = string(relhum,format='(f6.1)')
      dewpt = sxpar(hdr,'DEWPT')
      dewpt = string(dewpt,format='(f6.1)')
      cmd = 'insert into weather values ('
      cmd = [cmd,quote(filename)+c+temp+c+wspeed+c+wdirect+c+quote(wdir)+c]
      cmd = [cmd,barom+c+relhum+c+dewpt+');']

      ; check to see if it's already there
      chk='select filename from weather where filename='+quote(filename)+';'
      mysqlcmd,lun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from weather where filename='+quote(filename)+';'
            mysqlcmd,lun,chk,answer
         endif
         print,cmd
         mysqlcmd,lun,cmd,answer,nlines
         if nlines gt 0 then print,answer
      endif

   endfor

   ; single entry in runstat for each night
   obsdate = strmid(filename,0,2)
   if fix(obsdate) lt 80 then $
      obsdate='20'+obsdate $
   else $
      obsdate='19'+obsdate
   obsdate=obsdate+'-'+strmid(filename,2,2)+'-'+strmid(filename,4,2)
   rundate=strmid(filename,0,6)
   conditions='unknown'
   datastat='raw'
   calstat='unknown'
   cmd = 'insert into runstat values ('
   cmd = [cmd,quote(rundate)+c+quote(obsdate)+c+quote(conditions)+c]
   cmd = [cmd,quote(datastat)+c+quote(datafilt)+c+strn(obj)+c+quote(calstat)+c+strn(bias)+c]
   cmd = [cmd,quote(calstat)+c+strn(dark)+c+quote(calstat)+c+strn(flat)+c]
   cmd = [cmd,quote(flatfilt)+');']

   ; check to see if it's already there
   chk='select obsdate from runstat where obsdate='+quote(obsdate)+';'
   mysqlcmd,lun,chk,answer,nlines
   if overwrite or nlines eq 1 then begin
      if nlines gt 1 then begin
         chk='delete from runstat where obsdate='+quote(obsdate)+';'
         mysqlcmd,lun,chk,answer
      endif
      print,cmd
      mysqlcmd,lun,cmd,answer,nlines
      if nlines gt 0 then print,answer
   endif

   free_lun,lun

end

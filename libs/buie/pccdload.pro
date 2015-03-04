;+
; NAME:
;  pccdload
; PURPOSE:   (one line only)
;  Load information from PCCD (automated version) data into mysql database
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  pccdload,dir
; INPUTS:
;  dir - directory in which to look for data (default=current directory)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWRITE - flag, if set will force overwrite existing records
;  DBNAME    - database name (default='pccdobs')
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/07/21
;  2004/9/21, MWB, removed obsolete call to Findfile
;-
pro pccdload,dir,OVERWRITE=overwrite,DBNAME=dbname,QUICKSKIP=quickskip

   self='pccdload '
   if badpar(dir,[0,7],0,caller=self+'(dir) ',default='') then return
   if badpar(overwrite,[0,1,2,3],0,caller=self+'(OVERWRITE) ', $
                                   default=0) then return
   if badpar(dbname,[0,7],       0,caller=self+'(DBNAME) ', $
                                   default='pccdobs') then return
   if badpar(quickskip,[0,1,2,3],0,caller=self+'(QUICKSKIP) ', $
                                   default=0) then return

   c=','
   t="'"

   ; Get the list of images to process
   if dir ne '' then ldir=addslash(dir) else ldir=dir
   fnlist = file_search(ldir+'??????.???',count=count)

   print,'Found ',strn(count),' files to process.'
   if count eq 0 then return

   openmysql,lun,dbname

   first=1
   bias=0
   dark=0
   flat=0
   obj=0
   flatfilt=intarr(10)
   datafilt=intarr(10)
   goodinst=''
   goodname=''
   firstbad=1

   ; loop over the files found
   for i=0,count-1 do begin
      words=strsplit(fnlist[i],'.',/extract)
      if words[1] eq 'log' then continue
      print,fnlist[i]

      ; get header and the filename
      hdr=headfits(fnlist[i])
      sz=size(hdr,/type)
      if sz ne 7 then continue
      sz=size(hdr,/n_dimensions)
      if sz ne 1 then continue
      if first then begin
         programv=sxpar(hdr,'PRGRM1')
         pos=strpos(programv,'PCCD')
         if pos lt 0 then begin
            print,self,'data are not from PCCD generation 1.'
            free_lun,lun
            return
         endif
         first=0
      endif
      filename = strtrim(sxpar(hdr,'ORIGFILE'),2)
      inst = strtrim(sxpar(hdr,'INSTRUME'),2)

      image = readfits(fnlist[i])
      if n_elements(image) eq 1 and image[0] eq -1 then continue
      naxis = fix(sxpar(hdr,'NAXIS'))
      nx = strn(sxpar(hdr,'NAXIS1'))
      ny = strn(sxpar(hdr,'NAXIS2'))
      if naxis eq 3 then $
         nf = strn(sxpar(hdr,'NAXIS3')) $
      else $
         nf = 1

      ; check for file name consistency
      pos = strpos(fnlist[i],filename)
      if pos lt 0 then begin
         nfilename = strmid(fnlist[i],strlen(fnlist[i])-10,999)
         print,'Warning, filename discrepancy: hdr=',filename, $
            ', using ',nfilename,' instead.'
         filename = nfilename
      endif

      dateobs  = sxpar(hdr,'DATE-OBS')
      ut       = sxpar(hdr,'STRTTIME')
      jd = jdparse(dateobs+' '+ut)

      ; data for image table
      object   = strtrim(sxpar(hdr,'OBJECT'),2)
      shutter  = strtrim(sxpar(hdr,'SHUTTER'),2)
      exptime  = sxpar(hdr,'EXPTIME')
      if strpos(strlowcase(object),'flat') ge 0 then begin
         imagetype = 'flat'
      endif else if strlowcase(shutter) eq 'dark' and $
                    exptime le 0.002 then begin
         imagetype = 'bias'
      endif else if strlowcase(shutter) eq 'dark' and $
                    exptime gt 0.002 then begin
         imagetype = 'dark'
      endif else begin
         imagetype = 'object'
      endelse

      if imagetype eq 'bias' then bias=bias+1
      if imagetype eq 'dark' then dark=dark+1
      if imagetype eq 'flat' then flat=flat+1
      if imagetype eq 'object' then obj=obj+1
      jdmid = jd + exptime/86400.0d0
      jdstr,jdmid,300,jds
      jdmid = string(jdmid,format='(f13.5)')

      ; derive some values from data
      skysclim,image,lowval,hival,sky,skysig
      peak = max(image)
      z=where(image eq peak)
      if nf eq 1 then begin
         xpk = z[0] mod nx
         ypk = z[0] / nx
      endif else begin
         xpk = 0
         ypk = 0
      endelse

      if imagetype eq 'object' and nf eq 1 then $
         basphote,1.0,image,1.0,xpk,ypk,5,10,15,/silent,/nolog,fwhm=fwhm $
      else $
         fwhm=0.0

      fwhm = string(fwhm,format='(f7.1)')
      sky = string(sky,format='(f8.1)')
      skysig = string(skysig,format='(f7.1)')
      cmd = 'insert into image values ('
      cmd = [cmd,t+filename+t+c+t+inst+t+c+t+object+t+c+t+imagetype+t+c]
      cmd = [cmd,jdmid+c+jds+c+fwhm+c+strn(peak)+c+strn(xpk)+c+strn(ypk)+c]
      cmd = [cmd,sky+c+skysig+');']

      ; check to see if it's already there
      chk='select filename from image where filename='+t+filename+t+ $
          ' and inst='+t+inst+t+';'
      mysqlcmd,lun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from image where filename='+t+filename+t+ $
                ' and inst='+t+inst+t+';'
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
      jdstr,jd,300,jds
      exptime=string(exptime,format='(f8.1)')
      filnum = strn(sxpar(hdr,'FILTER'))
      filtname = strtrim(sxpar(hdr,'FILTNAME'),2)
      if imagetype eq 'object' then begin
         datafilt[fix(filnum)] = datafilt[fix(filnum)] + 1
      endif
      if imagetype eq 'flat' then begin
         flatfilt[fix(filnum)] = flatfilt[fix(filnum)] + 1
      endif
      xbin = strn(sxpar(hdr,'CDELT1'))
      ybin = strn(sxpar(hdr,'CDELT2'))
      gain = strn(sxpar(hdr,'GAINFAC'))
      nsub = strn(sxpar(hdr,'NUMSUBF'))
      trigger = strtrim(sxpar(hdr,'TRIGGER'),2)
      timestat = strtrim(sxpar(hdr,'TIMESTAT'),2)
      if strmid(timestat,0,1) eq '1' then gps1hz='Ok' else gps1hz='Bad'
      if strmid(timestat,1,1) eq '1' then gps1mhz='Ok' else gps1mhz='Bad'
      if strmid(timestat,2,1) eq '1' then gpsgood='Yes' else gpsgood='No'
      if strmid(timestat,3,1) eq '1' then gpspresent='Yes' else gpspresent='No'
      if strmid(timestat,4,1) eq '1' then usegps='Yes' else usegps='No'
      timesrc = strtrim(sxpar(hdr,'TIMESRC'),2)
      dt = sxpar(hdr,'CDELT3')
      if nf gt 0 then $
         dt=string(dt,format='(f12.6)') $
      else $
         dt='0.'
      timequal = strtrim(sxpar(hdr,'TIMEQUAL'),2)
      gpsstate = strtrim(sxpar(hdr,'GPSSTATE'),2)
      optics = strtrim(sxpar(hdr,'OPTICS'),2)
      if strpos(optics,'1:1') ge 0 then $
         optics = '1:1' $
      else if strpos(optics,'3:1') ge 0 then $
         optics = '3:1' $
      else if strpos(optics,'1:3') ge 0 then $
         optics = '1:3' $
      else if strpos(optics,'direct') ge 0 then $
         optics = 'direct'
      pfbin = strn(sxpar(hdr,'PFBIN'))
      pardelay = strn(sxpar(hdr,'PARDELAY'))
      seqprgm = strn(sxpar(hdr,'SEQ-PRGM'))
      nf = strn(nf)
      cmd = 'insert into instrument values ('
      cmd = [cmd,t+filename+t+c+t+inst+t+c+exptime+c+filnum+c+t+filtname+t+c]
      cmd = [cmd,jds+c+nx+c+ny+c+nf+c+xbin+c+ybin+c+nsub+c+gain+c]
      cmd = [cmd,t+trigger+t+c+t+shutter+t+c+t+gps1hz+t+c+t+gps1mhz+t+c]
      cmd = [cmd,t+gpspresent+t+c+t+gpsgood+t+c+t+usegps+t+c+t+timesrc+t+c]
      cmd = [cmd,dt+c+t+timequal+t+c+t+gpsstate+t+c+t+programv+t+c]
      cmd = [cmd,t+optics+t+c+pfbin+c+pardelay+c+seqprgm+c]
      cmd = [cmd,'NULL'+');']

      ; check to see if it's already there
      chk='select filename from instrument where filename='+t+filename+t+ $
          ' and inst='+t+inst+t+';'
      mysqlcmd,lun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from instrument where filename='+t+filename+t+ $
                ' and inst='+t+inst+t+';'
            mysqlcmd,lun,chk,answer
         endif
         print,cmd
         mysqlcmd,lun,cmd,answer,nlines
         if nlines gt 0 then print,answer
      endif

      ; data for telescope table
      ra = '0.'
      dec = '0.'
      equinox = '0.'
      epoch = '0.'
      hangle = '0.'
      airmass = '0.'
      observatory = strtrim(sxpar(hdr,'OBSERVAT'),2)
      catch, error_status
      if error_status ne 0 then begin
         if firstbad then begin
            ans=''
            read,ans,prompt='telescope string bad, enter name to use'
            telescope = ans
            firstbad=0
         endif
         catch, /cancel
      endif else begin
         telescope = strtrim(sxpar(hdr,'TELESCOP'),2)
      endelse
      catch, /cancel
;      if strmid(filename,0,6) eq '960325' then $
;         telescope = '50mm on south side of Hall 42" telescope' $
;      else $
;         telescope = strtrim(sxpar(hdr,'TELESCOP'),2)
      lat = strtrim(sxpar(hdr,'LAT-TEL'),2)
      if lat eq '' or lat eq '/' or strmid(lat,0,3) eq 'bad' then begin
         lat = 0.
      endif else begin
         lat = strsplit(lat,' ',/extract)
         lat = decparse(lat[0])
      endelse
      lat = string(lat,format='(f12.9)')
      lon = strtrim(sxpar(hdr,'LONG-TEL'),2)
      if lon eq '' or lon eq '/' or strmid(lon,0,3) eq 'bad' then begin
         lon = 0.
      endif else begin
         words = strsplit(lon,' ',/extract)
         lon = decparse(words[1])
         if words[0] eq 'E' then lon = -1.0 * lon
      endelse
      lon = string(lon,format='(f12.9)')
      alt = strtrim(sxpar(hdr,'ALT-TEL'),2)
      if alt eq '' or alt eq '/' or strmid(alt,0,3) eq 'bad' then begin
         alt = '0'
      endif else begin
         words = strsplit(alt,' ',/extract)
         alt = words[0]
      endelse
      datum = strtrim(sxpar(hdr,'DATUM'),2)
      observer = strtrim(sxpar(hdr,'OBSERVER'),2)
      cmd = 'insert into telescope values ('
      cmd = [cmd,t+filename+t+c+t+inst+t+c+ra+c+dec+c+epoch+c]
      cmd = [cmd,equinox+c+hangle+c+airmass+c+t+observatory+t+c]
      cmd = [cmd,t+telescope+t+c+lat+c+lon+c+alt+c+t+datum+t+c]
      cmd = [cmd,t+observer+t+');']

      ; check to see if it's already there
      chk='select filename from telescope where filename='+t+filename+t+ $
          ' and inst='+t+inst+t+';'
      mysqlcmd,lun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from telescope where filename='+t+filename+t+ $
                ' and inst='+t+inst+t+';'
            mysqlcmd,lun,chk,answer
         endif
         print,cmd
         mysqlcmd,lun,cmd,answer,nlines
         if nlines gt 0 then print,answer
      endif

      goodinst = inst
      goodname = filename

   endfor

   ; single entry in runstat for each night
   obsdate = strmid(goodname,0,2)
   if fix(obsdate) lt 80 then $
      obsdate='20'+obsdate $
   else $
      obsdate='19'+obsdate
   obsdate=obsdate+'-'+strmid(goodname,2,2)+'-'+strmid(goodname,4,2)
   rundate=strmid(goodname,0,6)
   conditions='unknown'
   datastat='raw'
   calstat='unknown'
   filtn=strcompress(string(indgen(10)))
   z=where(datafilt gt 0,count)
   if count gt 0 then $
      dfilt = strcompress(strjoin(filtn[z]),/remove_all) $
   else $
      dfilt = ''
   z=where(flatfilt gt 0,count)
   if count gt 0 then $
      ffilt = strcompress(strjoin(filtn[z]),/remove_all) $
   else $
      ffilt = ''
   cmd = 'insert into runstat values ('
   cmd = [cmd,t+rundate+t+c+t+goodinst+t+c+t+obsdate+t+c+t+conditions+t+c]
   cmd = [cmd,t+datastat+t+c+t+dfilt+t+c+strn(obj)+c+t+calstat+t+c+strn(bias)+c]
   cmd = [cmd,t+calstat+t+c+strn(dark)+c+t+calstat+t+c+strn(flat)+c]
   cmd = [cmd,t+ffilt+t+');']

   ; check to see if it's already there
   chk='select obsdate from runstat where obsdate='+t+obsdate+t+ $
       ' and inst='+t+goodinst+t+';'
   mysqlcmd,lun,chk,answer,nlines
   if overwrite or nlines eq 1 then begin
      if nlines gt 1 then begin
         chk='delete from runstat where obsdate='+t+obsdate+t+ $
             ' and inst='+t+goodinst+t+';'
         mysqlcmd,lun,chk,answer
      endif
      print,cmd
      mysqlcmd,lun,cmd,answer,nlines
      if nlines gt 0 then print,answer
   endif

   free_lun,lun

end

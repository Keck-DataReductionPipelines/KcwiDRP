;+
; NAME:
;  roboccdload
; PURPOSE:   (one line only)
;  Load information from Roboccd (automated version) data into mysql database
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  roboccdload,dir
; INPUTS:
;  dir - directory in which to look for data (default=current directory)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWRITE - flag, if set will force overwrite existing records
;  DBNAME    - database name (default='roboccd')
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
;  2008/1/25, PLC, add support for runs with 1000+ frames.
;-
pro roboccdload,dir,OVERWRITE=overwrite,DBNAME=dbname,QUICKSKIP=quickskip, $
      SILENT=silent,NFILES=nfiles

   self='ROBOCCDLOAD: '
   if badpar(dir,[0,7],0,caller=self+'(dir) ',default='') then return
   if badpar(overwrite,[0,1,2,3],0,caller=self+'(OVERWRITE) ', $
                                   default=0) then return
   if badpar(dbname,[0,7],       0,caller=self+'(DBNAME) ', $
                                   default='roboccd') then return
   if badpar(quickskip,[0,1,2,3],0,caller=self+'(QUICKSKIP) ', $
                                   default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                   default=0) then return

   c=','
   t="'"
   blanks='                                       '

   ; hard coded for Anderson Mesa
   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi

   ; Get the list of images to process
   if dir ne '' then ldir=addslash(dir) else ldir=dir
   fnlist = file_search(ldir+'[0-9][0-9]*.[0-9][0-9][0-9]',count=nfiles)

   if not silent then print,'Found ',strn(nfiles),' files to process.'
   if nfiles eq 0 then return

   openmysql,dblun,dbname

   first=1
   bias=0
   flat=0
   obj=0
   flatfilt=''
   datafilt=''

   for i=0,nfiles-1 do begin
      ;words=strsplit(fnlist[i],'.',/extract)
      if not silent then print,fnlist[i]

      ; get header and the filename
      hdr=headfits(fnlist[i])
      if first then begin
         programv=sxpar(hdr,'PROGRAMV')
         pos=strpos(programv,'ROBOCCD')
         if pos lt 0 then begin
            print,self,'data are not from ROBOCCD generation 2.'
            free_lun,dblun
            return
         endif
         first=0
         jdmin = jdparse(sxpar(hdr,'DATE-OBS')+' '+sxpar(hdr,'UT'))
         jdmax = jdmin
      endif

      filename = strtrim(sxpar(hdr,'CCDFNAME'),2)

      ; check for file name consistency
      pos = strpos(fnlist[i],filename)
      if pos lt 0 then begin
         fdecomp,fnlist[i],disk,dir,nfilename,qual
         nfilename = nfilename+'.'+qual
         if not silent then print,'Warning, filename discrepancy: hdr=',filename, $
            ', using ',nfilename,' instead.'
         filename = nfilename
      endif

      dateobs  = strtrim(sxpar(hdr,'DATE-OBS'),2)
      ut       = strtrim(sxpar(hdr,'UT'),2)
      jd = jdparse(dateobs+' '+ut)

      ; data for image table
      object   = strtrim(sxpar(hdr,'OBJECT'),2)
      imagetype= strtrim(sxpar(hdr,'IMAGETYP'),2)
      scale = sxpar(hdr,'SCALE')
      if imagetype eq 'bias' then begin
         bias=bias+1
         object = 'Bias'
         xpk = -1
         ypk = -1
         peak = -1
         seeing = '-1'
      endif else if imagetype eq 'flat' then begin
         flat=flat+1
         object = 'Flat'
         xpk = -1
         ypk = -1
         peak = -1
         seeing = '-1'
      endif else if imagetype eq 'object' then begin
         obj=obj+1
         xpk = sxpar(hdr,'XPEAK')
         ypk = sxpar(hdr,'YPEAK')
         peak = sxpar(hdr,'IMAGEMAX')
         fwhm = sxpar(hdr,'SEEING')
         seeing = ((fwhm*scale) > (-90)) < 90
         seeing = string(seeing,format='(f7.1)')
      endif else begin
         print,self,'FATAL ERROR!  Image type, [',imagetype,'] not recognized'
         free_lun,dblun
         return
      endelse

      exptime  = sxpar(hdr,'EXPTIME')
      jdmid = jd + exptime/86400.0d0
      if jdmid lt jdmin then jdmin = jdmid
      if jdmid gt jdmax then jdmax = jdmid
      jdstr,jdmid,300,jds
      jdmid = string(jdmid,format='(f13.5)')
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
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from image where filename='+quote(filename)+';'
            mysqlcmd,dblun,chk,answer
         endif
         if not silent then print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 and not silent then print,answer,nlines
      endif else if nlines gt 1 and quickskip then begin
         if not silent then print,'QUICKSKIP enabled, entry found for this directory, quitting.'
         free_lun,dblun
         return
      endif

      ; data for instrument table
      exptime=string(exptime,format='(f8.1)')
      filnum = sxpar(hdr,'FILPOS*')
      filtname = strtrim(sxpar(hdr,'FILTER*'),2)
      jdstr,jd,300,jds
      ntpstate = strtrim(sxpar(hdr,'NTPSTATE'),2)
      ntpmaxerr = sxpar(hdr,'NTPMAXER')
      ntpesterr = sxpar(hdr,'NTPESTER')
      nx = strn(sxpar(hdr,'NAXIS1'))
      ny = strn(sxpar(hdr,'NAXIS2'))
      xbin = strn(sxpar(hdr,'CDELT1'))
      ybin = strn(sxpar(hdr,'CDELT2'))
      gain = string(sxpar(hdr,'GAIN'),format='(f8.3)')
      rdnoise = string(sxpar(hdr,'RDNOISE'),format='(f8.2)')
      scale = string(sxpar(hdr,'SCALE'),format='(f8.3)')
      rotang = string(sxpar(hdr,'ROTANG'),format='(f11.6)')
      tdet = string(sxpar(hdr,'CCDTEMP'),format='(f8.1)')
      tset = string(sxpar(hdr,'CCDSETP'),format='(f8.1)')
      hpower = string(sxpar(hdr,'HTRPWR'),format='(f8.3)')
      ttip = string(sxpar(hdr,'COLDTIP'),format='(f8.1)')
      programv=strtrim(sxpar(hdr,'PROGRAMV'),2)
      utildspv=strtrim(sxpar(hdr,'UTILDSPV'),2)
      timdspv =strtrim(sxpar(hdr,'TIMDSPV'),2)
      pcidspv =strtrim(sxpar(hdr,'PCIDSPV'),2)
      cmd = 'insert into instrument values ('
      cmd = [cmd,quote(filename)+c+exptime+c]
      cmd = [cmd,strn(filnum[0])+c+quote(filtname[0])+c]
      cmd = [cmd,strn(filnum[1])+c+quote(filtname[1])+c]
      cmd = [cmd,jds+c+quote(ntpstate)+c+strn(ntpmaxerr)+c+strn(ntpesterr)+c]
      cmd = [cmd,nx+c+ny+c+xbin+c+ybin+c+gain+c+rdnoise+c+scale+c+rotang+c]
      cmd = [cmd,tdet+c+tset+c+hpower+c+ttip+c+quote(programv)+c]
      cmd = [cmd,quote(utildspv)+c+quote(timdspv)+c+quote(pcidspv)+');']

      ; check to see if it's already there
      chk='select filename from instrument where filename='+quote(filename)+';'
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from instrument where filename='+quote(filename)+';'
            mysqlcmd,dblun,chk,answer
         endif
         if not silent then print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 and not silent then print,answer
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
      tubetemp = string(sxpar(hdr,'TUBETEMP'),format='(f8.2)')
      ttube = string(sxpar(hdr,'TELTUBE'),format='(f8.2)')
      tprim = string(sxpar(hdr,'PRIMIR'),format='(f8.2)')
      tdomeair = string(sxpar(hdr,'DOMEAIR'),format='(f8.2)')
      domestat = strtrim(sxpar(hdr,'DOMESTAT'),2)
      cmd = 'insert into telescope values ('
      cmd = [cmd,quote(filename)+c+ra+c+dec+c+epoch+c]
      cmd = [cmd,equinox+c+hangle+c+airmass+c+focus+c]
      cmd = [cmd,tubetemp+c+ttube+c+tprim+c+tdomeair+c+quote(domestat)+');']

      ; check to see if it's already there
      chk='select filename from telescope where filename='+quote(filename)+';'
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from telescope where filename='+quote(filename)+';'
            mysqlcmd,dblun,chk,answer
         endif
         if not silent then print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 and not silent then print,answer
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
      if finite(barom) eq 0 then barom=0.0
      barom = string(barom,format='(f6.2)')
      relhum = sxpar(hdr,'RELHUM')
      if finite(relhum) eq 0 then relhum=0.0
      relhum = string(relhum,format='(f6.1)')
      dewpt = sxpar(hdr,'DEWPT')
      dewpt = string(dewpt,format='(f6.1)')
      cmd = 'insert into weather values ('
      cmd = [cmd,quote(filename)+c+temp+c+wspeed+c+wdirect+c+quote(wdir)+c]
      cmd = [cmd,barom+c+relhum+c+dewpt+',NULL);']

      ; check to see if it's already there
      chk='select filename from weather where filename='+quote(filename)+';'
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from weather where filename='+quote(filename)+';'
            mysqlcmd,dblun,chk,answer
         endif
         if not silent then print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 and not silent then print,answer
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
   cmd = 'insert into runstat values ('
   cmd = [cmd,quote(rundate)+c+quote(obsdate)+c+quote(conditions)+c]
   cmd = [cmd,quote(datastat)+c+strn(obj)+c+strn(bias)+c]
   cmd = [cmd,strn(flat)+',NULL);']

   ; check to see if it's already there
   chk='select obsdate from runstat where obsdate='+quote(obsdate)+';'
   mysqlcmd,dblun,chk,answer,nlines
   if overwrite or nlines eq 1 then begin
      if nlines gt 1 then begin
         chk='delete from runstat where obsdate='+quote(obsdate)+';'
         mysqlcmd,dblun,chk,answer
      endif
      if not silent then print,cmd
      mysqlcmd,dblun,cmd,answer,nlines
      if nlines gt 0 and not silent then print,answer
   endif

   jdmins = string(jdmin,format='(f13.5)')
   jdmaxs = string(jdmax,format='(f13.5)')

   ; create ASCII table of contents file
   fntoc = ldir+'toc.txt'
   if overwrite then file_delete,fntoc,/quiet
   if not exists(fntoc) then begin
      cmd='select object from image where jdmid>='+jdmins+ $
          ' and jdmid<='+jdmaxs+' order by filename;'
      mysqlcmd,dblun,cmd,answer,nlines
      object = answer[1:*]

      cmd='select image.filename,jdmid,exptime,fil1,filtname1,fil2,filtname2,'
      cmd = [cmd,'type,nx,ny,xbin,ybin,fwhm,sky,skysig,peak,xpk,ypk']
      cmd = [cmd,'from image,instrument']
      cmd = [cmd,'where image.filename=instrument.filename and']
      cmd = [cmd,'jdmid>='+jdmins+' and jdmid<='+jdmaxs]
      cmd = [cmd,'order by image.filename;']
      mysqlquery,dblun,cmd,fn,jdmid,exptime,fil1,filtname1,fil2,filtname2, $
         type,nx,ny,xbin,ybin,fwhm,sky,skyerr,peak,xpk,ypk, $
         format='a,d,f,i,a,i,a,a,i,i,i,i,f,f,f,l,i,i'
      jdstr,jdmid,1,jds
      jdstart = jdmid - (exptime/2.0)/86400.0d0
      jdstr,jdstart,1,jdstartstr

      cmd='select airmass,hangle,ra,decl,equinox,focus'
      cmd = [cmd,'from image,telescope']
      cmd = [cmd,'where image.filename=telescope.filename and']
      cmd = [cmd,'jdmid>='+jdmins+' and jdmid<='+jdmaxs]
      cmd = [cmd,'order by image.filename;']
      mysqlquery,dblun,cmd,airmass,hangle,ra,dec,equinox,focus, $
         format='f,d,d,d,f,i'
      hastr,hangle,-2,has
      lsidtim,jdmid,lon,lst
      rastr,lst,-2,lsts
      rastr,ra,1,ras
      decstr,dec,0,decs

      cmd='select tubetemp,ttube,tprim,tdomeair,airtemp,relhum,dewpt,barom,wspeed,wdir'
      cmd = [cmd,'from image,weather,telescope']
      cmd = [cmd,'where image.filename=weather.filename and']
      cmd = [cmd,'image.filename=telescope.filename and']
      cmd = [cmd,'jdmid>='+jdmins+' and jdmid<='+jdmaxs]
      cmd = [cmd,'order by image.filename;']
      mysqlquery,dblun,cmd,tubetemp,ttube,tprim,tdomeair,airtemp,relhum,dewpt,barom,wspeed,wdir, $
         format='f,f,f,f,f,f,f,f,i,a'

      nlines = n_elements(object)
      if nlines ne nfiles then begin
         print,self,'FATAL ERROR!   Database returns ',strn(nlines),' entries'
         print,'but there are ',strn(nfiles),' files being processed.'
         print,'Filenames'
         print,fnlist
         print,'database'
         print,fn
         free_lun,dblun
         return
      endif

      openw,lun,fntoc,/get_lun,width=1000
      for i=0,nlines-1 do begin
         printf,lun,fn[i],object[i]+blanks,exptime[i], $
            fil1[i],filtname1[i]+blanks,fil2[i],filtname2[i]+blanks, $
            type[i],jds[i],airmass[i],has[i],lsts[i],ras[i],decs[i], $
            equinox[i],nx[i],ny[i],xbin[i],ybin[i],focus[i],tubetemp[i], $
            ttube[i],tprim[i],tdomeair[i], $
            fwhm[i],peak[i],xpk[i],ypk[i],sky[i],skyerr[i], $
            format='(a,1x,a30,1x,f8.3,1x,2(i2,1x,a6),1x,a6,' + $
                   '1x,a,1x,f4.2,4(1x,a),1x,f7.2,2(1x,i4),2(1x,i1),' + $
                   '1x,i5,4(1x,f6.2),1x,f4.1,1x,i5,2(1x,i4),1x,f7.1,1x,f5.1)'
      endfor
      free_lun,lun
   endif

   ; Final thing to do, deduce the calibration file sets and generate the
   ;   database entries for them.  A set must contain 5 or more images to
   ;   be recorded.

   ; Check for biases
   if bias ge 5 then begin
      cmd='select filename from image'
      cmd = [cmd,'where jdmid>='+jdmins+' and jdmid<='+jdmaxs+' and']
      cmd = [cmd,'object='+quote('Bias')]
      cmd = [cmd,'order by filename;']
      mysqlquery,dblun,cmd,fn,format='a'
      n_files=n_elements(fn)
      if n_files ne bias then begin
         print,self,'FATAL ERROR!  The database says there are ',strn(n_files),' bias'
         print,'  files but earlier processing determined there were ',strn(bias)
      endif
      ;fnum = lonarr(n_files)
      ;for i=0,n_files-1 do begin
      ;   words=strsplit(fn[i],'.',/extract)
      ;   fnum[i] = long(words[1])
      ;endfor
      ;numstr=''
      ;i0 = fnum[0]
      ;i1 = fnum[0]
      ;for i=0,n_files-1 do begin
      ;   if fnum[i]-1 ne i1 then begin
      ;      if i0 ne i1 then begin
      ;         numstr += '-'+strn(i1)
      ;      endif 
      ;      i0 = fnum[i]
      ;      i1 = fnum[i]
      ;      if strlen(numstr) ne 0 then numstr += ','
      ;      numstr += strn(i0)
      ;   endif else begin
      ;      i1 = fnum[i]
      ;   endelse
      ;endfor
      ;if i0 ne i1 then begin
      ;   numstr += '-'+strn(i1)
      ;endif
      

      ; We COULD look for and remove flats too bright or faint here, but the
      ; fear is that the numstr generated would overflow the column in calib.
      numtoflist, strmid(fn,6), numstr, /RANGEPAR,/ROBOCCD

      cmd = 'insert into calib values ('
      cmd = [cmd,quote(obsdate)+c+quote('bias')+c+'NULL,NULL,NULL,NULL'+c]
      cmd = [cmd,quote(numstr)+c]
      cmd = [cmd,quote(obsdate+'.bias')+c+quote('unknown')+');']
      ; check to see if it's already there
      chk='select obsdate from calib where obsdate='+quote(obsdate)+ $
          ' and numstr='+quote(numstr)+';'
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from calib where obsdate='+quote(obsdate)+ $
                ' and numstr='+quote(numstr)+';'
            mysqlcmd,dblun,chk,answer
         endif
         if not silent then print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 and not silent then print,answer,nlines
      endif

   endif

   ; Check for flats
   if flat ge 5 then begin
      cmd='select image.filename,fil1,filtname1,fil2,filtname2 from image,instrument'
      cmd = [cmd,'where jdmid>='+jdmins+' and jdmid<='+jdmaxs+' and']
      cmd = [cmd,'image.filename=instrument.filename and']
      cmd = [cmd,'object='+quote('Flat')]
      cmd = [cmd,'order by filename;']
      mysqlquery,dblun,cmd,fn,fil1,name1,fil2,name2,format='a,i,a,i,a'
      n_files=n_elements(fn)
      if n_files ne flat then begin
         print,self,'FATAL ERROR!  The database says there are ',strn(n_files),' flat'
         print,'  files but earlier processing determined there were ',strn(flat)
         free_lun,dblun
         return
      endif

      allfil = strcompress(string(fil1)+'+'+string(fil2),/remove_all)
      fillist = allfil[uniq(allfil,sort(allfil))]

      for j=0,n_elements(fillist)-1 do begin
         words=strsplit(fillist[j],'+',/extract)
         f1=fix(words[0])
         f2=fix(words[1])
         z=where(fil1 eq f1 and fil2 eq f2,n_files)
         ;fnum = lonarr(n_files)
         ;for i=0,n_files-1 do begin
         ;   words=strsplit(fn[z[i]],'.',/extract)
         ;   fnum[i] = long(words[1])
         ;endfor
         ;numstr=''
         ;i0 = fnum[0]
         ;i1 = fnum[0]
         ;for i=0,n_files-1 do begin
         ;   if fnum[i]-1 ne i1 then begin
         ;      if i0 ne i1 then begin
         ;         numstr += '-'+strn(i1)
         ;      endif 
         ;      i0 = fnum[i]
         ;      i1 = fnum[i]
         ;      if strlen(numstr) ne 0 then numstr += ','
         ;      numstr += strn(i0)
         ;   endif else begin
         ;      i1 = fnum[i]
         ;   endelse
         ;endfor

         ;if i0 eq i1 then begin
         if n_files le 1 then begin
            print,'Flat field does not have enough files, skipping.'
            continue
         endif else begin
            numtoflist, strmid(fn[z],6), numstr, /RANGEPAR,/ROBOCCD
         endelse
         if name1[z[0]] eq 'Open' and name2[z[0]] eq 'Open' then $
            filname = 'Open' $
         else if name2[z[0]] eq 'Open' then $
            filname = name1[z[0]] $
         else $
            filname = name2[z[0]]
         cmd = 'insert into calib values ('
         cmd = [cmd,quote(obsdate)+c+quote('flat')+c]
         cmd = [cmd,strn(fil1[z[0]])+c+quote(name1[z[0]])+c]
         cmd = [cmd,strn(fil2[z[0]])+c+quote(name2[z[0]])+c]
         cmd = [cmd,quote(numstr)+c]
         cmd = [cmd,quote(obsdate+'.'+filname)+c+quote('unknown')+');']
         ; check to see if it's already there
         chk='select obsdate from calib where obsdate='+quote(obsdate)+ $
             ' and filt1='+strn(fil1[z[0]])+ $
             ' and filt2='+strn(fil2[z[0]])+ $
             ' and numstr='+quote(numstr)+';'
         mysqlcmd,dblun,chk,answer,nlines
         if overwrite or nlines eq 1 then begin
            if nlines gt 1 then begin
               chk='delete from calib where obsdate='+quote(obsdate)+ $
                   ' and filt1='+strn(fil1[z[0]])+ $
                   ' and filt2='+strn(fil2[z[0]])+ $
                   ' and numstr='+quote(numstr)+';'
               mysqlcmd,dblun,chk,answer
            endif
            if not silent then print,cmd
            mysqlcmd,dblun,cmd,answer,nlines
            if nlines gt 0 and not silent then print,answer,nlines
         endif
      endfor

   endif

   free_lun,dblun

end

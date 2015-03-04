;+
; NAME:
;  nasacamload
; PURPOSE:   (one line only)
;  Load information from NASACAM data into mysql database
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  nasacamload,dir
; INPUTS:
;  dir - directory in which to look for data (default=current directory)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWRITE - flag, if set will force overwrite existing records
;  DBNAME    - database name (default='nasacam')
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2006/01/02
;  2006/03/28, MWB, added support for new database fields.
;-
pro nasacamload,dir,OVERWRITE=overwrite,DBNAME=dbname,QUICKSKIP=quickskip

   self='NASACAMLOAD: '
   if badpar(dir,[0,7],0,caller=self+'(dir) ',default='') then return
   if badpar(overwrite,[0,1,2,3],0,caller=self+'(OVERWRITE) ', $
                                   default=0) then return
   if badpar(dbname,[0,7],       0,caller=self+'(DBNAME) ', $
                                   default='nasacam') then return
   if badpar(quickskip,[0,1,2,3],0,caller=self+'(QUICKSKIP) ', $
                                   default=0) then return

   scale = 0.515
   ; hard coded for Anderson Mesa
   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi

   c=','
   blanks='                                       '

   ; Get the list of images to process
   if dir ne '' then ldir=addslash(dir) else ldir=dir
   fnlist0 = file_search(ldir+'[0-9][0-9]*.[0-9][0-9][0-9]',count=count0)
   fnlist1 = file_search(ldir+'[0-9][0-9]*.[0-9][0-9][0-9].fits',count=count1)

   nfiles = count0+count1

   print,'Found ',strn(nfiles),' files to process.'
   if nfiles eq 0 then return

   if count0 gt 0 then begin
      if count1 gt 0 then begin
         fnlist = [fnlist0,fnlist1]
      endif else begin
         fnlist = fnlist0
      endelse
   endif else begin
      fnlist = fnlist1
   endelse

   ; sort file name list.  This is hard because the image number field of
   ;   the file name can be either 3 or 4 digits and there may or may not
   ;   be a .fits on the end of the file name (which must be ignored).
   filename = file_basename(fnlist)
   filename_c = strarr(nfiles)
   fnum = lonarr(nfiles)

   for i=0,nfiles-1 do begin
      words=strsplit(filename[i],'.',/extract)
      fnum[i] = long(words[1])
      filename_c[i] = words[0]+'.'+words[1]
   endfor
   z=sort(fnum)
   fnlist     = fnlist[z]
   fnum       = fnum[z]
   filename   = filename[z]
   filename_c = filename_c[z]

   openmysql,dblun,dbname

   first=1
   bias=0
   dark=0
   flat=0
   obj=0

   for i=0,nfiles-1 do begin
      print,filename_c[i]

      ; get header and the filename
      image = readfits(fnlist[i],hdr)
      sz=size(image,/dimen)
      nx = sz[0]
      ny = sz[1]
      if first then begin
         instrument=strtrim(sxpar(hdr,'LCAMMOD'),2)
         if instrument ne 'NASAcam' then begin
            print,self,'data are not from NASAcam.'
            free_lun,dblun
            return
         endif
         first=0
         jdmin = jdparse(sxpar(hdr,'DATE-OBS'))
         jdmax = jdmin
      endif
      xbin = fix(sxpar(hdr,'CRDELT1'))
      ybin = fix(sxpar(hdr,'CRDELT2'))
 
      jdstring = strtrim(sxpar(hdr,'DATE-OBS'),2)
      if jdstring eq 'T' then begin
         print,self,'FATAL ERROR!  Bad DATE-OBS string.'
         print,fnlist[i]
         free_lun,dblun
         return
      endif
      jd = jdparse(jdstring)

      ; data for image table
      object   = strtrim(sxpar(hdr,'OBJECT'),2)
      imagetype= strlowcase(strtrim(sxpar(hdr,'OBSTYPE'),2))
      if imagetype eq 'bias' then begin
         bias=bias+1
         object = 'Bias'
         fwhm=-1.0
         xpk = -1
         ypk = -1
         peak = -1
         seeing = '-1'
      endif else if imagetype eq 'dark' then begin
         dark=dark+1
         object = 'Dark'
         fwhm=-1.0
         xpk = -1
         ypk = -1
         peak = -1
         seeing = '-1'
      endif else if imagetype eq 'flat' then begin
         flat=flat+1
         object = 'Flat'
         fwhm=-1.0
         xpk = -1
         ypk = -1
         peak = -1
         seeing = '-1'
      endif else if imagetype eq 'object' then begin
         obj=obj+1
         boxm,image,nx/2,ny/2,500<(nx/2-1),500<(ny/2-1),xpk,ypk
         peak = image[xpk,ypk]
         basphote,1.0,image,1.0,xpk,ypk,10.0,15.0,30.0,/nolog,/silent, $
            boxmrad=-10,fwhm=fwhm
         seeing = ((fwhm*scale*xbin) > (-90)) < 90
         seeing = strtrim(string(seeing,format='(f7.1)'),2)
      endif else begin
         print,self,'FATAL ERROR!  Image type, [',imagetype,'] not recognized'
         free_lun,dblun
         return
      endelse

      exptime  = sxpar(hdr,'EXPTIME')
      jdmid = jd + (exptime/2.0)/86400.0d0
      if jdmid lt jdmin then jdmin = jdmid
      if jdmid gt jdmax then jdmax = jdmid
      jdstr,jdmid,300,jds
      jdmid = string(jdmid,format='(f13.5)')
      skysclim,image,lowval,hival,sky,skysig,npts=5000
      sky = strtrim(string(sky,format='(f7.1)'),2)
      skysig = strtrim(string(skysig,format='(f7.1)'),2)
      if nx gt 2080 then begin
         robomean,image[2072:*,*],3.0,0.5,overscan,dummy,osnoise
         overscan = string(overscan,format='(f10.1)')
         osnoise  = string(osnoise,format='(f10.1)')
      endif else begin
         overscan = '0.0'
         osnoise  = '0.0'
      endelse

      cmd = 'insert into image values ('
      cmd = [cmd,quote(filename_c[i])+c+quote(object)+c+quote(imagetype)+c]
      cmd = [cmd,jdmid+c+jds+c+overscan+c+osnoise+c]
      cmd = [cmd,seeing+c+strn(peak)+c+strn(xpk)+c+strn(ypk)+c]
      cmd = [cmd,sky+c+skysig+');']

      ; check to see if it's already there
      chk='select filename from image where filename='+quote(filename_c[i])+';'
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from image where filename='+quote(filename_c[i])+';'
            mysqlcmd,dblun,chk,answer
         endif
         print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 then print,answer,nlines
      endif else if nlines gt 1 and quickskip then begin
         print,'QUICKSKIP enabled, entry found for this directory, quitting.'
         free_lun,dblun
         return
      endif

      ; data for instrument table
      exptime=string(exptime,format='(f8.3)')
      fil1 = strn(sxpar(hdr,'FILT_1'))
      filtname1 = strtrim(sxpar(hdr,'FILTNME1'),2)
      fil2 = strn(sxpar(hdr,'FILT_2'))
      filtname2 = strtrim(sxpar(hdr,'FILTNME2'),2)
      timesrc=sxpar(hdr,'TIMESRC')
      if timesrc eq 'Unknown Clock Source' then begin
         timerror = -1L
      endif else begin
         timequal=sxpar(hdr,'TIMEQUAL')
         pos=strpos(timequal,'est_error=')
         timerror=long(strmid(timequal,pos+10,99))
      endelse
      prescan=sxpar(hdr,'PRESCAN')
      postscan=sxpar(hdr,'POSTSCAN')
      jdstr,jd,300,jdst
      gain = '3.0'
      tdet=sxpar(hdr,'DETTEMP')
      tset=sxpar(hdr,'SETTEMP')
      tct =sxpar(hdr,'CTTEMP')
      ttube =sxpar(hdr,'TUBETEMP')
      cmd = 'insert into instrument values ('
      cmd = [cmd,quote(filename_c[i])+c+exptime+c+strn(timerror)+c]
      cmd = [cmd,fil1+c+quote(filtname1)+c]
      cmd = [cmd,fil2+c+quote(filtname2)+c]
      cmd = [cmd,jdst+c+strn(prescan)+c+strn(postscan)+c]
      cmd = [cmd,strn(nx)+c+strn(ny)+c+strn(xbin)+c+strn(ybin)+c+gain+c]
      cmd = [cmd,strn(tdet)+c+strn(tset)+c+strn(tct)+c+strn(ttube)+');']

      ; check to see if it's already there
      chk='select filename from instrument where filename='+quote(filename_c[i])+';'
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from instrument where filename='+quote(filename_c[i])+';'
            mysqlcmd,dblun,chk,answer
         endif
         print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 then print,answer
      endif

      ; data for telescope table
      ras  = sxpar(hdr,'TELRA')
      ra   = raparse(ras)
      ras  = strtrim(string(ra,format='(f16.13)'),2)
      decs = sxpar(hdr,'TELDEC')
      dec  = decparse(decs)
      decs = strtrim(string(dec,format='(f16.13)'),2)
      equinox = sxpar(hdr,'EQUINOX')
      equinox = string(equinox,format='(f7.2)')
      jd2year,jd,epoch
      epoch = string(epoch,format='(f7.2)')
      lsidtim,jd,lon,lst
      ha = lst-ra
      if ha gt  !pi then ha -= 2.0*!pi
      if ha lt -!pi then ha += 2.0*!pi
      hangle = strtrim(string(ha,format='(f16.13)'),2)
      airmass = sxpar(hdr,'AIRMASS')
      airmass = strtrim(string(airmass,format='(f6.3)'),2)
      focus = long(sxpar(hdr,'TELFOCUS'))
      focus = strn(focus)
      cmd = 'insert into telescope values ('
      cmd = [cmd,quote(filename_c[i])+c+ras+c+decs+c+epoch+c]
      cmd = [cmd,equinox+c+hangle+c+airmass+c+focus+');']

      ; check to see if it's already there
      chk='select filename from telescope where filename='+quote(filename_c[i])+';'
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from telescope where filename='+quote(filename_c[i])+';'
            mysqlcmd,dblun,chk,answer
         endif
         print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 then print,answer
      endif

      ; data for weather table
      temp = sxpar(hdr,'TEMP_OUT')
      temp = strtrim(string(temp,format='(f6.1)'),2)
      wspeed = sxpar(hdr,'WIND_V')
      wspeed = strn(fix(wspeed))
      wdirect = sxpar(hdr,'WIND_D')
      wdir = windstr(wdirect)
      wdirect = strn(fix(wdirect))
      barom = sxpar(hdr,'BAROMETE')
      barom = strtrim(string(barom,format='(f6.2)'),2)
      relhum = sxpar(hdr,'HMID_OUT')
      relhum = strtrim(string(relhum,format='(f6.1)'),2)
      dewpt = sxpar(hdr,'DEWP_OUT')
      dewpt = strtrim(string(dewpt,format='(f6.1)'),2)
      tubetemp = sxpar(hdr,'TUBETEMP')
      tubetemp = tubetemp*9.0/5.0+32.0
      tubetemp = strtrim(string(tubetemp,format='(f6.1)'),2)
      cmd = 'insert into weather values ('
      cmd = [cmd,quote(filename_c[i])+c+temp+c+wspeed+c+wdirect+c+quote(wdir)+c]
      cmd = [cmd,barom+c+relhum+c+dewpt+c+tubetemp+');']

      ; check to see if it's already there
      chk='select filename from weather where filename='+quote(filename_c[i])+';'
      mysqlcmd,dblun,chk,answer,nlines
      if overwrite or nlines eq 1 then begin
         if nlines gt 1 then begin
            chk='delete from weather where filename='+quote(filename_c[i])+';'
            mysqlcmd,dblun,chk,answer
         endif
         print,cmd
         mysqlcmd,dblun,cmd,answer,nlines
         if nlines gt 0 then print,answer
      endif

   endfor

   jdmins = string(jdmin,format='(f13.5)')
   jdmaxs = string(jdmax,format='(f13.5)')

   ; create ASCII table of contents file
   fntoc = ldir+'toc.txt'
   if overwrite then file_delete,fntoc,/quiet
   fnlog = ldir+'toc.ps'
   if overwrite then file_delete,fnlog,/quiet
   if not exists(fntoc) or not exists(fnlog) then begin
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

      cmd='select tubetemp,airtemp,relhum,dewpt,barom,wspeed,wdir'
      cmd = [cmd,'from image,weather']
      cmd = [cmd,'where image.filename=weather.filename and']
      cmd = [cmd,'jdmid>='+jdmins+' and jdmid<='+jdmaxs]
      cmd = [cmd,'order by image.filename;']
      mysqlquery,dblun,cmd,tubetemp,airtemp,relhum,dewpt,barom,wspeed,wdir, $
         format='f,f,f,f,f,i,a'

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
      imid = nlines/2
      hdr = headfits(fnlist[imid])
      observer = sxpar(hdr,'OBSERVER')

      if not exists(fntoc) then begin
         openw,lun,fntoc,/get_lun,width=1000
         for i=0,nlines-1 do begin
            printf,lun,fn[i],object[i]+blanks,exptime[i], $
               fil1[i],filtname1[i]+blanks,fil2[i],filtname2[i]+blanks, $
               type[i],jds[i],airmass[i],has[i],lsts[i],ras[i],decs[i], $
               equinox[i],nx[i],ny[i],xbin[i],ybin[i],focus[i],tubetemp[i], $
               fwhm[i],peak[i],xpk[i],ypk[i],sky[i],skyerr[i], $
               format='(a,1x,a30,1x,f8.3,1x,2(i2,1x,a6),1x,a6,' + $
                      '1x,a,1x,f4.2,4(1x,a),1x,f7.2,2(1x,i4),2(1x,i1),' + $
                      '1x,i5,1x,f5.1,1x,f4.1,1x,i5,2(1x,i4),1x,f7.1,1x,f5.1)'
         endfor
         free_lun,lun
      endif

      if not exists(fnlog) then begin
         npages = ceil(float(nlines)/50)
         curjd=systime(/julian)
         jdstr,curjd,110,curjds
         openw,lun,fnlog,/get_lun,width=1000
         printf,lun,'%!PS-Adobe-1.0'
         printf,lun,'%%DocumentFonts: Courier Times-Roman Times-Bold Helvetica-Bold'
         printf,lun,'%%Title: nasacamload printout'
         printf,lun,'%%Creator: nasacamload.pro, Written by Marc W. Buie'
         printf,lun,'%%CreationDate: ',curjds
         printf,lun,'%%Pages: ',strn(npages)
         printf,lun,'%%BoundingBox: 0 0 792 612'
         printf,lun,'%%EndComments'

         printf,lun,'statusdict begin false setduplexmode'
         printf,lun,'/top 7 72 mul def'
         printf,lun,'/lintop top 40 sub def'
         printf,lun,'/bottom 52 def'
         printf,lun,'/height 9 def'
         printf,lun,'/lines 50 def'
         printf,lun,'/inch { 72 mul } def'
         printf,lun,'/cm {2.54 div inch} def'
         printf,lun,'/cshow { dup stringwidth pop 2 div neg 0 rmoveto show } def'
         printf,lun,'/rshow { dup stringwidth neg exch neg exch rmoveto show } def'
         printf,lun,'/logo {'
         printf,lun,' /h exch def'
         printf,lun,' /y exch def'
         printf,lun,' /x exch def'
         printf,lun,' /sc h 10 div def'
         printf,lun,' /logogray { 0.5 setgray } def'
         printf,lun,' logogray'
         printf,lun,' newpath'
         printf,lun,' x y moveto 9.2  sc mul 0 rlineto 0 1.2 sc mul rlineto'
         printf,lun,'           -8.85 sc mul 0 rlineto 0 8.8 sc mul rlineto'
         printf,lun,'           -0.35 sc mul 0 rlineto 0 -10 sc mul rlineto closepath fill'
         printf,lun,'  newpath'
         printf,lun,'  x 4.925 sc mul add   y 5.83 sc mul add  4.175 sc mul 0 360 arc fill'
         printf,lun,'  newpath'
         printf,lun,'  x 3.95 sc mul add y 2.38 sc mul add moveto'
         printf,lun,'        1.82 sc mul 0 rlineto 0 0.4 sc mul rlineto'
         printf,lun,'       -1.2  sc mul 0 rlineto 0 1.0 sc mul rlineto'
         printf,lun,'        0.9  sc mul 0.05 sc mul rlineto'
         printf,lun,'        1.0  sc mul 0'
         printf,lun,'        1.05 sc mul 2.35 sc mul'
         printf,lun,'       -0.3  sc mul 2.25 sc mul rcurveto'
         printf,lun,'       -0.6  sc mul 0.00 sc mul rlineto'
         printf,lun,'       -0.62 sc mul 0 rlineto closepath 1 setgray fill'
         printf,lun,' newpath'
         printf,lun,' x 4.57 sc mul add y 4.28 sc mul add moveto'
         printf,lun,'     0.6  sc mul 0 rlineto'
         printf,lun,'     0.75 sc mul 0'
         printf,lun,'     0.75 sc mul 1.3  sc mul'
         printf,lun,'     0           1.3  sc mul rcurveto'
         printf,lun,'      -0.6  sc mul 0 rlineto closepath logogray fill'
         printf,lun,'0 setgray'
         printf,lun,'} def'
         printf,lun,'/row {'
         printf,lun,' 1.35 add height 1 sub mul lintop exch sub'
         printf,lun,' /ypos exch def'
         printf,lun,' left'
         printf,lun,' 0.1  cm add dup ypos moveto exch show'
         printf,lun,' 0.6  cm add dup ypos moveto exch show'
         printf,lun,' 4.0  cm add dup ypos moveto exch show'
         printf,lun,' 1.4  cm add dup ypos moveto exch show'
         printf,lun,' 1.0  cm add dup ypos moveto exch show'
         printf,lun,' 1.5  cm add dup ypos moveto exch show'
         printf,lun,' 0.71 cm add dup ypos moveto exch show'
         printf,lun,' 1.00 cm add dup ypos moveto exch show'
         printf,lun,' 0.85 cm add dup ypos moveto exch show'
         printf,lun,' 1.65 cm add dup ypos moveto exch show'
         printf,lun,' 1.45 cm add dup ypos moveto exch show'
         printf,lun,' 1.05 cm add dup ypos moveto exch show'
         printf,lun,' 0.6  cm add dup ypos moveto exch show'
         printf,lun,' 0.7  cm add dup ypos moveto exch show'
         printf,lun,' 0.8  cm add dup ypos moveto exch show'
         printf,lun,' 2.1  cm add dup ypos moveto exch show'
         printf,lun,' pop'
         printf,lun,' } def'
         printf,lun,'/column {'
         printf,lun,'/cwide exch def'
         printf,lun,'/Helvetica-Narrow findfont [ cwide 0 0 height 0 0 ] makefont setfont'
         printf,lun,'3 1 roll'
         printf,lun,'2 copy'
         printf,lun,'5 2 roll'
         printf,lun,'3 copy add 2 div lintop height sub moveto cshow'
         printf,lun,'3 -1 roll pop'
         printf,lun,'lintop dup'
         printf,lun,'3 1 roll'
         printf,lun,'newpath moveto lineto stroke'
         printf,lun,'dup'
         printf,lun,'newpath lintop moveto bottom lineto stroke'
         printf,lun,'dup'
         printf,lun,'newpath lintop moveto bottom lineto stroke'
         printf,lun,'} def'
         printf,lun,'%%EndProlog'

         for i=0,npages-1 do begin

            printf,lun,'%%Page: ',strn(i+1),' ',strn(npages)

            printf,lun,'-90 rotate 72 -11 mul 0 translate'
            printf,lun,'0 -0.3 inch translate'
            printf,lun,'0.5 inch 7.2 inch 0.5 inch logo'
            printf,lun,'/Times-Bold findfont 14 scalefont setfont'
            printf,lun,'1.05 inch 7.5 inch moveto (Lowell Observatory) show'
            printf,lun,'1.45 inch 7.3 inch moveto (NASAcam) show'
            printf,lun,'/Times-Roman findfont 14 scalefont setfont'
            printf,lun,'5.5 inch 7.8 inch moveto (Lowell Observatory/Anderson Mesa) cshow'
            printf,lun,'5.5 inch 7.6 inch moveto (Observing Logsheet) cshow'
            printf,lun,'5.5 inch 7.4 inch moveto (31-in Telescope) cshow'
            printf,lun,'0.1 setlinewidth'
            printf,lun,'0.5 inch dup'
            printf,lun,'dup 0.6  cm add dup 3 1 roll (ID) height column'
            printf,lun,'dup 4.0  cm add dup 3 1 roll (Object Name) height column'
            printf,lun,'dup 1.5  cm add dup 3 1 roll (Filter) height column'
            printf,lun,'dup 0.9  cm add dup 3 1 roll (ExpT) height column'
            printf,lun,'dup 1.65 cm add dup 3 1 roll (Start Time) height column'
            printf,lun,'dup 0.6  cm add dup 3 1 roll (Airm) height column'
            printf,lun,'dup 1.0  cm add dup 3 1 roll (HA) height column'
            printf,lun,'dup 0.85 cm add dup 3 1 roll (LST) height column'
            printf,lun,'dup 1.60 cm add dup 3 1 roll (R.A.) height column'
            printf,lun,'dup 1.5  cm add dup 3 1 roll (Declination) height column'
            printf,lun,'dup 1.0  cm add dup 3 1 roll (Equinox) height column'
            printf,lun,'dup 0.8  cm add dup 3 1 roll (Focus) height column'
            printf,lun,'dup 0.7  cm add dup 3 1 roll (Temp) height column'
            printf,lun,'dup 0.9  cm add dup 3 1 roll (FWHM) height column'
            printf,lun,'dup 1.9  cm add dup 3 1 roll (Background) height column'
            printf,lun,'dup 2.3  cm add dup 3 1 roll (Peak) height column'
            printf,lun,'dup 3.35 cm add dup 3 1 roll (Comments) height column'
            printf,lun,'/right exch def'
            printf,lun,'/left exch def'
            printf,lun,'/index 0 def'
            printf,lun,'bottom'
            printf,lun,'lintop bottom sub 1.35 height mul sub lines div'
            printf,lun,'lintop 1.35 height mul sub 0.01 add'
            printf,lun,'{newpath dup left exch moveto right exch lineto'
            printf,lun,'index 5 mod 0 eq {0 setgray 1 setlinewidth}'
            printf,lun,'{0.7 setgray 0.2 setlinewidth} ifelse'
            printf,lun,'  stroke index /index exch 1 add def } for'
            printf,lun,'0.2 setlinewidth 0 setgray'
            printf,lun,'/Times-Roman findfont 11 scalefont setfont'
            printf,lun,'right 2.0 inch sub top 0.8 inch add moveto (UT Date) rshow'
            printf,lun,'newpath'
            printf,lun,'right 1.9 inch sub top 0.8 inch add moveto'
            printf,lun,'right top 0.8 inch add lineto stroke'
            printf,lun,'right 2.0 inch sub top 0.45 inch add moveto'
            printf,lun,'(Observer(s)) rshow'
            printf,lun,'newpath'
            printf,lun,'right 1.9 inch sub top 0.45 inch add moveto'
            printf,lun,'right top 0.45 inch add lineto stroke'
            printf,lun,'right top 0.1 inch add moveto'
            printf,lun,'(Page ',strn(i+1),' of ',strn(npages),') rshow'
            printf,lun,'/Courier findfont [ 10 0 0 12 0 0 ] makefont setfont'
            printf,lun,'right 1.0 inch sub top 0.83 inch add moveto (', $
                       strmid(jds[imid],0,10),') cshow'
            printf,lun,'right 1.0 inch sub top 0.48 inch add moveto (', $
                       strmid(observer,0,35),') cshow'
            printf,lun,'/Courier findfont [ height 2 sub 0 0 height 0 0 ] makefont setfont'
            for j=0,49 do begin
               k = i*50 + j
               if k eq nlines then break
               words=strsplit(fn[k],'.',/extract)
               id=words[1]
               if filtname1[k] eq 'Unknown' or filtname2[k] eq 'Unknown' then begin
                  filstr = 'Unknown'
               endif else if filtname1[k] eq 'open' and filtname2[k] eq 'open' then begin
                  filstr = 'Open('+strn(fil1[k])+','+strn(fil2[k])+')'
               endif else if filtname1[k] eq 'open' then begin
                  filstr = '2:'+strn(fil2[k])+'/'+strmid(filtname2[k],0,5)
               endif else if filtname2[k] eq 'open' then begin
                  filstr = '1:'+strn(fil1[k])+'/'+strmid(filtname1[k],0,5)
               endif else begin
                  filstr = '1:'+strn(fil1[k])+'/'+strmid(filtname1[k],0,3) + $
                           ' 2:'+strn(fil2[k])+'/'+strmid(filtname2[k],0,3)
               endelse

               if exptime[k] lt 100.0 then begin
                  fmt='(f6.3)'
               endif else if exptime[k] lt 1000.0 then begin
                  fmt='(f6.2)'
               endif else begin
                  fmt='(f6.1)'
               endelse
               expstr=string(exptime[k],format=fmt)

               startstr = strmid(jdstartstr[k],11,99)
               airmass_str = string(airmass[k],format='(f4.1)')
               equin_str = string(equinox[k],format='(f6.1)')
               temp_str = string(tubetemp[k],format='(f5.1)')
               if fwhm[k] ge 0.8 then $
                  fwhm_str = string(fwhm[k],format='(f4.1)') $
               else $
                  fwhm_str = ''
               if sky[k] lt 9999.0 then $
                  back_str = string(sky[k],skyerr[k],format='(f7.1,"/",f5.1)') $
               else $
                  back_str = string(sky[k],skyerr[k],format='(f7.0,"/",f5.0)')
               if peak[k] gt 0 then $
                  peak_str = string(peak[k],xpk[k],ypk[k],format='(i5,"@",i4,",",i4)') $
               else $
                  peak_str = ''

               printf,lun,'(',peak_str,')', $
                          '(',back_str,')', $
                          '(',fwhm_str,')', $
                          '(',temp_str,')', $
                          '(',strn(focus[k]),')', $
                          '(',equin_str,')', $
                          '(',decs[k],')', $
                          '(',ras[k],')', $
                          '(',lsts[k],')', $
                          '(',has[k],')', $
                          '(',airmass_str,')', $
                          '(',startstr,')', $
                          '(',expstr,')', $
                          '(',filstr,')', $
                          '(',object[k],')', $
                          '(',id,')', $
                          ' ',strn(j+1),' row'
            endfor


            printf,lun,'/Helvetica findfont [ 8 0 2 10 0 0 ] makefont setfont'
            printf,lun,'0.9 inch lintop 35 add moveto (Temperature) show'
            printf,lun,'2.7 inch lintop 35 add moveto (min/max) show'
            printf,lun,'0.9 inch lintop 23 add moveto (Relative Humidity) show'
            printf,lun,'2.7 inch lintop 23 add moveto (min/max) show'
            printf,lun,'0.9 inch lintop 11 add moveto (Dew Point) show'
            printf,lun,'2.7 inch lintop 11 add moveto (min/max) show'
            printf,lun,'4.5 inch lintop 35 add moveto (Barometer) show'
            printf,lun,'6.3 inch lintop 35 add moveto (min/max) show'
            printf,lun,'4.5 inch lintop 23 add moveto (Wind Speed) show'
            printf,lun,'6.3 inch lintop 23 add moveto (min/max) show'
            printf,lun,'4.5 inch lintop 11 add moveto (Wind Direction) show'
            printf,lun,'/Helvetica findfont [ 8 0 0 10 0 0 ] makefont setfont'

            str = string(airtemp[i*50],' to ',airtemp[i*50+49 < (nlines-1)],'F', $
                         format='(f6.1,a,f6.1,a)')
            str = strcompress(str)
            printf,lun,'1.9 inch lintop 35 add moveto (',str,') show'

            str = string(min(airtemp[i*50:i*50+49 < (nlines-1)]),'/', $
                         max(airtemp[i*50:i*50+49 < (nlines-1)]), $
                         format='(f6.1,a,f6.1)')
            str = strcompress(str,/remove_all)
            printf,lun,'3.2 inch lintop 35 add moveto (',str,') show'

            str = string(fix(relhum[i*50]+0.5),' to ', $
                         fix(relhum[i*50+49 < (nlines-1)]+0.5),'%', $
                         format='(i3,a,i3,a)')
            str = strcompress(str)
            printf,lun,'1.9 inch lintop 23 add moveto (',str,') show'

            str = string(fix(min(relhum[i*50:i*50+49 < (nlines-1)]+0.5)),'/', $
                         fix(max(relhum[i*50:i*50+49 < (nlines-1)]+0.5)), $
                         format='(i3,a,i3)')
            str = strcompress(str,/remove_all)
            printf,lun,'3.2 inch lintop 23 add moveto (',str,') show'

            str = string(dewpt[i*50],' to ',dewpt[i*50+49 < (nlines-1)],'F', $
                         format='(f6.1,a,f6.1,a)')
            str = strcompress(str)
            printf,lun,'1.9 inch lintop 11 add moveto (',str,') show'

            str = string(min(dewpt[i*50:i*50+49 < (nlines-1)]),'/', $
                         max(dewpt[i*50:i*50+49 < (nlines-1)]), $
                         format='(f6.1,a,f6.1)')
            str = strcompress(str,/remove_all)
            printf,lun,'3.2 inch lintop 11 add moveto (',str,') show'

            str = string(barom[i*50],' to ',barom[i*50+49 < (nlines-1)],'in', $
                         format='(f6.2,a,f6.2,a)')
            str = strcompress(str)
            printf,lun,'5.4 inch lintop 35 add moveto (',str,') show'

            str = string(min(barom[i*50:i*50+49 < (nlines-1)]),'/', $
                         max(barom[i*50:i*50+49 < (nlines-1)]), $
                         format='(f6.2,a,f6.2)')
            str = strcompress(str,/remove_all)
            printf,lun,'6.8 inch lintop 35 add moveto (',str,') show'

            str = string(wspeed[i*50],' to ', $
                         wspeed[i*50+49 < (nlines-1)],'mph', $
                         format='(i3,a,i3,a)')
            str = strcompress(str)
            printf,lun,'5.4 inch lintop 23 add moveto (',str,') show'

            str = string(min(wspeed[i*50:i*50+49 < (nlines-1)]),'/', $
                         max(wspeed[i*50:i*50+49 < (nlines-1)]), $
                         format='(i3,a,i3)')
            str = strcompress(str,/remove_all)
            printf,lun,'6.8 inch lintop 23 add moveto (',str,') show'

            printf,lun,'5.4 inch lintop 11 add moveto (',wdir[i*50],') show'

            if i eq 0 and (bias gt 0 or dark gt 0 or flat gt 0) then begin
               printf,lun,'8.0 inch lintop 40 add moveto (CALIBRATION DATA) show'
               lpos=30
               ldel=-10
               if bias gt 0 then begin
                  printf,lun,'8.0 inch lintop ',strn(lpos),' add moveto (', $
                             strn(bias,len=3),' bias  _________) show'
                  lpos += ldel
               endif
               if dark gt 0 then begin
                  printf,lun,'8.0 inch lintop ',strn(lpos),' add moveto (', $
                             strn(dark,len=3),' dark  _________) show'
                  lpos += ldel
               endif
               if flat gt 0 then begin
                  printf,lun,'8.0 inch lintop ',strn(lpos),' add moveto (', $
                             strn(flat,len=3),' flat  _________) show'
                  lpos += ldel
               endif
            endif

            printf,lun,'%  Footer - start'
            printf,lun,'showpage'
            printf,lun,'%  Footer - end'

         endfor

         printf,lun,'%%Trailer'
         free_lun,lun
         cmd='ps2pdf '+fnlog+' '+ldir+'toc.pdf'
         print,cmd
         spawn,cmd
      endif
   endif

   ; single entry in runstat for each night
   obsdate = strmid(filename_c[nlines-1],0,4) + '-' + $
             strmid(filename_c[nlines-1],4,2) + '-' + $
             strmid(filename_c[nlines-1],6,2)
   rundate=strmid(filename_c[nlines-1],2,6)
   conditions='unknown'
   datastat='raw'
   cmd = 'insert into runstat values ('
   cmd = [cmd,quote(rundate)+c+quote(obsdate)+c+quote(conditions)+c]
   cmd = [cmd,quote(datastat)+c+strn(obj)+c+strn(bias)+c]
   cmd = [cmd,strn(dark)+c+strn(flat)+');']

   ; check to see if it's already there
   chk='select obsdate from runstat where obsdate='+quote(obsdate)+';'
   mysqlcmd,dblun,chk,answer,nlines
   if overwrite or nlines eq 1 then begin
      if nlines gt 1 then begin
         chk='delete from runstat where obsdate='+quote(obsdate)+';'
         mysqlcmd,dblun,chk,answer
      endif
      print,cmd
      mysqlcmd,dblun,cmd,answer,nlines
      if nlines gt 0 then print,answer
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
      nfiles=n_elements(fn)
      if nfiles ne bias then begin
         print,self,'FATAL ERROR!  The database says there are ',strn(nfiles),' bias'
         print,'  files but earlier processing determined there were ',strn(bias)
      endif
      fnum = lonarr(nfiles)
      for i=0,nfiles-1 do begin
         words=strsplit(fn[i],'.',/extract)
         fnum[i] = long(words[1])
      endfor
      i0 = 0
      i1 = 1
      num1 = -1
      num2 = -1
      while i0 ge 0 do begin
         if i1 eq nfiles-1 then begin
            if i1-i0+1 ge 5 then begin
               num1 = i0
               num2 = i1
            endif
            i0 = -1
         endif else if fnum[i1]+1 ne fnum[i1+1] then begin
            if i1-i0+1 ge 5 then begin
               num1 = i0
               num2 = i1
            endif
            i0 = i1+1
            i1 = i0+1
         endif else begin
            i1++
         endelse

         if num1 ge 0 then begin
            cmd = 'insert into calib values ('
            cmd = [cmd,quote(obsdate)+c+quote('bias')+c+'NULL,NULL,NULL,NULL'+c]
            cmd = [cmd,strn(fnum[num1])+c+strn(fnum[num2])+c]
            cmd = [cmd,'NULL'+c+quote('unknown')+');']
            ; check to see if it's already there
            chk='select obsdate from calib where obsdate='+quote(obsdate)+ $
                ' and num1='+strn(fnum[num1])+';'
            mysqlcmd,dblun,chk,answer,nlines
            if overwrite or nlines eq 1 then begin
               if nlines gt 1 then begin
                  chk='delete from calib where obsdate='+quote(obsdate)+ $
                      ' and num1='+strn(fnum[num1])+';'
                  mysqlcmd,dblun,chk,answer
               endif
               print,cmd
               mysqlcmd,dblun,cmd,answer,nlines
               if nlines gt 0 then print,answer,nlines
            endif
            num1 = -1
            num2 = -1
         endif

      endwhile
   endif

   ; Check for darks
   if dark ge 5 then begin
      cmd='select filename from image'
      cmd = [cmd,'where jdmid>='+jdmins+' and jdmid<='+jdmaxs+' and']
      cmd = [cmd,'object='+quote('Dark')]
      cmd = [cmd,'order by filename;']
      mysqlquery,dblun,cmd,fn,format='a'
      nfiles=n_elements(fn)
      if nfiles ne dark then begin
         print,self,'FATAL ERROR!  The database says there are ',strn(nfiles),' dark'
         print,'  files but earlier processing determined there were ',strn(dark)
      endif
      fnum = lonarr(nfiles)
      for i=0,nfiles-1 do begin
         words=strsplit(fn[i],'.',/extract)
         fnum[i] = long(words[1])
      endfor
      i0 = 0
      i1 = 1
      num1 = -1
      num2 = -1
      while i0 ge 0 do begin
         if i1 eq nfiles-1 then begin
            if i1-i0+1 ge 5 then begin
               num1 = i0
               num2 = i1
            endif
            i0 = -1
         endif else if fnum[i1]+1 ne fnum[i1+1] then begin
            if i1-i0+1 ge 5 then begin
               num1 = i0
               num2 = i1
            endif
            i0 = i1+1
            i1 = i0+1
         endif else begin
            i1++
         endelse

         if num1 ge 0 then begin
            cmd = 'insert into calib values ('
            cmd = [cmd,quote(obsdate)+c+quote('dark')+c+'NULL,NULL,NULL,NULL'+c]
            cmd = [cmd,strn(fnum[num1])+c+strn(fnum[num2])+c]
            cmd = [cmd,'NULL'+c+quote('unknown')+');']
            ; check to see if it's already there
            chk='select obsdate from calib where obsdate='+quote(obsdate)+ $
                ' and num1='+strn(fnum[num1])+';'
            mysqlcmd,dblun,chk,answer,nlines
            if overwrite or nlines eq 1 then begin
               if nlines gt 1 then begin
                  chk='delete from calib where obsdate='+quote(obsdate)+ $
                      ' and num1='+strn(fnum[num1])+';'
                  mysqlcmd,dblun,chk,answer
               endif
               print,cmd
               mysqlcmd,dblun,cmd,answer,nlines
               if nlines gt 0 then print,answer,nlines
            endif
            num1 = -1
            num2 = -1
         endif

      endwhile
   endif

   ; Check for flats
   if flat ge 5 then begin
      cmd='select image.filename,fil1,filtname1,fil2,filtname2 from image,instrument'
      cmd = [cmd,'where jdmid>='+jdmins+' and jdmid<='+jdmaxs+' and']
      cmd = [cmd,'image.filename=instrument.filename and']
      cmd = [cmd,'object='+quote('Flat')]
      cmd = [cmd,'order by filename;']
      mysqlquery,dblun,cmd,fn,fil1,name1,fil2,name2,format='a,i,a,i,a'
      nfiles=n_elements(fn)
      if nfiles ne flat then begin
         print,self,'FATAL ERROR!  The database says there are ',strn(nfiles),' flat'
         print,'  files but earlier processing determined there were ',strn(flat)
         free_lun,dblun
         return
      endif
      fnum = lonarr(nfiles)
      for i=0,nfiles-1 do begin
         words=strsplit(fn[i],'.',/extract)
         fnum[i] = long(words[1])
      endfor
      i0 = 0
      i1 = 1
      num1 = -1
      num2 = -1
      while i0 ge 0 do begin
         if i1 eq nfiles-1 then begin
            if i1-i0+1 ge 5 then begin
               num1 = i0
               num2 = i1
            endif
            i0 = -1
         endif else if fnum[i1]+1 ne fnum[i1+1] or $
                       fil1[i1]   ne fil1[i1+1] or $
                       fil2[i1]   ne fil2[i1+1] then begin
            if i1-i0+1 ge 5 then begin
               num1 = i0
               num2 = i1
            endif
            i0 = i1+1
            i1 = i0+1
         endif else begin
            i1++
         endelse

         if num1 ge 0 then begin
            cmd = 'insert into calib values ('
            cmd = [cmd,quote(obsdate)+c+quote('flat')+c]
            cmd = [cmd,strn(fil1[num1])+c+quote(name1[num1])+c]
            cmd = [cmd,strn(fil2[num1])+c+quote(name2[num1])+c]
            cmd = [cmd,strn(fnum[num1])+c+strn(fnum[num2])+c]
            cmd = [cmd,'NULL'+c+quote('unknown')+');']
            ; check to see if it's already there
            chk='select obsdate from calib where obsdate='+quote(obsdate)+ $
                ' and filt1='+strn(fil1[num1])+ $
                ' and filt2='+strn(fil2[num1])+ $
                ' and num1='+strn(fnum[num1])+';'
            mysqlcmd,dblun,chk,answer,nlines
            if overwrite or nlines eq 1 then begin
               if nlines gt 1 then begin
                  chk='delete from calib where obsdate='+quote(obsdate)+ $
                      ' and filt1='+strn(fil1[num1])+ $
                      ' and filt2='+strn(fil2[num1])+ $
                      ' and num1='+strn(fnum[num1])+';'
                  mysqlcmd,dblun,chk,answer
               endif
               print,cmd
               mysqlcmd,dblun,cmd,answer,nlines
               if nlines gt 0 then print,answer,nlines
            endif
            num1 = -1
            num2 = -1
         endif

      endwhile
   endif

   free_lun,dblun

end

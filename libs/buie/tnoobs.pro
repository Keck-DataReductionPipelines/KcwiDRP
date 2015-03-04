;+
; NAME:
;  tnoobs
; PURPOSE:
;  Real-time planning and field selection tool for TNO survey observations.
; DESCRIPTION:
;  This is a non-blocking widget tool.  It expects to read a file with
;    a list of field coordinates.  Here's a few lines from a file with the
;    correct format (not including lead semi-colon):
;
;F09711   18 28 48.0    -22 04 27.1  2000.0  0.0  0.0
;F09767   18 36  0.0    -17 46 52.9  2000.0  0.0  0.0
;F09775   18 36  0.0    -22 34 52.9  2000.0  0.0  0.0
;F09850   18 45 36.0    -17 01 31.7  2000.0  0.0  0.0
;
;   The first field is the name of the field.  Next is ra, dec, and epoch.
;     The parser expects white space between each field.  Colons or commas
;     imbedded in the coordinates won't work.  This file format happens to be
;     what is expected by the CTIO telescope system.  You can load this file
;     using the "Open" option under the "File" menu.
;
;   There is a second file that is written as you tag fields with their
;     observation date and time.  This file is named 'fieldobs.dat' and is
;     written to the directory where you where when you start this program.
;     This file records the start times of the two visits to the field.
;     You must provide the y/m/d as well as h:m:s in the time field.  Here
;     are a few example lines from observed fields:
;
;F09711 2000/07/28 23:48:45 2000/07/29 02:38:50
;F09767 2000/07/28 23:41:58 2000/07/29 02:33:09
;F09775 2000/07/28 23:33:50
;F09850 2000/07/28 23:55:46 2000/07/29 02:45:07
;
;   If a field is not present, it hasn't yet been observed.  If a field has
;     only been observed once you'll only see one time (like F09775 above).
;
;   Because the tool is non-blocking, you can continue to use IDL normally
;     from the command line while this tool is running.  But, only one
;     copy of this tool is permitted at any given time.
;
; CATEGORY:
;  Astronomy
;
; CALLING SEQUENCE:
;  tnoobs
;
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  AMCRIT  - Airmass limit, default=2.5
;
;  DT      - Offset from true time in hours (default=0), this is used
;               mostly for testing.
;
;  OBSFILE - Name of the file containing the observatory code information.
;               Default is obscode.dat.  !PATH is scanned to find this file.
;
;  OBSCODE - Observatory id code, default is '688' (Lowell Observatory)
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  Only one instance at a time is allowed.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2000/07/30
;  2000/03/16, David Tucker, changed obsfile to support new rdobscod
;  2001/04/18, MWB, changed systime call.
;  2002/09/03, MWB, changed Str_sep call to strsplit
;  2002/09/09, MWB, added support for string obscode values
;-
pro tnoobs_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; save observing status
   tnoobs_obssave, state

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).alt
   ptr_free,(*state).am
   ptr_free,(*state).az
   ptr_free,(*state).dec
   ptr_free,(*state).fn
   ptr_free,(*state).fut1
   ptr_free,(*state).fut2
   ptr_free,(*state).id
   ptr_free,(*state).lha
   ptr_free,(*state).obscnt
   ptr_free,(*state).ra

   ; Free up the state structure itself.
   ptr_free, state

end

; Load the observation information file.
pro tnoobs_loadobs, state
   if (*state).nf eq 0 then return

   if exists('fieldobs.dat') then begin
      openr,lun,'fieldobs.dat',/get_lun
      line=''
      nlines=0
      while not eof(lun) do begin
         readf,lun,line,format='(a1)'
         nlines=nlines+1
      endwhile
      point_lun,lun,0
      id = strarr(nlines)
      ut1 = strarr(nlines)
      ut2 = strarr(nlines)
      for i=0,nlines-1 do begin
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         if n_elements(words) eq 3 then begin
            id[i] = words[0]
            ut1[i] = words[1]+' '+words[2] 
         endif else if n_elements(words) eq 5 then begin
            id[i] = words[0]
            ut1[i] = words[1]+' '+words[2] 
            ut2[i] = words[3]+' '+words[4] 
         endif
      endfor
      free_lun,lun
      for i=0,nlines-1 do begin
         if id[i] ne '' then begin
            z=where( (*(*state).id) eq id[i], count)
            z=z[0]
            if count ne 0 then begin
               (*(*state).fut1)[z] = ut1[i]
               (*(*state).fut2)[z] = ut2[i]
               if ut2[i] eq '' then $
                  (*(*state).obscnt)[z] = 1 $
               else $
                  (*(*state).obscnt)[z] = 2
            endif
         endif
      endfor
   endif
end

; Load a new observatory id code
pro tnoobs_newobscode, state, newid

   if newid ne 'man' then begin
      rdobscod,code,alllon,rhosinp,rhocosp,obsname,valid,FILE=(*state).obsfile
      if valid then begin
         idx=where(newid eq code,count)
         idx=idx[0]
         if (count eq 1) then begin
            lon = (360.0-alllon[idx])/180.0*!pi
            lat = atan(rhocosp[idx],rhosinp[idx])
            name=strtrim(obsname[idx],2)
            (*state).lon = lon
            (*state).lat = lat
            (*state).obscode = newid
            (*state).obsname = name
         endif else begin
            print,'Observatory code ',newid,' not found'
            newid = 'man'
         endelse
      endif else begin
         print,'Observatory code file ',(*state).obsfile,' not found.'
         newid = 'man'
      endelse
   endif

   if (*state).obscode eq 'man' then begin
      ; This is the GPS position for the 42", derived 1993 Sep 08
      (*state).lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
      (*state).lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
      (*state).obsname= 'Lowell Observatory - Anderson Mesa Station'
      (*state).obscode='688'
   endif

end

; Save the results of the observation info.
pro tnoobs_obssave, state
   if (*state).dirty eq 1 then begin
      openw,lun,'fieldobs.dat',/get_lun
      for i=0,(*state).nf-1 do begin
         if (*(*state).obscnt)[i] gt 0 then $
            printf,lun,(*(*state).id)[i],(*(*state).fut1)[i], $
               (*(*state).fut2)[i],format='(a,1x,a,1x,a)'
      endfor
      free_lun,lun
   endif
end

; This routine takes the current object list, current time, and list of
;   previous observations and generates a plot that shows current sky placement
;   of fields to be done.
pro tnoobs_plot, state

   curjd   = systime(/julian,/utc) + (*state).dt/24.0
   jdstr,curjd,-2,curtimestr
   widget_control,(*state).curtimeid,set_value='UT '+curtimestr

   if (*state).nf gt 0 then begin
      widget_control, (*state).drawwin, get_value=winnum
      wset,winnum

      moonpos,curjd,moonra,moondec,/radian
      sunpos,curjd,sunra,sundec,/radian
      mphase,curjd,moonphase
      mam = airmass(curjd,moonra,moondec,(*state).lat,(*state).lon, $
                    lst=lst,lha=mlha,alt=malt,/hardie)
      sam = airmass(curjd,sunra,sundec,(*state).lat,(*state).lon, $
                    lha=slha,alt=salt,/hardie)
      salt=salt*!radeg
      malt=malt*!radeg
      mlha = mlha*!radeg/15.0
      slha = slha*!radeg/15.0
      npts=73
      oppd = (findgen(npts)*180/(npts-1)-90)/!radeg
      oppr = replicate(sunra+!pi,npts)
      oppr1 = replicate(sunra+!pi+30.0/!radeg,npts)
      oppr2 = replicate(sunra+!pi-30.0/!radeg,npts)
      oppam = airmass(replicate(curjd,npts),oppr,oppd,(*state).lat,(*state).lon,lha=oppl,/hardie)
      oppam1 = airmass(replicate(curjd,npts),oppr1,oppd,(*state).lat,(*state).lon,lha=oppl1,/hardie)
      oppam2 = airmass(replicate(curjd,npts),oppr2,oppd,(*state).lat,(*state).lon,lha=oppl2,/hardie)
      oppl = oppl*!radeg/15.0
      oppl1 = oppl1*!radeg/15.0
      oppl2 = oppl2*!radeg/15.0

      ; Compute time to sunrise
      hatojd,!dpi,sunra,lst,curjd,jdlclmid
      altoha,-18.0/!radeg,sundec,(*state).lat,sunhorzha,suntype
      jdsset  = jdlclmid - (!dpi-sunhorzha)/2.0d0/!dpi
      jdsrise = jdlclmid + (!dpi-sunhorzha)/2.0d0/!dpi
      altoha,-0.8/!radeg,sundec,(*state).lat,sunhorzha,suntype
      jdshset  = jdlclmid - (!dpi-sunhorzha)/2.0d0/!dpi
      jdshrise = jdlclmid + (!dpi-sunhorzha)/2.0d0/!dpi

      moondesc=string(malt,moonphase,format='(f5.1," deg (",f4.2,")")')
      sundesc=string(salt,format='(f5.1," deg")')
      if salt lt -18.0 then begin
         ; NOP
      endif else if salt lt -12.0 then begin
         sundesc='AT '+sundesc
      endif else if salt lt -6.0 then begin
         sundesc='NT '+sundesc
      endif else if salt lt -0.8 then begin
         sundesc='CT '+sundesc
      endif else begin
         sundesc='UP '+sundesc
      endelse
      widget_control,(*state).sundescid,set_value=sundesc
      widget_control,(*state).moondescid,set_value=moondesc

      ; Night time, sun well down.  No day left, nightleft until twilight
      if salt lt -18.0 then begin
         type='night'
         dayleft = 0.0
         nightleft = (jdsrise - curjd)*24.0
         rastr,nightleft*15.0/!radeg,-2,timeleftstr
         widget_control,(*state).nfid,set_value=timeleftstr+' to A.T. '

      ; Dawn twilight (-18 < salt < 0)
      endif else if salt lt 0.0 and slha lt 0.0 then begin
         type='dawn'
         dayleft = -12.0
         nightleft = (jdshrise - curjd)*24.0
         rastr,nightleft*15.0/!radeg,-2,timeleftstr
         widget_control,(*state).nfid,set_value=timeleftstr+' to sunrise '

      ; Dusk twilight (-18 < salt < 0)
      endif else if salt lt 0.0 and slha gt 0.0 then begin
         type='dusk'
         nightleft = -12.0
         dayleft = (jdsset - curjd)*24.0
         rastr,dayleft*15.0/!radeg,-2,timeleftstr
         widget_control,(*state).nfid,set_value=timeleftstr+' to dark '

      ; daytime
      endif else begin
         type='daytime'
         nightleft = 0.0
         dayleft = (jdshset - curjd)*24.0
         rastr,dayleft*15.0/!radeg,-2,timeleftstr
         widget_control,(*state).nfid,set_value=timeleftstr+' to sunset '
      endelse

      am = airmass(replicate(curjd,(*state).nf),(*(*state).ra),(*(*state).dec), $
                   (*state).lat,(*state).lon,lha=lha,alt=alt,az=az,/hardie)
      am2= airmass(replicate(curjd+(*state).patlen/24.0,(*state).nf),(*(*state).ra),(*(*state).dec), $
                   (*state).lat,(*state).lon,/hardie)
      lha = lha*!radeg/15.0
      alt = alt*!radeg
      az  = az*!radeg

      ; Tag all those objects that are out of range on airmass and haven't
      ;   yet been observed once.  These will be tagged as -1, ie. don't
      ;   observe or show.
      z=where((am ge (*state).amcrit or am2 ge (*state).amcrit) and $
              (*(*state).obscnt) le 0, count)
      if count ne 0 then (*(*state).obscnt)[z] = -1

      ; Reset the obscnt flag for those that have become valid
      z=where(am lt (*state).amcrit and am2 lt (*state).amcrit and $
              (*(*state).obscnt) lt 0, count)
      if count ne 0 then (*(*state).obscnt)[z]=0

      ; Check all objects not yet seen twice that are above the airmass
      ;   threshold.  These will set the plot boundaries.
      z=where(am lt (*state).amcrit and $
              (*(*state).obscnt) le 1, count)
      if count ne 0 then xr=max([abs(lha[z]),3.0])*[-1,1] $
      else xr=[-3,3]
      yr=[(*state).amcrit,1.0]

      ; Generate plot axes
      plot,[0],[1],xr=xr,yr=yr,/nodata, $
         xtitle='(E)  Hour Angle  (W)',ytitle='Airmass'

      ; Plot sun relative lines
      oplot,oppl,oppam,color='600060'xl
      oplot,oppl1,oppam1,color='606000'xl
      oplot,oppl2,oppam2,color='606000'xl

      ; Select the unobserved objects and plot.
      z=where( (*(*state).obscnt) eq 0, count)
      widget_control,(*state).availnumid,set_value=strn(count)
      if count ne 0 and type ne 'daytime' and $
         ( (*state).viewtype eq 'All' or (*state).viewtype eq 'Pass 1' ) and $
         (type eq 'dusk' or nightleft gt (*state).patlen) then $
         oplot,lha[z],am[z],psym=4

      ; Select the in progress objects and plot.
      z=where( (*(*state).obscnt) eq 1, count)
      widget_control,(*state).pendingid,set_value=strn(count)
      setusym,10
      if count ne 0 and nightleft gt 0.0 and $
         ( (*state).viewtype eq 'All' or (*state).viewtype eq 'Pass 2' ) then $
         oplot,lha[z],am[z],psym=8,color='ff40af'xl
      setusym,-1
      if count eq 0 then begin
         widget_control,(*state).pass2lenid,set_value=''
      endif else begin
         pass2len = (*state).explen * count
         pass2len = pass2len/60.0
         comptime = curjd + pass2len/24.0
         pass2len = pass2len / 12.0 * !dpi
         rastr,pass2len,-2,timeleftstr
         jdstr,comptime,-2,comptimestr
         comptimestr=strmid(comptimestr,11,99)
         widget_control,(*state).pass2lenid, $
            set_value='  pass 2 time '+timeleftstr+' ['+comptimestr+']'
      endelse

      ; plot the sun and moon
      setusym,1
      oplot,[slha],[sam],psym=8,symsize=3.0,color='00ffff'xl
      oplot,[mlha],[mam],psym=8,symsize=3.0,color='808080'xl
      setusym,-1

      ; update number of completed fields
      z=where( (*(*state).obscnt) eq 2, count)
      widget_control,(*state).completeid,set_value=strn(count)

      ; update number of fields not available
      z=where( (*(*state).obscnt) eq -1, count)
      widget_control,(*state).downid,set_value=strn(count)

      ; save am and lha 
      ptr_free,(*state).alt
      ptr_free,(*state).am
      ptr_free,(*state).az
      ptr_free,(*state).lha
      (*state).alt = ptr_new(alt)
      (*state).am = ptr_new(am)
      (*state).az = ptr_new(az)
      (*state).lha = ptr_new(lha)
      tnoobs_updatesel, state, -1
   endif

end

; Update the widgets showing the currently selected object
pro tnoobs_updatesel, state, newsel
   if newsel ge 0 and newsel lt (*state).nf then (*state).select = newsel
   if (*state).select lt 0 then return
   setusym,1
   oplot,[(*(*state).lha)[(*state).select]], $
         [(*(*state).am)[(*state).select]],psym=8,color='0000ff'xl
   setusym,-1
   widget_control,(*state).objselid,set_value=(*(*state).id)[(*state).select]
   widget_control,(*state).ut1id,set_value=(*(*state).fut1)[(*state).select]
   widget_control,(*state).ut2id,set_value=(*(*state).fut2)[(*state).select]
   ra  = (*(*state).ra)[(*state).select]
   dec = (*(*state).dec)[(*state).select]
   case (*state).coordtype OF
      0: begin ; Equatorial coordinates
         rastr,ra,1,str1
         decstr,dec,0,str2
         widget_control,(*state).coordid,set_value=' RA '+str1+'  Dec '+str2
      end
      1: begin ; Ecliptic coordinates
         euler,ra*!radeg,dec*!radeg,ecllon,ecllat,3
         str1 = strn(ecllon,format='(f10.1)')
         str2 = strn(ecllat,format='(f10.1)')
         widget_control,(*state).coordid,set_value=' lon '+str1+'  lat '+str2
      end
      2: begin ; Galactic coordinates
         euler,ra*!radeg,dec*!radeg,gallon,gallat,1
         str1 = strn(gallon,format='(f10.1)')
         str2 = strn(gallat,format='(f10.1)')
         widget_control,(*state).coordid,set_value=' lon '+str1+'  lat '+str2
      end
      3: begin ; Local coordinates
         alt = (*(*state).alt)[(*state).select]
         az  = (*(*state).az)[(*state).select]
         str1 = strn(alt,format='(f10.1)')
         str2 = strn(az,format='(f10.1)')
         widget_control,(*state).coordid,set_value=' alt '+str1+'  az '+str2
      end
      else: begin
      end
   endcase
end

pro tnoobs_eve, event

   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   case event_name of

      'THE_MENU': begin
         case event.value of

            'Load field list': begin
               cd,current=current
               fn = dialog_pickfile( group=event.top, TITLE='Select file', $
                                     FILTER='*.ctr', /must_exist, path=current, $
                                     get_path=current)

               if fn ne '' then begin
                  filename = strmid(fn,strlen(current),999)
                  print,'Loading file ',filename
;                  readcol,fn,id,h,m,s,dd,dm,ds,epoch,format='a,a,a,a,a,a,a,f'
;                  ras = h+':'+m+':'+s
;                  ra=raparse(ras)
;                  decs = dd+':'+dm+':'+ds
;                  dec=decparse(decs)
                  readcol,fn,id,ras,decs,epoch,format='a,a,a,f'
                  ra=raparse(ras)
                  dec=decparse(decs)
                  nf = n_elements(ra)
                  fn = replicate(fn,nf)

                  (*state).nf = nf
                  widget_control, (*state).totalnumid, SET_VALUE=strn((*state).nf)

                  ptr_free,(*state).dec
                  ptr_free,(*state).fn
                  ptr_free,(*state).fut1
                  ptr_free,(*state).fut2
                  ptr_free,(*state).id
                  ptr_free,(*state).ra
                  ptr_free,(*state).obscnt

                  (*state).dec    = ptr_new(dec)
                  (*state).fn     = ptr_new(fn)
                  (*state).fut1   = ptr_new(strarr(nf))
                  (*state).fut2   = ptr_new(strarr(nf))
                  (*state).id     = ptr_new(id)
                  (*state).ra     = ptr_new(ra)
                  (*state).obscnt = ptr_new(intarr(nf))

                  tnoobs_loadobs, state

                  tnoobs_plot,state
               endif

            end

            'Refresh plot' : begin
                  tnoobs_plot,state
            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               RETURN
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Coord Select': begin
         (*state).coordtype = event.index
         tnoobs_plot, state
      end

      'Field Select': begin
         if (*state).nf gt 0 then begin
            widget_control,(*state).objselid,get_value=newfield
            newfield = strupcase(strtrim(newfield[0],2))
            z=where( (*(*state).id) eq newfield, count)
            if count gt 0 then (*state).select = z[0]
            tnoobs_plot, state
         endif
      end

      'Frame Time': begin
         widget_control,(*state).frmlenid,get_value=newfrmlen
         newfrmlen=float(newfrmlen[0])
         if newfrmlen gt 0.0 and newfrmlen lt 30.0 then $
            (*state).explen = newfrmlen
         widget_control, (*state).frmlenid, $
            set_value=strn((*state).explen,format='(1x,f10.3)')
         tnoobs_plot, state
      end

      'Mainbase': begin
         info=widget_info((*state).colbaseid,/geometry)
         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize
         tnoobs_plot, state
      end

      'Pattern Length': begin
         widget_control,(*state).patlenid,get_value=newpatlen
         newpatlen=float(newpatlen[0])
         if newpatlen gt 0.0 and newpatlen lt 24.0 then $
            (*state).patlen = newpatlen
         widget_control, (*state).patlenid, $
            set_value=strn((*state).patlen,format='(1x,f10.3)')
         tnoobs_plot, state
      end

      'Set View': begin
         (*state).viewtype=event.value
         tnoobs_plot, state
      end

      'Timer': begin
         widget_control,(*state).colbaseid,timer=60.0
         tnoobs_plot, state
      end

      'UT 1': begin
         if (*state).nf eq 0 or (*state).select lt 0 then return

         jdstr,systime(/julian,/utc),0,newut1

         widget_control,(*state).ut1id,set_value=newut1
         (*(*state).fut1)[(*state).select] = newut1
         if (*(*state).fut2)[(*state).select] ne '' then $
            (*(*state).obscnt)[(*state).select] = 2 $
         else $
            (*(*state).obscnt)[(*state).select] = 1
         (*state).dirty = 1
         tnoobs_plot, state
      end

      'UT1 Set': begin
         if (*state).nf eq 0 or (*state).select lt 0 then return

         widget_control,(*state).ut1id,get_value=newut1
         newut1 = strtrim(strcompress(newut1[0]),2)
         (*(*state).fut1)[(*state).select] = newut1
         if (*(*state).fut1)[(*state).select] ne '' then begin
            if (*(*state).fut2)[(*state).select] ne '' then $
               (*(*state).obscnt)[(*state).select] = 2 $
            else $
               (*(*state).obscnt)[(*state).select] = 1
         endif else begin
            (*(*state).obscnt)[(*state).select] = 0
         endelse
         (*state).dirty = 1
         tnoobs_plot, state
      end

      'UT 2': begin
         if (*state).nf eq 0 or (*state).select lt 0 then return
         if (*(*state).fut1)[(*state).select] eq '' then begin
            tnoobs_plot, state
            return
         endif

         jdstr,systime(/julian,/utc),0,newut2

         widget_control,(*state).ut2id,set_value=newut2
         (*(*state).fut2)[(*state).select] = newut2
         (*(*state).obscnt)[(*state).select] = 2
         (*state).dirty = 1
         tnoobs_plot, state
      end

      'UT2 Set': begin
         if (*state).nf eq 0 or (*state).select lt 0 then return
         if (*(*state).fut1)[(*state).select] eq '' then begin
            tnoobs_plot, state
            return
         endif

         widget_control,(*state).ut2id,get_value=newut2
         newut2 = strtrim(strcompress(newut2[0]),2)
         (*(*state).fut2)[(*state).select] = newut2
         if (*(*state).fut2)[(*state).select] eq '' then $
            (*(*state).obscnt)[(*state).select] = 1 $
         else $
            (*(*state).obscnt)[(*state).select] = 2
         (*state).dirty = 1
         tnoobs_plot, state
      end

      'Window': begin

         if (*state).nf gt 0 and event.type eq 0 and event.press eq 1 then begin

            if (*state).viewtype eq 'All' then $
               z = where( (*(*state).am) lt (*state).amcrit and $
	                  (*(*state).obscnt) ge 0 and $
                          (*(*state).obscnt) le 1, count ) $
            else if (*state).viewtype eq 'Pass 1' then $
               z = where( (*(*state).obscnt) eq 0, count) $
            else if (*state).viewtype eq 'Pass 2' then $
               z = where( (*(*state).obscnt) eq 1, count)

            if count ne 0 then begin
               cval = convert_coord((*(*state).lha)[z],(*(*state).am)[z],/data,/to_device)
               x    = cval[0,*]
               y    = cval[1,*]
               dist = sqrt( (x-event.x)^2 + (y-event.y)^2 )
               zz=where(dist eq min(dist))
               zz=zz[0]
               tnoobs_updatesel, state, z[zz]
               tnoobs_plot, state
            endif

         endif

      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro tnoobs,OBSFILE=obsfile,OBSCODE=obscode,AMCRIT=amcrit,DT=dt

   if xregistered('tnoobs') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. TNOOBS cannot be started.'
      return
   endif

   if badpar(obsfile,[0,7],0,CALLER='TNOOBS: (OBSFILE) ', $
                  DEFAULT='obscode.dat') then return
   if badpar(obscode,[0,1,2,3,7],0,caller='TNOOBS: (OBSCODE) ', $
                default='688',type=codetype) then return
   if badpar(amcrit,[0,2,3,4,5],0,caller='TNOOBS: (AMCRIT) ',default=2.5) then return
   if badpar(dt,[0,2,3,4,5],0,caller='TNOOBS: (DT) ',default=0.0d0) then return

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   setusym,-1
   patlen = 2.583
   frmlen = 5.833

   ;Define the main base.
   mainbase = widget_base( TITLE='TNOOBS: TNO Field Section Tool', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Load field list',$
                     '2\Exit',$
                     '1\Tools',$
                     '0\Update field info', $
                     '2\Refresh plot'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase,col=1)

   win1 = widget_draw( base, XSIZE=600, YSIZE=400, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Window' )

   colbase = widget_base(base,row=1,uvalue='Timer')
   b1 = widget_base(colbase,col=1)
   b2 = widget_base(b1,row=1)
   obscodeid = widget_label( b2, value='', /align_left, /dynamic_resize)
   obsnameid = widget_label( b2, value='', /align_left, /dynamic_resize)
   curtime = widget_label( b1, value='', /align_center, /dynamic_resize )

   b2 = widget_base(b1,col=2,/frame)
   t1 = widget_label( b2, value='sun @ ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value='moon @ ', /align_right, /dynamic_resize)
   nf = widget_label( b2, value='', /align_left, /dynamic_resize )
   sundescid = widget_label( b2, value='', /align_left, /dynamic_resize)
   moondescid = widget_label( b2, value='', /align_left, /dynamic_resize)
   pass2lenid = widget_label( b2, value='', /align_left, /dynamic_resize)

   viewset = cw_bgroup( b1, ['All','Pass 1','Pass 2'], /no_release, $
                        /exclusive, /return_name, /row, uvalue='Set View')

   c1 = widget_base(colbase,col=1)
   b1 = widget_base(c1,col=1,/frame)
   b2 = widget_base(b1,row=1)
   t1 = widget_button( b2, value='Field ',uvalue='Pick Field')
   objselid = widget_text( b2, value='', uvalue='Field Select',/align_left,/editable )
   b2 = widget_base(b1,row=1)
   t1 = widget_button( b2, value='UT 1 ',uvalue='UT 1')
   ut1id = widget_text( b2, value='', uvalue='UT1 Set',/align_left,/editable )
   b2 = widget_base(b1,row=1)
   t1 = widget_button( b2, value='UT 2 ',uvalue='UT 2')
   ut2id = widget_text( b2, value='', uvalue='UT2 Set',/align_left,/editable )
   b2 = widget_base(c1,row=1)
   coordselid  = widget_droplist(b2,VALUE=['Equ','Ecl','Gal','Lcl'],uvalue='Coord Select')
   coordid = widget_label(b2,value=' --- ',/align_left,/dynamic_resize)
   widget_control,coordselid,SET_DROPLIST_SELECT=0

   c1 = widget_base(colbase,col=1)
   b1 = widget_base(c1,col=2,/frame)
   totalnum = widget_label(b1,value='0',/align_right,/dynamic_resize)
   availnum = widget_label(b1,value='0',/align_right,/dynamic_resize)
   pending  = widget_label(b1,value='0',/align_right,/dynamic_resize)
   complete = widget_label(b1,value='0',/align_right,/dynamic_resize)
   down     = widget_label(b1,value='0',/align_right,/dynamic_resize)
   t1 = widget_label(b1,value=' Total Fields ',/align_left,/dynamic_resize)
   t1 = widget_label(b1,value=' Available Fields ',/align_left,/dynamic_resize)
   t1 = widget_label(b1,value=' In Progress Fields ',/align_left,/dynamic_resize)
   t1 = widget_label(b1,value=' Completed Fields ',/align_left,/dynamic_resize)
   t1 = widget_label(b1,value=' Unavailable Fields ',/align_left,/dynamic_resize)
   b2 = widget_base(c1,row=1)
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='PatLen(h)', /align_center, /dynamic_resize)
   patlenid = widget_text( b3, value=strn(patlen,format='(1x,f10.3)'),xsize=10, $
                           uvalue='Pattern Length',/align_center,/editable )
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='Frame Time(m)', /align_center, /dynamic_resize)
   frmlenid = widget_text( b3, value=strn(frmlen,format='(1x,f10.3)'),xsize=7, $
                           uvalue='Frame Time',/align_center,/editable )

   state = ptr_new({ $

      ; Data and information in the widget
      alt: ptr_new(), $          ; Altitude at last plot update
      am: ptr_new(), $           ; Airmass at last plot update
      amcrit: amcrit, $             ; Critical airmass
      az: ptr_new(), $           ; Azimuth at last plot update
      coordtype: 0, $            ; 0-Equatorial, 1-Ecliptic, 2-Galatic, 3-Local
      dec: ptr_new(), $          ; Dec of target field (input epoch)
      dirty: 0, $                ; Flag, if set means fieldobs.dat needs writing.
      dt: dt, $                  ; Offset from UT clock (hours).
      explen: frmlen, $          ; Exposure time plus overhead (minute).
      fn: ptr_new(), $           ; Filename field was loaded from.
      fut1: ptr_new(), $         ; UT of first observation
      fut2: ptr_new(), $         ; UT of second observation
      id: ptr_new(), $           ; field id
      lat: 0.0d0, $              ; Latitude of observatory (radians)
      lha: ptr_new(), $          ; Local hour angle at last plot update
      lon: 0.0d0, $              ; Longitude of observatory (radians)
      nf: 0L, $                  ; Number of target fields loaded.
      nobs: 0L, $                ; Number of fields observed.
      obscnt: ptr_new(), $       ; Number of observations of each field.
      obscode: 'man', $          ; Observatory id (-1 means manual entry)
      obsfile: obsfile, $        ; Name of observatory code file.
      obsname: '', $             ; Name of observatory
      patlen: patlen, $          ; Pattern length in hours.
      ra: ptr_new(), $           ; RA of target field (input epoch)
      select: -1, $              ; Currently selected object.
      viewtype: 'All', $         ; type of plot

      ; Widget ids
      availnumid: availnum, $    ; Label of number of available fields.
      colbaseid: colbase, $      ; ID of button bar on bottom
      completeid: complete, $    ; Label of number of completed fields.
      coordid: coordid, $        ; Shows selected coordinates of object.
      curtimeid: curtime, $      ; ID of current time label
      downid: down, $            ; Label of number of fields not available.
      drawwin: win1, $           ; ID of main draw window
      frmlenid: frmlenid, $      ; Shows exposure time plus overhead (min)
      moondescid: moondescid, $  ; Moon description field
      nfid: nf, $                ; ID of number of fields label
      objselid: objselid, $      ; Label of selected object.
      obscodeid: obscodeid, $        ; Observatory ID label
      obsnameid: obsnameid, $    ; Observatory Name label
      pass2lenid: pass2lenid, $  ; Label for pass 2 length.
      patlenid: patlenid, $      ; sets pattern length
      pendingid: pending, $      ; Label of number of fields in progress
      sundescid: sundescid, $    ; Sun description field
      totalnumid: totalnum, $    ; Label for total number of fields
      ut1id: ut1id, $            ; Label of UT1
      ut2id: ut2id, $            ; Label of UT2
      viewsetid: viewset, $      ; Control for type of plot

      mainbase: mainbase $       ; ID of top level base.

      })

   tnoobs_newobscode, state, obscode
   widget_control,(*state).obscodeid,set_value='('+(*state).obscode+') '
   widget_control,(*state).obsnameid,set_value=(*state).obsname

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE
   widget_control,(*state).colbaseid,timer=1.0
   widget_control, (*state).viewsetid, SET_VALUE=0

   ; Give control to the XMANAGER.
   xmanager, 'tnoobs', mainbase, $
             EVENT_HANDLER='tnoobs_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='tnoobs_cleanup'

end

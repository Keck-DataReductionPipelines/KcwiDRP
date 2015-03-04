;+
; NAME:
;  fieldobs
; PURPOSE:
;  Real-time display of where objects are in the sky
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  fieldobs,file,AMCRIT=amcrit,OBSFILE=obsfile,OBSCODE=obs
; INPUTS:
;  file - List of objects (for file format see LOADSTARS) to display
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  AMCRIT  - Maximum airmass for plotted display (default = 2.5)
;  DT      - Offset from UT clock in hours.  This is mostly for debugging
;              purposes but may prove useful otherwise.
;  OBSFILE - Location of observatory information file, default is
;              /pub/sac0/elgb/data/obscode.dat
;  OBSCODE - Observatory code, default = '688', Lowell Observatory
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
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  97/05/28, Written by Marc W. Buie, Lowell Observatory
;  98/04/30, MWB, added some items to the plot.
;  2001/03/19, David Tucker, changed use of rdobscod for new file handling.
;  2001/04/18, MWB, changed systime call
;  2002/09/09, MWB, added support for string obscode values
;-
pro fieldobs_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   if (*state).dirty then begin
      print,'Object list was changed, save NOT implemented yet.'
   endif

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).alt
   ptr_free,(*state).am
   ptr_free,(*state).az
   ptr_free,(*state).comment
   ptr_free,(*state).dec
   ptr_free,(*state).equinox
   ptr_free,(*state).id
   ptr_free,(*state).lha
   ptr_free,(*state).melong
   ptr_free,(*state).obsacod
   ptr_free,(*state).obsalat
   ptr_free,(*state).obsalon
   ptr_free,(*state).obsanam
   ptr_free,(*state).ra
   ptr_free,(*state).selong

   ; Free up the state structure itself.
   ptr_free, state

end

pro fieldobs_edit, state

   if (*state).select ge 0 then begin
      name    = (*(*state).id)[(*state).select]
      ra      = (*(*state).ra)[(*state).select]
      dec     = (*(*state).dec)[(*state).select]
      equinox = (*(*state).equinox)[(*state).select]
      comment = (*(*state).comment)[(*state).select]
      edtcoord,name,ra,dec,equinox,comment,/nopm
      (*(*state).id)[(*state).select]      = name
      (*(*state).ra)[(*state).select]      = ra
      (*(*state).dec)[(*state).select]     = dec
      (*(*state).equinox)[(*state).select] = equinox
      (*(*state).comment)[(*state).select] = comment
      (*state).dirty = 1
      fieldobs_savefile,state
      fieldobs_plot,state
   endif

end

pro fieldobs_loadfile, state, newfile

   if exists(newfile) and newfile ne '' then begin
      openr,lun,newfile,/get_lun
      sname=''
      sinfo=''
      sign=''
      num=0
      while not eof(lun) do begin
         readf,lun,sname,rh,rm,rs,sign,dd,dm,ds,ep,sinfo, $
            format='(a16,2(1x,i2),1x,f4.1,1x,a1,3(i2,1x),f6.1,1x,a)'
         newra   = rh + rm/60.0 + rs/3600.0
         newdec  = dd + dm/60.0 + ds/3600.0
         if sign eq '-' then newdec = -1.0*newdec
         if num eq 0 then begin
            id      = strtrim(sname,2)
            ra      = newra
            dec     = newdec
            equi    = ep
            comment = sinfo
         endif else begin
            id      = [id,     sname]
            ra      = [ra,     newra]
            dec     = [dec,    newdec]
            equi    = [equi,   ep]
            comment = [comment,sinfo]
         endelse
         num=num+1
      endwhile
      free_lun,lun

      ra = ra*15.0/!radeg
      dec = dec/!radeg

      nf = n_elements(ra)

      (*state).nf = nf
      (*state).fn = newfile
      (*state).select = -1

      ptr_free,(*state).comment
      ptr_free,(*state).dec
      ptr_free,(*state).equinox
      ptr_free,(*state).id
      ptr_free,(*state).ra

      (*state).comment= ptr_new(comment)
      (*state).dec    = ptr_new(dec)
      (*state).equinox= ptr_new(equi)
      (*state).id     = ptr_new(id)
      (*state).ra     = ptr_new(ra)

      widget_control, (*state).fileid, set_value=(*state).fn
   endif else if newfile ne '' then begin
      (*state).nf = 0
      (*state).fn = newfile
      (*state).select = -1

      ptr_free,(*state).comment
      ptr_free,(*state).dec
      ptr_free,(*state).equinox
      ptr_free,(*state).id
      ptr_free,(*state).ra

      (*state).comment= ptr_new()
      (*state).dec    = ptr_new()
      (*state).equinox= ptr_new()
      (*state).id     = ptr_new()
      (*state).ra     = ptr_new()
   endif

end

; Load a new observatory id code
pro fieldobs_loadobscod, state

   if (*state).validobscod or (*state).obsfile eq '' then return

   rdobscod,code,alllon,rhosinp,rhocosp,obsname,valid,FILE=(*state).obsfile
   if valid then begin
      lon = (360.0-alllon)/180.0*!pi
      lat = atan(rhocosp,rhosinp)
      name=strtrim(obsname,2)
      (*state).obsalon = ptr_new(lon)
      (*state).obsalat = ptr_new(lat)
      (*state).obsanam = ptr_new(name)
      (*state).obsacod = ptr_new(code)
      (*state).validobscod = 1
   endif else begin
      print,'Observatory code file ',(*state).obsfile,' not found.'
   endelse

end

; Load a new observatory id code
pro fieldobs_newobscode, state, newid

   fieldobs_loadobscod,state

   if newid ne 'man' and (*state).validobscod then begin
      idx=where(newid eq (*(*state).obsacod),count)
      idx=idx[0]
      if (count eq 1) then begin
         (*state).lon     = (*(*state).obsalon)[idx]
         (*state).lat     = (*(*state).obsalat)[idx]
         (*state).obscode = (*(*state).obsacod)[idx]
         (*state).obsname = (*(*state).obsanam)[idx]
      endif else begin
         print,'Observatory code ',strn(newid),' not found'
      endelse
   endif

   if (*state).obscode eq 'man' then begin
      ; This is the GPS position for the 42", derived 1993 Sep 08
      (*state).lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
      (*state).lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
      (*state).obsname= 'Lowell Observatory - Anderson Mesa Station'
      (*state).obscode= '688'
   endif

   widget_control,(*state).obscodeid,set_value='('+(*state).obscode+') '
   widget_control,(*state).obsnameid,set_value=(*state).obsname

end

; This routine takes the current object list, current time, and 
;   generates a plot that shows current sky placement of fields.
pro fieldobs_plot, state

   curjd = systime(/julian,/utc) + (*state).dt/24.0
   jdstr,curjd,-2,curtimestr
   newstr = 'UT '+curtimestr
   if (*state).dt ne 0.0 then $
      newstr = newstr + ' (dt=' + strn((*state).dt,format='(f10.2)') + ')'
   widget_control,(*state).curtimeid,set_value=newstr

   widget_control, (*state).drawwin, get_value=winnum
   wset,winnum

   if (*state).nf gt 0 then begin

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

      ; Compute time to sunrise
      hatojd,!dpi,sunra,lst,curjd,jdlclmid
      altoha,-18.0/!radeg,sundec,(*state).lat,sunhorzha,suntype
      jdsset  = jdlclmid - (!dpi-sunhorzha)/2.0d0/!dpi
      jdsrise = jdlclmid + (!dpi-sunhorzha)/2.0d0/!dpi
      altoha,-0.8/!radeg,sundec,(*state).lat,sunhorzha,suntype
      jdshset  = jdlclmid - (!dpi-sunhorzha)/2.0d0/!dpi
      jdshrise = jdlclmid + (!dpi-sunhorzha)/2.0d0/!dpi
      lsidtim,jdlclmid,(*state).lon,midlst
      (*state).jdlclmid = jdlclmid
      (*state).midlst   = midlst
      (*state).curjd    = curjd
      (*state).lst      = lst

      rastr,lst,0,curlststr
      newstr = 'LST '+curlststr
      widget_control,(*state).curlstid,set_value=newstr

      moondesc=string(malt,moonphase,format='(f5.1," deg (",f4.2,")")')
      sundesc=string(salt,format='(f5.1," deg")')
      if salt lt -18.0 then begin
         pc = 'ffffff'xl
      endif else if salt lt -12.0 then begin
         pc = 'ba4781'xl
         sundesc='AT '+sundesc
      endif else if salt lt -6.0 then begin
         pc = 'a169a1'xl
         sundesc='NT '+sundesc
      endif else if salt lt -0.8 then begin
         pc = '3366ff'xl
         sundesc='CT '+sundesc
      endif else begin
         pc = '00ffff'xl
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

      ; Get all coordinates into ofdate
      ra = (*(*state).ra)
      dec = (*(*state).dec)
      equin = (*(*state).equinox)
      jd2year,curjd,ofdate
      for i=0,n_elements(ra)-1 do begin
         ra0 = ra[i]
         dec0= dec[i]
         equ0= equin[i]
         precess,ra0,dec0,equ0,ofdate,/radian
         ra[i]  = ra0
         dec[i] = dec0
      endfor

      dt = 15.0/60.0/24.0
      jd=[curjd,curjd+dt,curjd+dt*2]
      am=fltarr(3,(*state).nf)
      lha=fltarr(3,(*state).nf)
      alt=fltarr((*state).nf)
      az=fltarr((*state).nf)
      for i=0,(*state).nf-1 do begin
         am[*,i] = airmass(jd,ra[i],dec[i], $
                      (*state).lat,(*state).lon, $
                      lha=lha0,alt=alt0,az=az0,/hardie)
         lha[*,i] = lha0
         alt[i]   = alt0[0]
         az[i]    = az0[0]
      endfor

      lha = lha*!radeg/15.0
      alt = alt*!radeg
      az  = az*!radeg

      ; compute solar and lunar elongation angles.
      selong = fix(angsep(sunra,sundec,ra,dec) * !radeg + 0.5)
      melong = fix(angsep(moonra,moondec,ra,dec) * !radeg + 0.5)

      ; Check all objects that are above the airmass
      ;   threshold.  These will set the plot boundaries.
      z=where(am[0,*] lt (*state).amcrit, count)
      if count ne 0 then xr=max([max(abs(lha[*,z])),3.0])*[-1,1] $
      else xr=[-3,3]
      yr=[(*state).amcrit,1.0]

      ; Generate plot axes
      plot,[0],[1],xr=xr,yr=yr,ymargin=[4,4],/nodata,xstyle=3,ystyle=3, $
         xtitle='(E)  Hour Angle  (W)',ytitle='Airmass',color=pc

      ; Label top side of top axis with the RA at the hour angle
      pra=findgen(24) / 12.0 * !pi
      pdec=(*state).lat
      pam = airmass(replicate(curjd,24),pra,0.0,(*state).lat,(*state).lon,lha=plha)
      plha = plha *!radeg / 15.0
      pra = pra / !pi * 12.0
      zp=where(plha ge xr[0] and plha lt xr[1] and pam lt (*state).amcrit)
      pra=pra[zp]
      plha=plha[zp]
      zp=sort(plha)
      pra = fix(pra[zp])
      plha = plha[zp]
      pstr = strtrim(string(pra),2)
      axis,xaxis=1,xstyle=3,xr=xr,xtitle='RA (hours)',xtickv=plha, $
         xticks=n_elements(plha)-1,xtickn=pstr,xticklen=-0.02,color=pc

      for i=0,count-1 do begin
         if melong[z[i]] lt 30 then $
            oplot,lha[*,z[i]],am[*,z[i]],psym=-4,color='0000ff'xl $
         else $
            oplot,lha[*,z[i]],am[*,z[i]],psym=-4
      endfor

      ; label objects (if wanted)
      if (*state).showlabel then begin
         for i=0,count-1 do begin
            if lha[0,z[i]] lt 0 then dy = 0.07 else dy = -0.03
            xyouts,lha[0,z[i]],am[0,z[i]]+dy,(*(*state).id)[z[i]],/data,align=0.0
         endfor
      endif

      ; plot the sun and moon
      setusym,1
      oplot,[slha],[sam],psym=8,symsize=3.0,color='00ffff'xl
      oplot,[mlha],[mam],psym=8,symsize=3.0,color='808080'xl
      setusym,-1

      ; save am and lha 
      ptr_free,(*state).alt
      ptr_free,(*state).am
      ptr_free,(*state).az
      ptr_free,(*state).lha
      ptr_free,(*state).melong
      ptr_free,(*state).selong
      am=am[0,*]
      am=am[*]
      lha=lha[0,*]
      lha=lha[*]
      (*state).alt = ptr_new(alt)
      (*state).am = ptr_new(am)
      (*state).az = ptr_new(az)
      (*state).lha = ptr_new(lha)
      (*state).melong = ptr_new(melong)
      (*state).selong = ptr_new(selong)
      fieldobs_updatesel, state, -1
   endif else begin
      erase
      fieldobs_updatesel, state, -1
   endelse

end

pro fieldobs_savefile, state

   if (*state).dirty and (*state).fn ne '' then begin
      blanks='                                                        '
      openw,lun,(*state).fn,/get_lun
      for i=0,(*state).nf-1 do begin
         rastr,(*(*state).ra)[i],1,str1
         decstr,(*(*state).dec)[i],0,str2
         printf,lun,(*(*state).id)[i]+blanks,str1,str2, $
                   (*(*state).equinox)[i], $
                   (*(*state).comment)[i]+blanks, $
            format='(a16,1x,a,1x,a,1x,f6.1,1x,a34)'
      endfor
      free_lun,lun
      (*state).dirty=0
   endif

end

; Update the widgets showing the currently selected object
pro fieldobs_updatesel, state, newsel
   if newsel ge 0 and newsel lt (*state).nf then (*state).select = newsel
   if (*state).select lt 0 then begin
      widget_control,(*state).objselid,set_value=''
      widget_control,(*state).raid,set_value=''
      widget_control,(*state).decid,set_value=''
      widget_control,(*state).eqid,set_value=''
      widget_control,(*state).commentid,set_value=''
   endif else begin
      setusym,1
      oplot,[(*(*state).lha)[(*state).select]], $
            [(*(*state).am)[(*state).select]],psym=8,color='ffbf00'xl
      setusym,-1
      widget_control,(*state).objselid,set_value=(*(*state).id)[(*state).select]
      ra  = (*(*state).ra)[(*state).select]
      rastr,ra,1,str
      widget_control,(*state).raid,set_value=str
      dec = (*(*state).dec)[(*state).select]
      decstr,dec,0,str
      widget_control,(*state).decid,set_value=str
      dec = (*(*state).dec)[(*state).select]
      str=strn((*(*state).equinox)[(*state).select],format='(f10.1)')
      widget_control,(*state).eqid,set_value=str
      widget_control,(*state).commentid,set_value=(*(*state).comment)[(*state).select]
      altoha,(*state).altcrit,dec,(*state).lat,horzha,type
      hatojd,0.0d0,ra,(*state).midlst,(*state).jdlclmid,jdtrans
      if type eq 0 then begin
         jdrise  = jdtrans - horzha/2.0d0/!dpi
         jdset   = jdtrans + horzha/2.0d0/!dpi
         jdstr,jdrise,-2,objrisestr
         jdstr,jdset,-2,objsetstr
      endif
      
      if type eq 1 then begin
         str = 'Object always up'
      endif else if type eq 0 then begin
         ; If alt > 0, time to set
         if (*(*state).alt)[(*state).select] gt (*state).altcrit then begin
            dt = (jdset - (*state).curjd) * 2.0 * !pi
            if dt gt 0.0 then begin
               rastr,dt,-2,str
               str=' Object sets in '+str
            endif else begin
               str=''
            endelse
         ; if alt <= 0, time to rise
         endif else begin
            dt = (jdrise - (*state).curjd) * 2.0 * !pi
            rastr,dt,-2,str
            str=' Object rises in '+str
         endelse

      endif else begin
         str = 'Object always down'
      endelse
      widget_control,(*state).objlenid,set_value=str

      str=strn((*(*state).selong)[(*state).select])
      widget_control,(*state).selid,set_value=str
      str=strn((*(*state).melong)[(*state).select])
      widget_control,(*state).melid,set_value=str
      str=strn((*(*state).alt)[(*state).select],format='(f10.1)')
      widget_control,(*state).altid,set_value=str
      str=strn((*(*state).az)[(*state).select],format='(f10.1)')
      widget_control,(*state).azid,set_value=str
      am = (*(*state).am)[(*state).select]
      if am lt 9.95 then $
         str=strn(am,format='(f10.2)') $
      else $
         str='down'
      widget_control,(*state).amid,set_value=str
      lha = (*(*state).lha)[(*state).select]*15.0/!radeg
      if lha lt 0.0 then ha_sign='E' else ha_sign='W'
      rastr,abs(lha),-2,lhastr,cc
      widget_control,(*state).haid,set_value=ha_sign+lhastr
   endelse

end

pro fieldobs_eve, event

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

            'Change Observatory': begin
               if (*state).validobscod then begin
                  list = '('+string((*(*state).obsacod),format='(a3)') + $
                         ') '+(*(*state).obsanam)
                  sel = picker(list,group=event.top,index=idx, $
                               title='Select Observatory')
                  fieldobs_newobscode, state, (*(*state).obsacod)[idx]
                  fieldobs_plot,state
               endif else begin
                  print,'No valid observatory information.'
               endelse
            end

            'Delete Object': begin
               if (*state).fn ne '' and (*state).select ge 0 then begin
                  z = where(lindgen((*state).nf ne (*state).select))
                  (*state).nf = (*state).nf-1

                  if (*state).nf eq 0 then begin
                     comment = ''
                     dec     = 0.0d0
                     equinox = 0.0
                     id      = ''
                     ra      = 0.0d0
                  endif else begin
                     comment = (*(*state).comment)[z]
                     dec     = (*(*state).dec)[z]
                     equinox = (*(*state).equinox)[z]
                     id      = (*(*state).id)[z]
                     ra      = (*(*state).ra)[z]
                  endelse
                  
                  ptr_free,(*state).comment
                  ptr_free,(*state).dec
                  ptr_free,(*state).equinox
                  ptr_free,(*state).id
                  ptr_free,(*state).ra

                  (*state).comment= ptr_new(comment)
                  (*state).dec    = ptr_new(dec)
                  (*state).equinox= ptr_new(equinox)
                  (*state).id     = ptr_new(id)
                  (*state).ra     = ptr_new(ra)
                  (*state).select = -1
                  fieldobs_plot,state
                  fieldobs_savefile,state
               endif
            end

            'Edit Object': begin
               fieldobs_edit,state
               fieldobs_plot,state
            end

            'Change delta T': begin
               newdt = qinput(prompt='New delta-T (hours) ',/floating)
               if newdt eq '' then begin
                  (*state).dt = 0.0
               endif else begin
                  (*state).dt = float(newdt)
               endelse
               fieldobs_plot,state
            end

            'Get ObsFile': begin
               if not (*state).validobscod then begin
                  cd,current=current
                  fn = dialog_pickfile( group=event.top, TITLE='Select file', $
                                        /must_exist, FILTER='*.dat', path=current)
                  if fn ne '' then begin
                     (*state).obsfile = fn
                     fieldobs_loadobscod, state
                  endif
               endif
            end

            'New': begin
               cd,current=current
               fn = dialog_pickfile( group=event.top, TITLE='Select file', $
                                     FILTER='*.dat', path=current, $
                                     get_path=current)

               if fn ne '' then begin
                  fieldobs_loadfile, state, fn
                  fieldobs_plot,state
               endif

            end

            'New Object': begin
               if (*state).fn ne '' then begin

                  (*state).nf = (*state).nf+1

                  if (*state).nf eq 1 then begin
                     comment = ''
                     dec     = 0.0d0
                     equinox = 2000.0
                     id      = 'new'
                     ra      = 0.0d0
                  endif else begin
                     comment = [(*(*state).comment),'']
                     dec     = [(*(*state).dec),0.0d0]
                     equinox = [(*(*state).equinox),2000.0]
                     id      = [(*(*state).id),'new']
                     ra      = [(*(*state).ra),0.0d0]
                  endelse
                  
                  ptr_free,(*state).comment
                  ptr_free,(*state).dec
                  ptr_free,(*state).equinox
                  ptr_free,(*state).id
                  ptr_free,(*state).ra

                  (*state).comment= ptr_new(comment)
                  (*state).dec    = ptr_new(dec)
                  (*state).equinox= ptr_new(equinox)
                  (*state).id     = ptr_new(id)
                  (*state).ra     = ptr_new(ra)
                  fieldobs_plot,state
                  fieldobs_updatesel,state,(*state).nf-1

               endif
            end

            'Open': begin
               cd,current=current
               fn = dialog_pickfile( group=event.top, TITLE='Select file', $
                                     FILTER='*.dat', /must_exist, path=current, $
                                     get_path=current)

               if fn ne '' then begin
                  fieldobs_loadfile, state, fn
                  fieldobs_plot,state
               endif

            end

            'Refresh plot' : begin
               fieldobs_plot,state
            end

            'Select Object' : begin
               if (*state).nf gt 0 then begin
                  sel = picker((*(*state).id),group=event.top, $
                               index=idx,title='Select Object')
               fieldobs_updatesel, state, idx
               fieldobs_plot, state
               endif
            end

            'Toggle Labels' : begin
               (*state).showlabel = not (*state).showlabel
               fieldobs_plot,state
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

      'Edit Object': begin
         fieldobs_edit,state
         fieldobs_plot,state
      end

      'Object Select': begin
         if (*state).nf gt 0 then begin
            widget_control,(*state).objselid,get_value=newfield
            newfield = strupcase(strtrim(newfield[0],2))
            z=where( (*(*state).id) eq newfield, count)
            if count gt 0 then (*state).select = z[0]
            fieldobs_plot, state
         endif
      end

      'Mainbase': begin
         info=widget_info((*state).colbaseid,/geometry)
         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize
         fieldobs_plot, state
      end

      'Timer': begin
         widget_control,(*state).colbaseid,timer=60.0
         fieldobs_plot, state
      end

      'Window': begin

         if (*state).nf gt 0 and event.type eq 0 and event.press eq 1 then begin

            z=where((*(*state).am) lt (*state).amcrit,count)

            if count ne 0 then begin
               cval = convert_coord((*(*state).lha)[z],(*(*state).am)[z],/data,/to_device)
               x    = cval[0,*]
               y    = cval[1,*]
               dist = sqrt( (x-event.x)^2 + (y-event.y)^2 )
               zz=where(dist eq min(dist))
               zz=zz[0]
               fieldobs_updatesel, state, z[zz]
               fieldobs_plot, state
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

pro fieldobs,file,OBSFILE=obsfile,OBSCODE=obscode,AMCRIT=amcrit,DT=dt

   if xregistered('fieldobs') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. FIELDOBS cannot be started.'
      return
   endif

   if badpar(file,[0,7],0,CALLER='FIELDOBS: (file) ',default='') then return
   if badpar(obsfile,[0,7],0,CALLER='FIELDOBS: (OBSFILE) ', $
                  DEFAULT='obscode.dat') then return
   if badpar(obscode,[0,2,3],0,caller='FIELDOBS: (OBSCODE) ', $
                default='688',type=codetype) then return
   if badpar(amcrit,[0,2,3,4,5],0,caller='FIELDOBS: (AMCRIT) ',default=2.5) then return
   if badpar(dt,[0,2,3,4,5],0,caller='FIELDOBS: (DT) ',default=0.0d0) then return

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   altcrit = 0.5*!pi - acos(1.0/amcrit)

   setusym,-1

   ;Define the main base.
   mainbase = widget_base( TITLE='FIELDOBS: Object Selection Tool', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\New',$
                     '0\Open',$
                     '0\Get ObsFile',$
                     '2\Exit',$
                     '1\Objects',$
                     '0\Select Object', $
                     '0\Edit Object', $
                     '0\New Object', $
                     '2\Delete Object', $
                     '1\Options', $
                     '0\Toggle Labels', $
                     '0\Change Observatory', $
                     '0\Change delta T', $
                     '2\Refresh plot'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase,col=1)

   win1 = widget_draw( base, XSIZE=600, YSIZE=400, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Window' )

   colbase = widget_base(base,row=1,uvalue='Timer')
   b1 = widget_base(colbase,col=1)
   fileid = widget_label( b1, value='', /align_center, /dynamic_resize )
   b2 = widget_base(b1,row=1)
   obscodeid = widget_label( b2, value='', /align_left, /dynamic_resize)
   obsnameid = widget_label( b2, value='', /align_left, /dynamic_resize)
   curtime = widget_label( b1, value='', /align_center, /dynamic_resize )
   curlst  = widget_label( b1, value='', /align_center, /dynamic_resize )

   b2 = widget_base(b1,col=2,/frame)
   t1 = widget_label( b2, value='sun @ ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value='moon @ ', /align_right, /dynamic_resize)
   nf = widget_label( b2, value='', /align_left, /dynamic_resize )
   sundescid = widget_label( b2, value='', /align_left, /dynamic_resize)
   moondescid = widget_label( b2, value='', /align_left, /dynamic_resize)
   objlenid = widget_label( b2, value='', /align_left, /dynamic_resize)

   c1 = widget_base(colbase,col=1)
   b1 = widget_base(c1,col=1,/frame)
   b2 = widget_base(b1,row=1)
   t1 = widget_button( b2, value='Edit',uvalue='Edit Object')
   objselid = widget_text( b2, value='', uvalue='Object Select',/align_left,/editable )
   b2 = widget_base(b1,col=2)
   t1 = widget_label( b2, value='RA ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value='Dec ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value='Equinox ', /align_right, /dynamic_resize)
   raid = widget_label( b2, value='', /align_left, /dynamic_resize)
   decid = widget_label( b2, value='', /align_left, /dynamic_resize)
   eqid = widget_label( b2, value='', /align_left, /dynamic_resize)
   commentid = widget_label( b1, value='', /align_center, /dynamic_resize)

   c1 = widget_base(colbase,col=1,/frame)
   b2 = widget_base(c1,col=2)
   t1 = widget_label( b2, value= 'Sel ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value= 'Mel ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value= 'Alt ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value=  'Az ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value=  'Am ', /align_right, /dynamic_resize)
   t1 = widget_label( b2, value=  'HA ', /align_right, /dynamic_resize)
   selid  = widget_label( b2, value='', /align_right, /dynamic_resize)
   melid  = widget_label( b2, value='', /align_right, /dynamic_resize)
   altid  = widget_label( b2, value='', /align_right, /dynamic_resize)
   azid   = widget_label( b2, value='', /align_right, /dynamic_resize)
   amid   = widget_label( b2, value='', /align_right, /dynamic_resize)
   haid   = widget_label( b2, value='', /align_right, /dynamic_resize)

   state = ptr_new({ $

      ; Data and information in the widget
      alt: ptr_new(), $          ; Altitude at last plot update
      altcrit: altcrit, $        ; Critical altitude
      am: ptr_new(), $           ; Airmass at last plot update
      amcrit: amcrit, $          ; Critical airmass
      az: ptr_new(), $           ; Azimuth at last plot update
      comment: ptr_new(), $      ; Comment for each object
      curjd: 0.0d0, $            ; current JD
      dec: ptr_new(), $          ; Dec of target field (input epoch)
      dirty: 0, $                ; Flag, if set means fieldobs.dat needs writing.
      dt: dt, $                  ; Offset from UT clock (hours).
      equinox: ptr_new(), $      ; Equinox of object.
      fn: '', $                  ; Filename list was loaded from.
      id: ptr_new(), $           ; field id
      jdlclmid: 0.0d0, $         ; JD of local midnight
      lat: 0.0d0, $              ; Latitude of observatory (radians)
      lha: ptr_new(), $          ; Local hour angle at last plot update
      lst: 0.0d0, $              ; Current local sidereal time (radians)
      lon: 0.0d0, $              ; Longitude of observatory (radians)
      melong: ptr_new(), $       ; Lunar elongation
      midlst: 0.0d0, $           ; LST of midnight
      nf: 0L, $                  ; Number of target fields loaded.
      obsacod: ptr_new(), $      ; All observatory codes
      obsalat: ptr_new(), $      ; All observatory latitudes
      obsalon: ptr_new(), $      ; All observatory longitudes
      obsanam: ptr_new(), $      ; All observatory names
      obscode: 'man', $          ; Observatory id ('man' means manual entry)
      obsfile: obsfile, $        ; Name of observatory code file.
      obsname: '', $             ; Name of observatory
      ra: ptr_new(), $           ; RA of target field (input epoch)
      selong: ptr_new(), $       ; Solar elongation
      select: -1L, $             ; Currently selected object.
      showlabel: 1, $            ; Flag, show id labels on objects.
      validobscod: 0, $          ; Flag, true if observatory code info valid.

      ; Widget ids
      altid: altid, $            ; Altitude
      amid: amid, $              ; Airmass
      azid: azid, $              ; Azimuth
      colbaseid: colbase, $      ; ID of button bar on bottom
      commentid: commentid, $    ; ID of object comment label
      curlstid:  curlst, $       ; ID of current lst label
      curtimeid: curtime, $      ; ID of current time label
      decid: decid, $            ; Label for object Dec
      drawwin: win1, $           ; ID of main draw window
      eqid: eqid, $              ; label for object equinox
      fileid: fileid, $          ; label for current file name
      haid: haid, $              ; Label for hour angle
      melid: melid, $            ; Lunar elongation
      moondescid: moondescid, $  ; Moon description field
      nfid: nf, $                ; ID of number of fields label
      objlenid: objlenid, $      ; Label for pass 2 length.
      objselid: objselid, $      ; Label of selected object.
      obscodeid: obscodeid, $    ; Observatory ID label
      obsnameid: obsnameid, $    ; Observatory Name label
      raid: raid, $              ; Label for object RA
      selid: selid, $            ; Solar elongation
      sundescid: sundescid, $    ; Sun description field

      mainbase: mainbase $       ; ID of top level base.

      })

   fieldobs_loadfile, state, file

   fieldobs_newobscode, state, obscode

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE
   widget_control,(*state).colbaseid,timer=1.0

   ; Give control to the XMANAGER.
   xmanager, 'fieldobs', mainbase, $
             EVENT_HANDLER='fieldobs_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='fieldobs_cleanup'

end

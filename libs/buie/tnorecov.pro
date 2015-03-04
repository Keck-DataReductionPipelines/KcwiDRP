;TODO
;
; For selected object show: am2?, time to set?, others?
; Show ghost object at am of earliest possible second frame.
; second plot with CCD orient and layout with overlain ellipse.
; widget for maxexp, maglim.
; Time to moonset or moonrise.
;
;+
; NAME:
;  tnorecov
; PURPOSE:
;  Real-time planning and field selection tool for TNO survey observations.
; DESCRIPTION:
;  This is a non-blocking widget tool.  It expects to read a file with
;    a list of object coordinates and other information.  The name of the
;    file is not arbitrary.  The root of the file name must be a 6-digit
;    encoding of the time for which the positions in the file are calculated
;    for.  YYMMDD (year, month, day) is thus required and the time is assumed
;    to be 0h UT on that day.  The suffix of the file name is normally '.dat',
;    but this is not a requirement but rather the initial value for the file
;    filter.
;  Files of the proper format are generated for each day on that day and can
;    be found at ftp://ftp.lowell.edu/pub/buie/kbo/recov.  The files in this
;    area are somewhat self-explanatory.  You can load the file of interest
;    using the "Open" option under the "File" menu.
;
;   There is a second file that is written as you tag fields with their
;     observation date and time.  This file is named YYMMDD.tim (same root as
;     the positions file that was read at the start) and is
;     written to the directory where you were when you started the program.
;     This file records the start times of the two visits to the field.
;     You must provide the y/m/d as well as h:m:s in the time field.  Here
;     are a few example lines from observed fields:
;
;2060 2000/07/28_23:48:45 2000/07/29_02:38:50
;20000 2000/07/28_23:41:58 2000/07/29_02:33:09
;95FB21 2000/07/28_23:33:50
;00CR105 2000/07/28_23:55:46 2000/07/29_02:45:07
;
;   If a field is not present, it hasn't yet been observed.  If a field has
;     only been observed once you'll only see one time (like 95FB21 above).
;
;   Because the tool is non-blocking, you can continue to use IDL normally
;     from the command line while this tool is running.  But, only one
;     copy of this tool is permitted at any given time.
;
; CATEGORY:
;  Astronomy
;
; CALLING SEQUENCE:
;  tnorecov
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
;  MAGLIM  - Faint magnitude cutoff.  Anything fainter will be filtered out
;               of the candidate list regardless of any other criteria.
;               This is not intended to limit the list based on variable
;               observing conditions.  Instead, this feature is intended to
;               control and limit the list so you don't see objects you can't
;               possibly do.
;
;  MIN1YERR - This sets the threshold of interesting objects as defined by
;               their error 1 year into the future.  Any object with a total
;               error less than this threshold will not appear as a candidate
;               for observation.  The default is 20 arcsec.  If you wish to
;               turn this feature off set MIN1YERR to 0.
;
;  OBSFILE - Name (including path) of the file containing the observatory
;              code information.  Default is /pub/sac0/elgb/data/obscode.dat'
;
;  OBSCDOE - Observatory id code, default is '688' (Lowell Observatory)
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
;  2001/03/12, MWB, numerous changes including online help files.
;  2001/03/14, MWB, changed for new format of support file.
;  2001/06/09, MWB, object coordinates updated for changing time.  Make the
;                 widget independent of the working directory of the main IDL
;                 window.
;  2001/06/19, MWB, added Show Object Info on Tools menu item.
;  2001/10/09, MWB, updated for new .[s]dat file format
;  2002/03/06, MWB, added 'Find Object' tool and 'Set Date/Time' tool.
;  2002/09/09, MWB, added support for string obscode values
;  2004/11/14, MWB, read two new fields on .dat files (error and error+1y)
;                      added MIN1YERR keyword
;                      changed "find object" so that = is optional
;-

;------------
pro tnorecov_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; save observing status
   tnorecov_obssave, state

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).alt
   ptr_free,(*state).am
   ptr_free,(*state).az
   ptr_free,(*state).arc
   ptr_free,(*state).cand
   ptr_free,(*state).code
   ptr_free,(*state).dec
   ptr_free,(*state).dra
   ptr_free,(*state).ddec
   ptr_free,(*state).ecc
   ptr_free,(*state).errora
   ptr_free,(*state).errorb
   ptr_free,(*state).fn
   ptr_free,(*state).id
   ptr_free,(*state).inc
   ptr_free,(*state).lastobs
   ptr_free,(*state).lha
   ptr_free,(*state).nastobs
   ptr_free,(*state).obsacod
   ptr_free,(*state).obsalat
   ptr_free,(*state).obsalon
   ptr_free,(*state).obsanam
   ptr_free,(*state).obscnt
   ptr_free,(*state).melong
   ptr_free,(*state).pang
   ptr_free,(*state).pexp
   ptr_free,(*state).ra
   ptr_free,(*state).rate
   ptr_free,(*state).rmag
   ptr_free,(*state).scat
   ptr_free,(*state).selong
   ptr_free,(*state).semi
   ptr_free,(*state).ut1
   ptr_free,(*state).ut2

   ; Free up the state structure itself.
   ptr_free, state

end

;------------
; Load a new observatory id code
pro tnorecov_loadobscod, state

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
      ans=dialog_message(['Observatory code file',(*state).obsfile,'not found.'])
      (*state).obsfile=''
   endelse

end

;------------
; Load the object information file and associated time event file.
pro tnorecov_loadobs, state, file

   if exists(file) then begin
      fdecomp,file,disk,dir,name,qual
      jdcnv,fix(strmid(name,0,2))+2000, $
            fix(strmid(name,2,2)), $
            fix(strmid(name,4,2)), $
            0.0,jd0
      (*state).jd0 = jd0
      pos=strpos(file,'.',/reverse_search)
      timefile=strmid(file,0,pos)+'.tim'
      readcol,file,name,elong,del,ra,dec,errora,errorb,pang,rmag, $
         dra,ddec,rate,inc,ecc,semi,arc,lastobs,nastobs,scat,code, $
         toterr,toterr1, $
         format='a,f,f,a,a,f,f,f,f,f,f,f,f,f,f,a,a,i,f,a,f,f'
      ra=raparse(ra)
      dec=decparse(dec)
      dra  = dra*15.0/3600.0/180.0*!dpi
      ddec = ddec/3600.0/180.0*!dpi
      nf=n_elements(ra)
      (*state).nf = nf
      (*state).objfile = file
      (*state).timefile = timefile
      if exists(timefile) then begin
         readcol,timefile,id,ut1,ut2,format='a,a,a'
         nobs=n_elements(id)
         z=where(ut1 eq '[[noobs]]',count)
         if count ne 0 then ut1[z] = ''
         z=where(ut2 eq '[[noobs]]',count)
         if count ne 0 then ut2[z] = ''
         ut1=repchar(ut1,'_',' ')
         ut2=repchar(ut2,'_',' ')
         ut1=jdparse(ut1)
         ut2=jdparse(ut2)
      endif else begin
         nobs=0
      endelse
      (*state).nobs=nobs

      ut1f = dblarr(nf)
      ut2f = dblarr(nf)
      obscnt=intarr(nf)

      ; Matchup observations with master list
      for i=0,nobs-1 do begin
         z=where(name eq id[i],count)
         if count gt 0 then begin
            z=z[0]
            ut1f[z] = ut1[i]
            ut2f[z] = ut2[i]
            count=0
            if ut1f[z] ne 0.0 then count=count+1
            if ut2f[z] ne 0.0 then count=count+1
            obscnt[z] = count
         endif
      endfor

      ; Cleanup
      ptr_free,(*state).id
      ptr_free,(*state).ra
      ptr_free,(*state).dec
      ptr_free,(*state).dra
      ptr_free,(*state).ddec
      ptr_free,(*state).errora
      ptr_free,(*state).errorb
      ptr_free,(*state).pang
      ptr_free,(*state).rmag
      ptr_free,(*state).rate
      ptr_free,(*state).inc
      ptr_free,(*state).ecc
      ptr_free,(*state).semi
      ptr_free,(*state).arc
      ptr_free,(*state).lastobs
      ptr_free,(*state).nastobs
      ptr_free,(*state).scat
      ptr_free,(*state).code
      ptr_free,(*state).toterr
      ptr_free,(*state).toterr1
      ptr_free,(*state).ut1
      ptr_free,(*state).ut2
      ptr_free,(*state).obscnt
      ptr_free,(*state).cand

      ; Stuff information into structure
      (*state).id     = ptr_new(name)
      (*state).ra     = ptr_new(ra)
      (*state).dec    = ptr_new(dec)
      (*state).dra    = ptr_new(dra)
      (*state).ddec   = ptr_new(ddec)
      (*state).errora = ptr_new(errora)
      (*state).errorb = ptr_new(errorb)
      (*state).pang   = ptr_new(pang)
      (*state).rmag   = ptr_new(rmag)
      (*state).rate   = ptr_new(rate)
      (*state).inc    = ptr_new(inc)
      (*state).ecc    = ptr_new(ecc)
      (*state).semi   = ptr_new(semi)
      (*state).arc    = ptr_new(arc)
      (*state).lastobs= ptr_new(lastobs)
      (*state).nastobs= ptr_new(nastobs)
      (*state).scat   = ptr_new(scat)
      (*state).code   = ptr_new(code)
      (*state).toterr = ptr_new(toterr)
      (*state).toterr1= ptr_new(toterr1)
      (*state).obscnt = ptr_new(obscnt)
      (*state).ut1    = ptr_new(ut1f)
      (*state).ut2    = ptr_new(ut2f)
      (*state).cand   = ptr_new(replicate(1,nf))
      (*state).pexp   = ptr_new(replicate((*state).refexp,nf))

      tnorecov_updatecand, state

   endif else begin
      ans=dialog_message([file,'File does not exist'])
   endelse
end

;------------
; Load a new observatory id code
pro tnorecov_newobscode, state, newid

   tnorecov_loadobscod,state

   if newid ne 'man' and (*state).validobscod then begin
      idx=where(newid eq (*(*state).obsacod),count)
      idx=idx[0]
      if (count eq 1) then begin
         (*state).lon     = (*(*state).obsalon)[idx]
         (*state).lat     = (*(*state).obsalat)[idx]
         (*state).obscode   = (*(*state).obsacod)[idx]
         (*state).obsname = (*(*state).obsanam)[idx]
      endif else begin
         ans=dialog_message('Observatory code '+newid+' not found')
      endelse
   endif

   if (*state).obscode eq 'man' then begin
      ; This is the GPS position for the 42", derived 1993 Sep 08
      (*state).lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
      (*state).lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
      (*state).obsname= 'Lowell Observatory - Anderson Mesa Station'
      (*state).obscode='688'
   endif

   widget_control,(*state).obscodeid,set_value='('+(*state).obscode+') '
   widget_control,(*state).obsnameid,set_value=(*state).obsname

end

;------------
; Save the results of the observation info.
pro tnorecov_obssave, state
   if (*state).dirty eq 1 then begin
      openw,lun,(*state).timefile,/get_lun
      for i=0,(*state).nf-1 do begin
         if (*(*state).obscnt)[i] gt 0 then begin
            if (*(*state).ut1)[i] gt 0.0d0 then $
               jdstr,(*(*state).ut1)[i],0,str1 $
            else $
               str1='[[noobs]]'
            str1=nobname(str1)
            if (*(*state).ut2)[i] gt 0.0d0 then $
               jdstr,(*(*state).ut2)[i],0,str2 $
            else $
               str2='[[noobs]]'
            str2=nobname(str2)
            printf,lun,(*(*state).id)[i],str1,str2,format='(a,1x,a,1x,a)'
         endif
      endfor
      free_lun,lun
      (*state).dirty = 0
   endif
end

;------------
; This routine takes the current object list, current time, and list of
;   previous observations and generates a plot that shows current sky placement
;   of fields to be done.
pro tnorecov_plot, state

   curjd   = systime(/julian,/utc) + (*state).dt/24.0
   (*state).curjd = curjd
   jdstr,curjd,0,curtimestr
   newstr = 'UT '+curtimestr
   if (*state).dt ne 0.0 then $
      newstr = newstr + ' (dt=' + strn((*state).dt,format='(f10.2)') + ')'
   widget_control,(*state).curtimeid,set_value=newstr

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
      oppam = airmass(replicate(curjd,npts),oppr,oppd, $
                         (*state).lat,(*state).lon,lha=oppl,/hardie)
      oppam1 = airmass(replicate(curjd,npts),oppr1,oppd, $
                         (*state).lat,(*state).lon,lha=oppl1,/hardie)
      oppam2 = airmass(replicate(curjd,npts),oppr2,oppd, $
                         (*state).lat,(*state).lon,lha=oppl2,/hardie)
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
      lsidtim,jdlclmid,(*state).lon,midlst

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

      deltat = (*state).minsep/((*(*state).rate) > 0.01)

      ; compute predicted exposure time
      magfac = 10.0^(((*state).refmag-(*(*state).rmag))/(-2.5))
      seefac = ((*state).fwhm/(*state).reffwhm)^2
      (*(*state).pexp) = magfac * seefac * (*state).refexp

      am = airmass(replicate(curjd,(*state).nf),(*(*state).ra),(*(*state).dec), $
                   (*state).lat,(*state).lon,lha=lha,alt=alt,az=az,/hardie)
      lha = lha*!radeg/15.0
      alt = alt*!radeg
      az  = az*!radeg
      altoha,(*state).altcrit,(*(*state).dec),(*state).lat,horzha,otype
      hatojd,0.0d0,(*(*state).ra),midlst,jdlclmid,jdtrans
      timetoset = fltarr(n_elements(otype))
      z=where(otype eq 0,count)
      if count ne 0 then $
         timetoset[z] = (jdtrans+horzha[z]/2.0d0/!dpi - curjd) * 24.0
      z=where(otype eq 1,count)
      if count ne 0 then $
         timetoset[z] = 48.0
      z=where(otype eq -1,count)
      if count ne 0 then $
         timetoset[z] = 0.0

      ; compute solar and lunar elongation angles.
      selong = fix(angsep(sunra,sundec,(*(*state).ra),(*(*state).dec)) * !radeg + 0.5)
      melong = fix(angsep(moonra,moondec,(*(*state).ra),(*(*state).dec)) * !radeg + 0.5)

      ; Tag all those objects that are out of range on airmass and haven't
      ;   yet been observed once.  These will be tagged as -1, ie. don't
      ;   observe or show.
      z=where(am ge (*state).amcrit and (*(*state).obscnt) lt 0, count)
      if count ne 0 then (*(*state).obscnt)[z] = -1

      ; Reset the obscnt flag for those that have become valid
      z=where(am lt (*state).amcrit and (*(*state).obscnt) lt 0, count)
      if count ne 0 then (*(*state).obscnt)[z]=0

      ; Check all objects not yet seen twice that are above the airmass
      ;   threshold.  These will set the plot boundaries.
      z=where(am lt (*state).amcrit and $
              (*(*state).obscnt) le 1, count)
      if count ne 0 then xr=max([abs(lha[z]),3.0])*[-1,1] $
      else xr=[-3,3]
      yr=[(*state).amcrit,1.0]

      ; Generate plot axes
      plot,[0],[1],xr=xr,yr=yr,ymargin=[4,4],/nodata,xstyle=3,ystyle=3, $
         xtitle='(E)  Hour Angle  (W)',ytitle='Airmass',color=pc

      ; Label top side of top axis with the RA at the hour angle
      pra=findgen(24) / 12.0 * !pi
      pam = airmass(replicate(curjd,24),pra,0.0,(*state).lat,(*state).lon,lha=plha)
      plha = plha *!radeg / 15.0
      pra = pra / !pi * 12.0
      zp=where(plha ge xr[0] and plha lt xr[1] and pam lt 10.0)
      pra=pra[zp]
      plha=plha[zp]
      zp=sort(plha)
      pra = fix(pra[zp])
      plha = plha[zp]
      pstr = strtrim(string(pra),2)
      axis,xaxis=1,xstyle=3,xr=xr,xtitle='RA (hours)',xtickv=plha, $
         xticks=n_elements(plha)-1,xtickn=pstr,xticklen=-0.02,color=pc

      ; plot the ecliptic
      ecllon=findgen(240) / 120.0 * 180.0
      euler,ecllon,replicate(0.,240),pra,pdec,4
      pra = pra/!radeg
      pdec = pdec/!radeg
      pam = airmass(replicate(curjd,240),pra,pdec,(*state).lat,(*state).lon,lha=plha)
      plha = plha * !radeg / 15.0
      zp=where(plha ge xr[0]-1.0 and plha lt xr[1]+1.0 and pam lt 90.0)
      plha = plha[zp]
      pam  = pam[zp]
      zp   = sort(plha)
      plha = plha[zp]
      pam  = pam[zp]
      oplot,plha,pam,color='007070'xl

      ; Plot sun relative lines
      oplot,oppl,oppam,color='600060'xl
      oplot,oppl1,oppam1,color='606000'xl
      oplot,oppl2,oppam2,color='606000'xl

      ; Select the unobserved objects and plot.
      z=where( (*(*state).cand) eq 1 and (*(*state).obscnt) eq 0 and $
               am lt (*state).amcrit, count)
      widget_control,(*state).availnumid,set_value=strn(count)
      if count ne 0 and $
         ( (*state).viewtype eq 'All' or (*state).viewtype eq 'Pass 1' ) then $
         oplot,lha[z],am[z],psym=4

      ; Flag objects that are still up but can't be done a second time tonight
      z=where( deltat gt timetoset and (*(*state).cand) eq 1 and $
         ( (*state).viewtype eq 'All' or (*state).viewtype eq 'Pass 1' ) and $
         ( (*(*state).obscnt) eq 0 ), count)
      if count ne 0 then begin
         oplot,lha[z],am[z],psym=4,color='8f40af'xl
      endif

      ; Select the in progress objects and plot.
      z=where( (*(*state).cand) eq 1 and (*(*state).obscnt) eq 1, count)
      widget_control,(*state).pendingid,set_value=strn(count)
      setusym,-9
      if count ne 0 and nightleft gt 0.0 and $
         ( (*state).viewtype eq 'All' or (*state).viewtype eq 'Pass 2' ) then $
         oplot,lha[z],am[z],psym=8,color='ffa04f'xl,symsize=1.4
      setusym,-1
      if count eq 0 then begin
         widget_control,(*state).pass2lenid,set_value=''
         widget_control,(*state).downid,set_value='0'
      endif else begin
         pass2len = (*state).ovrhd*count + total((*(*state).pexp)[z])/60.0
         pass2len = pass2len/60.0
         comptime = curjd + pass2len/24.0
         pass2len = pass2len / 12.0 * !dpi
         rastr,pass2len,-2,timeleftstr
         jdstr,comptime,-2,comptimestr
         comptimestr=strmid(comptimestr,11,99)
         widget_control,(*state).pass2lenid, $
            set_value='  pass 2 time '+timeleftstr+' ['+comptimestr+']'
         z=where( (*(*state).cand) eq 1 and (*(*state).obscnt) eq 1 and $
                  (*(*state).rate)*( (curjd-(*(*state).ut1))*24.0 ) gt  $
                      (*state).minsep, count)
         if count ne 0 and nightleft gt 0.0 and $
            ( (*state).viewtype eq 'All' or $
              (*state).viewtype eq 'Pass 2' ) then begin
            setusym,9
            oplot,lha[z],am[z],psym=8,color='003fff'xl,symsize=1.0
            setusym,1
            ; update number of fields ready for pass 2
            widget_control,(*state).downid,set_value=strn(count)
         endif else begin
            widget_control,(*state).downid,set_value='0'
         endelse

      endelse

      ; plot the sun and moon
      setusym,1
      oplot,[slha],[sam],psym=8,symsize=3.0,color='00ffff'xl
      oplot,[mlha],[mam],psym=8,symsize=3.0,color='808080'xl
      setusym,-1

      ; update number of completed fields
      z=where( (*(*state).obscnt) eq 2, count)
      widget_control,(*state).completeid,set_value=strn(count)

      ; save am and lha 
      ptr_free,(*state).alt
      ptr_free,(*state).am
      ptr_free,(*state).az
      ptr_free,(*state).lha
      ptr_free,(*state).selong
      ptr_free,(*state).melong
      (*state).alt = ptr_new(alt)
      (*state).am = ptr_new(am)
      (*state).az = ptr_new(az)
      (*state).lha = ptr_new(lha)
      (*state).selong = ptr_new(selong)
      (*state).melong = ptr_new(melong)
      tnorecov_updatesel, state, -1
   endif

end

;------------
pro tnorecov_updatecand, state

   if (*state).nf eq 0 then return

   ; Reset the candidate flag to true, then start taking them back out.
   (*(*state).cand)[*] = 1

   z = where( (*(*state).toterr) lt (*state).minerr or $
              (*(*state).toterr1) lt (*state).minerr1y or $
              (*(*state).toterr) gt (*state).maxerr or $
              (*(*state).rmag)   gt (*state).maglim, count )
   if count ne 0 then (*(*state).cand)[z] = 0

   widget_control, (*state).totalnumid, SET_VALUE=strn((*state).nf-count)

end

;------------
; Update the widgets showing the currently selected object
pro tnorecov_updatesel, state, newsel

   if newsel ge 0 and newsel lt (*state).nf then (*state).select = newsel

   if (*state).select lt 0 then begin
      widget_control,(*state).objselid,set_value=''
      widget_control,(*state).ut1id,set_value=''
      widget_control,(*state).ut2id,set_value=''
      widget_control,(*state).selid,set_value=''
      widget_control,(*state).melid,set_value=''
      widget_control,(*state).altid,set_value=''
      widget_control,(*state).azid,set_value=''
      widget_control,(*state).amid,set_value=''
      widget_control,(*state).raid,set_value=''
      widget_control,(*state).decid,set_value=''
      return
   endif

   deljd = ((*state).curjd-(*state).jd0)*24.0

   widget_control, (*state).drawwin, get_value=winnum
   wset,winnum
   setusym,1
   oplot,[(*(*state).lha)[(*state).select]], $
         [(*(*state).am)[(*state).select]],psym=8,color='0000ff'xl
   setusym,-1
   widget_control,(*state).objselid,set_value=(*(*state).id)[(*state).select]
   ut1=(*(*state).ut1)[(*state).select]
   if ut1 gt 0.0d0 then jdstr,ut1,0,str else str=''
   widget_control,(*state).ut1id,set_value=str
   ut2=(*(*state).ut2)[(*state).select]
   if ut2 gt 0.0d0 then jdstr,ut2,0,str else str=''
   widget_control,(*state).ut2id,set_value=str

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

   delra  = (*(*state).dra)[(*state).select]*deljd
   deldec = (*(*state).ddec)[(*state).select]*deljd
   ra  = (*(*state).ra)[(*state).select] + delra
   if ra lt 0.0 then ra = ra - 2.0d0*!dpi
   if ra ge 2.0d0*!dpi then ra = ra - 2.0d0*!dpi
   dec = (*(*state).dec)[(*state).select] + deldec
   rastr,ra,1,str
   widget_control,(*state).raid,set_value=str
   decstr,dec,0,str
   widget_control,(*state).decid,set_value=str

   euler,ra*!radeg,dec*!radeg,ecllon,ecllat,3
   str1 = strn(ecllon,format='(f10.1)')
   str2 = strn(ecllat,format='(f10.1)')
   widget_control,(*state).ecllonid,set_value=str1
   widget_control,(*state).ecllatid,set_value=str2

   euler,ra*!radeg,dec*!radeg,gallon,gallat,1
   str1 = strn(gallon,format='(f10.1)')
   str2 = strn(gallat,format='(f10.1)')
   widget_control,(*state).gallonid,set_value=str1
   widget_control,(*state).gallatid,set_value=str2

   str=strn((*(*state).rmag)[(*state).select],format='(f10.1)')
   widget_control,(*state).magid,set_value=str
   str=strn((*(*state).rate)[(*state).select],format='(f10.2)')
   widget_control,(*state).rateid,set_value=str+'"/hr'
   deltat = (*state).minsep/((*(*state).rate)[(*state).select]>0.01)
   str=strn(deltat,format='(f10.1)')
   widget_control,(*state).deltatid,set_value=str+' hr'
   str=strn((*(*state).errora)[(*state).select],format='(f10.1)')
   widget_control,(*state).erroraid,set_value=str
   str=strn((*(*state).errorb)[(*state).select],format='(f10.1)')
   widget_control,(*state).errorbid,set_value=str
   str=strn((*(*state).pexp)[(*state).select],format='(f10.0)')
   widget_control,(*state).pexpid,set_value=str+' s'

   ; info field
   if (*(*state).ut1)[(*state).select] eq 0.0d0 then begin
      widget_control,(*state).info1id,set_value=''
      widget_control,(*state).info2id,set_value=''
   endif else if (*(*state).ut2)[(*state).select] ne 0.0d0 then begin
      dt = ( (*state).curjd-(*(*state).ut2)[(*state).select] ) * 86400.0
      pexp = (*(*state).pexp)[(*state).select]
      if dt lt pexp then begin
         widget_control,(*state).info1id,set_value='left '
         dt = (pexp-dt) / 86400.0 * 2.0 * !dpi
         rastr,dt,0,str
         str=strmid(str,3,99)
         widget_control,(*state).info2id,set_value=str
      endif else begin
         widget_control,(*state).info1id,set_value=''
         widget_control,(*state).info2id,set_value=''
      endelse
   endif else begin
      dt = ( (*state).curjd-(*(*state).ut1)[(*state).select] ) * 86400.0
      pexp = (*(*state).pexp)[(*state).select]
      if dt le pexp then begin
         widget_control,(*state).info1id,set_value='left '
         dt = (pexp-dt) / 86400.0 * 2.0 * !dpi
         rastr,dt,0,str
         str=strmid(str,3,99)
         widget_control,(*state).info2id,set_value=str
      endif else begin
         widget_control,(*state).info1id,set_value='dist '
         dist=(*(*state).rate)[(*state).select] * dt/3600.0
         str=strn(dist,format='(f10.1)')
         widget_control,(*state).info2id,set_value=str+'"'
      endelse
   endelse

end

;------------
pro tnorecov_eve, event

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

            'Change delta T': begin
               newdt = qinput(prompt='New delta-T (hours) ', $
                                 default=(*state).dt,/floating)
               if newdt eq '' then begin
                  (*state).dt = 0.0
               endif else begin
                  (*state).dt = float(newdt)
               endelse
               tnorecov_plot,state
            end

            'Change Observatory': begin
               if (*state).validobscod then begin
                  list = '(' + (*(*state).obsacod) + $
                         ') '+(*(*state).obsanam)
                  sel = picker(list,group=event.top,index=idx, $
                               title='Select Observatory')
                  tnorecov_newobscode, state, (*(*state).obsacod)[idx]
                  tnorecov_plot,state
               endif else begin
                  print,'No valid observatory information.'
               endelse
            end

            'Get ObsFile': begin
               current=(*state).datadir
               fn = dialog_pickfile( group=event.top, TITLE='Select file', $
                                       /must_exist, FILTER='*.dat', path=current)
               if fn ne '' then begin
                  (*state).obsfile = fn
                  ptr_free,(*state).obsalon
                  ptr_free,(*state).obsalat
                  ptr_free,(*state).obsanam
                  ptr_free,(*state).obsacod
                  (*state).validobscod = 0
                  tnorecov_loadobscod, state
               endif
            end

            'Find Object': begin
               if (*state).nf gt 0 then begin
                  newfield = qinput(prompt='Object to Find? ',/string)
                  newfield = strupcase(strtrim(newfield[0],2))
                  if strmid(newfield,strlen(newfield)-1,1) eq '=' then $
                     newfield = strmid(newfield,0,strlen(newfield)-1)
                  z=where( (*(*state).id) eq newfield or $
                           (*(*state).id) eq newfield+'=', count)
                  if count gt 0 then (*state).select = z[0]
                  tnorecov_updatesel, state, -1
                  tnorecov_plot, state
               endif
            end

            'Load field list': begin
               current=(*state).datadir
               fn = dialog_pickfile( group=event.top, TITLE='Select file', $
                                     FILTER='*.dat', /must_exist, path=current, $
                                     get_path=current)

               if fn ne '' then begin
                  filename = strmid(fn,strlen(current),999)
                  (*state).datadir = current
                  tnorecov_loadobs, state, fn
                  tnorecov_plot,state
               endif

            end

            'Refresh plot' : begin
                  tnorecov_plot,state
            end

            'Select Object' : begin
               if (*state).nf gt 0 then begin
                  sel = picker((*(*state).id),group=event.top, $
                               index=idx,title='Select Object')
               tnorecov_updatesel, state, idx
               tnorecov_plot, state
               endif
            end
            
            'Set Date/Time': begin
               jdstr,(*state).curjd,0,curtimestr
               newtime = qinput(prompt='New Date/Time ',default=curtimestr,/string)
               newjd = jdparse(newtime[0])
               curjd   = systime(/julian,/utc)
               newdt = (newjd-curjd)*24.0

               if abs(newdt) lt 30.0*24.0 then begin
                  (*state).dt = newdt
                  tnorecov_plot,state
               endif
            end

            'Show Object Info' : begin
               if (*state).select ge 0 then begin
                  rastr,(*(*state).ra)[(*state).select],1,ras
                  decstr,(*(*state).dec)[(*state).select],0,decs
                  print,(*(*state).id)[(*state).select],ras,decs, $
                     (*(*state).rate)[(*state).select],'"/hr', $
                     '   R = ',(*(*state).rmag)[(*state).select], $
                     format='(a10,1x,a,1x,a,2x,f4.1,a,a,f4.1)'
                  print,(*(*state).inc)[(*state).select], $
                     format='("Inclination      ",f7.3,"  deg")'
                  print,(*(*state).ecc)[(*state).select], $
                     format='("Eccentricity     ",f8.4)'
                  print,(*(*state).semi)[(*state).select], $
                     format='("Semi-major axis ",f7.2,"   AU")'
                  print,(*(*state).nastobs)[(*state).select], $
                     (*(*state).arc)[(*state).select], $
                     (*(*state).lastobs)[(*state).select], $
                     format='(i10," observations over ",a,6x,"Last measured ",a)'
                  print,'error ellipse: ', $
                     'major ',(*(*state).errora)[(*state).select],'"', $
                     'minor ',(*(*state).errorb)[(*state).select],'"', $
                     'pang ',(*(*state).pang)[(*state).select],' deg', $
                     format='(a,a,1x,f7.1,a,3x,a,1x,f7.1,a,3x,a,1x,f5.1,a)'
                  print,'total error  : ', $
                    (*(*state).toterr)[(*state).select],'"', $ 
                    ' now, and ', $
                    (*(*state).toterr1)[(*state).select],'"', $ 
                    ' one year from now.', $
                    format='(a,f7.1,a,a,f7.1,a,a)'
                  case (*(*state).code)[(*state).select] of
                     'B': begin
                        ctext = 'Bernstein orbit'
                        end
                     'C': begin
                        ctext = 'large chisq'
                        end
                     'T': begin
                        ctext = 'Bowell orbit'
                        end
                     'V': begin
                        ctext = 'Vaisala orbit'
                        end
                     'I': begin
                        ctext = 'Unconverged solution'
                        end
                     else: begin
                        ctext = 'Unknown code ['+(*(*state).code)[(*state).select]+']'
                        end
                  endcase
                  print,(*(*state).scat)[(*state).select], $
                     ctext, $
                     format='("Scatter ",f5.2," arcsec      Orbit Code=",a)'
               endif
            end

            'General Help' : begin
               fn=find_with_def('tnorec1.txt',!path)
               if fn ne '' then begin
                  xdisplayfile,fn,title='General Help'
               endif else begin
                  ans=dialog_message('Help file, tnorec1.txt, could not be found.')
               endelse
            end

            'Plot Help' : begin
               fn=find_with_def('tnorec2.txt',!path)
               if fn ne '' then begin
                  xdisplayfile,fn,title='Plot Help'
               endif else begin
                  ans=dialog_message('Help file, tnorec2.txt, could not be found.')
               endelse
            end

            'Controls Help' : begin
               fn=find_with_def('tnorec3.txt',!path)
               if fn ne '' then begin
                  xdisplayfile,fn,title='Controls Help'
               endif else begin
                  ans=dialog_message('Help file, tnorec3.txt, could not be found.')
               endelse
            end

            'Files Help' : begin
               fn=find_with_def('tnorec4.txt',!path)
               if fn ne '' then begin
                  xdisplayfile,fn,title='Files Help'
               endif else begin
                  ans=dialog_message('Help file, tnorec4.txt, could not be found.')
               endelse
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

      'Field Select': begin
         if (*state).nf gt 0 then begin
            widget_control,(*state).objselid,get_value=newfield
            newfield = strupcase(strtrim(newfield[0],2))
            z=where( (*(*state).id) eq newfield, count)
            if count gt 0 then (*state).select = z[0]
            tnorecov_updatesel, state, -1
            tnorecov_plot, state
         endif
      end

      'FWHM': begin
         widget_control,(*state).fwhmid,get_value=newfwhm
         (*state).fwhm = float(newfwhm[0])
         widget_control, (*state).fwhmid, $
            set_value=strn((*state).fwhm,format='(1x,f10.1)')
         tnorecov_updatesel, state, -1
         tnorecov_plot, state
      end

      'Overhead': begin
         widget_control,(*state).ovrhdid,get_value=newovrhd
         newovrhd=float(newovrhd[0])
         if newovrhd gt 0.0 and newovrhd lt 30.0 then $
            (*state).ovrhd = newovrhd
         widget_control, (*state).ovrhdid, $
            set_value=strn((*state).ovrhd,format='(1x,f10.3)')
         tnorecov_plot, state
      end

      'Mainbase': begin
         info=widget_info((*state).colbaseid,/geometry)
         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize
         tnorecov_plot, state
      end

      'MaxErr': begin
         widget_control,(*state).maxerrid,get_value=newmaxerr
         (*state).maxerr = float(newmaxerr[0])
         widget_control, (*state).maxerrid, $
            set_value=strn((*state).maxerr,format='(1x,f10.1)')
         tnorecov_updatecand, state
         tnorecov_plot, state
      end

      'MinErr': begin
         widget_control,(*state).minerrid,get_value=newminerr
         (*state).minerr = float(newminerr[0])
         widget_control, (*state).minerrid, $
            set_value=strn((*state).minerr,format='(1x,f10.1)')
         tnorecov_updatecand, state
         tnorecov_plot, state
      end

      'MinSep': begin
         widget_control,(*state).minsepid,get_value=newminsep
         (*state).minsep = float(newminsep[0])
         widget_control, (*state).minsepid, $
            set_value=strn((*state).minsep,format='(1x,f10.1)')
         tnorecov_plot, state
      end

      'Pick Field': begin
         if (*state).nf gt 0 then begin
            z = where( (*(*state).cand) eq 1 and (*(*state).obscnt) eq 1 and $
                       (*(*state).rate)*( ((*state).curjd-(*(*state).ut1))*24.0 ) gt (*state).minsep and $
                       (*(*state).am) lt (*state).amcrit, count)
            if count ne 0 then begin
               zz = where((*(*state).lha)[z] eq max((*(*state).lha)[z]))
               tnorecov_updatesel, state, z[zz[0]]
               tnorecov_plot, state
            endif
         endif
      end

      'RefExp': begin
         widget_control,(*state).refexpid,get_value=newrefexp
         (*state).refexp = float(newrefexp[0])
         widget_control, (*state).refexpid, $
            set_value=strn((*state).refexp,format='(1x,f10.1)')
         tnorecov_updatesel, state, -1
         tnorecov_plot, state
      end

      'RefFWHM': begin
         widget_control,(*state).reffwhmid,get_value=newreffwhm
         (*state).reffwhm = float(newreffwhm[0])
         widget_control, (*state).reffwhmid, $
            set_value=strn((*state).reffwhm,format='(1x,f10.1)')
         tnorecov_updatesel, state, -1
         tnorecov_plot, state
      end

      'RefMag': begin
         widget_control,(*state).refmagid,get_value=newrefmag
         (*state).refmag = float(newrefmag[0])
         widget_control, (*state).refmagid, $
            set_value=strn((*state).refmag,format='(1x,f10.1)')
         tnorecov_updatesel, state, -1
         tnorecov_plot, state
      end

      'Set View': begin
         (*state).viewtype=event.value
         tnorecov_plot, state
      end

      'Timer': begin
         widget_control,(*state).colbaseid,timer=15.0
         tnorecov_plot, state
      end

      'UT 1': begin
         if (*state).nf eq 0 or (*state).select lt 0 then return

         curjd = systime(/julian,/utc) + (*state).dt/24.0
         jdstr,curjd,0,newut1

         widget_control,(*state).ut1id,set_value=newut1
         (*(*state).ut1)[(*state).select] = curjd
         if (*(*state).ut2)[(*state).select] gt 0.0 then $
            (*(*state).obscnt)[(*state).select] = 2 $
         else $
            (*(*state).obscnt)[(*state).select] = 1
         (*state).dirty = 1
         tnorecov_plot, state
      end

      'UT1 Set': begin
         if (*state).nf eq 0 or (*state).select lt 0 then return

         widget_control,(*state).ut1id,get_value=newut1
         newut1 = strtrim(strcompress(newut1[0]),2)
         jd = jdparse(newut1)
         (*(*state).ut1)[(*state).select] = jd
         if (*(*state).ut1)[(*state).select] gt 0.0 then begin
            if (*(*state).ut2)[(*state).select] gt 0.0 then $
               (*(*state).obscnt)[(*state).select] = 2 $
            else $
               (*(*state).obscnt)[(*state).select] = 1
         endif else begin
            (*(*state).obscnt)[(*state).select] = 0
         endelse
         (*state).dirty = 1
         if newut1 ne '' then jdstr,jd,0,newut1
         widget_control,(*state).ut1id,set_value=newut1
         tnorecov_plot, state
      end

      'UT 2': begin
         if (*state).nf eq 0 or (*state).select lt 0 then return
         if (*(*state).ut1)[(*state).select] eq 0.0d0 then begin
            tnorecov_plot, state
            return
         endif

         curjd = systime(/julian,/utc) + (*state).dt/24.0
         jdstr,curjd,0,newut2

         widget_control,(*state).ut2id,set_value=newut2
         (*(*state).ut2)[(*state).select] = curjd
         (*(*state).obscnt)[(*state).select] = 2
         (*state).dirty = 1
         tnorecov_plot, state
      end

      'UT2 Set': begin
         if (*state).nf eq 0 or (*state).select lt 0 then return
         if (*(*state).ut1)[(*state).select] eq 0.0d0 then begin
            tnorecov_plot, state
            return
         endif

         widget_control,(*state).ut2id,get_value=newut2
         newut2 = strtrim(strcompress(newut2[0]),2)
         jd=jdparse(newut2)
         (*(*state).ut2)[(*state).select] = jd
         if (*(*state).ut2)[(*state).select] eq 0.0d0 then $
            (*(*state).obscnt)[(*state).select] = 1 $
         else $
            (*(*state).obscnt)[(*state).select] = 2
         (*state).dirty = 1
         if newut2 ne '' then jdstr,jd,0,newut2
         widget_control,(*state).ut2id,set_value=newut2
         tnorecov_plot, state
      end

      'Window': begin

         if (*state).nf gt 0 and event.type eq 0 and event.press eq 1 then begin

            if (*state).viewtype eq 'All' then $
               z = where( (*(*state).cand) eq 1 and $
                          (*(*state).am) lt (*state).amcrit and $
                          (*(*state).obscnt) ge 0 and $
                          (*(*state).obscnt) le 1, count ) $
            else if (*state).viewtype eq 'Pass 1' then $
               z = where( (*(*state).cand) eq 1 and $
                          (*(*state).am) lt (*state).amcrit and $
                          (*(*state).obscnt) eq 0, count) $
            else if (*state).viewtype eq 'Pass 2' then $
               z = where( (*(*state).cand) eq 1 and $
                          (*(*state).am) lt (*state).amcrit and $
                          (*(*state).obscnt) eq 1, count)

            if count ne 0 then begin
               cval = convert_coord((*(*state).lha)[z],(*(*state).am)[z],/data,/to_device)
               x    = cval[0,*]
               y    = cval[1,*]
               dist = sqrt( (x-event.x)^2 + (y-event.y)^2 )
               zz=where(dist eq min(dist))
               zz=zz[0]
               tnorecov_updatesel, state, z[zz]
               tnorecov_plot, state
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

;------------
pro tnorecov,OBSFILE=obsfile,OBSCODE=obscode,AMCRIT=amcrit,DT=dt, $
       MAGLIM=maglim,MIN1YERR=minerr1y

   if xregistered('tnorecov') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. TNORECOV cannot be started.'
      return
   endif

   if badpar(obsfile,[0,7],0,CALLER='TNORECOV: (OBSFILE) ', $
                             DEFAULT='obscode.dat') then return
   if badpar(obscode,[0,1,2,3,7],0,caller='TNORECOV: (OBSCODE) ', $
                default='688',type=codetype) then return
   if badpar(amcrit,[0,2,3,4,5],0,caller='TNORECOV: (AMCRIT) ',default=2.5) then return
   if badpar(dt,[0,2,3,4,5],0,caller='TNORECOV: (DT) ',default=0.0d0) then return
   if badpar(maglim,[0,2,3,4,5],0,caller='TNORECOV: (MAGLIM) ',default=23.5) then return
   if badpar(minerr1y,[0,2,3,4,5],0,caller='TNORECOV: (MIN1YERR) ',default=20.0) then return

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   setusym,-1
   minsep = 3.0
   ovrhd = 0.833
   minerr = 2.0
   maxerr = 180.0
   refmag = 23.5
   refexp = 300.0
   reffwhm = 1.2
   fwhm = 1.5
   maxexp = 600.0

   cd,current=datadir

   ;Define the main base.
   mainbase = widget_base( TITLE='TNORECOV: TNO Field Section Tool', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File', $
                     '0\Load field list', $
                     '0\Get ObsFile', $
                     '2\Exit', $
                     '1\Tools', $
                     '0\Update field info', $
                     '0\Select Object', $
                     '0\Find Object', $
                     '0\Show Object Info', $
                     '0\Change Observatory', $
                     '0\Change delta T', $
                     '0\Set Date/Time', $
                     '2\Refresh plot', $
                     '1\Help', $
                     '0\General Help', $
                     '0\Plot Help', $
                     '0\Controls Help', $
                     '2\Files Help'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase,col=1)

   win1 = widget_draw( base, XSIZE=750, YSIZE=400, RETAIN=2, $
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

   b2 = widget_base(b1,row=1)
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='MinSep(")', /align_center, /dynamic_resize)
   minsepid = widget_text( b3, value=strn(minsep,format='(1x,f10.1)'),xsize=7, $
                           uvalue='MinSep',/align_center,/editable )
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='Overhead(m)', /align_center, /dynamic_resize)
   ovrhdid = widget_text( b3, value=strn(ovrhd,format='(1x,f10.3)'),xsize=7, $
                           uvalue='Overhead',/align_center,/editable )
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='MinErr(")', /align_center, /dynamic_resize)
   minerrid = widget_text( b3, value=strn(minerr,format='(1x,f10.1)'),xsize=7, $
                           uvalue='MinErr',/align_center,/editable )
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='MaxErr(")', /align_center, /dynamic_resize)
   maxerrid = widget_text( b3, value=strn(maxerr,format='(1x,f10.1)'),xsize=7, $
                           uvalue='MaxErr',/align_center,/editable )

   b2 = widget_base(b1,row=1)
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='RefExp(s)', /align_center, /dynamic_resize)
   refexpid = widget_text( b3, value=strn(refexp,format='(1x,f10.1)'),xsize=7, $
                           uvalue='RefExp',/align_center,/editable )
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='RefMag', /align_center, /dynamic_resize)
   refmagid = widget_text( b3, value=strn(refmag,format='(1x,f10.1)'),xsize=7, $
                           uvalue='RefMag',/align_center,/editable )
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='RefFWHM', /align_center, /dynamic_resize)
   reffwhmid = widget_text( b3, value=strn(reffwhm,format='(1x,f10.1)'),xsize=7, $
                           uvalue='RefFWHM',/align_center,/editable )
   b3 = widget_base(b2,col=1)
   t1 = widget_label( b3, value='FWHM', /align_center, /dynamic_resize)
   fwhmid = widget_text( b3, value=strn(fwhm,format='(1x,f10.1)'),xsize=7, $
                           uvalue='FWHM',/align_center,/editable )

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

   c1a = widget_base(c1,row=1)
   b1 = widget_base(c1a,col=2,/frame)
   t1 = widget_label( b1, value= 'RA ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= 'Dec ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value=  'HA ', /align_right, /dynamic_resize)
   info1id   = widget_label( b1, value= '',/align_righ, /dynamic_resize)
   raid      = widget_label( b1, value='', /align_right, /dynamic_resize)
   decid     = widget_label( b1, value='', /align_right, /dynamic_resize)
   haid   = widget_label( b1, value='', /align_right, /dynamic_resize)
   info2id = widget_label( b1, value='',/align_right, /dynamic_resize)
   b1 = widget_base(c1a,col=2,/frame)
   t1 = widget_label( b1, value= '  Ecl lon ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= '  Ecl lat ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= '  Gal lon ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= '  Gal lat ', /align_right, /dynamic_resize)
   ecllonid  = widget_label( b1, value='', /align_right, /dynamic_resize)
   ecllatid  = widget_label( b1, value='', /align_right, /dynamic_resize)
   gallonid  = widget_label( b1, value='', /align_right, /dynamic_resize)
   gallatid  = widget_label( b1, value='', /align_right, /dynamic_resize)

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
   t1 = widget_label(b1,value=' Ready for pass 2 ',/align_left,/dynamic_resize)

   c1a = widget_base(c1,row=1)
   b1 = widget_base(c1a,col=2,/frame)
   t1 = widget_label( b1, value= 'Sel ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= 'Mel ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= 'Alt ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value=  'Az ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value=  'Am ', /align_right, /dynamic_resize)
   selid  = widget_label( b1, value='', /align_right, /dynamic_resize)
   melid  = widget_label( b1, value='', /align_right, /dynamic_resize)
   altid  = widget_label( b1, value='', /align_right, /dynamic_resize)
   azid   = widget_label( b1, value='', /align_right, /dynamic_resize)
   amid   = widget_label( b1, value='', /align_right, /dynamic_resize)
   b1 = widget_base(c1a,col=2,/frame)
   t1 = widget_label( b1, value= 'R ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= 'Rate ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= 'dt ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= 'erra ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= 'errb ', /align_right, /dynamic_resize)
   t1 = widget_label( b1, value= 'pexp ', /align_right, /dynamic_resize)
   magid  = widget_label( b1, value='', /align_left, /dynamic_resize)
   rateid  = widget_label( b1, value='', /align_left, /dynamic_resize)
   deltatid  = widget_label( b1, value='', /align_left, /dynamic_resize)
   erroraid  = widget_label( b1, value='', /align_left, /dynamic_resize)
   errorbid  = widget_label( b1, value='', /align_left, /dynamic_resize)
   pexpid    = widget_label( b1, value='', /align_left, /dynamic_resize)

   altcrit = 0.5*!pi - acos(1.0/amcrit)

   state = ptr_new({ $

      ; Data and information in the widget
      alt: ptr_new(), $          ; Altitude at last plot update
      altcrit: altcrit, $        ; critical altitude (radians)
      am: ptr_new(), $           ; Airmass at last plot update
      amcrit: amcrit, $          ; Critical airmass
      arc: ptr_new(), $          ; Arc length for astrometric data (string).
      az: ptr_new(), $           ; Azimuth at last plot update
      cand: ptr_new(), $         ; Candidate flag
      code: ptr_new(), $         ; Generation code
      curjd: 0.0d0, $            ; JD of last plot update
      datadir: datadir, $        ; Directory where the data file was read from.
      dec: ptr_new(), $          ; Dec of target field (input epoch)
      dirty: 0, $                ; Flag, if set means TIMEFILE needs writing.
      ddec: ptr_new(), $         ; radians per hour, motion on sky.
      dra: ptr_new(), $          ; radians per hour, motion on sky.
      dt: dt, $                  ; Offset from UT clock (hours).
      ecc: ptr_new(), $          ; eccentricity of orbit
      errora: ptr_new(), $       ; error ellipse major axis
      errorb: ptr_new(), $       ; error ellipse minor axis
      fn: ptr_new(), $           ; Filename field was loaded from.
      fwhm: fwhm, $              ; Current FWHM (arcsec).
      id: ptr_new(), $           ; field id
      inc: ptr_new(), $          ; Inclination of orbit
      jd0: 0.0d0, $              ; JD of the positions in file.
      lastobs: ptr_new(), $      ; UT date of last observation (string)
      lat: 0.0d0, $              ; Latitude of observatory (radians)
      lha: ptr_new(), $          ; Local hour angle at last plot update
      lon: 0.0d0, $              ; Longitude of observatory (radians)
      maglim: maglim, $          ; faint magnitude limit
      maxerr: maxerr, $          ; Maximum error of interest
      maxexp: maxexp, $          ; Maximum exposure time.
      melong: ptr_new(), $       ; lunar elongation
      minerr: minerr, $          ; Minimum error of interest
      minerr1y: minerr1y, $          ; Minimum error of interest
      minsep: minsep, $          ; Pattern length in hours.
      nastobs: ptr_new(), $      ; Number of astrometric observations.
      nf: 0L, $                  ; Number of target fields loaded.
      nobs: 0L, $                ; Number of fields observed.
      objfile: '', $             ; Name of the object list file.
      obsacod: ptr_new(), $      ; All observatory codes
      obsalat: ptr_new(), $      ; All observatory latitudes
      obsalon: ptr_new(), $      ; All observatory longitudes
      obsanam: ptr_new(), $      ; All observatory names
      obscnt: ptr_new(), $       ; Number of observations of each field.
      obscode: 'man', $          ; Observatory id (-1 means manual entry)
      obsfile: obsfile, $        ; Name of observatory code file.
      obsname: '', $             ; Name of observatory
      ovrhd: ovrhd, $            ; Exposure time plus overhead (minute).
      pang: ptr_new(), $         ; Position angle of error ellipse
      pexp: ptr_new(), $         ; predicted exposure time.
      ra: ptr_new(), $           ; RA of target field (input epoch)
      rate: ptr_new(), $         ; Rate of motion, arcsec/hr
      refexp: refexp, $          ; Reference exposure time (seconds).
      reffwhm: reffwhm, $        ; Reference seeing, arcsec.
      refmag: refmag, $          ; Reference magnitude.
      rmag: ptr_new(), $         ; R magnitude of target.
      scat: ptr_new(), $         ; Scatter in astrometric observations (").
      select: -1, $              ; Currently selected object.
      selong: ptr_new(), $       ; solar elongation.
      semi: ptr_new(), $         ; semi-major axis of orbit.
      toterr: ptr_new(), $       ; total uncertainty in position
      toterr1: ptr_new(), $      ; position uncertainty at t+1 year.
      timefile: '', $            ; Name of the observation time file.
      ut1: ptr_new(), $          ; UT of first observation
      ut2: ptr_new(), $          ; UT of second observation
      validobscod: 0, $          ; Flag, true if observatory code info valid.
      viewtype: 'All', $         ; type of plot

      ; Widget ids
      altid: altid, $            ; Altitude
      amid: amid, $              ; Airmass
      azid: azid, $              ; Azimuth
      availnumid: availnum, $    ; Label of number of available fields.
      colbaseid: colbase, $      ; ID of button bar on bottom
      completeid: complete, $    ; Label of number of completed fields.
      curtimeid: curtime, $      ; ID of current time label
      decid: decid, $            ; ID of Dec label
      deltatid: deltatid, $      ; ID of delta t label (time for repeat)
      downid: down, $            ; Label of number of fields not available.
      drawwin: win1, $           ; ID of main draw window
      ecllatid: ecllatid, $      ; ID of ecliptic latitude
      ecllonid: ecllonid, $      ; ID of ecliptic longitude
      erroraid: erroraid, $      ; ID of major axis of error ellipse
      errorbid: errorbid, $      ; ID of minor axis of error ellipse
      fwhmid: fwhmid, $          ; FWHM widget
      gallatid: gallatid, $      ; ID of galactic latitude
      gallonid: gallonid, $      ; ID of galactic longitude
      haid: haid, $              ; Label for hour angle
      info1id: info1id, $        ; label field #1
      info2id: info2id, $        ; label field #2
      magid: magid, $            ; R mag label ID
      maxerrid: maxerrid, $      ; Maximum error of interest
      melid: melid, $            ; Lunar elongation
      minerrid: minerrid, $      ; Minimum error of interest
      minsepid: minsepid, $      ; sets pattern length
      moondescid: moondescid, $  ; Moon description field
      nfid: nf, $                ; ID of number of fields label
      objselid: objselid, $      ; Label of selected object.
      obscodeid: obscodeid, $    ; Observatory ID label
      obsnameid: obsnameid, $    ; Observatory Name label
      ovrhdid: ovrhdid, $        ; Shows exposure time plus overhead (min)
      pass2lenid: pass2lenid, $  ; Label for pass 2 length.
      pendingid: pending, $      ; Label of number of fields in progress
      pexpid: pexpid, $          ; Predicted exposure time label id.
      raid: raid, $              ; ID of RA label
      rateid: rateid, $          ; ID of rate label
      refexpid: refexpid, $      ; reference exposure widget
      reffwhmid: reffwhmid, $    ; reference FWHM widget
      refmagid: refmagid, $      ; reference magnitude widget
      selid: selid, $            ; Solar elongation
      sundescid: sundescid, $    ; Sun description field
      totalnumid: totalnum, $    ; Label for total number of fields
      ut1id: ut1id, $            ; Label of UT1
      ut2id: ut2id, $            ; Label of UT2
      viewsetid: viewset, $      ; Control for type of plot

      mainbase: mainbase $       ; ID of top level base.

      })

   tnorecov_newobscode, state, obscode

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE
   widget_control,(*state).colbaseid,timer=1.0
   widget_control, (*state).viewsetid, SET_VALUE=0

   ; Give control to the XMANAGER.
   xmanager, 'tnorecov', mainbase, $
             EVENT_HANDLER='tnorecov_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='tnorecov_cleanup'

end

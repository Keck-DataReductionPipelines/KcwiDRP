;+
; NAME:
;  obsprop
; PURPOSE:
;  Observing proposal planning table generation
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  obsprop,objcode,obs,date1,date2
; INPUTS:
;  objcode - String array of standard object codes (see ephem.pro)
;  obs     - Integer Marsden code of the observatory
;               688 - Lowell Observatory
;               500 - Geocentric
;               If you provide an invalid code, 688 is assumed.
;  date1   - UT date and time vector near midnight for first night,
;                 [year,month,day,hour]
;  date2   - UT date and time vector near midnight for last night,
;     
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  AMLIMIT - limiting airmass that defines observing window (default=3.0)
;  OBSFILE - Override on file name where observatory codes are to be found.
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
;  Written by Marc W. Buie, Lowell Observatory, 1997/02/18
;  2001/03/16, David Tucker, changed rdobscod call and file default
;  2001/05/01, MWB, fixed objcode bug.
;  2002/02/14, MWB, minor formatting changes of output.
;  2002/03/27, MWB, fixed AMLIMIT bug.
;  2002/11/04, MWB, added support for string obscode values
;  2009/11/10, MWB, fixed minor formatting problem.
;-
PRO obsprop,objcode,obs,date1,date2,AMLIMIT=amlimit,OBSFILE=obsfile

   if n_params() eq 0 then begin
      print,'obsprop,objcode,obs,date1,date2'
      return
   endif

   if badpar(objcode,7,[0,1],CALLER='OBSPROP: (objcode) ') then return
   if badpar(obs,[2,3,7],0,CALLER='OBSPROP: (obs) ',type=codetype) then return
   if badpar(date1,[2,3,4,5],[0,1],CALLER='OBSPROP: (date1) ', $
                npts=date1len) then return
   if badpar(date2,[2,3,4,5],[0,1],CALLER='OBSPROP: (date2) ', $
                npts=date2len) then return
   if badpar(obsfile,[0,7],0,CALLER='OBSPROP: (OBSFILE) ', $
                default='obscode.dat') then return
   if badpar(amlimit,[0,2,3,4,5],0,CALLER='OBSPROP: (AMLIMIT) ',default=3.0) then return

   if codetype ne 7 then begin
      obs = strn(obs,length=3,padchar='0')
   endif else begin
      obs = strupcase(obs)
   endelse

   ; Convert dates to JD.
   if date1len eq 4 then begin
      jdcnv,date1[0],date1[1],date1[2],date1[3],jd1
   endif else begin
      print,'OBSPROP: length of date1 must be 4'
      return
   endelse

   if date2len eq 4 then begin
      jdcnv,date2[0],date2[1],date2[2],date2[3],jd2
   endif else begin
      print,'OBSPROP: length of date2 must be 4'
      return
   endelse

   blanks='          '

   rdobscod,code,alllon,rhosinp,rhocosp,obsname,valid,FILE=obsfile
   if not valid then begin
      print,'Observatory code file ',obsfile,' not found.'
      return
   endif

   ; Fetch observatory information
   idx=where(obs eq code,count)
   idx=idx[0]
   if (count eq 1) then begin
      lon = (360.0-alllon[idx])/180.0*!pi
      lat = atan(rhocosp[idx],rhosinp[idx])
      name=strtrim(obsname[idx],2)
   endif else begin
      ; Hardcoded position for 42" if code not recognized
      ; This is the GPS position for the 42", derived 1993 Sep 08
      lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
      lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
      name = '42" at Anderson Mesa'
   endelse

   print,'Observatory ',name,' selected.'

   ; Set the critical altitude and airmass for observability.
   crital = 0.5*!pi - acos(1.0/amlimit)
print,'Critical altitude ',crital*!radeg,' deg'

   ; Compute ephemeris for object
   njd=fix(ceil(jd2-jd1))+1
   jdeph = jd1 + (jd2-jd1)*dindgen(njd)/(njd-1)
   ephem,jdeph,obs,70,objcode,eph
   ssgeom,eph,sun,earth,phang,elong

   print,'Observability for ',naifname(objcode)
   print,'  UT Date    pre  UT -Rise- lst         len' + $
         '  UT -Set- lst          post' + $
         '  Trnst  X   phang Sel Mel'
   FOR i=0,njd-1 DO BEGIN

      jd = jdeph[i]

      ; Sun position at input JD
      sunpos,jd,sunra,sundec,/radian
      moonpos,jd,moonra,moondec,/radian

      melong=sphdist(eph[0,i],eph[1,i],moonra,moondec)*180.0d0/!dpi

      ; Define night, Sun set to sun rise.
      am  = airmass(jd,sunra,sundec,lat,lon,alt=alt,lha=lha,lst=lst)
      hatojd,!dpi,sunra,lst,jd,jdlclmid ; jd of nearest local midnight
      lsidtim,jdlclmid,lon,midlst       ; LST at local midnight
      jdofmid = float(long(jdlclmid+0.5d0))-0.5d0
      jdstr,jdofmid,100,thisdate

      ; Hour angle of Sun at sunset, AT, NT, CT
      altoha,-18.0/!radeg,sundec,lat,sunatha,sunattype
      altoha,-12.0/!radeg,sundec,lat,sunntha,sunnttype
      altoha,-6.0/!radeg,sundec,lat,sunctha,suncttype
      altoha,-0.5/!radeg,sundec,lat,sunhorzha,sunhorztype

      ; JD of sunset/sunrise, AT, NT, CT
      jdatset  = jdlclmid - (!dpi-sunatha)/2.0d0/!dpi
      jdatrise = jdlclmid + (!dpi-sunatha)/2.0d0/!dpi
      jdntset  = jdlclmid - (!dpi-sunntha)/2.0d0/!dpi
      jdntrise = jdlclmid + (!dpi-sunntha)/2.0d0/!dpi
      jdctset  = jdlclmid - (!dpi-sunctha)/2.0d0/!dpi
      jdctrise = jdlclmid + (!dpi-sunctha)/2.0d0/!dpi
      jdsset   = jdlclmid - (!dpi-sunhorzha)/2.0d0/!dpi
      jdsrise  = jdlclmid + (!dpi-sunhorzha)/2.0d0/!dpi

      jdstr,jdsset,-12,jdssetstr
      jdstr,jdsrise,-12,jdsrisestr
      jdstr,jdatset,-12,jdatsetstr
      jdstr,jdatrise,-12,jdatrisestr
      jdstr,jdntset,-12,jdntsetstr
      jdstr,jdntrise,-12,jdntrisestr

      am  = airmass(jd,sunra,sundec,lat,lon,alt=alt,lha=lha,lst=lst)
      hatojd,!dpi,sunra,lst,jd,jdlclmid
      lsidtim,jdlclmid,lon,midlst
      hatojd,0.0d0,eph[0,i],midlst,jdlclmid,jdtrans
      jdstr,jdtrans,-12,objtransstr
      transam = airmass(jdtrans,eph[0,i],eph[1,i],lat,lon,alt=transalt)
      altoha,crital,eph[1,i],lat,horzha,type
      if type eq 0 then begin
         jdrise  = jdtrans - horzha/2.0d0/!dpi
         jdset   = jdtrans + horzha/2.0d0/!dpi
      endif
      obswind,midlst,lat,eph[0,i],eph[1,i],jdntrise,jdntset,rtime,rkind, $
         stime,skind,objtype,AMLIMIT=amlimit
      if objtype ne -1 and objtype ne 2 then begin
         jdstr,rtime,-12,risestr
         jdstr,stime,-12,setstr
         prior = (rtime-jdntset)*24.0d0
         post  = (jdntrise-stime)*24.0d0
         durat = (stime-rtime)*24.0d0
         
         lsidtim,rtime,lon,rlst       ; LST at window start
         lsidtim,stime,lon,slst       ; LST at window stop
         rastr,rlst,-2,rlststr
         rastr,slst,-2,slststr

         if prior lt 9.95 then $
            priorstr = string(prior,format='(f3.1)') $
         else $
            priorstr = string(prior,format='(f3.0)')

         if durat lt 9.95 then $
            duratstr = string(durat,format='(f3.1)') $
         else $
            duratstr = string(durat,format='(f3.0)')

         if post lt 9.95 then $
            poststr = string(post,format='(f3.1)') $
         else $
            poststr = string(post,format='(f3.0)')

         print,strmid(risestr,0,11),priorstr,strmid(risestr,12,5),rlststr, $
            rkind+blanks,duratstr,strmid(setstr,12,5),slststr,skind+blanks,poststr, $
            strmid(objtransstr,12,5),transam, $
            phang[i], $
            fix(ceil(elong[i])), $
            fix(ceil(melong)), $
            format='(a,1x,"(",a,")",1x,a,1x,"[",a,"]",1x,a6,1x,"(",a,")",1x,a,1x,"[",a,"]",1x,a7,1x,' + $
                   '"(",a,")",1x,a,1x,f3.1,1x,f6.2,1x,i3,1x,i3)'
      endif else begin
         jdstr,jdlclmid,-12,datestr
         print,strmid(datestr,0,11),'object never rises.',format='(a,1x,a)'
      endelse

   ENDFOR

END

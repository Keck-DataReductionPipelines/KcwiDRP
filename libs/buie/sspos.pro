;+
; NAME: 
;  sspos
; PURPOSE: 
;  Interactive program for generating solar system body ephemerides.
; DESCRIPTION:
;  Interactive front-end for EPHEM.  Given object code (format described
;    in EPHEM), this will compute positions and give local circumstances
;    of the object.  Reads the current time from the system clock which
;    may or may not be accurate.
;
;  All output is printed on the screen.
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  TIME    - optional time [y,m,d,h,m,s] for positions
;  AMCRIT  - Critical airmass for rise/set times.
;  OBSFILE - Override on file name where observatory codes are to be found.
;  OBSCODE - Integer Marsden code of the observatory, default 688 - Anderson Mesa
;  APO     - Flag, if set, enables a special output mode for Apache Pt. Obs.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This procedure requires an external ephemeris computation engine that
;    is accessed through pipe I/O.  It probably won't work away from Lowell
;    Observatory unless you provide a compatible engine.
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/04/11 - Initial version written by Marc W. Buie, Lowell Observatory.
;  95/05/03 - Added TIME keyword
;  95/09/10 - Fixed bug that causes program to crash for objects that never
;             rise or set
;  96/12/27 - MWB, added interactive 'T' time set option.
;  97/03/05 - MWB, added FILE and OBSCODE keywords
;  2001/03/22, David Tucker, changed obsfile support
;  2001/04/18, MWB, changed systime call.
;  2001/04/20, MWB, changed to support new geteph/ephem
;  2002/09/09, MWB, added support for string obscode values
;  2010/04/29, MWB, added APO keyword
;-
pro sspos,TIME=time,AMCRIT=amcrit,OBSFILE=obsfile,OBSCODE=obscode,DEBUG=debug, $
          ATWILIGHT=atwilight,NTWILIGHT=ntwilight,CTWILIGHT=ctwilight, $
          STWILIGHT=stwilight,APO=apo

   if badpar(amcrit,[0,2,3,4,5],0,caller='SSPOS: (amcrit) ',default=4.0) then return
   if badpar(obsfile,[0,7],0,CALLER='SSPOS: (OBSFILE) ', $
                  DEFAULT='obscode.dat') then return
   if badpar(obscode,[0,1,2,3,7],0,caller='SSPOS: (obscode) ', $
                default=688,type=codetype) then return
   if badpar(atwilight,[0,2,3],0,caller='SSPOS: (ATWILIGHT) ',default=0) then return
   if badpar(ntwilight,[0,2,3],0,caller='SSPOS: (NTWILIGHT) ',default=0) then return
   if badpar(ctwilight,[0,2,3],0,caller='SSPOS: (CTWILIGHT) ',default=0) then return
   if badpar(stwilight,[0,2,3],0,caller='SSPOS: (STWILIGHT) ',default=0) then return
   if badpar(apo,[0,1,2,3],0,caller='SSPOS: (APO) ',default=0) then return

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   check = atwilight+ntwilight+ctwilight+stwilight
   if check eq 0 then begin
      ntwilight=1
   endif else if check gt 1 then begin
      atwilight=0
      ntwilight=1
      ctwilight=0
      stwilight=0
   endif

   if atwilight eq 1 then begin
      suncrital = -18.0
      scritstr='A. twil  '
   endif
   if ntwilight eq 1 then begin
      suncrital = -12.0
      scritstr='N. twil  '
   endif
   if ctwilight eq 1 then begin
      suncrital =  -6.0
      scritstr='C. twil  '
   endif
   if stwilight eq 1 then begin
      suncrital =  -0.8
      scritstr='Sun Hrz  '
   endif

   ; Try to load coordinates, if failed, set to Lowell
   rdobscod,code,alllon,rhosinp,rhocosp,obsname,valid,FILE=obsfile
   if valid then begin
      idx=where(obscode eq code,count)
      idx=idx[0]
      if (count eq 1) then begin
         lon = (360.0-alllon[idx])/180.0*!pi
         lat = atan(rhocosp[idx],rhosinp[idx])
         name=strtrim(obsname[idx],2)
      endif else begin
         obscode = '688'
         name=''
      endelse
   endif else begin
      print,'Observatory code file ',obsfile,' not found.'
      obscode = '688'
      name=''
   endelse

   ; Hardcoded position for 42" to get the program running on failure.
   IF obscode eq '688' and name eq '' THEN BEGIN
      ; This is the GPS position for the 42", derived 1993 Sep 08
      lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
      lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
      name= 'Lowell Observatory - Anderson Mesa Station'
   ENDIF
   print,name

   ; Set the critical altitude and airmass for observability.
   crital = 0.5*!pi - acos(1.0/amcrit)

   ; Start up Larry's ephemeris generator (this assumes it's in the path).
   spawn,'geteph',unit=pipe

   ; Start up Larry's name and info fetch program
   spawn,'getinfo',unit=pipe2

   obid=''
   eph = dblarr(8,3)
   orbinf = dblarr(17)

   autotime = not keyword_set(time)
   if not autotime then begin
      jdcnv,time[0],time[1],time[2],time[3]+time[4]/60.0+time[5]/3600.0,curjd
   endif

   while obid ne 'end' do begin
      in_obid=''
      read,prompt='Enter object id (default='+obid+'): ',in_obid,format='(a)'
      in_obid = strupcase(in_obid)

      if strmid(in_obid,0,2) eq 'EN' or $
         strmid(in_obid,0,2) eq 'EX' or strmid(in_obid,0,1) eq 'Q' then begin
         obid='end'
         goto,skip
      endif

      if strmid(in_obid,0,1) eq 'T' then begin
         in_obid = strtrim(strmid(in_obid,1,99),2)
         if in_obid ne '' then begin
            ; try breaking by commas first.
            field=strsplit(in_obid,',',/extract)
            ; if only one field, try blanks
            if n_elements(field) eq 1 then begin
               field=strsplit(in_obid,/extract)
            endif
            val=fltarr(6)
            for i=0,n_elements(field)-1 do begin
               val[i]=float(field[i])
            endfor
            print,val
            autotime=0
            jdcnv,val[0],val[1],val[2],val[3]+val[4]/60.0+val[5]/3600.0,curjd
         endif else begin
            print,' auto time'
            autotime=1
         endelse
         goto,skip
      endif else if strmid(in_obid,0,1) eq 'O' and obid ne '' then begin
         showelem,obid,curjd,DEBUG=debug
         goto,skip
      endif else $
      if in_obid ne '' and strmid(in_obid,0,1) ne 'A' and $
                           strmid(in_obid,0,1) ne 'C' and $
                           strmid(in_obid,0,1) ne 'E' and $
                           strmid(in_obid,0,1) ne 'V' and $
                           strmid(in_obid,0,1) ne 'P' then begin
         print,'Illegal object code, must start with A, C, or P.'
         goto,skip
      endif

      if in_obid ne '' then obid = in_obid

      if obid eq '' then goto,skip

      if keyword_set(debug) then begin
         print,'getinfo: [',obid,']'
      endif
      printf,pipe2,obid
      obname=''
      readf,pipe2,obname,h,g,format='(a32,1x,f6.2,1x,f5.2)'

      if autotime then $
         curjd   = systime(/julian,/utc)
      prevhr = double(floor(curjd * 24.0d0))/24.0d0
      nexthr = prevhr + 4.0d0/24.0d0

      jdstr,curjd,0,curtimestr
      jdstr,prevhr,0,timestr1
      jdstr,nexthr,0,timestr2

      printf,pipe,curjd, obscode,22+50,obid,format='(f13.5,1x,a3,1x,i2,1x,a)'
      printf,pipe,prevhr,obscode,22+50,obid,format='(f13.5,1x,a3,1x,i2,1x,a)'
      printf,pipe,nexthr,obscode,22+50,obid,format='(f13.5,1x,a3,1x,i2,1x,a)'
      printf,pipe,curjd, obscode,11,   obid,format='(f13.5,1x,a3,1x,i2,1x,a)'

      if keyword_set(debug) then begin
         print,curjd, obscode,22+50,obid,format='(f13.5,1x,a3,1x,i2,1x,a)'
         print,prevhr,obscode,22+50,obid,format='(f13.5,1x,a3,1x,i2,1x,a)'
         print,nexthr,obscode,22+50,obid,format='(f13.5,1x,a3,1x,i2,1x,a)'
         print,curjd, obscode,11,   obid,format='(f13.5,1x,a3,1x,i2,1x,a)'
      endif

      readf,pipe,eph
      readf,pipe,orbinf

      goodobj = eph[0,0] ge -90.0

      if goodobj then begin

         ssgeom,eph,sun,earth,phang,elong
         moonpos,curjd,moonra,moondec
         sunpos,curjd,sunra,sundec
         sunra  = sunra/!radeg
         sundec = sundec/!radeg
         moonra  = moonra/!radeg
         moondec = moondec/!radeg
         ra  = [eph[0,0],sunra,moonra]
         dec = [eph[1,0],sundec,moondec]
         am  = airmass(replicate(curjd,3),ra,dec,lat,lon, $
                          alt=alt,lha=lha,lst=lst)
         if lha[0] lt 0.0 then ha_sign='E' else ha_sign='W'
         lha[0] = abs(lha[0])
         alt_d   = alt*180.0d0/!dpi
         decrate = (eph[1,2] - eph[1,1])*180.0d0/!dpi*3600.0/4.0d0
         rarate  = (eph[0,2] - eph[0,1])*180.0d0/!dpi*3600.0*cos(eph[1,2])/4.0d0
         melong=sphdist(eph[0,0],eph[1,0],moonra,moondec)*180.0d0/!dpi
         hatojd,!dpi,sunra,lst[0],curjd,jdlclmid
         lsidtim,jdlclmid,lon,midlst
         jdstr,jdlclmid,-2,lclmidstr
         hatojd,0.0d0,eph[0,0],midlst,jdlclmid,jdtrans
         jdstr,jdtrans,-2,objtransstr
         transam = airmass(jdtrans,ra[0],dec[0],lat,lon,alt=transalt)
         rastr,lst,0,lststr,cc

         if h gt -90 then $
            disphase,0.,sun[0],earth[0],phang[0],g,hmag

         altoha,crital,eph[1,0],lat,horzha,type
         if type eq 0 then begin
            jdrise  = jdtrans - horzha/2.0d0/!dpi
            jdset   = jdtrans + horzha/2.0d0/!dpi
            jdstr,jdrise,-2,objrisestr
            jdstr,jdset,-2,objsetstr
         endif

         altoha,suncrital/!radeg,sundec,lat,sunhorzha,suntype
         jdsset  = jdlclmid - (!dpi-sunhorzha)/2.0d0/!dpi
         jdsrise = jdlclmid + (!dpi-sunhorzha)/2.0d0/!dpi
         jdstr,jdsset,-2,sunsetstr
         jdstr,jdsrise,-2,sunrisestr

         ssetam  = airmass(jdsset,ra[0],dec[0],lat,lon,alt=ssetalt)
         sriseam = airmass(jdsrise,ra[0],dec[0],lat,lon,alt=srisealt)
         rastr,lha[0],-2,lhastr,cc

         if ssetalt lt crital then $
           ssetobjstr = '   Object down' $
         else $
           ssetobjstr = '   (Airmass='+string(ssetam,format='(f4.2)')+ $
                        ', Altitude='+string(ssetalt*!radeg,format='(f4.1)')+')'

         if srisealt lt crital then $
           sriseobjstr = '   Object down' $
         else $
           sriseobjstr = '   (Airmass='+string(sriseam,format='(f4.2)')+ $
                       ', Altitude='+string(srisealt*!radeg,format='(f4.1)')+')'

         if alt_d[1] lt -18.0 then begin
            sunstr = 'Sun is down      '
         endif else if alt_d[1] lt -12.0 then begin
            sunstr = 'Ast. twilight    '
         endif else if alt_d[1] lt -6.0 then begin
            sunstr = 'Nautical twilight'
         endif else if alt_d[1] lt -0.8 then begin
            sunstr = 'Civil twilight   '
         endif else begin
            sunstr = 'Sun is up        '
         endelse

         if alt_d[2] lt 0 then begin
            moonstr = 'Moon is down     '
         endif else begin
            moonstr = 'Moon is up       '
         endelse

         rastr,eph[0,0],4,ras,cc
         decstr,eph[1,0],3,decs
         rastr,eph[0,1],1,ras1,cc
         decstr,eph[1,1],0,decs1
         rastr,eph[0,2],1,ras2,cc
         decstr,eph[1,2],0,decs2

         print,'------------------------------------------------------------'
         print
         if strmid(obname,0,5) eq 'XXXXX' then $
            print,'Solar system object ',obid,'  ephemeris and local circumstances' $
         else if h lt -90 then $
            print,obname,format='(2x,a)' $
         else begin
            print,obname,'  Hv=',h,' Gv=',g, $
               format='(2x,a,1x,a,1x,f6.2,1x,a,1x,f5.2)'
            jdstr,orbinf[14],100,lobstr
            as='"'
            tmpstr=string(orbinf[12],as,lobstr, $
               format='("perr=",f10.1,a,"__",a)')
            tmpstr=repstr(strcompress(tmpstr,/remove_all),'_',' ')
            print,tmpstr,format='(a58)'
            tmpstr=string(orbinf[15]/365.25,fix(orbinf[16]), $
               format='("arc=",f8.2,"y,_nobs=",i8)')
            tmpstr=repstr(strcompress(tmpstr,/remove_all),'_',' ')
            print,tmpstr,format='(a58)'
         endelse

         print
         print,'  UT ',curtimestr,'  LST ',lststr[0]
         print,'  RA ',ras,'  Dec ',decs,'  J2000'
         print

         print,'   ',scritstr,sunsetstr,ssetobjstr
         if type eq 0 then begin
            if jdrise gt jdsset and jdrise lt jdsrise then $
               print,'    Rise    ',objrisestr, $
                     '   (Airmass=',string(amcrit,format='(f4.2)'), $
                     ', Altitude=',string(crital*!radeg,format='(f4.1)'),')'
         endif else if type eq 1 then begin
            print,'   Object never sets'
         endif else if type eq -1 then begin
            print,'   Object never rises'
         endif

         if jdtrans gt jdsset and jdtrans lt jdsrise then $
            print,'   Transit  ',objtransstr, $
                  '   (Airmass=',string(transam,format='(f4.2)'), $
                  ', Altitude=',string(transalt*!radeg,format='(f4.1)'),')'

         if type eq 0 then begin
            if jdset gt jdsset and jdset lt jdsrise then $
               print,'     Set    ',objsetstr, $
                     '   (Airmass=',string(amcrit,format='(f4.2)'), $
                     ', Altitude=',string(crital*!radeg,format='(f4.1)'),')'
         endif

         print,'   ',scritstr,sunrisestr,sriseobjstr
         print
         if h gt -90 then $
            print,'   Object to Sun    ',sun[0],  ' AU ', $
                  '   Apparent V mag',h-hmag, $
                  format='(a,f7.3,a,a,f8.1)' $
         else $
            print,'   Object to Sun    ',sun[0],  ' AU ', $
                  format='(a,f7.3,a)'
         if apo then begin
            print,'   Earth to Object  ',earth[0],' AU ', $
                  '   RA rate  ',rarate/3600.0/3600.0,  ' deg/sec', $
                  format='(a,f7.3,a,a,f12.9,a)'
            print,'   Phase angle      ',phang[0],'  deg', $
                  '   Dec rate ',decrate/3600.0/3600.0, ' deg/sec', $
                  format='(a,f6.2,a,a,f12.9,a)'
         endif else begin
            print,'   Earth to Object  ',earth[0],' AU ', $
                  '   RA rate      ',rarate,  ' "/hr', $
                  format='(a,f7.3,a,a,f9.1,a)'
            print,'   Phase angle      ',phang[0],'  deg', $
                  '   Dec rate     ',decrate, ' "/hr', $
                  format='(a,f6.2,a,a,f9.1,a)'
         endelse

         print
         print,sunstr,'(',alt_d[1],')','Solar elongation ',elong[0],' deg', $
               format='(3x,a,2x,a,f6.1,a,4x,a,f5.1,a)'
         print,moonstr,'(',alt_d[2],')','Lunar elongation ',melong, ' deg', $
               format='(3x,a,2x,a,f6.1,a,4x,a,f5.1,a)'
         print
         if alt_d[0] lt 1.0 then $
            print,'   Object ',abs(alt_d[0]),' degrees BELOW horizon', $
                  format='(a,f5.1,a)' $
         else $
            print,'   Airmass ',am[0],'   Altitude',alt_d[0], $
                  '   Hour Angle ',lhastr,ha_sign, $
                  format='(a,f7.3,a,f6.1,a,a,a)'
         print
         if apo then begin
            print,'tcc track ',eph[0,0]*180.0d0/!dpi, $
                               eph[1,0]*180.0d0/!dpi, $
                               rarate/3600.0/3600.0, $
                               decrate/3600.0/3600.0,obid, $
               format='(a,f10.6,", ",f10.6,", ",f13.10,", ",f13.10,' + $
                      '" Fk5=2000.0 /Rotation=90.0 /Rottype=Horizon /Name=",a)'
         endif else begin
            print,'  ',timestr1,'  RA ',ras1,'  Dec ',decs1,'  J2000'
            print,'  ',timestr2,'  RA ',ras2,'  Dec ',decs2,'  J2000'
            print
         endelse
         print,'------------------------------------------------------------'
      endif else begin
         print,'Object id code ',obid,' was not found in the database.'
      endelse
skip:
   endwhile

   ; Close the pipe
;   close,pipe
;   close,pipe2
   free_lun,pipe
   free_lun,pipe2
end

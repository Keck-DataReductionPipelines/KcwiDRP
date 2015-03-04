;+
; NAME:
;  kboplan
; PURPOSE:
;  KBO observing planning table generation
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  kboplan,objcode,obs,date
; INPUTS:
;  objcode - String array of standard object codes (see ephem.pro), if the
;              input is a single string with @ as the first character, then
;              the rest of the string is interpreted as a file to read the
;              object codes from.
;  obs     - Integer Marsden code of the observatory
;               688 - Lowell Observatory
;               500 - Geocentric
;               If you provide an invalid code, 688 is assumed.
;  date    - UT date and time vector near midnight for night,
;                 [year,month,day,hour]
;     
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
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
;  Written by Marc W. Buie, Lowell Observatory, 1997/05/23
;-
PRO kboplan,in_objcode,obs,date,OBSFILE=obsfile,OUTPUT=output

   if n_params() eq 0 then begin
      print,'kboplan,objcode,obs,date'
      return
   endif

   if badpar(in_objcode,7,[0,1],CALLER='KBOPLAN: (objcode) ') then return
   if badpar(obs,[2,3],0,CALLER='KBOPLAN: (obs) ') then return
   if badpar(date,[2,3,4,5],[0,1],CALLER='KBOPLAN: (date) ', $
                npts=datelen) then return
   if badpar(obsfile,[0,7],0,CALLER='KBOPLAN: (OBSFILE) ', $
                default='obscode.dat') then return
   if badpar(output,[0,7],0,CALLER='KBOPLAN: (OUTPUT) ', $
                default='<screen>') then return

   ; Convert dates to JD.
   if datelen eq 4 then begin
      jdcnv,date[0],date[1],date[2],date[3],jd
   endif else begin
      print,'KBOPLAN: length of date must be 4'
      return
   endelse

   rdobscod,code,alllon,rhosinp,rhocosp,obsname,valid,FILE=obsfile
   if not valid then begin
      print,'Observatory information file ',obsfile,' not found.'
      return
   endif

   if n_elements(in_objcode) eq 1 then begin
      if strmid(in_objcode,0,1) eq '@' then begin
         codnam=strmid(in_objcode,1,99)
         if not exists(codnam) then begin
            print,'Object file name: ',codnam,' does not exist. Aborting.'
            return
         endif
         rdmatch,codnam,objcode,objnames
      endif else begin
         objcode=[in_objcode]
         objnames=objcode
      endelse
   endif else begin
      objcode = in_objcode
      objnames=objcode
   endelse

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

   if output eq '<screen>' then begin
      print,'Observatory ',name,' selected.'
   endif else begin
      openw,lun,output,/get_lun
      printf,lun,'Observatory ',name,' selected.'
   endelse

   ; Set the critical altitude and airmass for observability.
   if keyword_set(amlimit) then $
      amcrit=amlimit $
   else $
      amcrit=3.0
   crital = 0.5*!pi - acos(1.0/amcrit)

   ; Compute ephemeris for object
   nobj=n_elements(objcode)
   ephem,replicate(jd,nobj),obs,23,objcode,eph
   ssgeom,eph[0:7,*],sun,earth,phang,elong
   ;idx=sort(sun)
   idx=sort(eph[0,*])

   ; Sun position at input JD
   sunpos,jd,sunra,sundec,/radian
   moonpos,jd,moonra,moondec,/radian

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
   jdstr,jdlclmid,-12,jdlclmidstr

   header = 'Object           '+ $
            ' Rise/Set    '+ $
            ' RA    Dec   '+ $
            ' perr   '+ $
            ' V    '+ $
            'Tran   X   '+ $
            '  r    d    ph   sel mel'

   if output eq '<screen>' then begin
      print,''
      print,'Night centered on UT: ',jdlclmidstr
      print,''
      print,'Sunset/NT  ',jdssetstr,' ',jdntsetstr
      print,'NT/Sunrise ',jdntrisestr,' ',jdsrisestr
      print,''
      print,header
   endif else begin
      printf,lun,''
      printf,lun,'Night centered on UT: ',jdlclmidstr
      printf,lun,''
      printf,lun,'Sunset/NT  ',jdssetstr,' ',jdntsetstr
      printf,lun,'NT/Sunrise ',jdntrisestr,' ',jdsrisestr
      printf,lun,''
      printf,lun,header
   endelse

   spawn,'getinfo',unit=pipe

   FOR j=0,nobj-1 DO BEGIN
      i = idx[j]
      printf,pipe,objcode[i]
      obname=''
      readf,pipe,obname,h,g,format='(a32,1x,f6.2,1x,f5.2)'
      if strmid(obname,0,5) eq 'XXXXX' then begin
         obname = objcode[i]
         h=40
         g=0.2
      endif
      disphase,0.,sun[i],earth[i],phang[i],g,hmag

      melong=sphdist(eph[0,i],eph[1,i],moonra,moondec)*180.0d0/!dpi

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
;         jdstr,jdrise,-12,objrisestr
;         jdstr,jdset,-12,objsetstr
      endif
      obswind,midlst,lat,eph[0,i],eph[1,i],jdntrise,jdntset,rtime,rkind, $
         stime,skind,objtype

      epherr = eph[20,i]
      if epherr gt 1.0e10 then epherr = -1.0
;          'f4.1,1x,f5.1,2x,f6.1,2x,f5.1,2x,'+ $
;          'f4.1,1x,f5.1,2x,e13.5,2x,f5.1,2x,'+ $

      fmt='(a16,1x,a,1x,a,2x,' + $
          'f4.1,1x,f5.1,2x,f6.1,2x,f5.1,2x,'+ $
          'a,1x,f3.1,2x,'+ $
          'f4.1,1x,f4.1,1x,f5.1,2x,i3,1x,i3)'

      if objtype ne -1 and objtype ne 2 then begin
         jdstr,rtime,-12,risestr
         jdstr,stime,-12,setstr
         if output eq '<screen>' then begin
            print,obname,strmid(risestr,12,5),strmid(setstr,12,5), $
               eph[0,i]*!radeg/15.0,eph[1,i]*!radeg,epherr, $
               h-hmag, $
               strmid(objtransstr,12,5),transam, $
               sun[i],earth[i],phang[i], $
               fix(ceil(elong[i])), $
               fix(ceil(melong)), $
               format=fmt
         endif else begin
            printf,lun,obname,strmid(risestr,12,5),strmid(setstr,12,5), $
               eph[0,i]*!radeg/15.0,eph[1,i]*!radeg,epherr, $
               h-hmag, $
               strmid(objtransstr,12,5),transam, $
               sun[i],earth[i],phang[i], $
               fix(ceil(elong[i])), $
               fix(ceil(melong)), $
               format=fmt
         endelse
      endif else begin
         if output eq '<screen>' then begin
            print,strmid(obname,0,16),' object not up at night.'
         endif else begin
            printf,lun,strmid(obname,0,16),' object not up at night.'
         endelse
      endelse

   ENDFOR

   free_lun,pipe

   if output ne '<screen>' then begin
      free_lun,lun
   endif

END

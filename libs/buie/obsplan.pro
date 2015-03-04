;+
; NAME: 
;  obsplan
; PURPOSE:
;  Generate a graphical summary of object location(s) for a given night.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  obsplan,objcode,obs,date
; INPUTS:
;  objcode - String array of standard object codes (see ephem.pro)
;              -or-
;            String containing file name of the list of objects but you
;              must preceed the file name with @ to mark it as a file
;              and not the name of an object.
;            If supplied as a file, this file is a correspondence list
;              of the type read by rdmatch.  Column 1 contains the standard
;              object code and column 2 has a full or common name.  The
;              columns are separated by a <tab> character and the common name
;              column MUST begin with a single quote (').  See RDMATCH
;              for more details about this file format.
;  obs     - Integer Marsden code of the observatory
;               688 - Lowell Observatory
;               500 - Geocentric
;               If you provide an invalid code, 688 is assumed.
;  date    - UT date and time vector near midnight,
;                 [year,month,day,hour,minutes,seconds]
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ALLDAY  - Flag, if set generates a plot for a full 24 hour period.
;  AMLIMIT - Cutoff airmass for plot, default = 2.6
;  OBSFILE - Override on file name where observatory codes are to be found.
;  STARCAT - Override on file name where star catalog is to be found (see
;              coord for default).
; OUTPUTS:
;  Generates a plot to the current graphics device.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/10/07, Initial version written by Marc W. Buie, Lowell Observatory
;  96/02/08, MWB, added parameter validation, airmass limit option,
;                    and file option on objcode
;  96/02/15, MWB, added support for star lookup and plotting.  Also, now
;                    computes object position throughout night instead of
;                    just at midnight.
;  96/03/19, MWB, allow JD for input date.
;  96/10/18, MWB, fixed bug that ignored h,m,s in input date.
;  96/11/1,  MWB, Added ALLDAY keyword
;  98/09/28, MWB, Fixed a coordinate interpolation bug.
;  99/01/27, MWB, added support for stars.
;  99/02/23, MWB, fixed some lingering problems.
;  2001/03/16, David Tucker, changed rdobscod default file call.
;  2002/12/04, MWB, added support for strings in obs input.
;-
pro obsplan,in_objcode,obs,in_date,OBSFILE=obsfile,AMLIMIT=amlimit, $
       STARCAT=starcat,ALLDAY=allday

   if n_params() eq 0 then begin
      print,'obsplan,objcode,obs,date,[FILE=]'
      return
   endif

   if badpar(in_objcode,7,[0,1],CALLER='OBSPLAN: (objcode) ') then return
   if badpar(obs,[2,3],0,CALLER='OBSPLAN: (obs) ',type=codetype) then return
   if badpar(in_date,[2,3,4,5],[0,1],CALLER='OBSPLAN: (date) ', $
                npts=datelen) then return
   if badpar(amlimit,[0,2,3,4,5],0,CALLER='OBSPLAN: (AMLIMIT) ', $
                default=2.6) then return
   if badpar(allday,[0,2,3],0,CALLER='OBSPLAN: (ALLDAY) ',default=0) then return
   if badpar(obsfile,[0,7],0,CALLER='OBSPLAN: (OBSFILE) ', $
		default='obscode.dat') then return

   if codetype ne 7 then begin
      obs = strn(obs,length=3,padchar='0')
   endif else begin
      obs = strupcase(obs)
   endelse

   ; Convert date to JD.
   if datelen eq 4 then begin
      jdcnv,in_date[0],in_date[1],in_date[2],in_date[3],jd
   endif else if datelen eq 6 then begin
      jdcnv,in_date[0],in_date[1],in_date[2], $
               in_date[3]+in_date[4]/60.0+in_date[5]/3600.0,jd
   endif else if datelen eq 1 then begin
      jd = in_date
   endif else begin
      print,'OBSPLAN: length of date must be either 1 or 4 or 6'
      return
   endelse

   rdobscod,code,alllon,rhosinp,rhocosp,obsname,valid,FILE=obsfile
   if not valid then begin
      print,'Observatory code file ',obsfile,' not found.'
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

   objcode=[objcode,'p301']
   objnames=[objnames,'Moon']

   ; Sun position at input JD
   sunpos,jd,sunra,sundec,/RADIAN

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

   ; JD range for plot
   if allday then begin
      jd1 = jdofmid
      jd2 = jd1 + 1.0
   endif else begin
      jd1 = jdsset
      jd2 = jdsrise
   endelse

   ; Vector of JDs for night to compute ephemeris for
   njd=5
   jdeph = jd1 + (jd2-jd1)*dindgen(njd)/(njd-1)
   timeeph = (jdeph - jdofmid)*24.0

   jd = dblarr(njd*n_elements(objcode)) 
   obj = strarr(njd*n_elements(objcode))
   for i=0,n_elements(objcode)-1 do begin
      obj[i*njd:(i+1)*njd-1] = objcode[i]
      jd[i*njd:(i+1)*njd-1]  = jdeph
   endfor
   coord,jd,obs,obj,'[[STANDARD]]',pra,pdec,/OFDATE,FILE=starcat

   ; Width of plot in hours
   tspan = (jd2-jd1)*24.0d0

   ; Time vector for night
   ntp=160
   time = float(jd1-jdofmid)*24.0 + findgen(ntp)/(ntp-1.0)*tspan
   jdn  = jdofmid + time/24.0d0

   ; Set range for plot and draw box
   yr=[amlimit,1.0]
   plot,[0],[1],/nodata,xr=minmax(time),yr=yr, $
      xtit='UTC (hours)',ytit='Airmass', $
      tit='Object visibility for '+thisdate+' UTC', $
      xstyle=3,ystyle=3

   ; Plot boundaries for different twilights
   oplot,replicate(float(jdatset-jdofmid)*24.0,2),yr,linestyle=1
   oplot,replicate(float(jdatrise-jdofmid)*24.0,2),yr,linestyle=1
   oplot,replicate(float(jdntset-jdofmid)*24.0,2),yr,linestyle=1
   oplot,replicate(float(jdntrise-jdofmid)*24.0,2),yr,linestyle=1
   oplot,replicate(float(jdctset-jdofmid)*24.0,2),yr,linestyle=1
   oplot,replicate(float(jdctrise-jdofmid)*24.0,2),yr,linestyle=1

   ; Lable twilight boundaries
   xyouts,float(jdctset-jdofmid)*24.0,yr[1],'C',align=0.5
   xyouts,float(jdntset-jdofmid)*24.0,yr[1],'N',align=0.5
   xyouts,float(jdatset-jdofmid)*24.0,yr[1],'A',align=0.5

   xyouts,float(jdctrise-jdofmid)*24.0,yr[1],'C',align=0.5
   xyouts,float(jdntrise-jdofmid)*24.0,yr[1],'N',align=0.5
   xyouts,float(jdatrise-jdofmid)*24.0,yr[1],'A',align=0.5

   ; text size for curve labels
   if !d.name eq 'PS' then cs = 0.5 else cs = 1.0

   ; Generate curves for all objects
   for i=0,n_elements(objcode)-1 do begin

      ephra  = pra[i*njd:(i+1)*njd-1]
      ephdec = pdec[i*njd:(i+1)*njd-1]

      ; Make sure all positions are in the same "quandrant", that is,
      ;  interpolation will not work if the RA crosses 0.  So, take the
      ;  middle position and make sure all the others are with PI of that.
      ;  This should work as long as the object doesn't move more than 12h
      ;  of RA in one night.
      radiff=ephra-ephra[njd/2]
      z=where(radiff gt !dpi,count)
      if count ne 0 then ephra[z]=ephra[z]-2.0*!dpi
      z=where(radiff lt -1.0*!dpi,count)
      if count ne 0 then ephra[z]=ephra[z]+2.0*!dpi

      interp,timeeph,ephra,time,ra
      interp,timeeph,ephdec,time,dec
      am = airmass(jdn,ra,dec,lat,lon,alt=alt,lha=lha,lst=lst)
      oplot,time,am,linestyle=i mod 6,max_value=amlimit
      top = where(am eq min(am))
      xyouts,time[top[0]],am[top[0]]+0.02,objnames[i],align=0.0, $
         orient=270.0,chars=cs

   endfor

   xyouts,0.05,0.02,name,align=0.0,/normal

end

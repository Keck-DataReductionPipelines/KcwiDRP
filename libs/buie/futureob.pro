;+
; NAME: 
;  futureob
; PURPOSE: 
;  Plot geometric circumstances for a solar system object for some years.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  futureob,start,length,obs,object,FULLNAME=fullname
; INPUTS:
;  start  - Start time for plotting window, (year,month,day,hour)
;  length - Length of time to plot, in years.
;  obs    - Observatory code, Marsden's IAUC codes (scalar).
;  object - Object code (see description in EPHEM.PRO).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FULLNAME - Full name or designation for the object (default=object)
;  DT - step size in time (days) between computed points in plot (default=7)
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  1995/02/23 - Written by Marc W. Buie, Lowell Observatory
;  1996/01/23 - MWB, Added getinfo call that gets Name, H, G
;-
pro futureob,start,length,obs,object,FULLNAME=fullname,DT=dt

   if n_params() eq 0 then begin
      print,'futureob,start,length,obs,object,[FULLNAME=, DT=]'
      return
   endif

   if badpar(fullname,[0,7],0,CALLER='FUTUREOB: (fullname) ',DEFAULT='DEFOBJ') then return
   if badpar(dt,[0,2,3,4,5],0,CALLER='FUTUREOB: (dt) ',DEFAULT=7.0d0) then return

   if n_elements(start) ne 4 then begin
      print,'FUTUREOB: ERROR!  Start time vector must have 4 elements [y,m,d,h]'
      return
   endif

   jdcnv,start[0],start[1],start[2],start[3],jd
   jd=jd+dindgen(365.0/dt*length)*dt

   if length le 3 then xminor=12 else xminor=0

   spawn,'getinfo',unit=pipe
   printf,pipe,object
   obname=''
   readf,pipe,obname,hv,gv,format='(a32,1x,f6.2,1x,f5.2)'
   free_lun,pipe

   ephem,jd,obs,20+50,object,eph
   ssgeom,eph,r,d,p,e
   jd2year,jd,yr
   if obname ne 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' then begin
      if fullname eq 'DEFOBJ' then fullname=obname
   endif else begin
      if fullname eq 'DEFOBJ' then fullname=object
   endelse
   if gv gt -9 then begin
      disphase,0.,r,d,p,gv,hmag
      dm = hv-hmag
      ytitle='Apparent V mag'
   endif else begin
      dm=5*alog10(r*d)
      ytitle='Mag (1,1)=0'
   endelse
   cs=2.
   kmscale = 1.0/(1.0/(d*1.49598e8)*!radeg*3600.0)

   z=where(e gt 90, goodz)
   if goodz gt 0 then begin
      a=z[0:goodz-2]
      b=z[1:goodz-1]
      ptr=where(b-a ne 1,cntptr)
      if cntptr ne 0 then begin
         i1 = [z[0],b[ptr]]
         i2 = [a[ptr],z[goodz-1]]
      endif else begin
         i1 = z[0]
         i2 = z[goodz-1]
      endelse
   endif

   setwin,0
   !p.multi=[0,1,3]
   if !d.name eq 'X' then begin
      loadct,39,/silent
      th = 4
      color = 100
   endif else begin
      th = 8
      color = 0
   endelse
   plot,yr,dm,chars=cs,yr=maxmin(dm),ytitle=ytitle, $
      title='Observing prospects for '+fullname,xminor=xminor
   if goodz gt 0 then $
      for i=0,n_elements(i1)-1 do $
         oplot,yr[i1[i]:i2[i]],dm[i1[i]:i2[i]],color=color,thick=th
   plot,yr,e,chars=cs,ytitle='Solar elongation',xminor=xminor,yr=[0,180]
   if goodz gt 0 then $
      for i=0,n_elements(i1)-1 do $
         oplot,yr[i1[i]:i2[i]],e[i1[i]:i2[i]],color=color,thick=th
   plot,yr,p,chars=cs,ytitle='Phase angle',xminor=xminor,yr=[0,180]
   if goodz gt 0 then $
      for i=0,n_elements(i1)-1 do $
         oplot,yr[i1[i]:i2[i]],p[i1[i]:i2[i]],color=color,thick=th

   setwin,1
   !p.multi=[0,1,3]
   plot,yr,r,chars=cs,ytitle='Heliocentric distance (AU)',xminor=xminor
   if goodz gt 0 then $
      for i=0,n_elements(i1)-1 do $
         oplot,yr[i1[i]:i2[i]],r[i1[i]:i2[i]],color=color,thick=th
   z = where(r eq min(r))
   jdstr,jd[z],0,str
   xyouts,0.2,0.9,'Min at '+str+' is '+string(min(r),format='(f7.4)')+' AU',/normal

   plot,yr,d,chars=cs,ytitle='Geocentric distance (AU)',xminor=xminor
   if goodz gt 0 then $
      for i=0,n_elements(i1)-1 do $
         oplot,yr[i1[i]:i2[i]],d[i1[i]:i2[i]],color=color,thick=th
   z = where(d eq min(d))
   jdstr,jd[z],0,str
   xyouts,0.2,0.6,'Min at '+str+' is '+string(min(d),format='(f7.4)')+' AU',/normal

   plot,yr,kmscale,chars=cs,ytitle='km per arcsec',xminor=xminor
   if goodz gt 0 then $
      for i=0,n_elements(i1)-1 do $
         oplot,yr[i1[i]:i2[i]],kmscale[i1[i]:i2[i]],color=color,thick=th
   z = where(kmscale eq min(kmscale))
   jdstr,jd[z],0,str
   xyouts,0.2,0.3,'Min at '+str+' is '+string(min(kmscale),format='(f10.3)')+' km/arcsec',/normal

   setwin,0
   !p.multi=0

end

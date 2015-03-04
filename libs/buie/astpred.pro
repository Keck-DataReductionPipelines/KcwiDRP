;+
; NAME:
;  astpred
; PURPOSE:
;  Simple asteroid position predictor when no orbit is available.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  astpred,astfile,jdi,ra,dec
;
; INPUTS:
;  astfile - file name for set of astrometric observations for object.
;  jdi     - Time(s) to predict location for.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  QUIET   - No printed or plotted output.
;
; OUTPUTS:
;  rai     - RA of predicted location.
;  deci    - Dec of predicted location.
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
; 1998/01/08, written by Marc W. Buie, Lowell Observatory
; 2009/10/02, MWB, removed obsolete Poly_fit arguments
;-
pro astpred,astfile,jdi,rai,deci,QUIET=quiet
   line=''
   openr,lun,astfile,/get_lun
   nobs=0
   while (not(eof(lun))) do begin
      readf,lun,line,format='(a1)'
      nobs = nobs+1
   endwhile
   point_lun,lun,0

   jd=dblarr(nobs)
   ra=dblarr(nobs)
   dec=dblarr(nobs)
   jd0=0.0d0
   fn=''
   sign=''

   for i=0,nobs-1 do begin
      readf,lun,fn,jd0,h,m1,s1,sign,d,m2,s2, $
         format='(a10,1x,d13.5,1x,i2,1x,i2,1x,f7.4,1x,a1,i2,1x,i2,1x,f6.3)'
;970508.011 2450576.68281 11:43:13.541 +20:09:32.69 17.8
;970508.012 2450576.68461 11:43:13.546 +20:09:31.85 17.7
;970508.017 2450576.70027 11:43:13.580 +20:09:22.28 17.8
;970508.018 2450576.70206 11:43:13.595 +20:09:21.39 17.8
;970508.029 2450576.74839 11:43:13.652 +20:08:54.69 17.8
      if sign eq '-' then si=-1 else si=1
      hmstorad,h,m1,s1,ra0
      dmstorad,si,d,m2,s2,dec0
      jd[i]=jd0
      ra[i]=ra0
      dec[i]=dec0
   endfor
   free_lun,lun

   time = (jd - jd[0])*24.0

   cra=poly_fit(time,ra,1,yfit=rafit,sigma=craerr)
   cdec=poly_fit(time,dec,1,yfit=decfit,sigma=cdecerr)

   if not keyword_set(quiet) then begin
      setwin,1,xsize=400,ysize=400
      plot,time,ra,psym=-8
      oplot,time,rafit

      setwin,2,xsize=400,ysize=400
      plot,time,dec,psym=-8
      oplot,time,decfit
   endif

   ti = (jdi-jd[0])*24.0

   rai = cra[0] + cra[1]*ti
   deci = cdec[0] + cdec[1]*ti

   rastr,rai,1,ras
   decstr,deci,0,decs
   jdstr,jdi,0,jds

   if not keyword_set(quiet) then begin
      for i=0,n_elements(jds)-1 do begin
         print,jds[i],' ',ras[i],' ',decs[i]
      endfor

      print, sqrt( craerr[0]^2 + (craerr[1]*ti)^2 )*!radeg*3600.0
      print, sqrt( cdecerr[0]^2 + (cdecerr[1]*ti)^2 )*!radeg*3600.0
   endif

end

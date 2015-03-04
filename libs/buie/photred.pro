;+
; NAME: 
;  photred
; PURPOSE: 
;  Reduction of non-variable point source absolute photometry.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  pro photred,stand,fil,jd,am,serial,mag,err,comp,dofil, $
;           object,redmag,rederr, $
;           NOPRINT=noprint, PLOT=plotit, BINFAC=binfac, FORCE=force
; INPUTS:
;  stand  - String array of standard names.  (See coord.)
;  fil    - String array of filter names for observations.
;  jd     - Double precision array of Julian dates.
;  am     - Floating point array of the airmass of observations.
;  serial - Serial number of observation.
;  mag    - Raw instrumental magnitudes.
;  err    - Uncertainties on the raw magnitudes.
;  comp   - Standard name of comparison star.
;  dofil  - Name of filter to reduce.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ALL     - Flag, if true reduces non-stars as well and returns a single
;              average value for non-stars.
;  BINFAC  - Maximum amount of comparison star point to bin (default=6)
;  FORCE   - Two element vector that contains override values for the
;              mean extinction and its uncertainty.  This replaces the
;              initial fit for mean extinction.
;  NOPRINT - Flag, if true, will inhibit the summary printout to the screen.
;  PLOT    - Flag, if true will enable the summary plot of the comp star.
;  STDMAG  - Standard magnitude of comparison star (default=0).
; OUTPUTS:
;  object - Name(s) of program object reduced against comp star.
;  redmag - Final reduced magnitude of object compared to comp star.
;  rederr - Final uncertainty.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  1993/07/28 - Written by Marc W. Buie, Lowell Observatory
;  1993/12/09 - MWB, added extinction override (FORCE)
;  1993/12/12 - MWB, added ALL flag
;  2009/10/02, MWB, changed Polyfitw call to poly_fit equivalent
;-
pro photred,stand,fil,jd,am,serial,mag,err,comp,dofil, $
         object,redmag,rederr, $
         NOPRINT=noprint, PLOT=plotit, BINFAC=binfac, STDMAG=stdmag, $
         FORCE=force, ALL=all

   if n_params() eq 0 then begin
      print,'photred,stand,fil,jd,am,serial,mag,err,comp,dofil, $'
      print,'   object,redmag,rederr, [NOPRINT, PLOT, BINFAC, STDMAG, FORCE]'
      return
   endif

   if badpar(stand, 7,        1,caller='LTCRV: (stand) ', npts=n1) then return
   if badpar(fil,   7,        1,caller='LTCRV: (fil) ',   npts=n2) then return
   if badpar(jd,    5,        1,caller='LTCRV: (jd) ',    npts=n3) then return
   if badpar(am,    [4,5],    1,caller='LTCRV: (am) ',    npts=n4) then return
   if badpar(serial,[1,2,3],  1,caller='LTCRV: (serial) ',npts=n5) then return
   if badpar(mag,   [4,5],    1,caller='LTCRV: (mag) ',   npts=n6) then return
   if badpar(err,   [4,5],    1,caller='LTCRV: (err) ',   npts=n7) then return
   if badpar(comp  ,7,        0,caller='LTCRV: (comp) '          ) then return
   if badpar(dofil ,7,        0,caller='LTCRV: (dofil) '         ) then return
   if badpar(binfac,[0,1,2,3],0,caller='LTCRV: [BINFAC] ', default=6) then return
   if badpar(stdmag,[0,4,5],  0,caller='LTCRV: [STDMAG] ', default=0.0) then return

   alln=[n1,n2,n3,n4,n5,n6,n7]
   if min(alln) ne max(alln) then begin
      print,'PHOTRED: Error!  stand,fil,jd,am,serial,mag,err must be the same length.'
      return
   endif

   ;Compute the time part of the Julian Date.
   time = (jd - long(jd[0]-0.5d0)-0.50d0)*24.0

   ; Select out all the stars and separate them into comp, non-comp
   star=where(stand eq comp and fil eq dofil,count1)
   if keyword_set(all) then $
      prog=where(stand ne comp and fil eq dofil,count2) $
   else $
      prog=where(stand ne comp and strmid(stand,0,1) eq 'S' and fil eq dofil,count2)

   if count1 eq 0 then begin
      print,'Comparison star ',comp,' is not found.  Cannot continue.'
      return
   endif

   if count2 eq 0 then begin
      print,'No objects to reduce were found.  Quitting.'
      return
   endif

   if keyword_set( force ) then begin
      meanext    = force[0]
      meanexterr = force[1]
      strcom = '  forced'
   endif else begin
      ; Do linear extinction fit to the comparison star.
      coeff=poly_fit(am[star],mag[star],1,sigma=sigma, $
                      measure_errors=err[star],status=status)
      meanext=coeff[1]
      meanexterr=sigma[1]
      strcom = '  fitted'
   endelse

   ; Compute mean airmass of comparison star.
   meanam=mean(am[star])

   ; Correct all star measurements to the mean airmass.
   dams=meanam-am[star]
   staratmean=mag[star]+dams*meanext
   staratmeanerr=sqrt( err[star]^2 + (dams*meanexterr)^2 )

   ; Compute mean, avgdev, and stddev of corrected star.
   moment4,staratmean,meanstar,scatter,meanstarsig

   ; Set the Zero airmass magnitude
   if keyword_set( force ) then begin
      zammag = meanstar - meanext*meanam
      zamerr = sqrt((meanam*meanexterr)^2 + meanstarsig^2)
   endif else begin
      zammag=coeff[0]
      zamerr=a[0,0]
   endelse

   ; Compute chi-squared and yfit from mean fit.
   yfit = meanext*am[star] + zammag
   redchi = sqrt(total(((mag[star]-yfit)/err[star])^2)/(n_elements(star)-2))

   ; Bin the data to it's "natural" grouping.  That is, if there were 3
   ;   consequtive comp star measurments, they would be averaged into one
   ;   final point.  This will be used to derive the time-variable extinction.
   if binfac ne 1 then begin
      avger,time[star],mag[star],err[star],binfac,3,tavgstar,avgmagstar,avgmagstarerr
      avger,time[star],staratmean,staratmeanerr,binfac,3,tavgstar,avgstar,avgstarerr
      avger,time[star],am[star],err[star],binfac,3,tavgstar,avgamstar
   endif else begin ; Ignore binning if binfac is 1.
      tavgstar = time[star]
      avgmagstar=mag[star]
      avgmagstarerr=err[star]
      avgstar=staratmean
      avgstarerr=staratmeanerr
      avgamstar=am[star]
   endelse

   ; From the binned comp and the binned comp corrected to the mean airmass,
   ;   compute the effective extinction.
   extin=(avgmagstar-zammag)/avgamstar
   extinerr = sqrt( ( avgmagstarerr^2 + zamerr^2 ) / avgamstar )

   ; Interpolate the time-variable extinction to the individual star measurement
   ;   times and apply the correction to see the scatter.
   interp,tavgstar,extin,E1=extinerr,time[star],kstar,kstarerr
   finalstar = mag[star] + kstar*(0.0-am[star]) + meanext*meanam
   finalstarerr = sqrt(err[star]^2+(am[star]*kstarerr)^2)
   meanerr,finalstar,finalstarerr,finalmeanstar,finalmeanstarerr
   finalredchi = sqrt(total(((finalstar-finalmeanstar)/finalstarerr)^2)/ $
                                    (n_elements(finalstar)-2))
   finalscat = mean(abs(finalstar-finalmeanstar))

   ; Interpolate the time-variable extinction to the program object measurements
   interp,tavgstar,extin,E1=extinerr,time[prog],kprog,kprogerr
   allmag = mag[prog] + kprog*(0.0-am[prog]) - zammag + stdmag
   allerr = sqrt(err[prog]^2+(am[prog]*kprogerr)^2)

   ; Print results to the screen.
   line=strarr(14)

   line[0] = 'Reducing objects with respect to '+comp+'.  Filter - '+dofil
   line[1] = 'Extinction             = '+string(meanext,form='(f8.4)')+ $
            ' +/- '+string(meanexterr,form='(f6.4)')+strcom
   line[2] = 'Zero airmass magnitude = '+string(zammag,form='(f8.4)')+ $
            ' +/- '+string(zamerr,form='(f6.4)')
   line[3] = 'Mean magnitude         = '+string(meanstar,form='(f8.4)')+ $
            ' +/- '+string(meanstarsig,form='(f6.4)')
   line[4] = 'Mean airmass           = '+string(meanam,form='(f8.4)')
   line[5] = 'Reduced chi-squared    = '+string(redchi,form='(f8.4)')
   line[6] = 'Scatter (avgdev)       = '+string(scatter,form='(f8.4)')
   line[7] = 'Number of observations = '+string(n_elements(star),form='(i3)')
   line[9] = 'Number of sets         = '+string(n_elements(tavgstar),form='(i3)')
   line[10] = 'Corrected star mean    = '+string(finalmeanstar,form='(f8.4)')+ $
            ' +/- '+string(finalmeanstarerr,form='(f6.4)')
   line[11] = 'Reduced chi-squared    = '+string(finalredchi,form='(f8.4)')
   line[12] = 'Scatter (avgdev)       = '+string(finalscat,form='(f8.4)')

   if not keyword_set(noprint) then $
      for i=0,13 do print,line[i]

   ; Contruct a list of names that include the serial number.
   allprog = stand[prog]+':'+string(serial[prog],form='(i4.4)')

   ; Sort the list and keep only unique names.
   object = allprog[uniq(allprog,sort(allprog))]

   ; Loop through the unique list and average all the objects found.
   redmag = fltarr(n_elements(object))
   rederr = fltarr(n_elements(object))
   for i=0,n_elements(object)-1 do begin
      z = where(allprog eq object[i],count)
      meanerr,allmag[z],allerr[z],avgmag,sigm,sigd
      meanerr,jd[z],allerr[z],avgjd
      redmag[i] = avgmag
      if count eq 1 then $
         rederr[i] = sigm $
      else $
         rederr[i] = max([sigm,sigd/sqrt(n_elements(z)-1)])
      if not keyword_set(noprint) then begin
         for j=0,n_elements(z)-1 do begin
            print,allmag[z[j]],' +/- ',allerr[z[j]], $
               allmag[z[j]]-redmag[i],(allmag[z[j]]-redmag[i])/rederr[i], $
               format='(14x,f9.4,a,f6.4,1x,f7.4,1x,f7.4)'
         endfor
         print,object[i],avgjd,n_elements(z),redmag[i],' +/- ',rederr[i],sigm,sigd, $
            format='(a10,1x,f13.5,i4,f9.4,a,f6.4,15x,f6.4,1x,f6.4)'
      endif
   endfor

   ; Plotting stuff.
   if keyword_set(plotit) then begin
      pmult=!p.multi
      !p.multi=[0,2,4]

      cs=1.5
      ss=0.5

      plot,am[star],mag[star],psym=8,yr=[max(mag[star]),min(mag[star])], $
            xtit='Airmass',ytit='Inst. mag',chars=cs,syms=ss
      oplerr,am[star],mag[star],err[star],psym=3

      plot,time[star],am[star],psym=8,yr=[max(am[star]),min(am[star])], $
            xtit='UT time (hours)',ytit='Airmass',chars=cs,syms=ss

      plot,am[star],mag[star]-yfit,psym=8,xtit='Airmass',ytit='mag residuals', $
            chars=cs,syms=ss
      oplerr,am[star],mag[star]-yfit,err[star],psym=3

      plot,time[star],mag[star]-yfit,psym=8,xtit='UT time (hours)', $
            ytit='mag residuals',chars=cs,syms=ss
      oplerr,time[star],mag[star]-yfit,err[star],psym=3

      yr=[min(extin-extinerr),max(extin+extinerr)]
      plot,tavgstar,extin,psym=8,chars=cs,xtit='UT time (hours)', $
            ytit='Extinction',syms=ss,xr=minmax([time[prog],tavgstar]),yr=yr
      oplerr,tavgstar,extin,extinerr,psym=3
      setusym,-1
      oplerr,time[prog],kprog,kprogerr,psym=8,syms=ss*0.8
      setusym,1

      yr=[max(finalstar-meanstar+finalstarerr),min(finalstar-meanstar-finalstarerr)]
      plot,time[star],finalstar-meanstar,psym=8,chars=cs,yr=yr, $
            xtit='UT time (hours)',ytit='star mag residuals',syms=ss
      oplerr,time[star],finalstar-meanstar,finalstarerr,psym=3

      x=.6
      y=0.23-findgen(15)/80
      for i=0,12 do xyouts,x,y[i],line[i],/normal,chars=0.5

   endif

end

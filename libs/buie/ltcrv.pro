;+
; NAME: 
;  ltcrv
; PURPOSE: 
;  Photometric lightcurve reductions against a single comparison star.
; DESCRIPTION:
; Generates lightcurve products including entries in the PHOT.data
; table, wrphot file and plots.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  pro ltcrv,stand,fil,jd,am,serial,mag,err,object,objno,comp,dofil, $
;       jdobs,redmag,rederr 
;
; INPUTS:
;  stand  - String array of standard names.  (See coord.)
;  fil    - String array of filter names for observations.
;  jd     - Double precision array of Julian dates.
;  am     - Floating point array of the airmass of observations.
;  serial - Serial number of observation.
;  mag    - Raw instrumental magnitudes.
;  err    - Uncertainties on the raw magnitudes.
;  object - Standard name of program object to reduce against comp star.
;  objno  - Serial number of program object.  (Usually 0)
;  comp   - Standard name of comparison star.
;  dofil  - Name of filter to reduce.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BAD     - Set of flags that will mark data bad (if 1), good if 0.
;  BINFAC  - Maximum amount of comparison star point to bin (default=6)
;  COLORNAME - Name of reduction color to put in data table, eg 'B-V'. Normally
;              this will be the color corresponding to the color of the transf
;              record fetched. For a non standard filter such as 'M' there will
;              be no transf record and colorname is passed as '' or skipped.
;  CTERM   - Color term and error (default=[0,0])
;  DATABASE- string, name of photometry MYSQL database, by default 'phot'
;  DB      - Flag, if set, reduced object observations will be saved to database.
;  DVERBOSE-  Code for dbphot and other routines for db transaction verbosity.
;  FILE    - name of file to write reduced object data using wrphot. By default, 
;               no file is written.
;  FILTNAME- Proper name of filter, should be just one or two characters.
;               if not given, 1=B, 2=V, 3=R is used for the default
;  FORCE   - Two element vector that contains override values for the
;                mean extinction and its uncertainty.  This replaces the
;                initial fit for mean extinction.
;  K2      - Second order extinction coefficient and error (default=[0,0])
;  NOEDIT  - Flag, if true will inhibit editing the bad flags of the data.
;  NOFILES - Flag, if true will inhibit generation of the summary file.
;  NOPLOT  - Flag, if true will inhibit the summary plot.
;  NOPRINT - Flag, if true, will inhibit the summary printout to the screen.
;  NOSAVE  -  Flag, if true, will inhibit modification of files or databases.
;  OCOLOR  - Standard color and error for object (default=[0,0])
;  OBJNAME - Proper name of object, default = object:objno
;  PLOTWIN - Plot window to use for output plots (default = current window)
;  REFID -   String uniquely identifying observing run. This is used in 
;               updating the data table in the phot 
;               database. It must be specified if DB is used.
;  SCOLOR  - Standard color and error for star (default=[0,0])
;  STDMAG  - Standard magnitude and error for star (default=[0,0])
;  TABLE   - string, name of photometry database table, by default 'data'.
;
; OUTPUTS:
;  jdobs  - Final Julian date of reduced observation of program object.
;  redmag - Final reduced magnitude of object compared to comp star.
;  rederr - Final uncertainty.
; KEYWORD OUTPUT PARAMETERS:
;  NOBS -   Final number of observations reduced. (Note: includes object
;             observations marked bad.)
; COMMON BLOCKS:
; SIDE EFFECTS:
; Update data table in photometry data base, after removing previous 
; observations for the same object, rundate, filter and color. Observations
; marked bad are not included in the data base. (see jdobs, NOBS).
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  93/07/28 - Written by Marc W. Buie.  Patterned after a similiar program
;              of David Tholen's.
;  93/10/25, MWB, added optional transformation coefficients.
;  93/12/10 - MWB, added extinction override (FORCE)
;  96/02/21 - MWB, total rewrite, added BAD flags
; 2006/08/09 - Peter L. Collins, Lowell Observatory.
;                add db output for reduced obs parallel to wrphot and header cleanup.
; 2006/09/26, PLC, add INSTRUMENT keyword to pass through to dbphot.
; 2006/10/16 - PLC, add NOSAVE flag.
; 2006/12/07 - PLC, replace instrument and rundate keywords by REFID and
;                   change calls to dbphot.
; 2006/12/14, MWB, added PLOTWIN keyword.
; 2006/12/28, PLC, added DVERBOSE keyword.
; 2007/01/05, MWB, added NOEDIT keyword.
; 2007/02/05, PLC, fixes to deal with bad flags, and special cases with
;                  all objects or all comp obs marked bad. It is now set
;                  up so all obs placed in db have bad flags = 0.
;                  Calls to dbphot now provide color field. Dbphot scrubbing
;                  now occurs at start. Added NOBS and COLORNAME keywords.
; 2009/10/02, MWB, changed Polyfitw call to poly_fit equivalent
;-
pro ltcrv,stand,fil,jd,am,serial,mag,err,object,objno,comp,dofil, $
       jdobs,redmag,rederr, $
       NOPRINT=noprint, NOPLOT=noplot, BINFAC=binfac, $
       K2=in_k2, CTERM=in_cterm, OCOLOR=ocolor, SCOLOR=scolor, STDMAG=stdmag, $
       COLORNAME=colorname, NOBS=nobs, $
       FORCE=in_force, FILTNAME=filtname, OBJNAME=objname, NOFILES=nofiles, $
       FILE=file,DATABASE=database,DB=db,TABLE=table,BAD=bad,REFID=refid, $
       NOSAVE=nosave,PLOTWIN=plotwin,DVERBOSE=dverbose,NOEDIT=noedit

   self= 'LTCRV: '

   if n_params() eq 0 then begin
      print,'ltcrv,stand,fil,jd,am,serial,mag,err,object,objno,comp,dofil, $'
      print,'   jdobs,redmag,rederr, [NOPRINT, NOPLOT, BINFAC, K2, CTERM, $'
      print,'                         OCOLOR, SCOLOR, STDMAG, FORCE, NOFILES,$'
      print,'                         FILE, DB, DATABASE, TABLE, BAD],REFID '
      return
   endif

   if badpar(stand, 7,        1,caller=self + ' (stand) ',npts=n1) then return
   if badpar(fil,   7,        1,caller=self + ' (fil) ',  npts=n2) then return
   if badpar(jd,    5,        1,caller=self + ' (jd) ',   npts=n3) then return
   if badpar(am,    [4,5],    1,caller=self + ' (am) ',   npts=n4) then return
   if badpar(serial,[1,2,3],  1,caller=self + ' (serial)',npts=n5) then return
   if badpar(mag,   [4,5],    1,caller=self + ' (mag) ',  npts=n6) then return
   if badpar(err,   [4,5],    1,caller=self + ' (err) ',  npts=n7) then return
   if badpar(object,7,        0,caller=self + ' (object) '        ) then return
   if badpar(colorname,7,     0,caller=self + ' (colorname) '     ) then return
   if badpar(objno ,[1,2,3],  0,caller=self + ' (objno) '         ) then return
   if badpar(comp  ,7,        0,caller=self + ' (comp) '          ) then return
   if badpar(dofil ,7,        0,caller=self + ' (dofil) '         ) then return
   if badpar(binfac,[0,1,2,3],0,caller=self + ' [BINFAC] ',  $
            default=6) then return
   if badpar(in_k2,   [0,4,5],1,caller=self + ' (k2) ',  npts=n8, $
            default=[999.,0.]) then return
   if badpar(in_cterm,[0,4,5],1,caller=self + ' (cterm)',npts=n9, $
            default=[999.,0.]) then return
   if badpar(ocolor,[0,4,5],  1,caller=self + ' (ocolor) ', npts=n10, $
            default=[0.,0.]) then return
   if badpar(scolor,[0,4,5],  1,caller=self + ' (scolor) ', npts=n11, $
            default=[0.,0.]) then return
   if badpar(stdmag,[0,4,5],  1,caller=self + ' (stdmag) ', npts=n12, $
            default=[0.,0.]) then return
   if badpar(in_force,[0,4,5],1,caller=self + ' (force) ',npts=n9, $
            default=[999.,0.]) then return
   if badpar(filtname,[0,7],  0,caller=self + ' (filtname) ', $
            default='default') then return
   if badpar(objname,[0,7],   0,caller=self + ' (objname) ', $
            default='default') then return
   if badpar(file,  [0,7],    0,caller=self + ' (file) ', $
            default='no save') then return
   if badpar(append,[0,1,2,3],0,caller=self + ' (append) ', $
            default=0) then return
   if badpar(bad,[0,1,2,3],   1,caller=self + ' (BAD) ',     npts=n13, $
            default=intarr(n1)) then return
   if badpar(db,[0,1,2,3],    0,caller=self + ' (DB) ', $
            default=0) then return
   if badpar(database,[0,7],  0,caller=self + '(DATABASE) ', $
            default='phot') then return
   if badpar(table,[0,7],     0,caller=self + '(TABLE) ', $
            default='data') then return
   if badpar(refid,[0,7],     0,caller=self + '(REFID) ', $
            default='') then return
   if badpar(noedit,[0,1,2,3],0,caller=self + '(NOEDIT) ', $
            default=0) then return
   if badpar(nosave,[0,1,2,3], 0,caller=self + ' (NOSAVE) ', $
            default=0) then return
   if badpar(plotwin,[0,1,2,3], 0,caller=self + ' (PLOTWIN) ', $
            default = -1) then return
   if badpar(dverbose,[1,2,3],  0,caller=self + ' (DVERBOSE) '         )  $
                          then return

   nobs = 0
   optpar=[n8,n9,n10,n11,n12]
   optbad=where(optpar ne 2, count_bad)
   if count_bad ne 0 then begin
      messarr=['K2','CTERM','OCOLOR','SCOLOR','STDMAG']
      for i=0,count_bad-1 do $
         print,'Optional parameter ',messarr[optbad[i]],' must contain only two elements.'
      return
   endif

   IF in_k2[0] eq 999.0 THEN k2=[0.,0.] ELSE k2=in_k2
   IF in_cterm[0] eq 999.0 THEN cterm=[0.,0.] ELSE cterm=in_cterm

   IF in_force[0] eq 999.0 THEN force=0 ELSE force=1

   if filtname eq 'default' then begin
      if dofil eq '1' then begin
         filstr = 'B'
      endif else if dofil eq '2' then begin
         filstr = 'V'
      endif else if dofil eq '3' then begin
         filstr = 'R'
      endif else if dofil eq '9' then begin
         filstr = 'M'
      endif else begin
         filstr = dofil
      endelse
   endif else begin
      filstr = filtname
   endelse

   plotit  = not keyword_set(noplot)
   saveit  = not keyword_set(nofiles)
   edit    = not keyword_set(noedit) and !d.name ne 'PS'

   IF objname eq 'default' THEN $
      objname = object+':'+string(objno,form='(i4.4)')

   ; Name of file where a summary of the fit is to be written
   sumfile = nobname(object)+':'+string(objno,form='(i4.4)')+'_'+filstr+'.dft'

   ;Compute the time part of the Julian Date.
   time = (jd - long(jd[0]-0.5d0)-0.50d0)*24.0
   jdstr,jd[0],100,datestr


   ; Select out the program object observations to reduce.
   ; this index will include data marked bad.
   obj = where(stand eq object and serial eq objno and fil eq dofil,nobs)

   if nobs eq 0 then begin
      print,self + ' No data found for ',object,':', $
            string(objno,format='(i4.4)'),' in filter ',dofil,'.  Aborting.'
      ; since all the bad flagged obj obs were included, we assume there were
      ; actually no data for this rule and we don't scrub the db.
      return
   endif

   ; we cleanse the db data table of any previous data for the object.
   if db and not nosave  then begin 
      if  refid ne '' then begin
         ; cleanse by obj, fil and color selected.
         dbphot, refid,object,'', filstr,0.,0., $
                 DATABASE=database, /CLEANONLY,/CLEANBYOBJFIL, $
                 COLOR=colorname,TABLE=table,NREMOV=nr,VERBOSE=dverbose 
         print, nr, ' previous observations of ', object,' were removed.'
      endif else begin
         print, self, ' cannot save to database without REFID.'
         return
      endelse
   endif


      ; Correct the star for second order extinction and color term.  This will
   editcount = 0

   repeat begin

      ; Select out the star(comp) observations to reduce.
      ; the star index will not include data marked bad.
      star=where(stand eq comp and  fil eq dofil and bad eq 0,cntstar)
      ; the badstar index will include only data marked bad.
      badstar = where(stand eq comp   and  fil eq dofil and bad ne 0,cntbadstar)

      if cntbadstar gt 0 and cntstar eq 0 and edit then begin
         print, 'All of the standard star pts are marked bad.'        
         ans=''
         read,prompt='Fix bad pts for standard star ' + comp +'? (y,n)',ans
         if ans eq 'y' then begin
            l_bad = bad[badstar]
            noerr = replicate(0.,cntbadstar)
            markdata,time[badstar],mag[badstar],noerr,l_bad, $
                  /yflip,xtitle='UT time in hours', $
                  ytitle=comp+' uncorrected instrumental mag'
            bad[badstar] = l_bad
            zchg=where(l_bad eq 0,editcount)
            if editcount gt 0 then continue
         endif
      endif

      if cntstar eq 0 then begin
         print,self + ' No data found for ',comp,' in filter ',dofil, $
              '.  Aborting.'
         return
      endif

      ;   be used for the extinction fit.
      starmag = mag[star] - k2[0]*scolor[0]*am[star] + cterm[0]*scolor[0]
      starerr = sqrt( err[star]^2 + (k2[1]*scolor[0]*am[star])^2 $
                                  + (k2[0]*scolor[1]*am[star])^2 $
                                  + (cterm[1]*scolor[0])^2 $
                                  + (cterm[0]*scolor[1])^2 )

      ; Find/Set the mean extinction coefficient.
      if force then begin
         meanext    = in_force[0]
         meanexterr = in_force[1]
         strcom = 'forced'
      endif else begin
         ; Do linear extinction fit to the comparison star.
         coeff=poly_fit(am[star],starmag,1,sigma=sigma, $
                         measure_errors=starerr,status=status)
         meanext=coeff[1]
         meanexterr=sigma[1]
         strcom = 'fitted'
      endelse

      ; Compute mean airmass of comparison star.
      meanam=mean(am[star])

      ; Correct all star measurements to the mean airmass.
      dams=meanam-am[star]
      staratmean=starmag+dams*meanext
      staratmeanerr=sqrt( starerr^2 + (dams*meanexterr)^2 )

      if edit then begin
         allstar=where(stand eq comp and fil eq dofil)
         alldams=meanam-am[allstar]
         allstarmag = mag[allstar] - k2[0]*scolor[0]*am[allstar] + $
                                     cterm[0]*scolor[0]
         allstarerr = sqrt( err[allstar]^2 + (k2[1]*scolor[0]*am[allstar])^2 $
                                  + (k2[0]*scolor[1]*am[allstar])^2 $
                                  + (cterm[1]*scolor[0])^2 $
                                  + (cterm[0]*scolor[1])^2 )
         allstaratmean=allstarmag+alldams*meanext
         allstaratmeanerr=sqrt( allstarerr^2 + (alldams*meanexterr)^2 )
         o_bad = bad[allstar]
         l_bad = bad[allstar]
         markdata,time[allstar],allstaratmean,allstaratmeanerr,l_bad, $
                  /yflip,xtitle='UT time in hours', $
                  ytitle=comp+' corrected to mean airmass'
         bad[allstar] = l_bad
         zchg=where(o_bad ne l_bad,editcount)
      endif 

   endrep until editcount eq 0

   ; Compute mean, avgdev, and stddev of corrected star.
   moment4,staratmean,meanstar,scatter,meanstarsig

   ; Set the Zero airmass magnitude
   if force then begin
      zammag = meanstar - meanext*meanam
      zamerr = sqrt((meanam*meanexterr)^2 + meanstarsig^2)
   endif else begin
      zammag=coeff[0]
      zamerr=a[0,0]
   endelse

   ; Compute chi-squared and yfit from mean fit.
   yfit = meanext*am[star] + zammag
   redchi = sqrt(total(((starmag-yfit)/starerr)^2)/(n_elements(star)-2))

   ; Bin the data to it's "natural" grouping.  That is, if there were 3
   ;   consequtive comp star measurments, they would be averaged into one
   ;   final point.  This will be used to derive the time-variable extinction.
   if binfac ne 1 then begin
      avger,time[star],starmag,starerr,binfac,3,tavgstar,avgmagstar,avgmagstarerr
      avger,time[star],staratmean,staratmeanerr,binfac,3,tavgstar,avgstar,avgstarerr
      avger,time[star],am[star],starerr,binfac,3,tavgstar,avgamstar
      avger,time[star],mag[star],err[star],binfac,3,tavgstar,avgrawstar,avgrawstarerr
   endif else begin ; Ignore binning if binfac is 1.
      tavgstar = time[star]
      avgmagstar=starmag
      avgmagstarerr=starerr
      avgstar=staratmean
      avgstarerr=staratmeanerr
      avgamstar=am[star]
      avgrawstar=mag[star]
      avgrawstarerr=err[star]
   endelse
   nsets = n_elements(tavgstar)

   ; From the binned comp and the binned comp corrected to the mean airmass,
   ;   compute the effective extinction.
   extin=(avgmagstar-zammag)/avgamstar
   extinerr = sqrt( ( avgmagstarerr^2 + zamerr^2 ) / avgamstar )

   ; Interpolate the time-variable extinction to the individual star measurement
   ;   times and apply the correction to see the scatter.
   interp,tavgstar,extin,E1=extinerr,time[star],kstar,kstarerr
   finalstar = starmag + kstar*(0.0-am[star]) + meanext*meanam
   finalstarerr = sqrt(starerr^2+(am[star]*kstarerr)^2)
   meanerr,finalstar,finalstarerr,finalmeanstar,finalmeanstarerr
   finalredchi = sqrt(total(((finalstar-finalmeanstar)/finalstarerr)^2)/ $
                                  (n_elements(finalstar)-2))
   finalscat = mean(abs(finalstar-finalmeanstar))

   ; Interpolate the time-variable extinction to the program object measurements
   interp,tavgstar,extin,E1=extinerr,time[obj],kobj,kobjerr

   ; Find the nearest (early) observation to each of the program object measurements
   staratobj = fltarr(nobs)
   staratobjerr = fltarr(nobs)
   stamatobj = fltarr(nobs)
   FOR i=0,nobs-1 DO BEGIN
      dt = tavgstar - time[obj[i]]
      zp = where(dt lt 0,count_prior)
      zf = where(dt gt 0,count_follow)
      IF count_prior gt 0 and count_follow gt 0 THEN BEGIN
         ip = where(dt eq max(dt[zp]))
         ia = where(dt eq min(dt[zf]))
         idx=[ip[0],ia[0]]
         meanerr,avgrawstar[idx],avgrawstarerr[idx],tmpmag,tmperr
         staratobj[i] = tmpmag
         staratobjerr[i] = tmperr
         stamatobj[i] = mean(avgamstar[[ip[0],ia[0]]])
      ENDIF ELSE IF count_prior gt 0 THEN BEGIN
         ip = where(dt eq max(dt[zp]))
         staratobj[i] = avgrawstar[ip[0]]
         staratobjerr[i] = avgrawstarerr[ip[0]]
         stamatobj[i] = avgamstar[ip[0]]
      ENDIF ELSE IF count_follow gt 0 THEN BEGIN
         ia = where(dt eq min(dt[zf]))
         staratobj[i] = avgrawstar[ia[0]]
         staratobjerr[i] = avgrawstarerr[ia[0]]
         stamatobj[i] = avgamstar[ia[0]]
      ENDIF ELSE BEGIN
         print,'fatal error, cannot happen'
         return
      ENDELSE
   ENDFOR

   ;Extract the time of observations.
   jdobs  = jd[obj]

   ; Reduce the object differentially against the star
   redmag = mag[obj] - staratobj $
                     - kobj*(am[obj]-stamatobj) $
                     - k2[0]*(ocolor[0]*am[obj] - scolor[0]*stamatobj) $
                     + cterm[0]*(ocolor[0] - scolor[0]) $
                     + stdmag[0]

   ; now the uncertainties
   rederr = err[obj]^2 + staratobjerr^2 $
                       + (kobjerr*(am[obj]-stamatobj))^2 $
                       + (k2[1]*(ocolor[0]*am[obj] - scolor[0]*stamatobj))^2 $
                       + (k2[0]*ocolor[1]*am[obj])^2 $
                       + (k2[0]*scolor[1]*stamatobj)^2 $
                       + (cterm[1]*(ocolor[0] - scolor[0]))^2 $
                       + (cterm[0]*ocolor[1])^2 $
                       + (cterm[0]*scolor[1])^2 $
                       + stdmag[1]^2
   rederr = sqrt(rederr)

   if edit then begin
      l_bad = bad[obj]
      markdata,time[obj],redmag,rederr,l_bad,/yflip, $
         xtitle='UT time in hours',ytitle='Final '+object+' magnitude'
      bad[obj] = l_bad
   endif
   redbad = bad[obj]

   ; Print results to the screen.
   line=strarr(17)

   fmt1 = '(f8.4," +/- ",f6.4,1x,a)'
   fmt2 = '(f8.4)'
   line[0] = objname+' with respect to '+comp+'.  Filter - '+filstr+ $
             ', Date '+datestr
   line[1] = 'Extinction             = '+string(meanext,meanexterr,strcom,form=fmt1)
   IF k2[0] eq 0.0 THEN BEGIN
      line[2] = 'No second order extinction term'
   ENDIF ELSE BEGIN
      line[2] = 'Second order extinction= '+string(k2,format=fmt1)
   ENDELSE
   IF cterm[0] eq 0.0 THEN BEGIN
      line[3] = 'No color term'
   ENDIF ELSE BEGIN
      line[3] = 'Color term             = '+string(cterm,format=fmt1)
   ENDELSE
   line[4] = 'Zero airmass magnitude = '+string(zammag,zamerr,form=fmt1)
   line[5] = 'Mean magnitude         = '+string(meanstar,meanstarsig,form=fmt1)
   line[6] = 'Mean airmass           = '+string(meanam,form=fmt2)
   line[7] = 'Reduced chi-squared    = '+string(redchi,form=fmt2)
   line[8] = 'Scatter (avgdev)       = '+string(scatter,form=fmt2)
   line[9] = 'Number of observations = '+string(n_elements(star),form='(i3)')
   line[11] = 'Number of sets         = '+string(nsets,form='(i3)')
   line[12] = 'Corrected star mean    = '+string(finalmeanstar,finalmeanstarerr,form=fmt1)
   line[13] = 'Reduced chi-squared    = '+string(finalredchi,form=fmt2)
   line[14] = 'Scatter (avgdev)       = '+string(finalscat,form=fmt2)
   line[16] = 'Mean object error      = '+string(mean(rederr),form=fmt2)

   if not keyword_set(noprint) then $
      for i=0,n_elements(line)-1 do print,line[i]

   ; This is for the output summary file.
   if saveit and not nosave then begin
      openw,lusum,sumfile,/get_lun
      for i=0,n_elements(line)-1 do printf,lusum,line[i]
      printf,lusum,' '
      printf,lusum,'  Set#   Time   Airmass   Star @ mean X   Diff       Extinction'
      FOR i=0,nsets-1 DO BEGIN
         printf,lusum,i,tavgstar[i],avgamstar[i],avgstar[i],avgstar[i]-meanstar,extin[i],extinerr[i], $
            format='(3x,i2,3x,f5.2,5x,f4.2,7x,f7.4,4x,f7.4,3x,f6.4," +/- ",f6.4)'
      ENDFOR
      printf,lusum,' '
      printf,lusum,objname,ocolor,format='(3x,a,2x,"color =",1x,f7.4," +/- ",f6.4)'
      printf,lusum,comp,filstr,stdmag,scolor, $
         format='(3x,a,2x,a,"=",f7.4," +/- ",f6.4,3x,"color =",1x,f7.4," +/- ",f6.4)'
      printf,lusum,k2,format='(3x,"second order extinction =",1x,f7.4," +/- ",f6.4)'
      printf,lusum,cterm,format='(3x,"color term              =",1x,f7.4," +/- ",f6.4)'
      free_lun,lusum
   endif

   print,' '
   print,'  Set#   Time   Airmass   Star @ mean X   Diff       Extinction'
   FOR i=0,nsets-1 DO BEGIN
      print,i,tavgstar[i],avgamstar[i],avgstar[i],avgstar[i]-meanstar,extin[i],extinerr[i], $
         format='(3x,i2,3x,f5.2,5x,f4.2,7x,f7.4,4x,f7.4,3x,f6.4," +/- ",f6.4)'
   ENDFOR

   zg = where(redbad eq 0,countzg)
   nsuppress = nobs - countzg  
   if not nosave then print, self, strn(countzg), ' obs will be saved, ', $
                strn(nsuppress), ' will not be saved because bad flag set.'
   ; Saving data to file
   if file ne 'no save' and not nosave and countzg gt 0 then begin
      if append then $
         print,filstr,' data appended to file ',file $
      else $
         print,filstr,' data written  to file ',file
      wrphot,jdobs[zg],filstr,redmag[zg],rederr[zg],file,APPEND=append
   endif
   ; Updating database -
   if db and not nosave then begin
      print, 'updating ', table, ' for ', object, $
             ' in filter ', filstr,  ' for RefID ', refid
      if countzg gt 0 then $
         ; bad flag implicitly 0 because this is clean data.
         dbphot, refid,object,jdobs[zg], filstr,redmag[zg],rederr[zg], $
                 DATABASE=database, COLOR=colorname, $
                 TABLE=table,/NOCLEAN, NREMOV=nr,VERBOSE=dverbose
   endif

   ; Plotting stuff.
   if plotit then begin

      if plotwin ge 0 then setwin,plotwin

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
      ploterror,tavgstar,extin,extinerr,psym=7,chars=cs,xtit='UT time (hours)', $
              ytit='Extinction',syms=ss,xr=minmax([time[obj],tavgstar]),yr=yr, $
              symsize=2
      setusym,-1
      oplerr,time[obj],kobj,kobjerr,psym=8,syms=ss*0.8
      setusym,1

      yr=[max(finalstar-meanstar+finalstarerr),min(finalstar-meanstar-finalstarerr)]
      plot,time[star],finalstar-meanstar,psym=8,chars=cs,yr=yr, $
              xtit='UT time (hours)',ytit='star mag residuals',syms=ss
      oplerr,time[star],finalstar-meanstar,finalstarerr,psym=3

      zg = where(redbad eq 0,countzg)
      if countzg gt 0 then begin
         yr=[max(redmag[zg]+rederr[zg]),min(redmag[zg]-rederr[zg])]
         plot,time[obj[zg]],redmag[zg],psym=8,chars=cs,yr=yr, $
                 xtit='UT time (hours)', $
                 ytit=object+' - '+comp,syms=ss, $
                 xr=minmax([time[obj[zg]],tavgstar])
         oplerr,time[obj[zg]],redmag[zg],rederr[zg],psym=3
      endif

      x=.6
      y=0.23-findgen(n_elements(line))/80
      for i=0,n_elements(line)-1 do xyouts,x,y[i],line[i],/normal,chars=0.5

      !p.multi=pmult
   endif

end

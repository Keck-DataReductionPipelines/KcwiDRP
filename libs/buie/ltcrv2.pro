;+
; NAME:
;  ltcrv2
; PURPOSE: (one line)
;  Photometric lightcurve reductions with known transformation.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;     ltcrv2,stand,fil,jd,time,am,serial,inst,instsig,color,colorsig, $
;        doobj,doser,dofil,tran,transig,jdref,jdobs,tobs,std,stdsig
; INPUTS:
;  stand    - String array of standard names.  (See coord.)
;  fil      - String array of filter names for observations.
;  jd       - Double precision array of the JD of observations.
;  time     - Floating point array of the UT time of observations.
;  am       - Floating point array of the airmass of observations.
;  serial   - Serial number of observation.
;  inst     - Instrumental magnitude
;  instsig  - Uncertainty of the instrumental magnitude
;  color    - Standard system color for object.
;  colorsig - Uncertainty on the standard color
;  doobj    - Name of object to reduce.
;  doser    - Serial number of object to reduce.
;  dofil    - Name of filter to reduce.
;  tran     - Transformation coefficients (vector)
;                tran(0) = principal extinction coefficient
;                tran(1) = second order extinction coefficient
;                tran(2) = color term
;                tran(3) = zero-point
;                tran(4) = time-dependent extinction term
;  transig  - Uncertainty on the transformation coefficients (vector).
;  jdref    - Time reference point for extinction
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  APPEND   - Flag, if true, data will be appended to FILE
;  BADFLAGS - Array of flags that mark data bad (if true).
;  COLORNAME- Name of color used for color, colorsig (such as 'B-V').
;  DATABASE - Name of MYSQL database to save reduced observations.
;             The default is 'phot'. 
;  DB       - Flag, if set, reduced observations will be saved to database.
;  DVERBOSE-  Code for dbphot and other routines for db transaction verbosity.
;  FILTNAME - String name for selected filter, default for 2=V and 3=R.
;  FILE     - If supplied, the reduced lightcurve will be saved to this file.
;  NOEDIT -   Flag, if set inhibits final interactive editing of fitted points.
;             This keyword has no effect and is not necessary if the current
;             plotting device is 'PS'.
;  NOPLOT   - Flag, if set, suppresses a plot of the final lightcurve.
;  NOSAVE  -  Flag, if true, will inhibit modification of files or databases.
;  PLOTWIN - Plot window to use for output plots (default = current window)
;  REFID -    String uniquely identifying observing run. This is used in 
;             updating the data table in the phot 
;             database. It must be specified if DB is used.
;  TABLE    - Name of table in MYSQL database to save reduced observations.
;             The default is 'data'. 
; KEYWORD OUTPUT PARAMETERS:
;  BADFLAGS - Array of flags that mark data bad (if true).
;  NOBS     - Number of points reduced.
; OUTPUTS:
;  jdobs    - JD of observation for each point.
;  tobs     - UT Time of observation for each point.
;  std      - Standard magnitude.
;  stdsig   - Uncertainty of the standard magnitude.
; COMMON BLOCKS:
; SIDE EFFECTS:
; Writes file of lightcurve data if FILE specified.
; Updates MYSQL database with lightcurve data if DB set.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written: 1992 Mar 31, Marc W. Buie, Lowell Observatory.
;  96/10/16 - MWB - added NOPLOT keyword
;  96/01/24, MWB, added BADFLAGS
; 2006/08/10, Peter L. Collins, Lowell Observatory, add data base
;             output for reduced obs, dbphot, slight header and print cleanup.
; 2006/09/26, PLC, add INSTRUMENT keyword to pass through to dbphot.
; 2006/10/16, PLC, add NOSAVE keyword.
; 2006/12/07, PLC, replace instrument and rundate keywords by REFID and
;                   change calls to dbphot.
; 2006/12/14, MWB, added PLOTWIN keyword.
; 2006/12/28, PLC, added DVERBOSE keyword.
; 2007/02/15, PLC, added NOBS and COLORNAME keywords. Include color in 
;                  photometry database. Somewhat better editing options for
;                  badflags with markdata.
;-
pro ltcrv2,stand,fil,jd,time,am,serial,inst,instsig,in_color,in_colorsig, $
   doobj,doser,dofil,tran,transig,jdref,jdobs,tobs,std,stdsig, $
   NOPLOT=noplot,FILTNAME=filtname,FILE=file,APPEND=append,BADFLAGS=bad, $
   NOEDIT=noedit,DB=db,DATABASE=database,TABLE=table,REFID=refid, $
   NOBS=nobs, COLORNAME=colorname, $
   NOSAVE=nosave,PLOTWIN=plotwin,DVERBOSE=dverbose

   if n_params() eq 0 then begin
      print,'ltcrv2,stand,fil,jd,time,am,serial,inst,instsig,in_color,' + $
             'in_colorsig, $'
      print,'       doobj,doser,dofil,tran,transig,jdobs,std,stdsig'
      return
   endif
   self = 'LTCRV2: '

   if badpar(stand, 7,        1,caller=self + '(stand) ', npts=n1) then return
   if badpar(fil,   7,        1,caller=self + '(fil) ',   npts=n2) then return
   if badpar(jd,    5,        1,caller=self + '(jd) ',    npts=n3) then return
   if badpar(time,  [4,5],    1,caller=self + '(time) ',  npts=n12) then return
   if badpar(am,    [4,5],    1,caller=self + '(am) ',    npts=n4) then return
   if badpar(serial,[1,2,3],  1,caller=self + '(serial) ',npts=n5) then return
   if badpar(inst,  [4,5],    1,caller=self + '(inst) ',  npts=n6) then return
   if badpar(instsig,[4,5],   1,caller=self + '(instsig) ',npts=n7) then return
   if badpar(in_color,[1,2,3,4,5],[0,1], $
                                caller=self + '(color) ',npts=n8) then return
   if badpar(in_colorsig,[1,2,3,4,5],[0,1], $
                                caller=self + '(colorsig) ',npts=n9) then return
   if badpar(doobj, 7,        0,caller=self + '(doobj) '         ) then return
   if badpar(doser, [1,2,3],  0,caller=self + '(doser) '         ) then return
   if badpar(dofil, 7,        0,caller=self + '(dofil) '         ) then return
   if badpar(tran,  [4,5],    1,caller=self + '(tran) ',npts=n10 ) then return
   if badpar(transig,[4,5],   1,caller=self + '(transig) ',npts=n11) then return
   if badpar(jdref, 5,        0,caller=self + '(jdref) ') then return
   if badpar(filtname,[0,7],  0,caller=self + '(filtname) ', $
                                default='default') then return
   if badpar(file,  [0,7],    0,caller=self + '(file) ', $
                                default='no save') then return
   if badpar(append,[0,1,2,3],0,caller=self + '(append) ',default=0) then return
   if badpar(bad,[0,1,2,3],[0,1],caller=self + '(BADFLAGS) ', npts=n13,$
                                default=intarr(n1)) then return
   if badpar(noedit,[0,1,2,3],0,caller=self + '(NOEDIT) ', $
                                default=0) then return
   if badpar(db,[0,1,2,3],0,    caller=self + '(DB) ', $
                                default=0) then return
   if badpar(database,[0,7],0,  caller=self + '(DATABASE) ', $
                                default='phot') then return
   if badpar(table,[0,7],0,     caller=self + '(TABLE) ', $
                                default='data') then return
   if badpar(refid,[0,7],0,     caller=self + '(REFID) ', $
                                default='') then return
   if badpar(nosave,[0,1,2,3],0,caller=self + '(NOSAVE) ', $
                                default=0) then return
   if badpar(plotwin,[0,1,2,3], 0,caller=self + ' (PLOTWIN) ', $
            default = -1) then return
   if badpar(dverbose,[0,1,2,3],0,caller=self + '(DVERBOSE) ')  then return

   alln=[n1,n2,n3,n4,n5,n6,n7,n12,n13]
   if min(alln) ne max(alln) then begin
      print,self + 'Error!  stand,fil,jd time,am,serial,mag,err,{bad}' + $
            ' must be the same length.'
      return
   endif

   if n8 eq 1 then color = replicate(in_color,n1) $
   else if n8 ne n1 then begin
      print,self + 'Error!  color must be scalar or same length as the others.'
      return
   endif else color=in_color

   if n9 eq 1 then colorsig = replicate(in_colorsig,n1) $
   else if n9 ne n1 then begin
      print,self+'Error! colorsig must be scalar or same length as the others.'
      return
   endif else colorsig=in_colorsig

   if n10 ne 5 or n11 ne 5 then begin
      print,self + 'Error!  tran and transig must be 5 element vectors.'
      return
   endif

   if not nosave and db and refid eq '' then begin
      print,'self' + ': Error!  REFID must be set if using /SAVE and /DB.'
      return
   endif

   edit = not keyword_set(noedit) and !d.name ne 'PS'

   if filtname eq 'default' then begin
      if dofil eq '2' then begin
         filstr = 'V'
      endif else if dofil eq '3' then begin
         filstr = 'R'
      endif else begin
         filstr = dofil
      endelse
   endif else begin
      filstr = filtname
   endelse

   ; prescrub the db, if used.
   if db and not nosave then begin
      print, 'Prescrubbing ',  table, ' for ', doobj, $
              ' in filter ', filstr,  ' for ReFID ', refid
      dbphot, refid,doobj,0.0D,filstr,0.0,0.0,DATABASE=database, $
              TABLE=table,/CLEANONLY, /CLEANBYOBJFIL,NREMOV=nr, $
              COLOR=colorname,VERBOSE=dverbose
      print, nr, ' previous observations were removed.'
   endif

   
   ; Select out all the measurements for the requested filter and object.
   z=where(fil eq dofil and stand eq doobj and serial eq doser and bad eq 0, $
            nobs)
   if nobs eq 0 and edit then begin
      ; maybe there are data but the bad flags are set?
      z=where(fil eq dofil and stand eq doobj and serial eq doser, $
               nobs)
      if nobs gt 0 then begin
         print, 'There are no observations for ', doobj, ' in filter ', $
                 dofil, ' except ', strn(nobs), ' marked bad.'
         ans=''
         read,prompt='Fix bad pts ? (y,n)',ans
         if ans eq 'y' then begin
            l_bad = bad[z]
            markdata,time[z],inst[z],l_bad,/yflip,xtitle='UT time in hours', $
            ytitle='Instrumental '+filstr+' magnitude'
            bad[z] = l_bad
            z=where(fil eq dofil and stand eq doobj and serial eq doser $
                    and bad eq 0, nobs)
         endif else nobs = 0
      endif
   endif
   if nobs eq 0 then begin
;         z=where(stand eq doobj, nobs)
;         if nobs gt 0 then print, stand[z], fil[z], serial[z]
      print,'No measurements found for ',doobj,' in filter ',dofil, $
            '. Quitting.'
      return
   endif

   ; Compute standard magnitudes for observations.
   inst2std,jd[z],am[z],inst[z],instsig[z],color[z],colorsig[z], $
      tran,transig,jdref,std,stdsig

   jdobs = jd[z]
   tobs  = time[z]

   jd0=long(jdobs[0]+0.5)-0.5
   jdstr,jd0,100,date

   if not keyword_set(noplot) then begin
      if plotwin ge 0 then setwin,plotwin
      ploterror,tobs,std,stdsig,psym=8,yr=maxmin(std), $
         xtit='UT time in hours',ytit='Apparent '+filstr+' magnitude', $
         tit=doobj+', '+date,xr=minmax(tobs),charsize=1.5
   endif

   if edit then begin
      l_bad = bad[z]
      markdata,tobs,std,l_bad,/yflip, $
         xtitle='UT time in hours',ytitle='Apparent '+filstr+' magnitude'
      bad[z] = l_bad
   endif

   if file ne 'no save' and not nosave  then begin
      if append then $
         print,filstr,' data appended to file ',file $
      else $
         print,filstr,' data written  to file ',file
      wrphot,jdobs,filstr,std,stdsig,file,APPEND=append,bad=bad[z]
   endif

   ; Select out all the good measurements.
   zg=where(bad[z] eq 0, nobs)
   if db and not nosave and nobs gt 0 then begin
      print, 'updating ',  table, ' for ', doobj, $
              ' in filter ', filstr,  ' for ReFID ', refid
      dbphot,refid,doobj,jdobs[zg],filstr,std[zg],stdsig[zg], $
             DATABASE=database, $
              TABLE=table, COLOR=colorname,/NOCLEAN,VERBOSE=dverbose
      jdobs = jdobs[zg]
   endif
end

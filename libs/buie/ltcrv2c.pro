;+
; NAME:
;  ltcrv2c
; PURPOSE:
;  2-color lightcurve reductions with known transformation.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  ltcrv2c,stand,fil,jd,time,am,serial,inst,instsig, $
;      doobj,doser,dofil,tran,transig,jdref, $
;      color,colorsig, $
;      NOPLOT=noplot,FILTNAME=filtname,FILE=file
; INPUTS:
;  stand    - String array of standard names.  (See coord.)
;  fil      - String array of filter names for observations.
;  jd       - Double precision array of the JD of observations.
;  time     - Floating point array of the UT time of observations.
;  am       - Floating point array of the airmass of observations.
;  serial   - Serial number of observation.
;  inst     - Instrumental magnitude
;  instsig  - Uncertainty of the instrumental magnitude
;  doobj    - Name of object to reduce.
;  doser    - Serial number of object to reduce.
; The following have one extra dimension relative to LTCRV2 arguments.
;  The first dimension is for the first color, the second is for the other.
;  dofil    - Name of filter to reduce.
;  tran     - Transformation coefficients (2-d vector)
;                tran(0,i) = principal extinction coefficient
;                tran(1,i) = second order extinction coefficient
;                tran(2,i) = color term
;                tran(3,i) = zero-point
;  transig  - Uncertainty on the transformation coefficients (2-d vector).
;  jdref    - Time reference point for extinction
;  color    - Starting guess for color of object (default=0.)
;  colorsig - Starting value for uncertainty of color (default=0.)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BADFLAGS - Array of flags that mark data bad (if true).
;  DB       - Flag, if set, tells ltcrv2 to use data base.
;  DVERBOSE-  Flag, passed to gettran and other routine to print db queries.
;  EPSILON  - Convergence criterion for color, default=0.001 mag
;  FILE     - If supplied, the reduced lightcurve will be saved to these files.
;  FILTNAME - String name for selected filter, default for 2=V and 3=R.
;  NOEDIT   - Flag, if set inhibits final interactive editing of fitted points.
;             This keyword has no effect and is not necessary if the current
;             plotting device is 'PS'.
;  NOPLOT   - Flag, if set, suppresses a plot of the final lightcurve.
;  NOSAVE  -  Flag, if true, will inhibit modification of files or databases.
;  PLOTWIN -  Plot window to use for output plots (default = current window)
;  REFID   -  String uniquely identifying observing run. This is used in 
;             updating the data table in the phot database. It must be 
;             specified if DB is used.
;  TABLE   -  Name of table in MYSQL database to save reduced observations.
;                Passed transparently to ltcrv2, which supplies the default.

; OUTPUTS:
;  color    - Standard system color for object.
;  colorsig - Uncertainty on the standard color
; KEYWORD OUTPUT PARAMETERS:
;  BADFLAGS - Array of flags that mark data bad (if true).
;  JDOBS    - Array of jd for observations that were reduced. 
;  NOBS     - Number of obs that were used. This is the sum of
;             reduced obs in each filter, but will be zero if the count
;             is zero in either filter.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written 96/10/17, Marc W. Buie, Lowell Observatory
;  96/01/24, MWB, added BADFLAGS
;  97/02/11, MWB, modified to support new LTCRV2 program (k(t))
; 2006/10/16, PLC, modified to pass keywords INSTRUMENT, DB and NOSAVE
;                  to ltcrv2.
; 2006/12/07, PLC, replace instrument keyword by REFID and
;                   change calls to dbphot- add keyword TABLE.
; 2006/12/10, PLC, forgot a call or two to ltcrv2.
; 2006/12/14, MWB, added PLOTWIN keyword.
; 2006/12/27, PLC, added DVERBOSE keyword.
; 2007/02/05, PLC, added NOBS and JDOBS output keywords- rationalize
;                  handling of bad flags and data base scrubbing.
;-
pro ltcrv2c,stand,fil,jd,time,am,serial,inst,instsig, $
      doobj,doser,dofil,tran,transig,jdref, $
      color,colorsig,PLOTWIN=plotwin, $
      NOPLOT=noplot,FILTNAME=filtname,FILE=file,EPSILON=epsilon,ERROR=error, $
      BADFLAGS=bad, NOEDIT=noedit,DB=db, JDOBS=jdobs, NOBS=nobs, REFID=refid, $
      TABLE=table, NOSAVE=nosave,DVERBOSE=dverbose

   error=1
   self='LTCRV2C: '
   if badpar(stand, 7,        1,caller='LTCRV2C: (stand) ') then return
   if badpar(fil,   7,        1,caller='LTCRV2C: (fil) '  ) then return
   if badpar(jd,    5,        1,caller='LTCRV2C: (jd) '   ) then return
   if badpar(time,  [4,5],    1,caller='LTCRV2C: (time) ' ) then return
   if badpar(am,    [4,5],    1,caller='LTCRV2C: (am) '   ) then return
   if badpar(serial,[1,2,3],  1,caller='LTCRV2C: (serial) ') then return
   if badpar(inst,  [4,5],    1,caller='LTCRV2C: (inst) ' ) then return
   if badpar(instsig,[4,5],   1,caller='LTCRV2C: (instsig) ') then return
   if badpar(color,[0,1,2,3,4,5],0,caller='LTCRV2C: (color) ', $
                                default=0.) then return
   if badpar(colorsig,[0,1,2,3,4,5],0,caller='LTCRV2C: (colorsig) ', $
                                default=0.) then return
   if badpar(doobj, 7,        0,caller='LTCRV2C: (doobj) '         ) then return
   if badpar(doser, [1,2,3],  0,caller='LTCRV2C: (doser) '         ) then return
   if badpar(dofil, 7,        1,caller='LTCRV2C: (dofil) '         ) then return
   if badpar(tran,  [4,5],    2,caller='LTCRV2C: (tran) '   ) then return
   if badpar(transig,[4,5],   2,caller='LTCRV2C: (transig) ') then return
   if badpar(jdref, 5,        1,caller='LTCRV2C: (jdref) '  ) then return
   if badpar(filtname,[0,7],  1,caller='LTCRV2C: (filtname) ', $
                                default=['default','default']) then return
   if badpar(file,  [0,7],    1,caller='LTCRV2C: (file) ', $
                                default='no save') then return
   if badpar(epsilon,[0,1,2,3,4,5],0,caller='LTCRV2C: (EPSILON) ', $
                                default=0.001) then return
   if badpar(bad,[0,1,2,3],[0,1],caller='LTCRV2C: (BADFLAGS) ', $
                                default=intarr(n_elements(jd))) then return
   if badpar(noedit,[0,1,2,3],0,caller='LTCRV2C: (NOEDIT) ', $
                                default=0) then return
   if badpar(refid,[0,7],0,     caller=self + '(REFID) ', $
                                default='') then return
   if badpar(plotwin,[0,1,2,3], 0,caller=self + ' (PLOTWIN) ', $
            default = -1) then return
   if badpar(dverbose,[0,1,2,3], 0,caller=self + ' (DVERBOSE) ') $
                          then return
   if badpar(table,[0,7],     0,caller=self + '(TABLE) ', $
            default='data') then return

   edit = noedit ne 0 and !d.name ne 'PS'
   if not nosave and db and refid eq '' then begin
      print,'self' + ': Error!  REFID must be set if using /SAVE and /DB.'
      return
   endif

   filstr=strarr(2)
   for i=0,1 do begin
      if filtname[i] eq 'default' then begin
         if dofil[i] eq '2' then begin
            filstr[i] = 'V'
         endif else if dofil[i] eq '3' then begin
            filstr[i] = 'R'
         endif else begin
            filstr[i] = dofil[i]
         endelse
      endif else begin
         filstr[i] = filtname[i]
      endelse
   endfor
   colorname =  filstr[0] + '-' + filstr[1] 

   ; prescrub db for both filters now.
   if db and not nosave then begin
      for i=0,1 do begin
         ; remove obs in the individual filters.
         print, 'Prescrubbing ',  table, ' for ', doobj, $
                 ' in filter ', filstr[i],  ' for ReFID ', refid
         dbphot, refid,doobj,0.0D,filstr[i],0.0,0.0,DATABASE=database, $
                 TABLE=table,/CLEANONLY, /CLEANBYOBJFIL,NREMOV=nr, $
                 COLOR=colorname,VERBOSE=dverbose
         print, nr, ' previous observations were removed.'
      endfor
      print, 'Prescrubbing ',  table, ' for ', doobj, $
              ' in filter ', colorname,  ' for ReFID ', refid
      ; remove the composite color observation.
      dbphot, refid,doobj,0.0D,colorname,0.0,0.0,DATABASE=database, $
              TABLE=table,/CLEANONLY, /CLEANBYOBJFIL,NREMOV=nr, $
              VERBOSE=dverbose
      print, nr, ' previous observations were removed.'
   endif

after_edit:

   repeat begin
;      print,'      ',filtname(0),'-',filtname(1), $
;         ' color for ',doobj,' is ',color,' +/- ',colorsig

      oldcolor=color

      ltcrv2,stand,fil,jd,time,am,serial,inst,instsig, $
         color,colorsig,doobj,doser,dofil[0],tran[*,0],transig[*,0],jdref[0], $
         jd1,t1,mag1,err1,FILTNAME=filtname[0],/noplot,badflags=bad,/NOEDIT, $
         DB=db,REFID=refid,NOBS=nobs,TABLE=table,/NOSAVE,DVERBOSE=dverbose

      if nobs eq 0 then return  

      ltcrv2,stand,fil,jd,time,am,serial,inst,instsig, $
         color,colorsig,doobj,doser,dofil[1],tran[*,1],transig[*,1],jdref[1], $
         jd2,t2,mag2,err2,FILTNAME=filtname[1],/noplot,badflags=bad,/NOEDIT, $
         DB=db,REFID=refid,NOBS=nobs,TABLE=table,/NOSAVE,DVERBOSE=dverbose
      
      if nobs eq 0 then return   

      if n_elements(t1) gt n_elements(t2) then begin
         interp,t1,mag1,E1=err1,t2,mag1at2,err1at2
         c = mag1at2 - mag2
         ce = sqrt( err1at2^2 + err2^2 )
      endif else begin
         interp,t2,mag2,E1=err2,t1,mag2at1,err2at1
         c = mag1 - mag2at1
         ce = sqrt( err1^2 + err2at1^2 )
      endelse

      meanerr,c,ce,color,colorsig

   endrep until abs(color-oldcolor) le epsilon

   if plotwin ge 0 then setwin,plotwin

   pmulti=!p.multi
   !p.multi=[0,1,3]

   jd0=long(jd[0]+0.5)-0.5
   jdstr,jd0,100,date

   print,'Final ',filtname[0],'-',filtname[1], $
      ' color for ',doobj,' is ',color,' +/- ',colorsig

   oldbad=bad
   ltcrv2,stand,fil,jd,time,am,serial,inst,instsig, $
      color,colorsig,doobj,doser,dofil[0],tran[*,0],transig[*,0],jdref[0], $
      jd1,t1,mag1,err1,FILTNAME=filtname[0],/noplot,file=file[0], $
      badflags=bad,noedit=noedit,DB=db,REFID=refid,TABLE=table, $
      NOSAVE=nosave,COLORNAME=colorname,NOBS=nobs1,DVERBOSE=dverbose
   if long(total(bad eq oldbad)) ne n_elements(bad) then begin
      print,n_elements(bad)-long(total(bad eq oldbad)),' ',filtname[0],' observations removed.'
      goto,after_edit
   endif

   oldbad=bad
   ltcrv2,stand,fil,jd,time,am,serial,inst,instsig, $
      color,colorsig,doobj,doser,dofil[1],tran[*,1],transig[*,1],jdref[1], $
      jd2,t2,mag2,err2,FILTNAME=filtname[1],/noplot,file=file[1],/append, $
      badflags=bad,noedit=noedit,DB=db,REFID=refid,TABLE=table, $
      NOSAVE=nosave,COLORNAME=colorname,NOBS=nobs2,DVERBOSE=dverbose
   if long(total(bad eq oldbad)) ne n_elements(bad) then begin
      print,n_elements(bad)-long(total(bad eq oldbad)),' ',filtname[1],' observations removed.'
      goto,after_edit
   endif

   ; nobs1 and nobs2 are both non-zero here.
   jdobs = [jd1, jd2]
   nobs = nobs1 + nobs2

   xr=minmax([t1,t2])
   dm=ceil(max([max(mag1+err1)-min(mag1+err1), $
                max(mag2+err2)-min(mag2+err2)]) * 10.0 ) / 10.0

   ploterror,t1,mag1,err1,psym=8,yr=mean(minmax(mag1))+[dm,-dm]/2, $
      xtit='UT time in hours',ytit='Apparent '+filstr[0]+' magnitude', $
      tit=doobj+', '+date,xr=xr,charsize=1.5

   ploterror,t2,mag2,err2,psym=8,yr=mean(minmax(mag2))+[dm,-dm]/2, $
      xtit='UT time in hours',ytit='Apparent '+filstr[1]+' magnitude', $
      tit=doobj+', '+date,xr=xr,charsize=1.5

   if n_elements(t1) gt n_elements(t2) then begin
      interp,t1,mag1,E1=err1,t2,mag1at2,err1at2
      t = t2
      c = mag1at2 - mag2
      ce = sqrt( err1at2^2 + err2^2 )
   endif else begin
      interp,t2,mag2,E1=err2,t1,mag2at1,err2at1
      t = t1
      c = mag1 - mag2at1
      ce = sqrt( err1^2 + err2at1^2 )
   endelse

   ploterror,t,c,ce,psym=8,yr=mean(minmax(c))+[-dm,dm]/2, $
      xtit='UT time in hours',ytit=filstr[0]+'-'+filstr[1]+' color', $
      tit=doobj+', '+date,xr=xr,charsize=1.5

   !p.multi=pmulti
   error=0

end

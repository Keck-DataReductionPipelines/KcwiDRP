;+
; NAME:
;   transf
;
; PURPOSE: (one line)
;   Determine transformation coefficients from instrumental to standard mags.
;
; DESCRIPTION:
;
;   This will take a set of all-sky standard star measurements and derive
;     the best fitting set of transformation coefficients from instrumental
;     to standard magnitudes.
;
;   Normally, you give this routine ALL the data taken with a single filter.
;     Only those objects that are standards in this filter and the alternate
;     color will be used in the fit.  Only those that should be standards
;     can end up marked as bad from this program.
;
;   My conventions for the photometric fit are patterned after Hardie's
;     nomenclature.  However, there is one additional term I've added which
;     is a first order expansion term of extinction with respect to time.
;     The basic formula for the transformation is:
;
;       m0 = m - kX - n(t-t0)X - k"CX + eC + Z
;
;        where
;           m  = instrumental magnitude
;           k  = extinction coefficient, mag/airmass
;           X  = airmass
;           n  = coefficient of the 1st order expansion of extinction as a
;                  function of time
;           t  = Time of observation (in hours)
;           t0 = Reference time for n, time dependent correction is zero at
;                  this time, usually is the middle of the observation set.
;           k" = second order extinction coefficient
;           C  = Standard system color of the object
;           e  = color term
;           Z  = zero point
;           m0 = Standard magnitude
;
;   The fitting process takes advantage of the uncertainties on the instrumental
;     magnitude and the uncertainties on the forced terms.  The uncertainties
;     on the catalog standard magnitudes are ignored.  The coefficients, k",
;     e, and n are considered to be auxillary coefficients and can be either
;     fit for or forced.  k" and e can be forced to explicit values.  n is
;     either fit for or set to 0.  The extinction and zero-point are always
;     fit for.
;
; CATEGORY:
;   Photometry
;
; CALLING SEQUENCE:
;  transf,names,jd,filter,color1,color2,airm,mag,err,bad,
;    [ RESID=resid,FILE=landfile,NOPLOT=noplot,OTHER=other,
;      OLAB=olab,TITLE=title ]
;
; INPUTS:
;  names  - String array of standard names.
;  jd     - Julian date of observations.
;  filter - Filter number of data. (0-U, 1-B, 2-V, 3-R, 4-I)
;  color1 - First filter for color.
;  color2 - Second filter for color.  (eg., B-V is 1,2)
;  airm   - Vector of airmasses for observations.
;  mag    - Vector of instrumental magnitudes.
;  err    - Vector of uncertainties.
;  bad    - Vector of flags, 0=good, 1=bad, default is good
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  These keywords affect how the fit is done.
;
;  CTERM  - Optional, fixed color term.  If provided, the color term is
;              assumed, not fitted.
;  K2     - Second order extinction coefficient.  If provided, this term
;              is assumed, not fitted.
;  KTIME  - Flag, enable time dependent extinction.
;
; -------------------
;
;  These keywords affect the plotting output
;
;  NOPLOT   - Flag, if true inhibits the summary plots.
;  OLAB     - Label for the plot of residuals vs. OTHER.
;  OTHER    - If provided, allows plotting the residuals against some other
;              variable, eg., exposure time.  Must match the length of
;              the data input vectors.
;
; -------------------
;
;  Other control options
;
;  FILE     - Optional name for the Landolt "system" Standards catalog.
;  NOEDIT   - Flag, if set inhibits final interactive editing of fitted points.
;             This keyword has no effect and is not necessary if the current
;             plotting device is 'PS'.
;  RESID    - Flag, if true enables printing a full residual table.
;  TITLE    - Title for plots.
;  NOFILES  - Flag, if true supresses saving information to summary files.
;  NOSAVE  -  Flag, if true supresses saving information to databases or
;             summary files. NOSAVE implies NOFILES.
;  CHISCALE -Flag, if true rescales final solution to force reduced Chi=1.0
;  TAGDATE  -String identifying date of data.
;  TAGINST  -String identifying instrument.
;  HISTFILE -File name of history file to update extintion values in.
;             Only one line for TAGDATE/TAGINST is allowed.  If a duplicate
;             is found, the appropriate line is replaced.
;  MAGRESID - Edit bad values by mag residual, not sigma residuals
;  DB       - flag, if set will use MYSQL server and validly populate outputs
;                fitted, nobs, chi2, quality and comments.
;  DATABASE - Name of MYSQL database for transformation search. Passed 
;                to puttran.  The default is 'phot'. Used only when DB set.
;  DVERBOSE  - flag passed to gettran and puttran to set  db screen verbosity.
;  TABLENAME- Name of table in MYSQL database for transformation search.
;                Passed to puttran. The default is 'transf'. Used only if DB set
;
;
; OUTPUTS:
;  No explicit vector outputs.  All output is graphical, printed to the
;    screen, or saved to external files.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;  None
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  93/08/05, Initial version completed, Marc W. Buie, Lowell Observatory.
;  93/10/05, MWB, added support for 1993 catalog.
;  95/02/22, MWB, modified plots.
;  96/10/16, MWB, added summary save to file, NOFILES, CHISCALE, TAG*, and
;                   HISTFILE keywords actions added.
;  96/11/22, MWB, changed to new master photometry catalog
;  97/01/24, MWB, added MAGRESID flag
;  97/2/7, MWB, massive rewrite
;  2004/02/09, MWB, changed path to transf files.
;  2006/08/02, MWB, added DB, DATABASE, TABLENAME keywords
;  2006/10/16, PLC, added NOSAVE keyword
;  2006/12/28, PLC, added DVERBOSE flag.
;-
pro transf,names,jd,filter,color1,color2,airm,mag,err,bad, $
      CTERM=cterm,K2=k2,KTIME=ktime,RESID=resid,FILE=landfile,NOPLOT=noplot, $
      NOEDIT=noedit, OTHER=other,OLAB=olab,TITLE=title,NOFILES=nofiles, $
      CHISCALE=chiscale,TAGDATE=in_tagdate,TAGINST=in_taginst, $
      HISTFILE=histfile,MAGRESID=magresid,DB=db,DATABASE=database, $
      TABLENAME=tablename,NOSAVE=nosave,DVERBOSE=dverbose

   self='TRANSF: '
   if badpar(names,7,1,caller=self+'(names) ',npts=len1) then return
   if badpar(jd,5,1,caller=self+'(jd) ',npts=len6) then return
   if badpar(filter,[1,2,3],0,caller=self+'(filter) ') then return
   if badpar(color1,[1,2,3],0,caller=self+'(color1) ') then return
   if badpar(color2,[1,2,3],0,caller=self+'(color2) ') then return
   if badpar(airm,[4,5],1,caller=self+'(airm) ',npts=len2) then return
   if badpar(mag,[4,5],1,caller=self+'(mag) ',npts=len3) then return
   if badpar(err,[4,5],1,caller=self+'(err) ',npts=len4) then return
   if badpar(bad,[0,2,3],[0,1],caller=self+'(bad) ', $
         default=intarr(len4),npts=len5) then return
   if badpar(db,[0,1,2,3],0,   caller=self+' (db) ', default=0) then return
   if badpar(database,[0,7],0, caller=self+' (database) ', $
                             default='phot') then return
   if badpar(tablename,[0,7],0, caller=self+' (tablename) ', $
                             default='transf') then return

   if badpar(cterm,[0,4,5],1,caller=self+'[CTERM] ',default=999.0) then return
   if badpar(k2,[0,4,5],1,caller=self+'[K2] ',default=999.0) then return
   if badpar(ktime,[0,1,2,3],0,caller=self+'[KTIME] ',default=0) then return
   if badpar(chiscale,[0,1,2,3],0,caller=self+'[CHISCALE] ',default=0) then return
   if badpar(olab,[0,7],0,caller=self+'[OLAB] ',default=' ') then return
   if badpar(title,[0,7],0,caller=self+'[TITLE] ',default='Transformation') then return
   if badpar(in_tagdate,[0,7],0,caller=self+'[TAGDATE] ',default='') then return
   if badpar(in_taginst,[0,7],0,caller=self+'[TAGINST] ',default='') then return
   if badpar(dverbose,[0,1,2,3],0,   caller=self+' (DVERBOSE) ') then return

   lens=[len1,len2,len3,len4,len5,len6]
   if min(lens) ne max(lens) then begin
      print,'TRANSF: Error!  names, jd, airm, mag, and err must be equal in length.'
      return
   endif

   ; Maximum possible number of variables.
   nvars=5

   blanks='          '
   fmt='(a,f7.4,a,f6.4,3x,a,f7.4)'
   fmt1='(a,f7.4,a,f8.4,3x,a,f7.4)'
   pm=' +/- '

   sfnames=['U','B','V','R','I']   ; standard filter names
   ifnames=['u','b','v','r','i']   ; instrumental filter names
   fname = sfnames[filter] ; Filter name (standard) that is being reduced
   iname = ifnames[filter] ; Filter name (instrumental) that is being reduced
   cname = '('+sfnames[color1]+'-'+sfnames[color2]+')'  ; name of color

   tagdate=strmid(in_tagdate+blanks,0,6)   ; string that identifies the date
   taginst=strmid(in_taginst+blanks,0,10)  ; string that identifies the instrument

   ; Name of file where a summary of the fit is to be written
   sumfile = iname+'_'+ifnames[color1]+'m'+ifnames[color2]+'.sum'

   ; information on how each term was treated
   transtr=strarr(nvars)

   ; Grab the Landolt standard photometric catalog
   rdland2,lname,lmags,lcode

   ; Build index that points to the Landolt catalog entry for each observation.
   ;   If that star is not a useful standard, set the index to -1
   idx=intarr(len1)
   for i=0,len1-1 do begin
      z=where(names[i] eq lname)
      z=z[0]
      if z ne -1 then begin
         if lcode[filter,z] eq 1 and $
            lcode[color1,z] eq 1 and $
            lcode[color2,z] eq 1 then idx[i]=z $
         else idx[i] = -1
      endif else idx[i] = -1
   endfor
   land = where(idx ne -1, count_land)

   ; Setup the transformation coefficient vectors.
   trans   = fltarr(nvars)
   transig = fltarr(nvars)
   fit     = replicate(1,nvars)

   IF k2[0] ne 999.0 THEN BEGIN
      trans[1]   = k2[0]
      transig[1] = k2[1]
      fit[1]     = 0
   ENDIF

   IF cterm[0] ne 999.0 THEN BEGIN
      trans[2]   = cterm[0]
      transig[2] = cterm[1]
      fit[2]     = 0
   ENDIF

   IF not ktime THEN BEGIN
      trans[4]   = 0.
      transig[4] = 0.
      fit[4]     = 0
   ENDIF

   ; On the first pass, the fit will proceed with the given errors.  Later there
   ;   may be an aposteriori adjustment of the uncertainties.
   first=1
   sigscale=1.0
   edit    = not keyword_set(noedit) and !d.name ne 'PS'
   plotit  = not keyword_set(noplot)
   saveit  = not keyword_set(nofiles)
   if keyword_set(nosave) then  saveit = 0

   if count_land ne 0 then begin

      ; This is for the output summary file.
      if saveit then openw,lusum,sumfile,/get_lun

      ; Declare working variables for all stars.
      y = fltarr(count_land)       ; Dependent variables.
      ysig = fltarr(count_land)    ; Uncertainty on y.
      x = fltarr(nvars,count_land) ; Independent variables.

      ; Extract the standard values
      n = count_land-1
      std_mag = lmags[filter,idx[land]]
      std_mag = std_mag[*]
      std_col = lmags[color1,idx[land]] - lmags[color2,idx[land]]
      std_col = std_col[*]
      omag    = mag[land]
      oerr    = err[land]
      oairm   = airm[land]
      if keyword_set(other) then l_other = other[land]
      l_names = names[land]
      l_bad   = bad[land]
      jdref   = total(jd[land]-jd[land[0]])/double(count_land)+jd[land[0]]
      dt      = float(jd[land]-jdref)*24.0

      ; Fill independent variable vectors
      x[0,*] = -oairm
      x[1,*] = -oairm*std_col
      x[2,*] = std_col
      x[3,*] = 1.0
      x[4,*] = -dt*oairm

   ; restart here on second pass for chisqr adjustmet
   chi_scale:

      ; scale errors
      oerr = oerr*sigscale

   after_edit:

      ; predefine variables for mysvdfit
      yfit=1
      var=1
      chisq=1
      sing=1

      ; Weed out bad measurements
      zbad = where(omag gt 90,badcount)
      if badcount gt 0 then begin
         l_bad[zbad] = 1
         oerr[zbad]  = 0.1
      endif

      zgood = where(l_bad eq 0,goodcount)
      if goodcount eq 0 then begin
         print,'TRANSF: no good observations to fit, all marked bad!'
         return
      endif

      if not first or not chiscale or edit then begin
         print,'Landolt transformation for ',fname,' and ',cname,' from ', $
            string(goodcount,form='(i3)'),' stars'

         if saveit and ( not first or not edit ) then $
            printf,lusum,'Landolt transformation for ',fname,' and ',cname,' from ', $
               string(goodcount,form='(i3)'),' stars'
      endif

      ; Setup the indepedent vector and uncertainty
      y = std_mag - omag
      ysig = oerr^2
      FOR i=0,nvars-1 DO BEGIN
         IF fit[i] eq 0 THEN BEGIN
            y = y - x[i,*]*trans[i]
            ysig = ysig + (x[i,*]*transig[i])^2
            transtr[i]='forced'
         ENDIF
      ENDFOR
      ysig = sqrt(ysig)

      ; Call the fitting routine
      zfit = where(fit eq 1,nfit)
      xfit = x[*,zgood]
      xfit = xfit[zfit,*]
      coeff = mysvdfit(xfit,y[zgood],1,weight=1/ysig[zgood], $
                       yfit=yfit,var=var,chisq=chisq,sing=sing)
      sigma = sqrt(var)

      ; copy the fitted values back out to results
      ic = 0
      yallfit=fltarr(count_land)
      FOR i=0,nvars-1 DO BEGIN
         IF fit[i] eq 1 THEN BEGIN
            trans[i] = coeff[ic]
            transig[i] = sigma[ic]
            yallfit = yallfit + x[i,*] * trans[i]
            ic = ic+1
         ENDIF
      ENDFOR

      ; Compute some summary quantities
      scatter = mean(abs(y[zgood]-yfit))
      mnerr = mean(oerr)
      csq = sqrt(chisq/(float(goodcount-nfit)))

      mag_calc = omag
      mag_err  = oerr^2
      FOR i=0,nvars-1 DO BEGIN
         mag_calc = mag_calc + x[i,*]*trans[i]
         mag_err  = mag_err  + (x[i,*]*transig[i])^2
      ENDFOR
      mag_err  = sqrt(mag_err)

      ; Print summary
      if not first or not chiscale or edit then begin
         print,'extinction              ',trans[0],pm,transig[0],transtr[0],format=fmt
         print,'second order extinction ',trans[1],pm,transig[1],transtr[1],format=fmt
         print,'color term              ',trans[2],pm,transig[2],transtr[2],format=fmt
         print,'zero point              ',trans[3],pm,transig[3],transtr[3],format=fmt
         print,'extin/time (mag/hr/am)  ',trans[4],pm,transig[4],transtr[4],format=fmt
         if first then $
            print,'Reduced Chi             ',csq,format=fmt1 $
         else $
            print,'Reduced Chi             ',csq,'  <<   ',sigscale,'>>',format=fmt1
         print,'Scatter (mag)           ',scatter,format=fmt
         print,'Mean error (mag)        ',mnerr,format=fmt
         if saveit and ( not first or not edit ) then begin
            printf,lusum,'extinction              ',trans[0],pm,transig[0],transtr[0],format=fmt
            printf,lusum,'second order extinction ',trans[1],pm,transig[1],transtr[1],format=fmt
            printf,lusum,'color term              ',trans[2],pm,transig[2],transtr[2],format=fmt
            printf,lusum,'zero point              ',trans[3],pm,transig[3],transtr[3],format=fmt
            printf,lusum,'extin/time (mag/hr/am)  ',trans[4],pm,transig[4],transtr[4],format=fmt
            if first then $
               printf,lusum,'Reduced Chi             ',csq,format=fmt $
            else $
               printf,lusum,'Reduced Chi             ',csq,'  <<   ',sigscale,'>>',format=fmt
            printf,lusum,'Scatter (mag)           ',scatter,format=fmt
            printf,lusum,'Mean error (mag)        ',mnerr,format=fmt
         endif

         if plotit then begin
            ps = !d.name eq 'PS'
            pmult=!p.multi
            if ps then begin
               cs=0.75
               ss=0.4
            endif else begin
               cs=1.8
               ss=0.5
            endelse

            ; compute data vectors for plot that leaves only one variable.

            ; compute magnitudes correcting for second order extinction and color
            ;   leaving extinction in place.
            y_var1 = y - trans[1]*x[1,*] $
                       - trans[2]*x[2,*] $
                       - trans[3]*x[3,*] $
                       - trans[4]*x[4,*]

            ; compute magnitudes correcting for extinction and color leaving
            ;   second order extinction in place.
            y_var2 = y - trans[0]*x[0,*] $
                       - trans[2]*x[2,*] $
                       - trans[3]*x[3,*] $
                       - trans[4]*x[4,*]

            ; compute magnitudes correcting for extinction and 2nd order ext.
            ;   leaving color term in place.
            y_var3 = y - trans[0]*x[0,*] $
                       - trans[1]*x[1,*] $
                       - trans[3]*x[3,*] $
                       - trans[4]*x[4,*]

            ; compute magnitudes correcting for all but k(t)
            y_var4 = y - trans[0]*x[0,*] $
                       - trans[1]*x[1,*] $
                       - trans[2]*x[2,*] $
                       - trans[3]*x[3,*]

            if ps then begin
               !p.multi=[0,2,8,0,1]
            endif else begin
               setwin,0
               !p.multi=[0,1,4]
            endelse
            ylab1=fname+'-'+iname
            ylab2='Residual'
            ylab3='Residual (sigma)'
            ploterror,-x[0,zgood],y_var1[zgood],oerr[zgood],psym=8, $
               xtit='Airmass',ytit=ylab1,charsize=cs,symsiz=ss,tit=title
            ploterror,-x[1,zgood],y_var2[zgood],oerr[zgood],psym=8, $
               xtit='Airmass*'+cname,ytit=ylab1,charsize=cs,symsiz=ss
            ploterror,x[2,zgood],y_var3[zgood],oerr[zgood],psym=8, $
               xtit=cname,ytit=ylab1,charsize=cs,symsiz=ss
            ploterror,-x[4,zgood],y_var4[zgood],oerr[zgood],psym=8, $
               xtit='Delta Time in hours * Airmass',ytit=ylab1,charsize=cs,symsiz=ss
   ;         ploterror,std_mag[zgood],y[zgood]-yfit,oerr[zgood],psym=8, $
   ;            charsize=cs,xtit=fname+' Magnitude',ytit=ylab2,symsiz=ss

            if ps then begin
               !p.multi=[8,2,8,0,1]
            endif else begin
               setwin,1
               !p.multi=[0,1,4]
            endelse
            ploterror,-x[0,zgood],y[zgood]-yfit,oerr[zgood],psym=8, $
               xtit='Airmass',ytit=ylab2,charsize=cs,symsiz=ss,tit=title
            ploterror,-x[1,zgood],y[zgood]-yfit,oerr[zgood],psym=8, $
               xtit='Airmass*'+cname,ytit=ylab2,charsize=cs,symsiz=ss
            ploterror,x[2,zgood],y[zgood]-yfit,oerr[zgood],psym=8, $
               xtit=cname,ytit=ylab2,charsize=cs,symsiz=ss
            ploterror,-x[4,zgood],y[zgood]-yfit,oerr[zgood],psym=8, $
               xtit='Delta Time in hours * Airmass',ytit=ylab1,charsize=cs,symsiz=ss
   ;         ploterror,y[zgood]-yfit,oerr[zgood],psym=8, $
   ;            xtit='Point number',ytit=ylab2,charsize=cs,symsiz=ss

            if ps then begin
               !p.multi=[3,2,6,0,1]
            endif else begin
               setwin,2
               !p.multi=[0,1,3]
            endelse
            plot,oairm[zgood],std_col[zgood],charsize=cs,symsiz=ss,xtit='Airmass', $
                 ytit=cname,psym=8,tit=title
            xi = findgen(51)/50.0*(max(oairm)-min(oairm)) + min(oairm)
            oplot,xi,0.5/xi
            oplot,xi,1.0/xi
            oplot,xi,2.0/xi
            oplot,xi,3.0/xi
            plot,oairm[zgood],oairm[zgood]*std_col[zgood],charsize=cs,symsiz=ss, $
                 xtit='Airmass',ytit='Airmass*'+cname,psym=8
            plot,std_mag[zgood],std_col[zgood],psym=8,symsiz=ss, $
                 charsize=cs,xtit=fname,ytit=cname
            for i=0,goodcount-1 do $
               xyouts,std_mag[zgood[i]],std_col[zgood[i]],l_names[zgood[i]], $
                      align=0.0,orient=90.0,charsize=cs*0.5

            if keyword_set(other) then begin
               IF ps THEN !p.multi=[12,2,8,0,1] ELSE !p.multi=[0,1,3]
            endif else begin
               IF ps THEN !p.multi=[ 9,2,6,0,1] ELSE !p.multi=[0,1,2]
            endelse

            if not ps then setwin,3

            if keyword_set(other) then $
               plot,l_other[zgood],(y[zgood]-yfit)/oerr[zgood],psym=8, $
                  xtit=olab,ytit=ylab3,charsize=cs,tit=title,symsiz=ss
            ploterror,y[zgood]-yfit,oerr[zgood],psym=8, $
               xtit='Point number',ytit=ylab2,charsize=cs,symsiz=ss
            plot,(y[zgood]-yfit)/oerr[zgood],psym=8, $
               xtit='Point number',ytit=ylab3,charsize=cs,symsiz=ss

            if ps then begin
               x = 0.06
               y = 0.122
               dy = -0.0114
               cs=cs*0.9
               str=string('Landolt transformation for ',fname,' and ',cname,' from ')
               xyouts,x,y,str,charsize=cs,/normal
               str=string(goodcount,form='(i3)')+' stars'
               y=y+dy
               xyouts,x,y,str,charsize=cs,/normal
               str=string('extinction              ',trans[0],pm,transig[0],transtr[0],format=fmt)
               y=y+dy*2
               xyouts,x,y,str,charsize=cs,/normal
               str=string('second order extinction ',trans[1],pm,transig[1],transtr[1],format=fmt)
               y=y+dy
               xyouts,x,y,str,charsize=cs,/normal
               str=string('color term              ',trans[2],pm,transig[2],transtr[2],format=fmt)
               y=y+dy
               xyouts,x,y,str,charsize=cs,/normal
               str=string('zero point              ',trans[3],pm,transig[3],transtr[3],format=fmt)
               y=y+dy
               xyouts,x,y,str,charsize=cs,/normal
               str=string('extin/time (mag/hr/am)  ',trans[4],pm,transig[4],transtr[4],format=fmt)
               y=y+dy
               xyouts,x,y,str,charsize=cs,/normal
               if first then $
                  str=string('Reduced Chi             ',csq,format=fmt1) $
               else $
                  str=string('Reduced Chi             ',csq,'  <<   ',sigscale,'>>',format=fmt1)
               y=y+dy
               xyouts,x,y,str,charsize=cs,/normal
               str=string('Scatter (mag)           ',scatter,format=fmt)
               y=y+dy
               xyouts,x,y,str,charsize=cs,/normal
               str=string('Mean error (mag)        ',mnerr,format=fmt)
               y=y+dy
               xyouts,x,y,str,charsize=cs,/normal
            endif else begin
               setwin,0
            endelse

            !p.multi=pmult

         endif   ; End of plotting section.

         ;Print a summary table of the calibrated averages of each standard star.
         ; Compute the corrected magnitudes.
         if not first then begin
            ; Sort the list of standard names and keep only unique names.
            tmpid = l_names[zgood]
            object = tmpid[uniq(tmpid,sort(tmpid))]
            for i=0,n_elements(object)-1 do begin
               z1 = where(l_names eq object[i] and l_bad eq 0,count)
               meanerr,mag_calc[z1],mag_err[z1],avgmag,sigm,sigd
               if count ne 1 then sigd = sigd/sqrt(count-1)
               if keyword_set(resid) then begin
                  for j=0,count-1 do begin
                     if abs((y[z1[j]]-yallfit[z1[j]])/oerr[z1[j]]) gt 3.0 then $
                        sigflag = '*' else sigflag = ' '
                     if abs(y[z1[j]]-yallfit[z1[j]]) gt 0.02 then $
                        resflag = '+' else resflag = ' '
                     print,sigflag,resflag,z1[j], $
                           (y[z1[j]]-yallfit[z1[j]])/oerr[z1[j]], $
                           y[z1[j]]-yallfit[z1[j]], $
                           y[z1[j]],yallfit[z1[j]],'   ',l_names[z1[j]], $
                           omag[z1[j]],oairm[z1[j]], $
                           format='(5x,a,1x,a,2x,i5,4f8.4,a,a,2f8.4)'
                  endfor
               endif
               print,object[i]+blanks,count, $
                     avgmag,sigm,' or ',sigd,'[',std_mag[z1[0]],']', $
                     avgmag-std_mag[z1[0]],  (avgmag-std_mag[z1[0]])/sigm, $
                     (avgmag-std_mag[z1[0]])/sigd, $
                     format='(a16,1x,i3,1x,f7.4,1x,f6.4,a,f6.4,2x,a,'+ $
                            'f6.3,a,1x,f6.3,1x,f6.2,1x,f6.2)'
               if saveit then $
                  printf,lusum,object[i]+blanks,count, $
                     avgmag,sigm,' or ',sigd,'[',std_mag[z1[0]],']', $
                     avgmag-std_mag[z1[0]],  (avgmag-std_mag[z1[0]])/sigm, $
                     (avgmag-std_mag[z1[0]])/sigd, $
                     format='(a16,1x,i3,1x,f7.4,1x,f6.4,a,f6.4,2x,a,'+ $
                            'f6.3,a,1x,f6.3,1x,f6.2,1x,f6.2)'
            endfor
         endif

      endif

      if edit and first then begin
         oldbad=l_bad
         if not keyword_set(magresid) then $
            markdata,indgen(count_land),(y-yallfit)/oerr,l_bad, $
               xtitle='Point number',ytitle='Residuals (sigma)',/connect $
         else $
            markdata,indgen(count_land),y-yallfit,l_bad, $
               xtitle='Point number',ytitle='Residuals (mag)',/connect
         if long(total(l_bad eq oldbad)) ne count_land then begin
            z=where(l_bad ne oldbad and l_bad eq 1,count)
            if count ne 0 then print,'Observations removed:'
            for i=0,count-1 do begin
               print,l_names[z[i]]+blanks, $
                     omag[z[i]],oerr[z[i]],oairm[z[i]], $
                     mag_calc[z[i]],std_mag[z[i]], $
                     format='(a16,1x,f7.4,1x,f6.4,1x,f5.3,'+ $
                            '2x,"m",f7.4,1x,"s",f7.4)'
            endfor
            goto,after_edit
         endif
      endif

      if chiscale and first then begin
         sigscale=csq
         first=0
         goto,chi_scale
      endif

      if saveit then free_lun,lusum

      if keyword_set(histfile) and not (keyword_set(nosave)) then begin
         histname='/net/frakir/raid/buie/Reduced/transf_'+iname+'.'+ $
            ifnames[color1]+'m'+ifnames[color2]

         ; prepare new history line
         outstr=''
         for i=0,4 do begin
            if transtr[i] eq '' then fill=' ' else fill='f'
            outstr=outstr+string(trans[i],fill,transig[i],format='(1x,f7.4,a1,f6.4)')
         endfor

         outstr=outstr+string(jdref,format='(1x,f13.5)')

         outstr=outstr+string(goodcount,format='(1x,i3)')

         if first then $
            outstr=outstr+string(csq,format='(1x,f5.2)') $
         else $
            outstr=outstr+string(sigscale,format='(1x,f5.2)')

         ; update history file (if found).
         newobs=taginst+' '+tagdate
         repwrite,histname,newobs,newobs+outstr

      endif

      if db and not(keyword_set(nosave)) then begin
         inst=strtrim(taginst,2)
         date=strtrim(tagdate,2)
         if inst ne '' and date ne '' then begin
            puttran,inst,date,filter,color1,color2, $
               trans,transig,jdref,goodcount, $
               replaced,fit,sigscale,'default',DATABASE=database, $
               TABLENAME=tablename,/overwrite,VERBOSE=dverbose
         endif else begin
            print,self,' something wrong with data to be saved.  No DB save.'
         endelse
      endif

      bad[land] = l_bad

   endif else begin
      print,'No Landolt stars were found, unable to continue'
   endelse

end

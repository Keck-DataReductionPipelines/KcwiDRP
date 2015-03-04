;+
; NAME:
;  astsolve
; PURPOSE:
;  Solve for astrometric transformation from image to sky coordinates.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astsolve,x,y,xi,eta,terms,renormfac,bad,cxi,ceta
; INPUTS:
;  x        - Image x-coordinate  (should be "normalized" to range from -1 to 1)
;  y        - Image y-coordinate  (should be "normalized" to range from -1 to 1)
;  xi       - Standard tanget plane coordinate (should be in arcsec)
;  eta      - Standard tanget plane coordinate (should be in arcsec)
;  terms    - Which fitting terms to use (see ASTTERMS.PRO)
;  renormfac - Re-normalization factor for converting from normalized x,y to
;                the original x,y values.
;  bad      - array of flags that mark bad data on input (modified).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  EDIT - Flag, if set allows interactive bad point editing.
;  MAXPASS - Maximum number of passes allowed when weeding out bad points
;              during the fit.  (default=1000)
;  NOAUTOCLEAN - Flag, if set suppresses all attempts to weed out bad points.
;  XFLIP - Flag, if set flips x axis plot when editing bad points.
;  YFLIP - Flag, if set flips y axis plot when editing bad points.
;  FORCETERMS - String array, if provided, is a list of terms (from "terms"
;                  input variable) that will be forced to a constant
;                  value rather than being fitted.
;  FORCEVAL - This can either be an array of values or a scalar string.
;                If it is an array it must be a Nx2 array where N matches the
;                  length of FORCETERMS and is used as the forced values in
;                  the same order as FORCETERMS.  Row 0 of this array holds
;                  the forced xi term values and row 1 holds the eta terms.
;                If it is a string, this is taken to be the name of a file
;                  in the current directory that contains the fit
;                  coefficients.  This file is the same format as that
;                  used for storing normal fitting coefficients (see
;                  rdastfc.pro for details).  However, this file uses the
;                  first field to identify the name of the filter for
;                  the coefficients rather than the filename as is normal
;                  for this type of file.
;                The default value of this keyword is 'forcecoeff.dat'.
;  FORCEFILTER - String value of the name of the filter.  This is relevant
;                   only when using a file for the coefficients.  If the
;                   file contains information for a single filter then
;                   that filter name is the default value.  However, if there
;                   are two or more filters in the file then this keyword
;                   must be supplied.
;  SILENT - Flag, if set suppresses all (non-error) printed output messages.
;
; OUTPUTS:
;  cxi  - coefficients of xi fit.
;  ceta - coefficients of eta fit.
;  bad  - array of flags that mark bad data on output.
;
; KEYWORD OUTPUT PARAMETERS:
;  WORSTRESID - Worst residual in "good" data in either axis (arcsec)
;  XISCAT     - Scatter of xi fit (arcsec).
;  ETASCAT    - Scatter of eta fit (arcsec).
;  CXISIG     - Uncertainty of the xi fit coefficients.
;  CETASIG    - Uncertainty of the eta fit coefficients.
;  XICOVAR    - covariance matrix of the xi fit coefficients.
;  ETACOVAR   - covariance matrix of the eta fit coefficients.
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
;  98/03/13, Written by Marc W. Buie, Lowell Observatory
;  98/11/23, MWB, added renormfac and fixed documentation
;  2000/09/14, MWB, added WORSTRESID keyword
;  2001/08/27, MWB, changed to auto-scale errors with /edit
;  2003/06/24, MWB, changed call to MARKDATA to use new features.
;  2003/10/27, MWB, fixed subtle bug during bad point cleanup.  The test
;                       was done on O-C against the std deviation of a
;                       robust mean.  The test needs to remove the mean of
;                       the surviving sample.
;  2006/02/03, MWB, added XISCAT and ETASCAT output keywords.
;  2009/07/14, MWB, added CXISIG, CETASIG, XICOVAR, ETACOVAR output keywords.
;  2009/12/02, MWB, calling seqeunce CHANGED!  xiterms and etaterms
;                   are now consolidated into a single input argument.
;  2009/12/23, MWB, Major enhancement to add the ability to force some or
;                     all of the terms.  This mode is supported by the new
;                     keywords, FORCETERMS, FORCEVAL, and FORCEFILTER.
;
;-
pro astsolve,x,y,in_xi,in_eta,terms,renormfac,bad,cxi,ceta, $
       CETASIG=cetasig, $
       CXISIG=cxisig, $
       EDIT=edit, $
       ETACHI=etachi, $
       ETACOVAR=etacovar, $
       ETASCAT=etascat, $
       FORCEFILTER=forcefilter, $
       FORCETERMS=forceterms, $
       FORCEVAL=forceval, $
       MAXPASS=maxpass, $
       NOAUTOCLEAN=noautoclean, $
       SILENT=silent, $
       WORSTRESID=worstresid, $
       XFLIP=xflip, $
       XICHI=xichi, $
       XICOVAR=xicovar, $
       XISCAT=xiscat, $
       YFLIP=yflip

   self='ASTSOLVE: '
   if badpar(x,[4,5],1,caller=self+'(x) ') then return
   if badpar(y,[4,5],1,caller=self+'(y) ') then return
   if badpar(in_xi,[4,5],1,caller=self+'(xi) ') then return
   if badpar(in_eta,[4,5],1,caller=self+'(eta) ') then return
   if badpar(bad,[1,2,3],1,caller=self+'(bad) ') then return
   if badpar(terms,7,1,caller=self+'(terms) ',npts=nterms) then return
   if badpar(renormfac,[0,2,3,4,5],0,caller=self+'(renormfac) ') then return
   if badpar(edit,[0,1,2,3],0,caller=self+'(EDIT) ',default=0) then return
   if badpar(xflip,[0,1,2,3],0,caller=self+'(XFLIP) ',default=0) then return
   if badpar(yflip,[0,1,2,3],0,caller=self+'(YFLIP) ',default=0) then return
   if badpar(maxpass,[0,2,3],0,caller=self+'(MAXPASS) ', $
                               default=1000) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return
   if badpar(noautoclean,[0,1,2,3],0,caller=self+'(NOAUTOCLEAN) ', $
                                     default=0) then return
   if badpar(forceterms,[0,7],[0,1],caller=self+'(FORCETERMS) ', $
                                    default='') then return
   if forceterms[0] ne '' then begin
      if badpar(forceval,[0,4,5,7],[0,2],caller=self+'(FORCEVAL) ', $
                         default='forcecoeff.dat',type=forcetype) then return
      if forcetype eq 7 then begin
         fixterms=dblarr(n_elements(forceterms),2)
         if not exists(forceval) then begin
            print,self,'Forcing coefficients file ',forceval,' not found.'
            return
         endif
         rdastfc,forceval,filter,ftype,xc,yc,prot,xrenormfac,cra,cdec,photzp, $
                 xterms,coeffarr,ncoeffs,nlines,ERROR=error
         if error then begin
            print,self,'Error reading ',forceval
            return
         endif
         if nlines eq 2 then deffilter=filter[0] else deffilter=''
         if badpar(forcefilter,[0,7],0,caller=self+'(FORCEFILTER) ', $
                                    default=deffilter) then return
         if forcefilter eq '' then begin
            print,self,forceval,' has more than one filter, you must specify.'
            return
         endif
         zx=where(filter eq forcefilter and ftype eq 'xi',count)
         if count ne 1 then begin
            print,self,'Xi fit for filter ',forcefilter,' not found.'
            return
         endif
         ze=where(filter eq forcefilter and ftype eq 'eta',count)
         if count ne 1 then begin
            print,self,'Eta fit for filter ',forcefilter,' not found.'
            return
         endif
         for i=0,nterms-1 do begin
            zf=where(forceterms eq terms[i],count)
            if count eq 1 then begin
               z=where(xterms eq terms[i],count)
               if count eq 1 then begin
                  fixterms[zf[0],0] = coeffarr[zx[0],z[0]]
                  fixterms[zf[0],1] = coeffarr[ze[0],z[0]]
               endif
            endif
         endfor
      endif else begin
         if n_elements(forceval)/2 ne n_elements(forceterms) then begin
            print,self,'FORCEVAL and FORCETERMS must have the same length.'
            return
         endif
         fixterms=forceval
      endelse

      nfterms = n_elements(forceterms)
      ; initially set it so that all terms are fit, none are forced
      zforce=replicate(-1,nterms)
      zfit  =indgen(nterms)
      ; setup pointers
      for i=0,nfterms-1 do begin
         z=trimrank(where(terms eq forceterms[i],count))
         if count eq 1 then begin
            zfit[z] = -1
            zforce[z] = i
         endif
      endfor
      zf=where(zfit ge 0)
   endif else begin
      nfterms=0
      zf=indgen(nterms)
   endelse

   if nfterms gt 0 then begin

      ; remove forced terms from xi
      xi  = in_xi
      eta = in_eta
      xind=astterms(x,y,forceterms)
      for i=0,nfterms-1 do begin
         xi  = xi  - fixterms[i,0]*xind[i,*]
         eta = eta - fixterms[i,1]*xind[i,*]
      endfor

   endif else begin
      xi  = in_xi
      eta = in_eta
   endelse

   xfit=1
   efit=1
   var=1
   covar=1
   chisq=1
   sing=1

   pass=1
   repeat begin
      worstresid=0.0
      oldbad=bad

      ; Now setup independent variable vectors for x
      zg = where(bad eq 0,countgood)
      if countgood lt 2 then begin
         print,self,'Error! not enough points!'
         return
      endif
      xind=astterms(x[zg],y[zg],terms[zf])
      weight  = replicate(1.0,countgood)

      cxi = mysvdfit(xind,xi[zg],1,weight=weight,covar=covar, $
                     yfit=xfit,var=var,chisq=chisq,sing=sing)
      if sing ne 0 then $
         print,self,'pass ',strn(pass),', ',strn(sing),' singular values found.'

      robomean,xi[zg]-xfit,3.0,0.5,avg,avgdev,xiscat,vars,skew,kurt,nfinal
      cxisig   = sqrt(var)
      xicovar  = covar
      worstresid = max([worstresid,abs(xi[zg]-xfit)])
      if not silent then $
         print,'xi:  chisq=',chisq/float(countgood-1),', scatter=',xiscat, $
            ', worst resids',minmax(xi[zg]-xfit),countgood,pass, $ $
            format='(a,f6.2,a,f4.2,a,2(1x,f5.2),1x,i4," stars, pass ",i2)'
      xichi=chisq/float(countgood-1)

      if edit then begin
         setwin,10,xsize=400,ysize=800
         !p.multi=[0,1,5]
         plot,x[zg]*renormfac,xi[zg]-xfit,psym=7,symsize=0.5
         plot,y[zg]*renormfac,xi[zg]-xfit,psym=7,symsize=0.5
         plot,sqrt(x[zg]^2+y[zg]^2)*renormfac,xi[zg]-xfit,psym=7,symsize=0.5
         plot,(x[zg]*renormfac)^2,xi[zg]-xfit,psym=7,symsize=0.5
         plot,(y[zg]*renormfac)^2,xi[zg]-xfit,psym=7,symsize=0.5
      endif

      if not noautoclean then begin
         zbad=where(abs(xi[zg]-xfit-avg) gt 3.0*xiscat,countbad)
         if countbad ne 0 then bad[zg[zbad]]=1
      endif

      ; Eta fit setup
      zg = where(bad eq 0,countgood)
      if countgood eq 0 then begin
         print,self,'no good points going into eta fit'
         worstresid=0.0
         return
      endif
      xind=astterms(x[zg],y[zg],terms[zf])
      weight = replicate(1.0,countgood)

      ; Fit to eta coordinate
      ceta = mysvdfit(xind,eta[zg],1,weight=weight,covar=covar, $
                      yfit=efit,var=var,chisq=chisq,sing=sing)
      if sing ne 0 then $
         print,self,'pass ',strn(pass),', ',strn(sing),' singular values found.'

      robomean,eta[zg]-efit,3.0,0.5,avg,avgdev,etascat,vars,skew,kurt,nfinal
      cetasig = sqrt(var)
      etacovar = covar
      worstresid = max([worstresid,abs(eta[zg]-efit)])
      if not silent then $
         print,'eta: chisq=',chisq/float(countgood-1),', scatter=',etascat, $
            ', worst resids',minmax(eta[zg]-efit),countgood,pass, $ $
            format='(a,f6.2,a,f4.2,a,2(1x,f5.2),1x,i4," stars, pass ",i2)'
      etachi=chisq/float(countgood-1)
      if edit then begin
         setwin,11,xsize=400,ysize=800
         plot,x[zg]*renormfac,eta[zg]-efit,psym=7,symsize=0.5
         plot,y[zg]*renormfac,eta[zg]-efit,psym=7,symsize=0.5
         plot,sqrt(x[zg]^2+y^2)*renormfac,eta[zg]-efit,psym=7,symsize=0.5
         plot,(x[zg]*renormfac)^2,eta[zg]-efit,psym=7,symsize=0.5
         plot,(y[zg]*renormfac)^2,eta[zg]-efit,psym=7,symsize=0.5
         !p.multi=0
      endif

      if not noautoclean then begin
         zbad=where(abs(eta[zg]-efit-avg) gt 3.0*etascat,countbad)
         if countbad ne 0 then bad[zg[zbad]]=1
      endif

      ; Manual edit (if requested)
      if edit then begin
         zchg = where(bad ne oldbad, countchange)
         if countchange eq 0 then $
            print,'All points kept on this pass.'
         newbad=bad[zg]
;         xirange=max(xi[zg])-min(xi[zg])
;         etarange=max(eta[zg])-min(eta[zg])
;         range = max([xirange,etarange])
;         errbar = sqrt((xi[zg]-xfit)^2+(eta[zg]-efit)^2)
;         if (max(errbar) lt 0.05*range) then $
;            errbar = errbar/max(errbar)*0.05*range
;         markdata,xi[zg],eta[zg],errbar, $
;            newbad,xtitle='xi (arcsec)',ytitle='eta (arcsec)', $
;            xsize=600,ysize=600,xflip=xflip,yflip=yflip
         markdata,xi[zg],abs(xi[zg]-xfit),eta[zg],abs(eta[zg]-efit), $
            newbad,xtitle='xi (arcsec)',ytitle='eta (arcsec)', $
            xsize=600,ysize=600,xflip=xflip,yflip=yflip,plottype=12
         bad[zg]=newbad
      endif

      zg = where(bad eq 0,countgood)

      ; Check to see if any additional bad points were flagged.
      zchg = where(bad ne oldbad, countchange)
      pass=pass+1
   endrep until countchange eq 0 or pass gt maxpass

   ; Put all the terms back together if there were forced terms
   if nfterms gt 0 then begin
      outxi  = dblarr(nterms)
      outxisig = dblarr(nterms)
      outeta = dblarr(nterms)
      outetasig = dblarr(nterms)
      z=where(zfit ge 0)
      outxi[z]  = cxi
      outxisig[z]  = cxisig
      outeta[z] = ceta
      outetasig[z] = cetasig
      for i=0,nterms-1 do begin
         if zforce[i] ge 0 then begin
            outxi[i]  = fixterms[zforce[i],0]
            outeta[i] = fixterms[zforce[i],1]
         endif
      endfor
      cxi  = outxi
      cxisig  = outxisig
      ceta = outeta
      cetasig = outetasig
   endif

end

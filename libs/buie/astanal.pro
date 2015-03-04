;+
; NAME:
;  astanal
; PURPOSE:   (one line only)
;  Analyze and provide summary plots and averages for one night of astrometry
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astanal,fnfit,fncenter
; INPUTS:
;  fnfit - Fit coefficient file to read, default is fitcoeff.dat
;  fncenter - Centers file to read, default is centers.dat
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PATH      - Location of the images, default is the current directory.
;  KEYFILE   - header keyword correspondence file (see loadkeys.pro)
;  NODISPLAY - Flag, if set suppresses all plots
;  SAVEMEAN  - Flag, if set will cause the program to save the filter by
;               filter averages of the coefficients into a file named
;               forcecoeff.dat
;  SILENT    - Flag, if set suppresses all printed output
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;  CONVERGED - Output flag, if set then the CONST term of all of the fits
;                never exceeds 1 milli-arcsecond
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2009/12/30
;  2010/11/22, MWB, added PATH keyword
;-
pro astanal,fnfit,fncenter,NODISPLAY=nodisplay,SILENT=silent, $
       KEYFILE=keyfile,SAVEMEAN=savemean,CONVERGED=converged,PATH=path

   self='astanal: '
   if badpar(fnfit,[0,7],0,caller=self+'(fnfit) ', $
                          default='fitcoeff.dat') then return
   if badpar(fncenter,[0,7],0,caller=self+'(fncenter) ', $
                          default='centers.dat') then return
   if badpar(keyfile,[0,7],0,CALLER=self+'(KEYFILE) ', $
                              default='[[DEFAULT]]') then return
   if badpar(savemean,[0,1,2,3],0,CALLER=self+'(SAVEMEAN) ', $
                              default=0) then return
   if badpar(nodisplay,[0,1,2,3],0,CALLER=self+'(NODISPLAY) ', $
                              default=0) then return
   if badpar(silent,[0,1,2,3],0,CALLER=self+'(SILENT) ', $
                              default=0) then return
   if badpar(path,[0,7],0,CALLER=self+'(PATH) ', $
                              default='') then return

   if path ne '' then path=addslash(path)

   loadkeys,keyfile,keylist

   if not exists(fnfit) then begin
      print,self,'Error, fit coefficient file, ',fnfit,' not found.'
      return
   endif
   rdastfc,fnfit,ffn,ftype,xc,yc,prot,renormfac,cra,cdec,photzp, $
           terms,coeffarr,ncoeffs,nlines

   terms=strupcase(terms)

   ze = where(ftype eq 'eta',nze)
   effn       = ffn[ze]
   exc        = xc[ze]
   eyc        = yc[ze]
   eprot      = prot[ze]
   erenormfac = renormfac[ze]
   ecra       = cra[ze]
   ecdec      = cdec[ze]
   ephotzp    = photzp[ze]
   ecoeffarr  = coeffarr[ze,*]

   zx = where(ftype eq 'xi',nzx)
   xffn       = ffn[zx]
   xxc        = xc[zx]
   xyc        = yc[zx]
   xprot      = prot[zx]
   xrenormfac = renormfac[zx]
   xcra       = cra[zx]
   xcdec      = cdec[zx]
   xphotzp    = photzp[zx]
   xcoeffarr  = coeffarr[zx,*]

   ; Assume that effn and xffn are the same.  This true for a valid file.
   ;   If not true, the file is corrupted in some way.
   if nze ne nzx then begin
      print,self,'Error!  Something is wrong with the fit coefficient file.'
      return
   endif

   ; collect the filter and image mid-time for each frame
   filter=replicate('unk',nze)
   jdmid =dblarr(nze)
   for i=0,nze-1 do begin
      hdr=headfits(path+effn[i])
      parsekey,hdr,keylist,info
      filter[i] = info.filter
      jdmid[i]  = info.jd
   endfor
   time=(jdmid - jdmid[0])*24.0

   fillist = filter[uniq(filter,sort(filter))]
   nfils=n_elements(fillist)
   if not silent then print,strn(nfils),' filters in this dataset'

   z1 = trimrank(where(terms eq 'X'))
   z2 = trimrank(where(terms eq 'Y'))

   scale_eta = sqrt(ecoeffarr[*,z1]^2 + ecoeffarr[*,z2]^2)/erenormfac
   scale_xi  = sqrt(xcoeffarr[*,z1]^2 + xcoeffarr[*,z2]^2)/xrenormfac
   scale_eta = trimrank(scale_eta)
   scale_xi  = trimrank(scale_xi)
   eta_rot   = atan(ecoeffarr[*,z2],ecoeffarr[*,z1])*180.0d0/!dpi
   xi_rot    = atan(xcoeffarr[*,z2],xcoeffarr[*,z1])*180.0d0/!dpi
   eta_rot   = trimrank(eta_rot)
   xi_rot    = trimrank(xi_rot)

   if not exists(fncenter) then begin
      print,self,'Warning, fit centers file, ',fnfit,' not found.'
      goodcenter=0
   endif else goodcenter=1
   if goodcenter then begin
      readcol,fncenter,cfn,ccra,ccdec,crahdr,cdechdr,craoff,cdecoff, $
         format='a,a,a,a,a,d,d'
      ; Link up the center offsets with the information from rdastfc
      raoff=replicate(1.0d20,nze)
      decoff=replicate(1.0d20,nze)
      for i=0,nze-1 do begin
         z=where(effn[i] eq cfn,count)
         raoff[i] = craoff[z[0]]
         decoff[i] = cdecoff[z[0]]
      endfor
      ; convert to arc-sec
      raoff = raoff*!radeg*3600.0
      decoff = decoff*!radeg*3600.0
   endif

   blanks='     '

   ; Setup the output arrays for the means
   mfil = strarr(2*nfils)
   mftype = strarr(2*nfils)
   mcoeffarr = dblarr(2*nfils,ncoeffs)

   ; These are dummy arrays
   mxc = replicate(xc[0],2*nfils)
   myc = replicate(yc[0],2*nfils)
   mprot = replicate(prot[0],2*nfils)
   mrenormfac = replicate(renormfac[0],2*nfils)
   mcra = dblarr(2*nfils)
   mcdec = dblarr(2*nfils)
   mphotzp = dblarr(2*nfils)

   bad=bytarr(nze)
   converged=1

   k=0
   for i=0,nfils-1 do begin
      z=where(filter eq fillist[i],nobs)
      if not silent then begin
         print,''
         print,'Filter ',fillist[i],' with a total of ',strn(nobs),' observations.'
      endif
      tmpbad=bad[z]

      ; First pass for just weeding out bad points
      robomean,scale_eta[z],3.0,0.5,avg,avgdev,stdev,stdmean=stdmean,bad=tmpbad
      robomean,scale_xi[z],3.0,0.5,avg,avgdev,stdev,stdmean=stdmean,bad=tmpbad
      robomean,eta_rot[z],3.0,0.5,avg,avgdev,stdev,stdmean=stdmean,bad=tmpbad
      robomean,xi_rot[z],3.0,0.5,avg,avgdev,stdev,stdmean=stdmean,bad=tmpbad

      for j=0,ncoeffs-1 do begin
         robomean,ecoeffarr[z,j],3.0,0.5,avg,avgdev,stdev,bad=tmpbad
         robomean,xcoeffarr[z,j],3.0,0.5,avg,avgdev,stdev,bad=tmpbad
      endfor

      fmt='(a,a,f14.8,a,f10.8,1x,f10.8," arcsec/pixel")'
      robomean,scale_eta[z],3.0,0.5,avg,avgdev,stdev,stdmean=stdmean,bad=tmpbad
      if not silent then $
         print,blanks,'Eta derived scale ',avg,' +/- ',stdev,stdmean,format=fmt

      robomean,scale_xi[z],3.0,0.5,avg,avgdev,stdev,stdmean=stdmean,bad=tmpbad
      stats=moment(scale_xi[z])
      if not silent then $
         print,blanks,'Xi  derived scale ',avg,' +/- ',stdev,stdmean,format=fmt

      fmt='(a,a,f13.8,a,f10.8,1x,f10.8," deg")'
      robomean,eta_rot[z],3.0,0.5,avg,avgdev,stdev,stdmean=stdmean,bad=tmpbad
      if not silent then $
         print,blanks,'Eta Position Angle ',avg,' +/- ',stdev,stdmean,format=fmt

      robomean,xi_rot[z],3.0,0.5,avg,avgdev,stdev,stdmean=stdmean,bad=tmpbad
      if not silent then $
         print,blanks,'Xi  Position Angle ',avg,' +/- ',stdev,stdmean,format=fmt

      fmt='(5x,a,a5,1x,f14.8," +/- ",f11.8,1x,f10.8)'
      for j=0,ncoeffs-1 do begin
         robomean,ecoeffarr[z,j],3.0,0.5,avg,avgdev,stdev, $
            stdmean=stdmean,bad=tmpbad
         if not silent then $
            print,'Eta ',terms[j],avg,stdev,stdmean,format=fmt
         mcoeffarr[2*i,j] = avg
         if terms[j] eq 'CONST' and stdev gt 0.001 then converged=0
         robomean,xcoeffarr[z,j],3.0,0.5,avg,avgdev,stdev, $
            stdmean=stdmean,bad=tmpbad
         if not silent then $
            print,'Xi  ',terms[j],avg,stdev,stdmean,format=fmt
         mcoeffarr[2*i+1,j] = avg
         if terms[j] eq 'CONST' and stdev gt 0.001 then converged=0
      endfor

      bad[z]=tmpbad

      zb=where(bad eq 1 and filter eq fillist[i],nbad)
      if not silent then $
         print,'     A total of ',strn(nbad),' observations were removed.'
      if not silent and nbad gt 0 then begin
         print,effn[zb]
      endif

      ; save final results to the output arrays
      mfil[k:k+1] = fillist[i]
      mftype[k:k+1] = ['eta','xi ']
      k += 2

   endfor
   if not converged and not silent then $
      print,'***** Solutions not fully converged! *****'

   if goodcenter and not nodisplay then begin
      setwin,0
      plot,raoff,decoff,/nodata, $
         xtitle='RA offset (arcsec, actual-header)', $
         ytitle='Dec offset (arcsec, actual-header)'
      for i=0,nfils-1 do begin
         z=where(filter eq fillist[i] and bad eq 0,count)
         if count gt 0 then $
            oplot,raoff[z],decoff[z],psym=(i mod 5) + 4
         zb=where(filter eq fillist[i] and bad eq 1,countb)
         if countb gt 0 then $
            oplot,raoff[zb],decoff[zb],psym=(i mod 5) + 4,color='0000ff'xl
      endfor
   endif

   z=where(bad eq 0,count)
   zb=where(bad eq 1,countb)
   xtit='Time (hours from first point)'

   if not nodisplay then begin
      setwin,2
      !p.multi=[0,2,ncoeffs]
      for i=0,ncoeffs-1 do begin
         plot,time[z],xcoeffarr[z,i],ytitle=terms[i],title='Xi',charsize=1.8, $
              xtitle=xtit,psym=8,symsize=0.3
         if countb ne 0 then $
            oplot,time[zb],xcoeffarr[zb,i],psym=8,symsize=0.3,color='0000ff'xl

         plot,time[z],ecoeffarr[z,i],ytitle=terms[i],title='Eta',charsize=1.8, $
              xtitle=xtit,psym=8,symsize=0.3
         if countb ne 0 then $
            oplot,time[zb],ecoeffarr[zb,i],psym=8,symsize=0.3,color='0000ff'xl

      endfor

      setwin,1
      !p.multi=[0,2,2]
      plot,time[z],scale_eta[z],psym=8,symsize=0.5, $
           xtitle=xtit,ytitle='Eta scale ("/pix)'
      plot,time[z],eta_rot[z],psym=8,symsize=0.5, $
           xtitle=xtit,ytitle='Eta PA (deg)'
      plot,time[z],scale_xi[z],psym=8,symsize=0.5, $
           xtitle=xtit,ytitle='Xi scale ("/pix)'
      plot,time[z],xi_rot[z],psym=8,symsize=0.5, $
           xtitle=xtit,ytitle='Xi PA (deg)'
      !p.multi=0

   endif

   if savemean then begin
      wrastfc,'forcecoeff.dat',mfil,mftype,mxc,myc,mprot,mrenormfac, $
              mcra,mcdec,mphotzp,terms,mcoeffarr
   endif

end

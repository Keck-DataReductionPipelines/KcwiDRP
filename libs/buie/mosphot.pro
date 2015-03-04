;+
; NAME:
;  mosphot
; PURPOSE:
;  Plot Mosaic astrometric solution for DES data and do photometric calibration.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  mosphot,root,first,last
; INPUTS:
;  root - root of data area and file names
;  first - number of first image to plot
;  last  - number of last image to plot
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  KPNO - If set, generates overview plot of plane-of-sky orientation for
;           the Kitt Peak Mosaic camera.  Otherwise, the orientation for
;           Cerro Tololo is used.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2008/07/31
;  2009/08/05, MWB, modified for new rdastfc and astxy2rd versions.
;  2009/12/30, MWB, modified for new fitting coefficients methodology
;-
pro mosphot,root,first,last,KPNO=kpno,BINFAC=binfac

   self='MOSPLOT '
   if badpar(root,7,0,caller=self+'(root) ') then return
   if badpar(first,[2,3],0,caller=self+'(first) ') then return
   if badpar(last,[2,3],0,caller=self+'(last) ') then return
   if badpar(kpno,[0,1,2,3],0,caller=self+'(KPNO) ',default=0) then return
   if badpar(binfac,[0,1,2,3],0,caller=self+'(BINFAC) ',default=2) then return

   ddir='/net/amber/raid1/buie/rawfits/des/'+root+'/'

   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renorm, $
      cra,cdec,photzp,terms,coeffarr,ncoeffs,nlines

   dbname='phot'
   table='data'

   lastans='a'
   defaultans=''

   openmysql,dblun,dbname

   for num=first,last do begin

      imnameroot = root+'.'+strn(num,format='(i3.3)')

      ; Figure out the name of this field
      cmd='select name from des.des_obs where image='+quote(imnameroot)+';'
      mysqlquery,dblun,cmd,fieldname,ngood=nlines
      if nlines eq 1 then fieldname=fieldname[0] $
      else fieldname='unknown '+strn(nlines)

      ; Get the current zero-point calibration information.  There should
      ;   already be a record in the database for all chips but there may
      ;   well be missing fields (they will be marked as NULL)
      cmd='select ccd,airmass,fwhm,imaglimit,USNOzp,SDSSzp,sdsszperr,' + $
          'nstars,catalog,rMagLimit,maglimit,skylevel '+ $
          ' from des.photcal where image='+quote(imnameroot)+ $
          ' order by ccd;'
      mysqlquery,dblun,cmd,ccd,airmass,fwhm,imaglimit,usnozp,sdsszp,sdsszperr, $
         nstars,catalog,rmaglimit,maglimit,skylevel, $
         format='i,f,f,f,f,a,a,a,a,a,f,f'

      z=where(sdsszp eq 'NULL',count)
      if count eq 0 then begin
         dodiffzp = 1
         diffzp = float(sdsszp)-float(sdsszp[0])
         zperr = float(sdsszperr)
      endif else begin
         dodiffzp = 0
      endelse

      npts=10

      lside = findgen(npts+1)/npts * 2048.0
      sside = findgen(npts+1)/npts * 1024.0
      strut = fltarr(npts+1)

      ; This a vector pair of points that trace out the perimeter of a CCD.
      xpos=[strut,sside,     strut+1024,reverse(sside)]
      ypos=[lside,strut+2048,reverse(lside),     strut]

      a_dx  = fltarr(n_elements(xpos),8)
      a_dy  = fltarr(n_elements(xpos),8)
      a_ra  = dblarr(n_elements(xpos),8)
      a_dec = dblarr(n_elements(xpos),8)
      valid = intarr(8)

      print,'Photometric calibration of image ',imnameroot, $
         ' on field ',fieldname,', airmass=',airmass[0], $
         format='(a,a,a,a,a,f4.2)'
      infolist=strarr(9)
      infolist[0]='C# FWHM Ilim  Uzp Ulim   Cat    Ns Szp  Slim    Dfzp    zperr'

      for i=1,8 do begin

         exttag = 'x'+strn(i)
         imname = imnameroot+exttag

         info = string(ccd[i-1],fwhm[i-1],imaglimit[i-1],usnozp[i-1], $
            maglimit[i-1],format='(i2,1x,f4.1,1x,f4.1,2x,f3.1,1x,f4.1)')
         if catalog[i-1] eq 'NULL' then info += '   ----' $
         else info += '   '+strmid(catalog[i-1]+'     ',0,5)
         if nstars[i-1] eq 'NULL' then info += '   --' $
         else info += string(long(nstars[i-1]),format='(1x,i3)')
         if sdsszp[i-1] eq 'NULL' then info += '  --' $
         else info += string(float(sdsszp[i-1]),format='(1x,f4.2)')
         if rmaglimit[i-1] eq 'NULL' then info += '   --  ' $
         else info += string(float(rmaglimit[i-1]),format='(1x,f5.2)')
         if not dodiffzp then info += '      --' $
         else info += string(diffzp[i-1],format='(2x,f6.3)')
         if not dodiffzp then info += '    --' $
         else info += string(zperr[i-1],format='(2x,f6.3)')
         infolist[i] = info

         ; get pointer into the astrometric solutions for this image:ccd
         z = where(imname eq ffn,count)

         if count ne 0 then begin

            valid[i-1] = 1
            ; Get the index to the coefficients
            if ftype[z[0]] eq 'eta' then begin
               floce = z[0]
               flocx = z[1]
            endif else begin
               floce = z[1]
               flocx = z[0]
            endelse

            ; Extract the coefficients
            cxi  = trimrank(coeffarr[flocx,*])
            ceta = trimrank(coeffarr[floce,*])

            ; Constants needed for converting to xi,eta
            if renorm[flocx] le 0.0 then begin
               nx = 1024.0
               ny = 2048.0
               renormfac=sqrt(float(nx)^2+float(ny)^2)
            endif else begin
               renormfac=renorm[flocx]
            endelse

            info = { $
               renormfac: renormfac, $
               cxi: cxi, $
               ceta: ceta, $
               terms: terms, $
               prot: prot[flocx], $
               renormfac: renorm[flocx],$
               xcref: xc[flocx], $  ; xi and eta values are always the same
               ycref: yc[flocx], $
               raref: cra[flocx], $
               decref: cdec[flocx] $
               }

            ; compute ra,dec
            astxy2rd,xpos,ypos,info,ra,dec,dx=dx,dy=dy,xi=xi,eta=eta,/full

            ; store in master array
            a_dx[*,i-1] = dx
            a_dy[*,i-1] = dy

            ; save ra,dec in master array
            a_ra[*,i-1]  = ra
            a_dec[*,i-1] = dec

         endif
      endfor ; loop over chips

      zv=where(valid eq 1,countv)

      if countv eq 0 then begin
         print,'no valid solutions ',imnameroot
      endif else begin

         rarange=minmax(a_ra[*,zv])
         decrange=minmax(a_dec[*,zv])

         raref = mean(rarange)
         decref = mean(decrange)

         cmd=['select ra,decl,mag,err from data', $
             ' where objname='+quote('CAT:DES')+' and']
         cmd2=['select ra,decl,gmag,gerr,rmag,rerr from sdss where']

         cmdtail=''
         if rarange[1]-rarange[0] lt !pi then begin
            cmdtail = [ cmdtail, $
                    'ra >= '+strcompress(string(rarange[0]),/remove_all), $
                    'and ra <= '+strcompress(string(rarange[1]),/remove_all)]
         endif else begin
            cmdtail = [ cmdtail, $
                   '(ra <= '+strcompress(string(rarange[1]),/remove_all), $
                   'or ra <= '+strcompress(string(rarange[1]),/remove_all)+')']
         endelse

         cmdtail = [cmdtail, $
                'and decl >= '+strcompress(string(decrange[0]),/remove_all), $
                'and decl <= '+strcompress(string(decrange[1]),/remove_all)]

         cmd  = [ cmd,  cmdtail[1:*], ';' ]
         cmd2 = [ cmd2, cmdtail[1:*], $
                  'and gmag-rmag>0.35 and gmag-rmag<0.8;' ]

         mysqlquery,dblun,cmd,ra,dec,mag,err,format='d,d,f,f'
         mysqlquery,dblun,cmd2,ra2,dec2,gmag,gerr,rmag,rerr,format='d,d,f,f,f,f'
print,strn(n_elements(ra)),' DES cal stars'
print,strn(n_elements(ra2)),' SDSS stars'

         astrd2sn,a_ra,a_dec,raref,decref,a_xi,a_eta ; perimeters
         astrd2sn,ra,dec,raref,decref,xi,eta ; DES cal stars
         astrd2sn,ra2,dec2,raref,decref,xi2,eta2 ; SDSS stars

         xi  = xi  * 180.0/!dpi * 3600.0
         eta = eta * 180.0/!dpi * 3600.0
         a_xi  = a_xi  * 180.0/!dpi * 3600.0
         a_eta = a_eta * 180.0/!dpi * 3600.0
         xi2  = xi2  * 180.0/!dpi * 3600.0
         eta2 = eta2 * 180.0/!dpi * 3600.0

         setwin,1
         if kpno then begin
            xr=[1200,-1200]
            yr=[1200,-1200]
            xtitle='(N)  eta  (S)'
            ytitle='(E)  xi  (W)'
         endif else begin
            xr=[-1200,1200]
            yr=[-1200,1200]
            xtitle='(S)  eta  (N)'
            ytitle='(W)  xi  (E)'
         endelse
         plot,[0],[1],/nodata,xr=xr,yr=yr,/iso,title=imnameroot+' '+fieldname, $
            ytitle=ytitle,xtitle=xtitle

         ; Loop over all the valid solutions, for each one plot perimeter,
         ;    CCD number, and chip corner
         newns = intarr(8)
         newzp = replicate(-999.,8)
         newzperr = replicate(-999.,8)
         satlim = replicate(-999.,8)
         for i=0,countv-1 do begin

            ; grab the source data list
            exttag = 'x'+strn(zv[i]+1)
            fnsrd=imnameroot+'.srd'+exttag
            if exists(fnsrd) then begin
               data=readfits(fnsrd,hdr)
               ft='.fits'
               if not exists(ddir+imnameroot+ft) then ft=''
               image=readfits(ddir+imnameroot+ft,exten=zv[i]+1)
               xloc = round(data[*,0])
               yloc = round(data[*,1])
               ipeak=image[xloc,yloc]-skylevel[zv[i]]
               imag = data[*,3]
               ierr = data[*,4]

               idx=sort(imag)
               lpeak=alog10(ipeak > 1)
               magstep=0.25
               magspread=max(imag)-min(imag)
               magsyn=findgen(ceil(magspread/magstep))*magstep+min(imag)
               lowess,imag[idx],lpeak[idx],magstep,lpeaksmoo,newx=magsyn
               coeff=goodpoly(magsyn,lpeaksmoo,1,2.0,yfit,goodx,goody)
               goodmax = 10.0^(goody[0])-2000.0 ; 2000 should be variablized
               goodval = alog10(goodmax)
               syn = poly(magsyn,coeff)
               sdiff = syn-goodval
               zz=where(sdiff le 0)
               satlim[zv[i]]=magsyn[zz[0]]
               infolist[zv[i]+1] += string(satlim[zv[i]],format='(2x,f6.1)')

if zv[i] eq 6 then begin
   savex = round(data[*,0])
   savey = round(data[*,1])
   peak=image[savex,savey]-skylevel[zv[i]]
   savemag = data[*,3]
   saveerr = data[*,4]
endif

               zg=where(data[*,3] gt satlim[zv[i]],countg)
               if countg gt 0 then begin
                  photcal,data[zg,5],data[zg,6],data[zg,3],data[zg,4], $
                     ra,dec,mag,err,ngood,photzp,photzperr,magrange,/nodisplay
print,fnsrd,fix(ngood),countg,photzp,photzperr,magrange
                  if ngood gt 0 then begin
                     newns[zv[i]] = ngood
                     newzp[zv[i]] = photzp
                     newzperr[zv[i]] = photzperr
                     infolist[zv[i]+1] +=  $
                        string(newns[zv[i]],format='(2x,i3)') + $
                        string(newzp[zv[i]],format='(1x,f6.2)') + $
                        string(newzperr[zv[i]],format='(1x,f6.2)')
                  endif
               endif

            endif

            txi=a_xi[*,zv[i]]
            teta=a_eta[*,zv[i]]
            oplot,teta,txi
            xyouts,mean(minmax(teta)),mean(minmax(txi)),strn(zv[i]+1)
            oplot,[teta[0]],[txi[0]],psym=8,color='ff00ff'xl

         endfor ; loop over valid CCDs

         ; overplot the SDSS stars
         oplot,xi2,eta2,psym=3,color='0000ff'xl

         ; overplot the DES calibration stars
         oplot,xi,eta,psym=3

      endelse

      setwin,2
      idx=sort(savemag)
      lpeak=alog10(peak > 1)
      magstep=0.25
      magsyn=findgen(ceil((max(savemag)-min(savemag))/magstep))*magstep+min(savemag)
      lowess,savemag[idx],lpeak[idx],magstep,lpeaksmoo,newx=magsyn
;      coeff=goodpoly(savemag,lpeak,1,2.0,yfit,goodx,goody)
      coeff=goodpoly(magsyn,lpeaksmoo,1,2.0,yfit,goodx,goody)
      plot,savemag,alog10(peak),psym=3,xr=maxmin(savemag)
;      oplot,savemag[idx],yfit[idx]
      oplot,magsyn,yfit
      oplot,goodx,goody,color='ff00ff'xl,thick=3
;      oplot,magsyn,lpeaksmoo,color='0000ff'xl
print,max(goody),10.0^max(goody)
goodmax = 10.0^(goody[0])-2000.0
goodval = alog10(goodmax)
print,goody[0],10.0^(goody[0]),goodmax,goodval
syn = poly(magsyn,coeff)
sdiff = syn-goodval
zz=where(sdiff le 0)
print,magsyn[zz[0]]

      print,infolist

      a=''
      prompt=strn(num,format='(i3.3)')
      if defaultans ne '' then prompt=prompt+':'+defaultans
      prompt=prompt+'>'
      read,a,format='(a)',prompt=prompt
      a=strcompress(strlowcase(a),/remove_all)
      if strmid(a,0,1) eq 'q' then break
      if a eq '' then a=defaultans

   endfor

   free_lun,dblun

end

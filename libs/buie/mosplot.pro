;+
; NAME:
;  mosplot
; PURPOSE:
;  Plot Mosaic astrometric solution for DES data and do astrometry.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  mosplot,root,first,last,ext
; INPUTS:
;  root - root of data area and file names
;  first - number of first image to plot
;  last  - number of last image to plot
;  ext   - (single) extension number to work
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  KPNO - If set, generates overview plot of plane-of-sky orientation for
;           the Kitt Peak Mosaic camera.  Otherwise, the orientation for
;           Cerro Tololo is used.
;  SKIP - If set, quietly skips frames with no valid solution.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/08/28
;  2001/10/09, MWB, added KPNO keyword flag
;  2002/04/16, MWB, changed the command option structure and added help
;  2002/04/25, MWB, changed the default option operation to be adaptive
;  2002/08/11, MWB, added SKIP keyword flag
;  2008/10/31, MWB, changed to use rdref
;-
pro mosplot,root,first,last,ext,KPNO=kpno,SKIP=skip,BINFAC=binfac

   self='MOSPLOT '
   if badpar(root,7,0,caller=self+'(root) ') then return
   if badpar(first,[2,3],0,caller=self+'(first) ') then return
   if badpar(last,[2,3],0,caller=self+'(last) ') then return
   if badpar(ext,[2,3],0,caller=self+'(ext) ') then return
   if badpar(kpno,[0,1,2,3],0,caller=self+'(KPNO) ',default=0) then return
   if badpar(skip,[0,1,2,3],0,caller=self+'(SKIP) ',default=0) then return
   if badpar(binfac,[0,1,2,3],0,caller=self+'(BINFAC) ',default=2) then return

;   root='010818'

   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr, $
      coeffarr,ncoeffs,nlines

   lastans='a'
   defaultans=''

   for num=first,last do begin

checkit:
      imnameroot = root+'.'+strn(num,format='(i3.3)')

      nx = 1024.0
      ny = 2048.0
      renormfac=sqrt(float(nx)^2+float(ny)^2)

      npts=10

      lside = findgen(npts+1)/npts * 2048.0
      sside = findgen(npts+1)/npts * 1024.0
      strut = fltarr(npts+1)

      xpos=[strut,sside,     strut+1024,reverse(sside)]
      ypos=[lside,strut+2048,reverse(lside),     strut]

      a_dx  = fltarr(n_elements(xpos),8)
      a_dy  = fltarr(n_elements(xpos),8)
      a_ra  = dblarr(n_elements(xpos),8)
      a_dec = dblarr(n_elements(xpos),8)
      valid = intarr(8)

      for i=1,8 do begin
         exttag = 'x'+strn(i)
         imname = imnameroot+exttag

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
            cxi  = coeffarr[flocx,0:ncoeffs[flocx]-1]
            ceta = coeffarr[floce,0:ncoeffs[floce]-1]
            cxi  = cxi[*]
            ceta = ceta[*]
            xiterms  = flagarr[flocx,*]
            etaterms = flagarr[floce,*]
            xiterms  = xiterms[*]
            etaterms = etaterms[*]

            dx=(xpos-xc[flocx])/renormfac
            dy=(ypos-yc[flocx])/renormfac

            a_dx[*,i-1] = dx
            a_dy[*,i-1] = dy

            xi = asteval(dx,dy,cxi,xiterms)/3600.0d0*!dpi/180.0d0
            eta= asteval(dx,dy,ceta,etaterms)/3600.0d0*!dpi/180.0d0

            astsn2rd,xi,eta,cra[flocx],cdec[floce],ra,dec

            a_ra[*,i-1]  = ra
            a_dec[*,i-1] = dec

            dra  = (ra-cra[flocx])*180.0/!dpi*cos(dec) * 20.0
            ddec = (dec-cdec[floce])*180.0/!dpi * 20.0

         endif
      endfor

      z=where(valid eq 1,count)

      if count eq 0 then begin
         if not skip then print,'no valid solutions ',imnameroot
         found=1   ; this logically strange but it works.
         goodref=2
      endif else begin

;         setwin,0
;         plot,[0],[1],/nodata,xr=[-1,1],yr=[-1,1],/iso,xtitle='X',ytitle='Y',title=imnameroot
;         for i=0,count-1 do begin
;            dx=a_dx[*,z[i]]
;            dy=a_dy[*,z[i]]
;            oplot,dx,dy
;            xyouts,mean(minmax(dx)),mean(minmax(dy)),strn(z[i]+1)
;         endfor

         rarange=minmax(a_ra[*,z])
         decrange=minmax(a_dec[*,z])

         raref = mean(rarange)
         decref = mean(decrange)

         astrd2sn,a_ra,a_dec,raref,decref,xi,eta

         xi  = xi  * 180.0/!dpi * 3600.0
         eta = eta * 180.0/!dpi * 3600.0

         reffile = 'Refstars/'+string(num,format='(i3.3)')+'x'+strn(ext)+'.ref'
         if exists(reffile) then begin
            goodref=1
            rdref,reffile,ref,referr
            if referr ne 0 then goodref=0
            astrd2sn,ref.ra,ref.dec,raref,decref,rxi,reta
            rxi  = rxi  * 180.0/!dpi * 3600.0
            reta = reta * 180.0/!dpi * 3600.0
            defaultans=''
         endif else begin
            defaultans=lastans
            goodref=0
         endelse

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
         plot,[0],[1],/nodata,xr=xr,yr=yr,/iso,title=imnameroot, $
            ytitle=ytitle,xtitle=xtitle
         if (goodref) then $
            oplot,reta,rxi,color='00ffff'xl,psym=3
         found=0
         for i=0,count-1 do begin
            txi=xi[*,z[i]]
            teta=eta[*,z[i]]
            if ext eq z[i]+1 then begin
               oplot,teta,txi,color='0000ff'xl
               xyouts,mean(minmax(teta)),mean(minmax(txi)),strn(z[i]+1), $
                  color='0000ff'xl
               found=1
            endif else begin
               oplot,teta,txi
               xyouts,mean(minmax(teta)),mean(minmax(txi)),strn(z[i]+1)
            endelse
            oplot,[teta[0]],[txi[0]],psym=8,color='ff00ff'xl
         endfor

      endelse

;      if found then begin
         a=''
;print,'[',defaultans,']   [',lastans,']'
         prompt=strn(num,format='(i3.3)')
         if defaultans ne '' then prompt=prompt+':'+defaultans
         prompt=prompt+'>'
         if not skip or goodref ne 2 then begin
            read,a,format='(a)',prompt=prompt
            a=strcompress(a,/remove_all)
         endif else begin
            a=''
         endelse
         if a eq '' then a=defaultans
;      endif else begin
;         a='y'
;      endelse

      if a eq 't' then begin
         mosast,num,/twostar,ext=ext,/killref,binfac=binfac
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr, $
            coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'T' then begin
         mosast,num,/twostar,ext=ext,/killref,/edit,binfac=binfac
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr, $
            coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'm' then begin
         mosast,num,/twostar,ext=ext,/killref,/ignoresrc,binfac=binfac
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr, $
            coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'M' then begin
         mosast,num,/twostar,ext=ext,/killref,/ignoresrc,/edit,binfac=binfac
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr, $
            coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'a' then begin
         mosast,num,ext=ext,/killref,binfac=binfac
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr, $
            coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'A' then begin
         mosast,num,ext=ext,/killref,/edit,binfac=binfac
         rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr, $
            coeffarr,ncoeffs,nlines
         lastans=a
         goto,checkit
      endif else if a eq 'v' then begin
         mosast,num,ext=ext,binfac=binfac
         goto,checkit
      endif else if a eq '?' then begin
         print,''
         print,'Available options: '
         print,'t - Twostar reduction using source list (if available)'
         print,'a - Automatic reduction (requires source list)'
         print,'m - Twostar reduction (full manual, ignores source list)'
         print,''
         print,'v - Displays image and shows previous solution.'
         print,'<cr> - Go on to the next image'
         print,'q - Quit program now'
         print,''
         print,'Upper case option turns on /EDIT for astrometry'
         print,''
         goto,checkit
      endif else if a eq 'q' then begin
         return
      endif else if a ne '' then begin
         print,'***Unrecognized option!***'
         print,''
         print,'Here are the valid options: '
         print,'t - Twostar reduction using source list (if available)'
         print,'a - Automatic reduction (requires source list)'
         print,'m - Twostar reduction (full manual, ignores source list)'
         print,''
         print,'v - Displays image and shows previous solution.'
         print,'<cr> - Go on to the next image'
         print,'q - Quit program now'
         print,''
         print,'Upper case option turns on /EDIT for astrometry'
         print,''
         goto,checkit
      endif

   endfor

end

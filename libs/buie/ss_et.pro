;+
; NAME:
;  ss_et
; PURPOSE:   (one line only)
;  Simple spectral extraction tool
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  ss_et,root,fnum1,fnum2
; INPUTS:
;  root   - String with root of file name (no trailing dot)
;  fnum1  - Frame number to reduced (starting number)
;  fnum2  - Frame number to run through (default fnum1)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NOEDIT - Flag, if set will suppress interactive removal of bad rows.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/06/13 - Written by Marc W. Buie, Lowell Observatory
;-
pro ss_et,root,fnum1,fnum2,NOEDIT=noedit,HWIDTH=hwidth

   if n_params() lt 2 then begin
      print,'Syntax: ss_et,root,fnum1,fnum2'
      return
   endif

   self = 'ss_et: '

   if badpar(fnum1,[2,3],0,caller=self+'(fnum1)') then return
   if badpar(fnum2,[0,2,3],0,caller=self+'(fnum2) ',default=fnum1) then return
   if badpar(noedit,[0,1,2,3],0,caller=self+'(NOEDIT) ',default=0) then return
   if badpar(hwidth,[0,2,3],0,caller=self+'(HWIDTH) ',default=40) then return

   ; read in the spectrum information file
   if exists('spec.inf') then begin
      openr,lun,'spec.inf',/get_lun
      x0=0
      x1=0
      y0=0
      y1=0
      xref=0
      nrows=0
      maskstr=''
      readf,lun,x0,format='(i)'
      readf,lun,x1,format='(i)'
      readf,lun,y0,format='(i)'
      readf,lun,y1,format='(i)'
      readf,lun,xref,format='(i)'
      readf,lun,nrows,format='(i)'
      readf,lun,dlam
      readf,lun,lam0
      readf,lun,maskstr,format='(a)'
      free_lun,lun
      rangepar,maskstr,mask

   ; if not found, then create one
   endif else begin
      openw,lun,'spec.inf',/get_lun
      printf,lun,' 581 starting x value for interesting part of array'
      printf,lun,'1306 ending x value for interesting part of array'
      printf,lun,'  -1 starting y value, negative means bottom'
      printf,lun,'  -1 ending y value, negative means top'
      printf,lun,' 981 x reference location'
      printf,lun,'   5 maximum number of rows to extract around object'
      printf,lun,' -7.21  Angstroms/pixel'
      printf,lun,'14185.0 Wavelength at x=0'
      printf,lun,'-1'
      free_lun,lun
      print,'Default spec.inf file written.  Please review and run again.'
      return
   endelse


   for fnum=fnum1,fnum2 do begin

      ; file names of interest
      fnim = root + '.' + string(fnum,format='(i4.4)') + '.fits'
      fnsp = root + '.' + string(fnum,format='(i4.4)') + '.spec'
      fnyb = root + '.' + string(fnum,format='(i4.4)') + '.ybad'

      if not exists(fnim) then begin
         print,'File ',fnim,' not found.'
         continue
      endif

      image = readfits(fnim,imhdr)
      sz = size(image,/dimen)
      sphdr = imhdr

      object = strtrim(sxpar(imhdr,'OBJECT'),2)
      exptime = sxpar(imhdr,'EXPTIME')
      title = object+' '+strn(exptime)+' sec'
      print,title

      ; check to see if there is an old spec file, if so, check it for
      ;   an AIRMASS keyword.  If found, keep it.  If not found or if
      ;   the spec file doesn't exist, then ask for the airmass.
      if exists(fnsp) then begin
         tmphdr = headfits(fnsp)
         am=sxpar(tmphdr,'AIRMASS')
      endif else begin
         am=0.
      endelse
      if am lt 1.0 then begin
         read,am,prompt=fnim+': enter airmass '
      endif

      if x0 lt 0 then x0 = 0
      if x1 lt 0 then x1 = sz[0]-1
      if y0 lt 0 then y0 = 0
      if y1 lt 0 then y1 = sz[1]-1

      nx = x1-x0+1
      ny = y1-y0+1

      ; Load old bad flags (if found)
      if exists(fnyb) then begin
         data = readfits(fnyb)
         xb = data[*,0]
         yb = data[*,1]
         bad = yb[*]
      endif else begin
         bad = bytarr(ny)
      endelse
      lastbad = bad
      dirty = 0

      xidx = indgen(nx)+x0
      yidx = indgen(ny)+y0

      wave = dlam*xidx + lam0
      wave = wave/10000.0

      ; interactive removal of rows that contain data to be ignored
      repeat begin

         z=where(bad eq 1,count)

         setwin,0
         cut = image[xref,y0:y1]
         cut = cut[*]
         if count ne 0 then cut[z] = 65535
         plot,yidx,cut,psym=-8,symsize=0.5,max_value=65534

         setwin,1,xsize=x1-x0+1,ysize=y1-y0+1
         skysclim,image[x0:x1,y0:y1],loval,hival
         bim = bytscl(image[x0:x1,y0:y1],min=loval,max=hival,top=255)
         tv,bim
         plot,[0],[1],/nodata,/noerase,xr=[x0,x1],yr=[y0,y1], $
            xstyle=5,ystyle=5,xmargin=[0,0],ymargin=[0,0]
         if count ne 0 then begin
            for i=0,count-1 do begin
               oplot,[x0,x1],yidx[z[i]]*[1,1],color='000020'xl
            endfor
         endif
         if mask[0] ne -1 then begin
            for i=0,n_elements(mask)-1 do begin
               oplot,mask[i]*[1,1],[y0,y1],color='200000'xl
            endfor
         endif
         oplot,[xref,xref],[y0,y1],color='00ffff'xl

         if not noedit then markdata,yidx,cut,bad

         done = total(abs(lastbad-bad)) eq 0

         if not done then dirty=1

         lastbad = bad

      endrep until done

      ; Save bad flags (if changed)
      if dirty then begin
         writefits,fnyb,[[yidx],[bad]]
      endif

      mimage = image[*,y0:y1]
      for i=0,ny-1 do begin
         if bad[i] then $
            mimage[*,i] = 0
      endfor

      ; Find centerline of spectrum (this should be the strongest thing by now).
      maxloc,mimage,xp,yp,x0,x1,/x

      print,'Peak counts in spectrum   (raw image)  is ', $
               strn(max(image[xp,yp+y0]))

      ; Dominant y position for object
      yobj = fix(median(yp))+y0
      print,'Object is at y position of ',strn(yobj)
      setwin,1,xsize=x1-x0+1,ysize=y1-y0+1
      plot,[0],[1],/nodata,/noerase,xr=[x0,x1],yr=[y0,y1], $
         xstyle=5,ystyle=5,xmargin=[0,0],ymargin=[0,0]
      oplot,[x0,x1],[yobj,yobj]-nrows/2-1,color='ffff00'xl
      oplot,[x0,x1],[yobj,yobj]+nrows/2+1,color='ffff00'xl

      ; Now, let's subtract the sky signal
      exclude = [indgen(y0),indgen(sz[1]-y1-1)+y1+1]
      z=where(bad eq 1,count)
      if count ne 0 then exclude=[exclude,yidx[z]]
      exclude=[exclude,indgen(nrows)-nrows/2+yobj]
      exclude=exclude[sort(exclude)]

      setwin,2,xsize=500,ysize=200
      plot,xp,yp+y0,xtitle='X (pixels)',ytitle='Y location of peak'

      image=float(image)
      backsub,image,/col,order=1,exclude=exclude

      setwin,3,xsize=x1-x0+1,ysize=y1-y0+1
      skysclim,image[x0:x1,y0:y1],loval,hival
      bim = bytscl(image[x0:x1,y0:y1],min=loval,max=hival,top=255)
      tv,bim
      plot,[0],[1],/nodata,/noerase,xr=[x0,x1],yr=[y0,y1], $
         xstyle=5,ystyle=5,xmargin=[0,0],ymargin=[0,0]
      if mask[0] ne -1 then begin
         for i=0,n_elements(mask)-1 do begin
            oplot,mask[i]*[1,1],[y0,y1],color='200000'xl
         endfor
      endif

      print,'Peak counts in spectrum  (no sky image) is ', $
               strn(max(image[xp,yp+y0]))

      ; Fit a line to the peak signal location
      rawcoeff=goodpoly(xp,yp+y0,1,3.0,yfit,newx,newy)
      print,'Raw centerline ',rawcoeff[*]

      ; Fitted centerline from the first pass
      yc=poly(xp,rawcoeff)

      ; Given the centerline find the first three moments of the data at the
      ;   position where the data fit the centerline well.
      a0=fltarr(nx)  ; total signal (this is the spectrum)
      a1=fltarr(nx)  ; center-of-light (position)
      a2=fltarr(nx)  ; FWHM of profile.
      for i=0,n_elements(yc)-1 do begin
         zp=indgen(nrows)-nrows/2+fix(yc[i]+0.5)
         a0[i] = total(image[xidx[i],zp])
         a1[i] = total(image[xidx[i],zp]*zp)/a0[i]
         a2[i] = total(image[xidx[i],zp])/max(image[xidx[i],zp])
      endfor

      ; Fit a line to the signal position
      coeffs2=goodpoly(xidx,a1,2,3.0,yfit,newx,newy)
      ; Fitted centerline from the second pass
      yc2=poly(xidx,coeffs2)
      print,'Improved centerline ',coeffs2[*]
      yc2f=replicate(mean(yc2),nx)

      cfwhm=goodpoly(xidx,a2,5,3.0,yfit)
      fwhmfit = poly(xidx,cfwhm)

      setwin,4,xsize=500,ysize=200
      plot,wave,a0,psym=3,xtitle='Wavelength (nm)', $
         ytitle='Flux (DN)',title=title

      setwin,5,xsize=500,ysize=200
      plot,wave,a1,psym=3,xtitle='Wavelength (nm)', $
         ytitle='y position (pixels)',title=title
      oplot,wave,yc2,color='0000ff'xl
      oplot,wave,yc2f,color='ff00ff'xl

      setwin,6,xsize=500,ysize=200
      plot,wave,a2,psym=3,xtitle='Wavelength (nm)', $
         ytitle='FWHM (pixels)',title=title
      oplot,wave,fwhmfit,color='0000ff'xl

      spec = a0

      ; build a PSF
      ospec = fltarr(nx)
      for i=0,nx-1 do begin
         j0 = i-hwidth > 0
         j1 = i+hwidth < nx-1
         for j=j0,j1 do begin
            ; xposition is xidx[j]
            y = indgen(nrows)-nrows/2 + yc2f[j]
            prof0 = image[j+x0,fix(y+0.5)]
            prof0 = prof0[*]/a0[j]
            if j eq j0 then begin
               xp = y-fix(yc2f[j]+0.5)
               yp = prof0
            endif else begin
               xp = [xp,y-fix(yc2f[j]+0.5)]
               yp = [yp,prof0]
            endelse
         endfor

         ; boil down the profile to an average that will be used for the
         ;   optimal extraction
         prof = fltarr(nrows)
         for j=0,nrows-1 do begin
            z = where(abs(xp-(j-nrows/2)) lt 0.5,count)
            val = yp[z]
            robomean,val,3.0,0.5,avg
            prof[j] = avg
         endfor

         ; renormalize
         prof = prof/total(prof)

         y = indgen(nrows)-nrows/2 + yc2f[j]
         ospec[i] = total(image[i+x0,fix(y+0.5)]*prof)/total(prof^2)

      endfor

      ospec[mask-x0] = !values.f_nan

      setwin,7,xsize=500,ysize=200
      plot,wave,ospec,xtitle='Wavelength (nm)', $
         ytitle='Flux (DN)',title=title+'  (optimum)'

      sxaddpar,sphdr,'AIRMASS',am

      writefits,fnsp,[[wave],[ospec]],sphdr

   endfor

end

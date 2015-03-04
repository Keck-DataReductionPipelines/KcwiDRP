;+
; NAME:
;  nic1scan
; PURPOSE:
;  Scan for best spectrum location of a weak-signal NICMOS spectrum.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  nic1scan,obs,dy,nstep
; INPUTS:
;  obs - anonymous structure containing the NICMOS data.  This structure
;           is initially created by NICINIT.PRO
;  dy  - Delta y, step size for scan.
;  nstep - number of steps to go away from "center".  There will be 2*nsteps+1
;            trial spectra computed for this.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  SLOPE = slope override value to impose on spectral extraction
;  YPOS  = position override value to impose on spectral extraction,
;            this sets the new "zero-point" or the center of the scan.
;  PSF   - PSF to use for extraction.  Can either be an array or a file name.
;            Default=use last known PSF.
; OUTPUTS:
;  Changes are made to some tags of the obs structure:
;    .coeffn - modified by SLOPE/YPOS and result of scan
;    .spec   - Best spectrum (most flux) found.
;    .yc     - Y location of best spectrum
;    .xp     - new spatial profile
;    .yp     - new spatial profile
;    .pnp
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
; Note, this routine does not apply the pixel response function correction.
; This means you must make sure all objects are done without the PRF
;   correction to get a consistent flux value.
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/01/30
;-
pro nic1scan,obs,dy,nstep,SLOPE=slope,YPOS=ypos,PSF=in_psf

   if badpar(obs,8,1,CALLER='NIC1SCAN: (obs) ') then return
   if badpar(dy,[2,3,4,5],0,CALLER='NIC1SCAN: (dy) ') then return
   if badpar(nstep,[2,3,4,5],0,CALLER='NIC1SCAN: (nstep) ') then return
   if badpar(slope,[0,2,3,4,5],0,CALLER='NIC1SCAN: (nstep) ') then return
   if badpar(ypos,[0,2,3,4,5],0,CALLER='NIC1SCAN: (ypos) ') then return

   if badpar(in_psf,[0,4,7],[0,2],CALLER='NIC1SCAN: (PSF) ', $
                               DEFAULT='',RANK=forcepsf) then return

   if forcepsf eq 0 then begin
      if in_psf ne '' then begin
         if exists(in_psf) then begin
            psf = readfits(in_psf,/silent)
            rank = size(psf,/n_dimen)
            if rank ne 2 then begin
               print,'NIC1SCAN: Error!  psf in file ',in_psf,' has rank of ', $
                     strn(rank),' but should be 2.'
               return
            endif
            forcepsf = 1
         endif else begin
            print,'NIC1SCAN: Error!  psf file ',in_psf,' was not found.  Aborting!'
            return
         endelse
      endif else begin
         psf=''
      endelse
   endif else begin
      psf = in_psf
      forcepsf = 1
   endelse

   if forcepsf and n_elements(obs.xgrid) gt n_elements(psf[*,0]) then begin
      print,'NIC1SCAN: Error!  psf array in file ',in_psf,' is the wrong size.'
      return
   endif else if forcepsf and n_elements(obs.xgrid) lt n_elements(psf[*,0]) then begin
      z=where(psf[*,0] ge min(obs.xgrid) and psf[*,0] le max(obs.xgrid))
      psf=psf[z,*]
   endif

   if obs.nobj ne 1 then begin
      print,'NIC1SCAN:  Error!  The observation structure has more than one object.'
      return
   endif

   ; Get setup for fitting, install forced psf if used.
   if forcepsf then begin

      obs.ypsi = psf[*,1]
      obs.yps  = psf[*,1]
      obs.forcepsf = 1

      oplot,obs.xgrid,obs.ypsi,color=110

   endif else begin
      obs.forcepsf = 0
   endelse
   x1l = obs.x1>0

   if size(slope,/tname) ne 'UNDEFINED' then obs.coeffn[1] = slope
   if size(ypos,/tname) ne 'UNDEFINED' then obs.coeffn[0] = ypos

   y0 = obs.coeffn[0]

   yloc = fltarr(nstep*2+1)
   sigt = fltarr(nstep*2+1)
   for j=0,nstep*2 do begin
      yval = y0-dy*(nstep-j)
      yloc[j] = yval
      obs.coeffn[0] = yval
      yc2=poly(obs.x,obs.coeffn)

      ; Extract the spectrum
      spec = fltarr(obs.npts)
      for i=x1l,obs.x2 do begin
         cx = obs.y-yc2[i]
         z=where(cx le -5.0 or cx ge 5.0)
         robomean,obs.image[i,z],3.0,0.5,back
         z=where(cx gt -obs.hwid and cx lt obs.hwid)
         interp,obs.xgrid,obs.yps,cx[z],cw
         spec[i-obs.x1] = total(((obs.image[i,z]-back)*obs.prfcorr[i,z])*cw)/total(cw^2)
   ;      spec[i-obs.x1] = total((obs.image[i,z]-back)*cw)/total(cw^2)
      endfor
      sigt[j] = total(spec)
;      print,yloc[j],sigt[j]
   endfor
   z=where(sigt eq max(sigt))
   ybest = yloc[z[0]]
   print,'Best y at ',ybest,'  total signal=',sigt[z[0]]

   setwin,12,xsize=400,ysize=300
   plot,yloc,sigt,psym=-4,xtitle='Y location of centerline (pixels)', $
      ytitle='Total signal in spectrum (DN)'

   ; Extract the spectrum
   obs.coeffn[0] = ybest
   yc2=poly(obs.x,obs.coeffn)
   spec = fltarr(obs.npts)
   for i=x1l,obs.x2 do begin
      cx = obs.y-yc2[i]
      z=where(cx le -5.0 or cx ge 5.0)
      robomean,obs.image[i,z],3.0,0.5,back
      z=where(cx gt -obs.hwid and cx lt obs.hwid)
      interp,obs.xgrid,obs.yps,cx[z],cw
      spec[i-obs.x1] = total(((obs.image[i,z]-back)*obs.prfcorr[i,z])*cw)/total(cw^2)
;      spec[i-obs.x1] = total((obs.image[i,z]-back)*cw)/total(cw^2)
   endfor

   ; Construct a master profile of discrete points
   x1 = (obs.x1 > 0) < (obs.nx-1)
   x2 = (obs.x2 > 0) < (obs.nx-1)
   for i=x1,x2 do begin
      xprof=obs.y[obs.y1:obs.y2]-yc2[i]
      yprof=obs.image[i,obs.y1:obs.y2]/spec[i-obs.x1]
      z=where(abs(xprof) le obs.hwid,count)
      if i eq x1 then begin
         xp = xprof[z]
         yp = yprof[z]
      endif else begin
         xp = [xp,xprof[z]]
         yp = [yp,yprof[z]]
      endelse
   endfor

   setwin,5,xsize=400,ysize=300
   plot,xp,yp,psym=3,yr=[0,max(obs.yps)+0.2], $
      xtitle='Row position (pixels)', $
      ytitle='Relative signal', $
      title='Phased spatial profile, pass '+strn(obs.pass)
   oplot,obs.xgrid,obs.yps+0.2,color=90
   if forcepsf then $
      oplot,obs.xgrid,obs.ypsi,color=80

   setwin,6,xsize=400,ysize=300
   str = strtrim(sxpar(obs.hdr,'TARGNAME'),2)
   plot,obs.wave,spec, $
      xtitle='Wavelength (microns)',ytitle='DN/sec', $
      title='Spectrum of '+str+'  '+obs.root+obs.obsid

   obs.spec = spec

   if n_elements(xp) eq n_elements(obs.xp) then begin
      obs.xp     = xp
      obs.yp     = yp
      obs.pnp = n_elements(xp)
   endif else if n_elements(xp) lt n_elements(obs.xp) then begin
      obs.xp[0:n_elements(xp)-1] = xp
      obs.yp[0:n_elements(yp)-1] = yp
      obs.pnp = n_elements(xp)
   endif else begin
      obs.xp = xp[0:n_elements(obs.xp)-1]
      obs.yp = yp[0:n_elements(obs.yp)-1]
      obs.pnp = n_elements(obs.xp)
   endelse

   obs.pass = -2

end

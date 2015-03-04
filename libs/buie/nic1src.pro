;+
; NAME:
;  nic1src
; PURPOSE:
;  Improved 1 source extraction from single NICMOS Grism image data.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  nic2src,obs
; INPUTS:
;  obs - anonymous structure containing the NICMOS data.  This structure
;           is initially created by NICINIT.PRO
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
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
;  Written by Marc W. Buie, Lowell Observatory, 1999 Sep 29
;
;-
pro nic1src,obs,PSF=in_psf,NOPRF=noprf,NOFOURIER=nofourier

   if badpar(obs,8,1,CALLER='NIC1SRC: (obs) ') then return
   if badpar(in_psf,[0,4,7],[0,2],CALLER='NIC1SRC: (PSF) ', $
                               DEFAULT='',RANK=forcepsf) then return
   if badpar(noprf,[0,2,3],0,CALLER='NIC1SRC: (NOPRF) ',DEFAULT=0) then return
   if badpar(nofourier,[0,2,3],0,CALLER='NIC1SRC: (NOFOURIER) ',DEFAULT=0) then return

   if forcepsf eq 0 then begin
      if in_psf ne '' then begin
         if exists(in_psf) then begin
            psf = readfits(in_psf,/silent)
            rank = size(psf,/n_dimen)
            if rank ne 2 then begin
               print,'NIC1SRC: Error!  psf in file ',in_psf,' has rank of ', $
                     strn(rank),' but should be 2.'
               return
            endif
            forcepsf = 1
         endif else begin
            print,'NIC1SRC: Error!  psf file ',in_psf,' was not found.  Aborting!'
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
      print,'NIC1SRC: Error!  psf array in file ',in_psf,' is the wrong size.'
      return
   endif else if forcepsf and n_elements(obs.xgrid) lt n_elements(psf[*,0]) then begin
      z=where(psf[*,0] ge min(obs.xgrid) and psf[*,0] le max(obs.xgrid))
      psf=psf[z,*]
   endif

   if obs.nobj ne 1 then begin
      print,'NIC1SRC:  Error!  The observation structure has more than one object.'
      return
   endif

   setwin,0,xsize=400,ysize=300
   plot,obs.xgrid,obs.ypsi, $
      xtitle='Distance from centerline (pixels)', $
      ytitle='Relative signal',title='Input PSF'
   oplot,obs.xgrid,obs.yps,color=100

   ; Get setup for fitting, install forced psf if used.
   if forcepsf then begin

      obs.ypsi = psf[*,1]
      obs.forcepsf = 1

      oplot,obs.xgrid,obs.ypsi,color=110

   endif else begin
      obs.forcepsf = 0
   endelse
   x1l = obs.x1>0

   if not noprf then begin
      ; Compute a pixel response function correction factor.
      uval = replicate(0.0,obs.nx,obs.ny)
      dval = replicate(0.0,obs.nx,obs.ny)
      uval[x1l:obs.x2,obs.y1:obs.y2] = $
         ( (obs.prfimg[x1l:obs.x2,obs.y1+1:obs.y2+1] + $
          obs.prfimg[x1l:obs.x2,obs.y1:obs.y2])/2.0 ) / $
             obs.prfimg[x1l:obs.x2,obs.y1:obs.y2]
      dval[x1l:obs.x2,obs.y1:obs.y2] = $
         ( (obs.prfimg[x1l:obs.x2,obs.y1:obs.y2] + $
          obs.prfimg[x1l:obs.x2,obs.y1-1:obs.y2-1])/2.0 ) / $
             obs.prfimg[x1l:obs.x2,obs.y1:obs.y2]
   ;   obs.prfcorr = (udiff+ddiff)/2.0
      fixval=0.413
      z=where(finite(uval) eq 0,count)
      if count ne 0 then uval[z] = 0.0
      z=where(finite(dval) eq 0,count)
      if count ne 0 then dval[z] = 0.0
      obs.prfcorr = ( 0.25/fixval*(2.0*uval-1.0) + 0.5 + 0.25/fixval*(2.0*dval-1.0) ) / $
                    ( 0.25*(2.0*uval-1.0) + 0.5 + 0.25*(2.0*dval-1.0) )
      z=where(finite(obs.prfcorr) eq 0,count)
      if count ne 0 then obs.prfcorr[z] = 1.0
   endif

   xgrid = obs.xgrid
   yps   = obs.ypsi

   a1=fltarr(obs.nx)  ; center-of-light (position)

   hwid  = obs.hwid

   ; recompute positions of object at all columns, this is done by
   ;    computing an "optimal" centroid using the current psf imposed
   ;    on the current centerline
   for pick=x1l,obs.x2 do begin
      i=pick-x1l
      yv = obs.y-obs.yc[pick]
      z=where(yv ge -obs.hwid and yv le obs.hwid)
      yv=yv[z]
      dat = obs.image[pick,z] ;*obs.prfcorr[pick,z]
      dat=dat[*]
      interp,xgrid,yps,yv,colprof
      denom = total(colprof*dat)
      if denom gt 0.0 then $
         a1[pick] = total(colprof*obs.y[z]*dat)/total(colprof*dat)
   endfor

   setwin,1,xsize=400,ysize=300
   plot,obs.x[x1l:obs.x2],obs.yc[x1l:obs.x2], $
      title='Original centerline and original points', $
      yr=[max([obs.y1, min([obs.yc[x1l:obs.x2],obs.yg[0:obs.ngood-1]])]), $
          min([obs.y2, max([obs.yc[x1l:obs.x2],obs.yg[0:obs.ngood-1]])])], $
      xtitle='Column number',ytitle='Centerline position (pixels)'
   oplot,obs.xg[0:obs.ngood-1],obs.yg[0:obs.ngood-1],psym=5,color=100

   ; Fit a line to the signal position
   z=where(a1 gt 0.0)
   coeffs1=goodpoly(obs.x[z],a1[z],1,2.0,yfit,newx,newy)
   yc2=poly(obs.x,coeffs1)

   if not nofourier then begin

      ; Fitted centerline from the second pass
      setwin,2,xsize=400,ysize=300
      plot,obs.x[x1l:obs.x2],obs.yc[x1l:obs.x2], $
         yr=[max([obs.y1, min([obs.yc[x1l:obs.x2],newy])]), $
             min([obs.y2, max([obs.yc[x1l:obs.x2],newy])])], $
         xtitle='Column number',ytitle='Centerline position (pixels)', $
         title='Original,new centerline and new points'
      oplot,obs.x[x1l:obs.x2],a1[x1l:obs.x2],psym=4
      oplot,obs.x,yc2,linestyle=1

      ; convert ylocation (from the fitted line) to its fractional position,
      ;   that is, subtract the integer portion of the position.
      yphase=yc2[x1l:obs.x2]-fix(yc2[x1l:obs.x2])

      ; subtract the fitted line prediction from the measured positions
      omc=a1[x1l:obs.x2]-yc2[x1l:obs.x2]

      ; generate a smoothed curve of the residual pattern, data must be sorted
      ;   by the fractional y value before passing to lowess for smoothing.
      z=where(a1[x1l:obs.x2] gt 0.0)
      tmpy=yphase[z]
      tmpo=omc[z]
      idx=sort(tmpy)
      lowess,[tmpy[idx]-1.0,tmpy[idx],tmpy[idx]+1.0], $
         [tmpo[idx],tmpo[idx],tmpo[idx]],0.3,ycsm,order=3,newx=yphase

      ; Compute residuals between the smoothed curve and the residuals.
      ;    Use this to weed out bogus points.
      resid=omc-ycsm
      bres=bytarr(n_elements(resid))
      z=where(a1[x1l:obs.x2] le 0.0,count)
      if count ne 0 then bres[z]=1B
      robomean,resid,3.0,0.5,bad=bres
      xpos=obs.x[x1l:obs.x2]
      ; select good points
      z=where(bres eq 0)

      ; Residuals against new centerline
      setwin,3,xsize=400,ysize=300
      tmpx=obs.x[x1l:obs.x2]
      tmpr=a1[x1l:obs.x2]-yc2[x1l:obs.x2]
      plot,tmpx,tmpr,psym=4,yr=minmax(tmpr[z]), $
         xtitle='Column number',ytitle='Residual (pixels)', $
         title='Residuals from new centerline fit'
      ; add to plot an indication of those points thrown out.
      z1=where(bres eq 1,count)
      if count ne 0 then oplot,tmpx[z1],tmpr[z1],psym=6

      ; Take the good points and do a Fourier fit to the residuals.
      fourfit,yphase[z],omc[z],replicate(0.01,n_elements(z)),2,c,csig
      fourfunc,yphase,c,ycsm

      ; add this new fitted curve to the plot
      oplot,obs.x[x1l:obs.x2],ycsm

      ; Another plot of the same thing only this time as a function of yphase
      setwin,4,xsize=400,ysize=300
      plot,yphase[z],omc[z],psym=4, $
         xtitle='Fractional part of row position (pixels)', $
         ytitle='Residual (pixels)', $
         title='Residuals from new centerline fit'
      fourfunc,findgen(101)/100.0,c,fitcur
      oplot,findgen(101)/100.0,fitcur

      ; Take the wobbly part off the position and refit a line to the
      ;    signal position
      a1[x1l:obs.x2]=a1[x1l:obs.x2]-ycsm
      z=where(a1 gt 0.0)
      coeffs2=goodpoly(obs.x[z],a1[z],1,2.0,yfit,newx,newy)
   print,coeffs1[*],coeffs2[*],format='(2(2x,f7.3,1x,f7.5))'
      yc2=poly(obs.x,coeffs1)
   endif else begin
      print,coeffs1[*],format='(2x,f7.3,1x,f7.5)'
      coeffs2=coeffs1
   endelse

   ; Extract the spectrum
   spec = fltarr(obs.npts)
   for i=x1l,obs.x2 do begin
      cx = obs.y-yc2[i]
      z=where(cx le -5.0 or cx ge 5.0)
      robomean,obs.image[i,z],3.0,0.5,back
      z=where(cx gt -obs.hwid and cx lt obs.hwid)
      interp,obs.xgrid,yps,cx[z],cw
      spec[i-obs.x1] = total(((obs.image[i,z]-back)*obs.prfcorr[i,z])*cw)/total(cw^2)
;      spec[i-obs.x1] = total((obs.image[i,z]-back)*cw)/total(cw^2)
   endfor

   ; Construct a master profile of discrete points
   for i=0,n_elements(newx)-1 do begin
      xprof=obs.y[obs.y1:obs.y2]-yc2[newx[i]]
;     yprof=obs.image[newx[i],obs.y1:obs.y2]*obs.prfcorr[i,obs.y1:obs.y2]/spec[newx[i]-obs.x1]
      yprof=obs.image[newx[i],obs.y1:obs.y2]/spec[newx[i]-obs.x1]
      z=where(abs(xprof) le obs.hwid,count)
      if i eq 0 then begin
         xp = xprof[z]
         yp = yprof[z]
      endif else begin
         xp = [xp,xprof[z]]
         yp = [yp,yprof[z]]
      endelse
   endfor

   ; First pass smoothed profile, one that allows lots of curvature
   ;   for the center and one that is heavily smoothed for the wings.
   lowess,xp,(yp<1.0) > (-0.1),0.5,ypsmoo,order=3,newx=obs.xgrid
   lowess,xp,(yp<1.0) > (-0.1),1.0,ypsmoo1,order=1,newx=obs.xgrid
   wprof = exp(-(obs.xgrid/3.0)^2)
   yps = (ypsmoo*wprof + ypsmoo1*(1.0-wprof)) > (-0.1)

   ; compare all points against the new profile, toss out all points that
   ;   are unusual deviants and then resmooth.
   interp,obs.xgrid,yps,xp,ypfit
   sbad=bytarr(n_elements(yp))
   robomean,yp-ypfit,3.0,0.5,savg,dummy,sstdev,bad=sbad
   zg = where(sbad eq 0)
   xp = xp[zg]
   yp = yp[zg]
   lowess,xp,(yp<1.0) > (-0.1),0.5,ypsmoo,order=3,newx=obs.xgrid
   lowess,xp,(yp<1.0) > (-0.1),1.0,ypsmoo1,order=1,newx=obs.xgrid
   yps = (ypsmoo*wprof + ypsmoo1*(1.0-wprof)) ; > 0.0

   dyth = 0.00
   ; Scan the left side of the profile from 1/e to edge.  The profile
   ;    must descend, enforce this.
   left = max(where(obs.xgrid lt -1.0))
   for is=left-1,0,-1 do $
      if yps[is]-yps[is+1] gt dyth and yps[is] gt 0.0 then yps[is]=yps[is+1]+dyth

   ; Do the same for the right side of the profile.
   right = min(where(obs.xgrid gt 1.0))
   for is=right+1,n_elements(obs.xgrid)-1 do $
      if yps[is]-yps[is-1] gt dyth and yps[is] gt 0.0 then yps[is]=yps[is-1]+dyth

   ; Subtract the psf baseline
   z=where(abs(obs.xgrid) ge 5.0,count)
   baseline = mean(yps[z])
   yps = yps - baseline
   yp = yp - baseline

   ; force the first two and last two points in the profile to zero
   yps[0:1] = 0.0
   yps[n_elements(yps)-2:n_elements(yps)-1] = 0.0

   ; renormalize
   renorm = int_tabulated(xgrid,yps)

   yps = yps/renorm
   yp  = yp/renorm

   setwin,5,xsize=400,ysize=300
   plot,xp,yp,psym=3,yr=[0,max(yp)+0.2], $
      xtitle='Row position (pixels)', $
      ytitle='Relative signal', $
      title='Phased spatial profile, pass '+strn(obs.pass)

;   oplot,obs.xgrid,wprof*0.5+0.1,color=40
;   oplot,obs.xgrid,(1.0-wprof)*0.5+0.1,color=40

   oplot,obs.xgrid,yps+0.2,color=90
   if forcepsf then $
      oplot,obs.xgrid,obs.ypsi,color=80

   setwin,6,xsize=400,ysize=300
   str = strtrim(sxpar(obs.hdr,'TARGNAME'),2)
   plot,obs.wave,spec, $
      xtitle='Wavelength (microns)',ytitle='DN/sec', $
      title='Spectrum of '+str+'  '+obs.root+obs.obsid

   obs.yps  = yps
   obs.ypsi = yps

   obs.yc    = yc2
   obs.ycnew = yc2
   obs.xg[0:n_elements(newx)-1] = newx
   obs.ygn[0:n_elements(newx)-1]= newy
   obs.ngood = n_elements(newx)
   obs.yg    = obs.ygn
   obs.coeffn = coeffs2
   obs.coeff  = coeffs2
   obs.ycnewg = 1

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

   obs.pass = obs.pass+1

end


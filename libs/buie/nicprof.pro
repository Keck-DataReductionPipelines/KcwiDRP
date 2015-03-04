;+
; NAME:
;  nicprof
; PURPOSE:
;  Update NICMOS observation structure based on a new centerline.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
;
; CALLING SEQUENCE:
;  nicprof,obs
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
;  Written by Marc W. Buie, Lowell Observatory, 1999 Sep 2
;
;-
pro nicprof,obs

   if not obs.ycnewg then begin
      print,'NICPROF:  Error!  There is no valid new centerline use.'
      return
   endif

   ; Construct a master profile of discrete points
   for i=0,obs.ngood-1 do begin
      xprof=obs.y[obs.y1:obs.y2]-obs.ycnew[obs.xg[i]]
      yprof=obs.image[obs.xg[i],obs.y1:obs.y2]
      c2=goodpoly(xprof,yprof,1,3.0,yfit)
      yprof=yprof-yfit
      zp=indgen(5)-2+fix(obs.ycnew[obs.xg[i]]+0.5)
      a0 = total(obs.image[obs.xg[i],zp])
      z=where(abs(xprof) le obs.hwid,count)
      if i eq 0 then begin
         xp = xprof[z]
;         yp = yprof[z]/obs.spec[obs.xg[i]]
         yp = yprof[z]/a0
      endif else begin
         xp = [xp,xprof[z]]
;         yp = [yp,yprof[z]/obs.spec[obs.xg[i]-obs.x1]]
         yp = [yp,yprof[z]/a0]
      endelse
   endfor

   ; Smooth the points into a regularly gridded curve
   lowess,xp,yp,0.5,ypsmoo,order=3,newx=obs.xgrid

   ; Look at the deviations from the smoothed curve and eliminate those points
   ;    that are unusually deviant from the curve.  Then redo the smoothing.
   interp,obs.xgrid,ypsmoo,xp,ypfit
   sbad=bytarr(n_elements(yp))
   robomean,yp-ypfit,3.0,0.5,savg,dummy,sstdev,bad=sbad
   zg = where(sbad eq 0)
   xp = xp[zg]
   yp = yp[zg]
   lowess,xp,yp,0.5,ypsmoo,order=3,newx=obs.xgrid

   ; Normalize the profile
   renorm = int_tabulated(obs.xgrid,ypsmoo)
   ypsmoo = ypsmoo/renorm
   yp = yp/renorm

   ; Extract the spectrum
   spec = fltarr(obs.npts)
   for i=obs.x1,obs.x2 do begin
      cx = obs.y-obs.ycnew[i]
      z=where(cx gt -obs.hwid and cx lt obs.hwid)
      interp,obs.xgrid,ypsmoo,cx[z],cw
      spec[i-obs.x1] = total(obs.image[i,z]*cw)/total(cw^2)
   endfor

   ; Copy new stuff to final area
   obs.coeff  = obs.coeffn
   obs.spec   = spec
   obs.yc     = obs.ycnew
   obs.yps    = ypsmoo
   obs.ypsi   = ypsmoo
   obs.yg     = obs.ygn
   obs.pass   = 0
   obs.ycnewg = 0

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

   setwin,2,xsize=400,ysize=300
   plot,xp,yp,psym=3,yr=[-0.05,0.65]
   oplot,obs.xgrid,ypsmoo

end

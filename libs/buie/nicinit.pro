;+
; NAME:
;  nicinit
; PURPOSE:
;  Extract initial spectrum and profile from NICMOS Grism image data.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  nicinit,root,obsid,obs,PATH=path
; INPUTS:
;  root  - Root of observation set (visit id), usually 6 characters
;  obsid - Observation id, usually 3 characters.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  PATH  - Directory where the raw data can be found.  (Default = current)
; OUTPUTS:
;  obs   - Anonymous structure that contains the extracted spectrum and
;             ancillary information about the observation.
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
;  Written by Marc W. Buie, Lowell Observatory, 1999 July 13
;  1999 Sep 16, added DYRANGE keyword, added kludge to get around problems
;      with the extraction region being slightly out of bounds to the left.
;-
pro nicinit,root,obsid,obs,PATH=path,FLAT=flat,DYRANGE=dyrange,HWID=hwid

   if badpar(root,7,0,CALLER='NICINIT: (root) ') then return
   if badpar(obsid,7,0,CALLER='NICINIT: (obsid) ') then return
   if badpar(path,[0,7],0,CALLER='NICINIT: (PATH) ',default='') then return
   if badpar(flat,[0,4,5,7],[0,3],CALLER='NICINIT: (FLAT)',default='') then return
   if badpar(dyrange,[0,2,3],0,CALLER='NICINIT: (DYRANGE)',default=100) then return
   if badpar(hwid,[0,2,3,4,5],0,CALLER='NICINIT: (HWID)',default=10.0) then return
   if badpar(thresh,[0,2,3,4,5],0,CALLER='NICINIT: (THRESH)',default=0.0) then return

   if path ne '' then path=addslash(path)

   obsfile = path+root+'.pos'

   if not exists(obsfile) then begin
      print,'NICINIT: Error!  observation information file ',obsfile,' not found.'
      return
   endif

   rdnicobs,obsfile,objrefx,objrefy,obsidlist,reflist,xdither,ydither
   nobj = n_elements(objrefx)

   ; Verify that the requested observation is included in the observation
   ;    set information that was just loaded.
   pick=where(obsid eq obsidlist,count)
   if count eq 0 then begin
      print,'NICINIT: Error!  observation ',obsid,' was not found in the observation'
      print,'         information file: ',obsfile
      return
   endif else if count gt 1 then begin
      print,'NICINIT: Warning.  observation ',obsid,' was found ',strn(count),' times'
      print,'         in the information file: ',obsfile,'.  Using the first entry.'
   endif
   pick=pick[0]

   if n_elements(flat) eq 1 then begin
      if flat ne '' then begin
         if exists(flat) then begin
            flat=readfits(flat,/silent)
            doflat = 1
         endif else begin
            print,'NICINIT: Error!  Flat field ',flat,' does not exist.'
            return
         endelse
      endif else begin
         doflat = 0
      endelse
   endif else begin
      doflat = 1
   endelse

   if doflat then begin
      fsz=size(flat,/dimen)
      forder = fsz[2]
   endif else begin
      forder=0
   endelse

   ; Get the object position for this image
   xobj = objrefx+xdither[pick]
   yobj = objrefy+ydither[pick]

   ; Load the image
   imfile = root+obsid + '_cal.fits'
   if not exists(path+imfile) then begin
      print,'File ',path+imfile,' does not exist.'
      return
   endif

   ; check to make sure sky is different from data
   if reflist[pick] eq obsid then begin
      print,'NICINIT: ERROR! sky reference [',reflist[pick], $
            '] is the same as the object.'
      return
   endif

   fits_read,path+imfile,raw,hdr,exten=1

   ; Which grism?
   grism = strcompress(sxpar(hdr,'FILTER'),/remove_all)

   ; Load the sky reference image
   if reflist[pick] eq 'sky' then begin
      reffile = root+'_'+strlowcase(grism)+'.sky'
      fits_read,path+reffile,sky
   endif else begin
      reffile = root+reflist[pick] + '_cal.fits'
      fits_read,path+reffile,sky,exten=1
   endelse

   ; this is the full data image
   data=raw-sky

   ; some useful dimensions
   sz=size(data,/dimen)
   nx=sz[0]
   ny=sz[1]

   if grism eq 'G206' then begin
      ; Area of extraction region
      x1 = fix(xobj[0] - 46 + 0.5)
      x2 = x1+105
      y1 = fix(yobj[0] - 2 - dyrange/2 + 0.5)
      y2 = y1 + dyrange
      ; Compute the wavelength scale
      npts = x2-x1+1
      dx = findgen(npts)+x1 - xobj[0]
      wave = 2.045 -0.01152*dx
      clipup   =  2
      clipdown = -6
   endif else if grism eq 'G141' then begin
      ; Area of extraction region
      x1 = fix(xobj[0] - 70 + 0.5)
      x2 = x1+111
      y1 = fix(yobj[0] - 2 - dyrange/2 + 0.5)
      y2 = y1 + dyrange
      ; Compute the wavelength scale
      npts = x2-x1+1
      dx = findgen(npts)+x1 - xobj[0]
      wave = 1.401 -0.007992*dx
      clipup   =  -3
      clipdown = -11
   endif else if grism eq 'G096' then begin
      ; Area of extraction region
      x1 = fix(xobj[0] - 55 + 0.5)
      x2 = x1+91
      y1 = fix(yobj[0] - 2 - dyrange/2 + 0.5)
      y2 = y1 + dyrange
      ; Compute the wavelength scale
      npts = x2-x1+1
      dx = findgen(npts)+x1 - xobj[0]
      wave = 0.9487 -0.005369*dx
      clipup   =  -3
      clipdown = -11
   endif else begin
      print,'NICINIT:  Sorry, grism ',grism,' is not currently supported.'
      return
   endelse

   ; Create index vectors
   x=findgen(nx)
   y=findgen(ny)

   ; Apply flat field for region that will be extracted.
   if doflat then begin
      x1l = x1>0
      if grism eq 'G206' then begin
         wim = 2.045 - 0.01152 * rebin(x-xobj[0],nx,ny,/sample)
      endif else if grism eq 'G141' then begin
         wim = 1.401 - 0.007992 * rebin(x-xobj[0],nx,ny,/sample)
      endif else if grism eq 'G096' then begin
         wim = 0.9487 - 0.005369 * rebin(x-xobj[0],nx,ny,/sample)
      endif
      fim = flat[x1l:x2,y1:y2,forder-1]
      for i=forder-2,0,-1 do $
         fim = fim*wim[x1l:x2,y1:y2] + flat[x1l:x2,y1:y2,i]
      data[x1l:x2,y1:y2] = data[x1l:x2,y1:y2]*fim
   endif

   ; Find centerline as defined by maximum signal
   x1l=x1>0
   y1c = yobj[0] + clipdown
   y2c = yobj[0] + clipup
   maxloc,data[*,y1c:y2c],xpos,ypos,x1l,x2,/x
   if x1l ne x1 then begin
      xpos = [ indgen(x1l-x1)+x1,     xpos ]
      ypos = [ replicate(-y1c,x1l-x1), ypos ]
   endif
   ypos=ypos+y1c

   ; Fit a line to the peak signal location
   z=where(ypos ne 0.0)
   coeffs=goodpoly(xpos[z],ypos[z],1,3.0,yfit,newx,newy)

   ; Fitted centerline from the first pass
   yc=poly(x,coeffs)

   ; Given the centerline find the first three moments of the data at the
   ;   position where the data fit the centerline well.
   a0=fltarr(nx)  ; total signal (this is the spectrum)
   a1=fltarr(nx)  ; center-of-light (position)
   a2=fltarr(nx)  ; FWHM of profile.
   for i=0,n_elements(newx)-1 do begin
      zp=indgen(5)-2+fix(yc[newx[i]]+0.5)
      a0[newx[i]] = total(data[newx[i],zp])
      a1[newx[i]] = total(data[newx[i],zp]*zp)/a0[newx[i]]
      a2[newx[i]] = total(data[newx[i],zp])/max(data[newx[i],zp])
   endfor

   ; Copy the new positions over the old positions
   xpos = newx
   ypos = a1[newx]

   ; Fit a line to the signal position
   coeffs2=goodpoly(xpos,ypos,1,3.0,yfit,newx,newy)

   ; Save the good position values
   xg = fltarr(npts)
   yg = fltarr(npts)
   xg[0] = newx
   yg[0] = newy
   ngood = n_elements(newx)

   ; Fitted centerline from the second pass
   yc2=poly(x,coeffs2)

   ; Set a signal threshold to eliminate low signal or bogus points.
   is=sort(a0[newx])
   a0thresh=a0[newx[is[10]]]
   if a0thresh gt 0.0 then a0thresh=0.0

   ; Construct a master profile of discrete points
   new=1
   for i=0,n_elements(newx)-1 do begin
      if a0[newx[i]] gt a0thresh then begin
         xprof=y[y1:y2]-yc2[newx[i]]
         yprof=data[newx[i],y1:y2]
         c2=goodpoly(xprof,yprof,1,3.0,yfit)
         yprof=yprof-yfit
         z=where(abs(xprof) le hwid,count)
         if new then begin
            xp = xprof[z]
            yp = yprof[z]/a0[newx[i]]
            new=0
         endif else begin
            xp = [xp,xprof[z]]
            yp = [yp,yprof[z]/a0[newx[i]]]
         endelse
      endif
   endfor

   prfimg = fltarr(nx,ny)
   spec = fltarr(npts)
   xgrid=(findgen(hwid*40+1)-hwid*20)/20.0

   ; Smooth the points into a regularly gridded curve
   lowess,xp,yp,0.5,ypsmoo,order=3,newx=xgrid

   ; Look at the deviations from the smoothed curve and eliminate those points
   ;    that are unusually deviant from the curve.  Then redo the smoothing.
   interp,xgrid,ypsmoo,xp,ypfit
   sbad=bytarr(n_elements(yp))
   robomean,yp-ypfit,3.0,0.5,savg,dummy,sstdev,bad=sbad
   zg = where(sbad eq 0)
   xp = xp[zg]
   yp = yp[zg]
   lowess,xp,yp,0.5,ypsmoo,order=3,newx=xgrid

   ; Put some extra padding on xp,yp for later use.
   pnp = n_elements(xp)
   xp = [xp,fltarr((y2-y1)*10)]
   yp = [yp,fltarr((y2-y1)*10)]

   ; Normalize the profile
   renorm = int_tabulated(xgrid,ypsmoo)
   ypsmoo = ypsmoo/renorm
   yp     = yp/renorm

   ; Extract the spectrum
   for i=x1,x2 do begin
      if i gt 0 then begin
         cx = y-yc2[i]
         z=where(cx gt -hwid and cx lt hwid)
         interp,xgrid,ypsmoo,cx[z],cw
         spec[i-x1] = total(data[i,z]*cw)/total(cw^2)
         prfimg[i,z] = cw
      endif
   endfor

   ; Put all the information into the final output structure
   obs = { $
      coeff: coeffs2, $              ; polynomial coefficients that determine centerline
      coeffn: coeffs2, $             ; coefficients for new centerline
      doflat: doflat, $              ; Flag, was data flat-fielded?
      fn:    imfile, $               ; File name without path
      forcepsf: 0, $                 ; Flag, true means PSF was imposed, not fit.
      forder: forder, $              ; Order of flat field polynomial fit.
      grism: grism, $                ; Grism name
      hdr:   hdr, $                  ; The full FITS header
      hwid:  hwid, $                 ; half width of extraction area
      image: data, $                 ; Full image after sky subtraction
      ngood: ngood, $                ; Number of good values in xg,yg
      nobj:  nobj, $                 ; Number of objects for extraction
      npts:  npts, $                 ; Number of points in spectrum
      nx:    nx, $                   ; X width of full image
      ny:    ny, $                   ; Y height of full image
      obsid: obsid, $                ; Observation id (3 characters)
      oerr:  fltarr(npts,nobj), $    ; Uncertainties on object spectra
      ospec: fltarr(npts,nobj), $    ; Individual object spectra
      path:  path, $                 ; Path to the file
      pass:  0, $                    ; How many passes in the processing.
      pnp:   pnp, $                  ; Points valid in xp and yp
      prfcorr: replicate(1.0,nx,ny), $ ; Pixel response function correction
      prfimg: prfimg, $              ; Image derivative
      ref:   reffile, $              ; File name of sky image.
      refid: reflist[pick], $        ; Observation id of sky image (or 'sky')
      root:  root, $                 ; Root of observation set (proposal id)
      spec:  spec, $                 ; Optimally extracted spectrum
      wave:  wave, $                 ; Wavelength vector for spectrum (microns)
      wspec: fltarr(npts,nobj), $    ; Wavelength for individual object spectra
      x:     x, $                    ; Index vector for x pixel coordinates
      x1:    x1, $                   ; lower bound in x of extraction region
      x2:    x2, $                   ; upper bound in x of extraction region
      xg:    xg, $                   ; X values of columns used in centerline fit
      xgrid: xgrid, $                ; X value relative to centerline for master profile
      xo:    xobj, $                 ; X location of object(s)
      xp:    xp, $                   ; Discrete spatial profile, x coordinate
      y:     y, $                    ; Index vector for y pixel coordinates
      y1:    y1, $                   ; lower bound in y of extraction region
      y2:    y2, $                   ; upper bound in y of extraction region
      yc:    yc2, $                  ; Y location of centerline of spectrum
      ycnew: yc2, $                  ; (possibly) improved centerline
      ycnewg: 0, $                   ; Flag, if true ycnew is valid.
      yg:    yg, $                   ; Y location of object in columns used in centerline fit
      ygn:   yg, $                   ; Iterated location of object in columns used in centerline
      yo:    yobj, $                 ; Y location of object(s)
      yp:    yp, $                   ; Discrete spatial profile, y coordinate
      yps:   ypsmoo, $               ; Original master profile (mate with xgrid)
      ypsi:  ypsmoo $                ; Iterated master profile (mate with xgrid)
      }

end

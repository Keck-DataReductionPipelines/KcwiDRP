;+
; NAME: 
;  slidefil
; PURPOSE: 
;  Sliding spatial filter on time series data.
; DESCRIPTION:
;  This program scan a data stream looking for short duration, non-random
;  excursions.  Primarily designed for scanning stellar occultation data,
;  you provide the data (y), the independent variable (x) and the filter
;  widths.  The equivalent width within the sliding window is computed
;  and returned alone with the x location for the window.  The spacing in
;  x does not need to be precisely constant but should not vary by much
;  across the window.
;
;  The data are scanned from the specified starting and ending POINT numbers.
;  This allows for x being double-valued with respect to the point index.
;
;  The sliding aperture looks like:
;
;     |----------------------|+++++X+++++|---------------------|
;      ^                      ^     ^ center of window
;      |                      edge of sliding window (hhw from center)
;      +- edge of background window (bhw from center)
;
;  A polynomial is fitted to the background points (-), default order is 1.
;  This polynomial extrapolated across the "gap" (+).  This extrapolation
;  is either divided into the window points (default), or subtracted (and
;  and 1.0 is added back.  The sum of the window points multipled by the
;  local dx is then the equivalent width of the signal in the window.
;
;  This filter is designed for scanning normalized data (full intensity=1
;  and no intensity=0).  The returned values of equivalent width are in
;  the same units as x.
;
; CATEGORY:
;  Numerical
; CALLING SEQUENCE:
;  slidefil,x,y,bhw,hhw,start,stop,xout,ew
; INPUTS:
;  x     - Independent vector (such as distance in km).
;  y     - Data vector.
;  bhw   - Half width (in units of x) of inner sliding window.
;  hhw   - Half width (in units of x) of outer (background) window.
;  start - Point number (not x) at start of scan
;  stop  - Point number (not x) near end of scan (won't go past, may stop
;             up to hhw-1 points early).
;
;   If either start or stop are too close to the edge for a full window,
;   the start and stop are adjusted inward as needed.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DX       - This is the derivative of X.  If the length is not the same
;                as X, or, if it is undefined, the derivative is computed
;                and returned to the supplied argument.  If the length
;                matches on the input, then this variable is assumed to
;                be the derivative and not recomputed.  This can save
;                significant CPU time on large vectors.
;  FULLPLOT - Flag, enable full plotting of all data.
;  NOPLOT   - Flag, supress all plots.
;  ORDER    - Order of polynomial to fit to background (def=1)
;  SILENT   - Flag, if true suppresses all printout to the IDL "console"
;  SUBTRACT - Flag, if true - background is subtracted, otherwise background
;               is ratioed.
;  XUNITS   - String that describes the units of X (default=km)
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;  IDXOUT - point index number for output points.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  1995/03/28 - Written by Marc W. Buie, Lowell Observatory
;  2009/10/02, MWB, removed obsolete Poly_fit arguments
;-
pro slidefil,x,y,hhw,bhw,start,stop,xout,ew, $
       IDXOUT=iout,ORDER=order,SUBTRACT=subtract, $
       NOPLOT=noplot,FULLPLOT=fullplot,SILENT=silent, $
       XUNITS=xunits,TITLE=title,DX=dx

   if badpar(x,[2,3,4,5],1,CALLER="SLIDEFIL (x) ",NPTS=in_npts) then return
   if badpar(y,[2,3,4,5],1,CALLER="SLIDEFIL (y) ",NPTS=ynpts) then return
   if badpar(hhw,[2,3,4,5],0,CALLER="SLIDEFIL (hhw) ") then return
   if badpar(bhw,[2,3,4,5],0,CALLER="SLIDEFIL (bhw) ") then return
   if badpar(start,[2,3],0,CALLER="SLIDEFIL (start) ") then return
   if badpar(stop,[2,3],0,CALLER="SLIDEFIL (stop) ") then return
   if badpar(order,[0,1,2,3],0,CALLER="SLIDEFIL (order) ",DEFAULT=1) then return
   if badpar(xunits,[0,7],0,CALLER="SLIDEFIL (xunits) ", $
                DEFAULT='km') then return
   if badpar(title,[0,7],0,CALLER="SLIDEFIL (title) ",DEFAULT='') then return
   if badpar(dx,[0,2,3,4,5],[0,1],CALLER="SLIDEFIL (dx) ",DEFAULT=0,NPTS=dxpts) then return

   if in_npts ne ynpts then begin
      print,'SLIDEFIL:  X and Y vectors must be the same length'
      return
   endif

   start=long(start)
   stop =long(stop)

   ; first compute dx/di (if needed)
   if dxpts ne in_npts then begin
      if not keyword_set(silent) then $
         print,'Computing DX vector'
	   dx = fltarr(in_npts)
	   dx[0:in_npts-2]=x[1:in_npts-1]-x[0:in_npts-2]
	   dx[in_npts-1]=dx[in_npts-2]
   endif

   maxdx=max(abs(dx))
   maxdi=long(bhw/maxdx)

   if not keyword_set(silent) then begin
	   print,'Maximum |dx| per point is ',maxdx,' km/point'
	   print,'Sliding window half-width is  ',hhw,' km'
	   print,'Baseline window half-width is ',bhw,' km'
	   print,'Maximum baseline window half-width is ',maxdi,' points'
   endif

   i_start = max([long(abs(bhw/dx[start])),start])
   i_stop  = min([stop,long(in_npts-maxdi-1)])

   if keyword_set(fullplot) and not keyword_set(noplot) then begin
	   !p.multi=0
	   lidx    = lindgen(in_npts)

	   setwin,2
	   plot,lidx,y,xtitle='Point Number',ytitle='Intensity',title=title,nsum=30
	   oplot,lidx[i_start:i_stop],y[i_start:i_stop],color=90,nsum=30

	   setwin,3
	   plot,lidx[i_start:i_stop],y[i_start:i_stop], $
         xtitle='Point Number',ytitle='Intensity',title=title

   endif

   if not keyword_set(silent) then begin
      print,'Actual point range for computation is ',i_start,i_stop
      print,'X range for computation is ',x[i_start],x[i_stop]
   endif

   if not keyword_set(noplot) then begin
	   loadct,39,/silent
	   setwin,0
	   !p.multi=[0,1,3]
      xr=minmax(x[i_start:i_stop])
	   plot,x[i_start:i_stop],y[i_start:i_stop], $
         xtitle=xunits,ytitle='Intensity',title=title,xr=xr
   endif

   ; allocate output vector for max possible points
   nest = 2*long((i_stop-i_start+1)/(hhw/maxdx))
   if not keyword_set(silent) then begin
      print,nest,' points allocated for output'
   endif
   ew = fltarr(nest)
   xout = fltarr(nest)
   iout = lonarr(nest)
   ybase = fltarr(nest)

   i    = i_start
   npts = 0L
   while i lt i_stop do begin
      xout[npts] = x[i]
      iout[npts] = i
      swhw  = long(abs(hhw/dx[i]))
      bwhw  = long(abs(bhw/dx[i]))
      i1=i-bwhw
      i2=i-swhw
      i3=i+swhw
      i4=i+bwhw

      ; grab the relevant bits of data
		xgrab=[x[i1:i2-1],x[i3+1:i4]]
		ygrab=[y[i1:i2-1],y[i3+1:i4]]

		xcen=x[i2:i3]
		ycen=y[i2:i3]

      ; fit polynomial to background
      c=poly_fit(xgrab-x[i],ygrab,order,yfit=yfit)
      fitcen=poly(xcen-x[i],c)
      ybase[npts]=mean(fitcen)
      if keyword_set(subtract) then $
         ycen=ycen-fitcen + 1.0 $
      else $
         ycen=ycen/fitcen
      ew[npts] = total( (1.0-ycen)*dx[i2:i3] )

      ; increment i
      i = min([i+swhw,in_npts-1])
      npts = npts +1
   endwhile

   if not keyword_set(silent) then begin
      print,npts,' total points actually computed'
   endif

   ; truncate to actual, if needed
   if nest ne npts then begin
      xout = xout[0:npts-1]
      iout = iout[0:npts-1]
      ybase = ybase[0:npts-1]
      ew   = ew[0:npts-1]
   endif

   if not keyword_set(noplot) then begin
      if !d.name ne 'PS' then $
   	   oplot,xout,ybase,color=100,psym=-8,symsize=0.5
	   plot,xout,ybase,xr=xr, $
         xtitle=xunits,ytitle='Intensity',title=title
	   plot,xout,ew,xr=xr, $
         xtitle=xunits,ytitle='Intensity',title=title

	   setwin,1
	   !p.multi=0
	   stats,ew,xtitle='Equivalent width in '+xunits,title=title,nbins=100,/robo
      setwin,0
   endif

end

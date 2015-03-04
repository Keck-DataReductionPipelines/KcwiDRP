;+
; NAME: 
;  plotspec
; PURPOSE: 
;  Plot OSIRIS XD spectral data with wavelength scale.
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  plotspec,calib,spec,YRANGE=yrange,TITLE=title,SMOOTH=smo_fac
; INPUTS:
;  calib- Anonymous structure containing all pertinent calibration
;           information.  This structure is usually loaded beforehand using
;           the routine, "ldcalir"
;  spec - 1-D spectrum vector.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BADFLAGS = Flag array, same length as spec.  If set, the data point is bad
;                and should not be plotted.  Default is to plot all.
;  COLOR  = color index for data plot, does not affect the axes and labels.
;             If a scalar is provided, the same color is applied to all orders.
;             A vector implies that the colors in the vector should be applied
;             to each order.  If the color "array" has fewer elements than
;             there are orders, then the last color is replicated to all the
;             remaining orders.
;  OPLOT  = flag, if set, plot is overplotted, axes are not redrawn.
;  XRANGE = (same as PLOT control, default is min to max), ignored if OPLOT set
;  YRANGE = (same as PLOT control, default is min to max), ignored if OPLOT set
;  TITLE  = (same as PLOT control, default is blank)
;  SMOOTH = smoothing factor for data, default is no smoothing.  Uses
;           the smooth() function on each order individually.
;  LOWESS = order of polynomial for lowess smoothing.  If lowess=0 (default)
;              then the smoothing is done using the IDL built-in smooth function.
;              for values of lowess > 0, lowess smoothing is done with this order
;              smoothing polynomial function.
;  NORDER = Order of spectrum to plot (default=plot all orders).  This can be
;             a vector that lists more than one order to plot.
;  NOWAVENUM = Flag, if set suppresses the wavenumber axis labeling on the
;             top of the plot.
;  Accepts all other PLOT and OPLOT keywords.
; OUTPUTS:
;  plot to current output device with wavelength scale.  Each order is
;    plotted a disconnected line from the other orders.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/03/24, Written by Marc W. Buie, Lowell Observatory
;  95/09/15, MWB, added calib structure usage.
;  96/05/28, MWB, mods for new calib structure.
;  98/06/12, MWB, added BADFLAGS keyword and wavenumber scale on top.
;  98/08/28, MWB, added LOWESS keyword smoothing option.
;-
pro plotspec,calib,spec,yrange=in_yr,TITLE=title,SMOOTH=smo_fac, $
   OPLOT=oplot,COLOR=in_color,xrange=in_xr,THICK=thick,YTITLE=ytitle, $
   NORDER=norder,BADFLAGS=bad,NOWAVENUM=nowavenum,LOWESS=lowess,_EXTRA=e

if badpar(calib,8,1,CALLER='plotspec (calib) ') then return
if badpar(spec,[1,2,3,4,5],1,CALLER='plotspec (spec) ',npts=nspec) then return
if badpar(smo_fac,[0,1,2,3],0,CALLER='plotspec (SMOOTH) ',DEFAULT=0) then return
if badpar(norder,[0,1,2,3],[0,1],CALLER='plotspec (NORDER) ',DEFAULT=-1) then return
if !d.name eq 'PS' then defcolor=0 else defcolor=!d.n_colors-1
if badpar(in_color,[0,1,2,3],[0,1],CALLER='plotspec (COLOR) ',DEFAULT=defcolor) then return
if badpar(thick,[0,1,2,3,4,5],0,CALLER='plotspec (THICK) ',DEFAULT=1.0) then return
if badpar(ytitle,[0,7],0,CALLER='plotspec (YTITLE) ', $
            DEFAULT='Arbitrary Flux') then return
if badpar(bad,[0,1,2,3],1,CALLER='plotspec (BADFLAGS) ', $
            DEFAULT=bytarr(nspec)) then return
if badpar(lowess,[0,1,2,3],[0,1],CALLER='plotspec (LOWESS) ',DEFAULT=0) then return
lowess=max([lowess,0])

if not keyword_set(title) then title=' '

if n_elements(in_color) lt calib.nor then begin
   color = [ in_color, $
             replicate(in_color[n_elements(in_color)-1], $
                          calib.nor-n_elements(in_color))  ]
endif else begin
   color = in_color[0:calib.nor-1]
endelse

if max(norder) ge calib.nor then begin
   print,'NORDER=',norder,' is invalid.  Ignoring request and plotting all orders.'
   norder=-1
endif

if norder[0] eq -1 then begin
   orlist = indgen(calib.nor)
endif else begin
   orlist = norder
endelse

nor = n_elements(orlist)

; Setup the wavelength (x) plotting range.
if keyword_set(in_xr) then begin
   xr = in_xr
endif else begin
   xr = minmax(calib.w[calib.o[orlist[0],0]:calib.o[orlist[0],1]])
   for i=1,nor-1 do begin
      xr = minmax([calib.w[calib.o[orlist[i],0]:calib.o[orlist[i],1]],xr])
   endfor
endelse

; Make a working copy of data and set all bad values to NaN
wspec = spec
z=where(bad eq 1,count)
if count ne 0 then wspec[z] = !values.f_nan

; Smooth the spectrum if requested.
if smo_fac ne 0 then begin
   if lowess eq 0 then begin
      for i=0,nor-1 do begin
         wspec[calib.o[orlist[i],0]:calib.o[orlist[i],1]] = $
            smooth(wspec[calib.o[orlist[i],0]:calib.o[orlist[i],1]],smo_fac,/nan,/edge)
      endfor
   endif else begin
      zg=where(bad eq 0,countg)
      swidth = smo_fac*max(abs(calib.cof[*,1]))
      lowess,calib.w[zg],spec[zg],swidth,ysmoo,order=lowess
      wspec[zg]=ysmoo
   endelse
endif

; Setup the intensity (y) plotting range.
if keyword_set(in_yr) then begin
   yr = in_yr
endif else begin
   yr = minmax(wspec[calib.o[orlist[0],0]:calib.o[orlist[0],1]],/nan)
   for i=1,nor-1 do begin
      yr = minmax([wspec[calib.o[orlist[i],0]:calib.o[orlist[i],1]],yr],/nan)
   endfor
endelse

; Generate the axes, no data yet.
if not keyword_set(oplot) then begin

   if keyword_set(nowavenum) then begin
      plot,[0],[1],/nodata,xr=xr,yr=yr,_EXTRA=e, $
         xtit='Wavelength (microns)',ytit=ytitle,title=title,ystyle=3,xstyle=3
   endif else begin
      plot,[0],[1],/nodata,xr=xr,yr=yr,_EXTRA=e, $
         ytit=ytitle,ystyle=3,xstyle=7, $
         ymargin=!y.margin+[0,4]
         
      axis,xaxis=0,xtit='Wavelength (microns)'
      if keyword_set(title) then $
         xyouts,!x.window[0],!y.window[1]+0.015+0.020/(!p.multi[2]>1), $
            title,charsize=1.25,align=0.0,/normal

      ; Major ticks (labeled)
      dw = xr[1]-xr[0]
      if dw gt 1.0 then       dwv = 1000L $
      else if dw gt 0.5  then dwv = 500L $
      else if dw gt 0.25 then dwv = 250L $
      else if dw gt 0.15 then dwv = 200L $
      else                    dwv = 100L
      wvnum0 = ceil(10000.0/xr[1]/dwv)*dwv
      wvnum1 = floor(10000.0/xr[0]/dwv)*dwv
      wvnum  = lindgen((wvnum1-wvnum0)/dwv+1)*dwv+wvnum0
      wave = 10000.0/wvnum
      wvnumlab = strcompress(wvnum)
if n_elements(wvnum) lt 1 or n_elements(wvnum) gt 60 then begin
   print,'PLOTSPEC:  xtickname is out of range, (not between 1 and 60)'
   help,wvnumlab,wave,wvnum
   help,dw,dwv
   print,'xrange ',xr[*]
   print,'wvnum0/1',wvnum0,wvnum1
   print,'wvnum',wvnum
   print,'wave',wave
   print,'wvnumlab ',wvnumlab
endif
      axis,xaxis=1,xtickv=wave,xticks=n_elements(wave)-1,xtickname=wvnumlab, $
         xtitle='Wavenumber (cm-1)'

      ; Minor ticks (not labeled)
      if keyword_set(xticklen) then ticklen=xticklen/2.0 $
      else if !x.ticklen ne 0.0 then ticklen=!x.ticklen $
      else ticklen = 0.01
      dwv = dwv/10
      wvnum0 = ceil(10000.0/xr[1]/dwv)*dwv
      wvnum1 = floor(10000.0/xr[0]/dwv)*dwv
      wvnum  = lindgen((wvnum1-wvnum0)/dwv+1)*dwv+wvnum0
      wave = 10000.0/wvnum
      wvnumlab = strarr(n_elements(wvnum))+' '
      axis,xaxis=1,xtickv=wave,xticks=n_elements(wave)-1,xtickname=wvnumlab, $
         xticklen=ticklen
   endelse

endif

for i=0,nor-1 do begin
   oplot,calib.w[calib.o[orlist[i],0]:calib.o[orlist[i],1]], $
         wspec[calib.o[orlist[i],0]:calib.o[orlist[i],1]], $
         color=color[i],thick=thick,_EXTRA=e
endfor

end

;+
; NAME:
;	occplot
; PURPOSE: (one line)
;	Plot small pieces of stellar occultation data.
; DESCRIPTION:
;	This procedure is intended to be used in plotting a very large 1-d
;	vector as a function of time in which you are only interested in
;	plotting a small subset of the data.  The time calibration is specified
;	through the tstart and dt input parameters and the time axis is
;	assumed to be strictly linear.  All time calculations are done in
;	double precision to ensure the full precision of the time axis is
;	saved.  Single precision gives out at the few millisecond level.
;
;	The current plotting device is used for output.  Some attempt has
;	been made to see that sensible plots come out even when using the
;	!p.multi system variable to make multiple plots on a page.
; CATEGORY:
;       2-D plotting
; CALLING SEQUENCE:
;	occplot,data,tstart,dt,tmid,width[,yspan]
; INPUTS:
;	data    - Vector containing the time-series occultation data.
;	tstart  - Time, in double precision hours, of the first data point
;	           or a three element vector [h,m,s]
;	dt      - Time increment between points in milliseconds.
;	tmid    - Mid-point of the plot in days or as a three element
;	           vector [h,m,s]
;	width   - Full width of the plot in seconds.
; OPTIONAL INPUT PARAMETERS:
;	yspan   - yrange of the plot (centered on the mean).  If not supplied
;	           the plot runs from the max to the min of the portion plotted.
;	           If yspan is two-element vector, the two values are used
;	           as the actual yrange on the plot.
; KEYWORD PARAMETERS:
;	connect - When set, the points are connected by a line.
;	over    - When set, the plot is done as an overlay on the current plot.
;	psym    - Symbol to use for plot.
;	title   - Title for plot
; OUTPUTS:
;	Plot goes to the current graphics device.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; MODIFICATION HISTORY:
;	12/29/92 - Written by Marc W. Buie, Lowell Observatory.  Patterned
;	             after the zodiac function, 'occgraf'
;
;	2/9/93   - Added option for yspan to allow direct specification of
;	             y plotting range.  MWB
;
;	2/10/93  - Fixed bug with top axis, axis cannot handle long or double
;	             precision numbers.
;-
pro occplot,data,tstart,dt,tmid,width,yspan, $
            over=over,psym=psym,title=title,connect=connect

;Verify command-line.  There MUST be 5 parameters, the sixth is optional.
   if n_params() lt 5 or n_params() gt 6 then begin
      print,'occplot,data,tstart,dt,tmid,width[,yspan]'
      return
   endif

;Setup the plotting symbol.
   if not keyword_set(psym) then psym=0

;Get the time of the first point.  If it is an array, it must be 3 element and
; is interpreted to be [hours,minutes,seconds].  If it is a scalar, it is
; taken to be in decimal hours.  The time is converted to decimal hours and
; is placed in t0 for use by the program so as not to modify the passed
; value.
   if (n_elements(tstart) eq 3) then begin
      t0 = double(tstart[0] + tstart[1]/60.0d0 + tstart[2]/3600.0d0)
   endif else if n_elements(tstart) eq 1 then begin
      t0 = double(tstart)
   endif else begin
      print,'tstart must be a scalar (in hours) or a 3 element vector [h,m,s]'
      return
   endelse

;Get the mid-time of the plot.  It can either be an array or scalar just like
; tstart.  Four variables are created from this input.  TCENP, TMP specify the
; center of the plot as [hm,s] and hours, respectively.  TCENL, TML are the
; same quantities only they have been rounded to the nearest second and are
; for use in labeling the x axis.
   if (n_elements(tmid) eq 3) then begin
      tcenp = tmid
      tmp   = double(tcenp[0] + tcenp[1]/60.0d0 + tcenp[2]/3600.0d0)
   endif else if n_elements(tmid) eq 1 then begin
      tmp   = tmid
      tcenp=fltarr(3)
      tcenp[0] = fix(tmid)
      tcenp[1] = fix((tmid-tcenp[0])*60.0)
      tcenp[2] = ((tmid-tcenp[0])*60.0-tcenp[1])*60.0
   endif else begin
      print,'tmid must be a scalar (in hours) or a 3 element vector [h,m,s]'
      return
   endelse

   tcenl = [ fix(tcenp[0]), fix(tcenp[1]), fix(tcenp[2] + 0.5) ]
   tml   = double(tcenl[0] + tcenl[1]/60.0d0 + tcenl[2]/3600.0d0)

;Convert dt to spacing in hours.
   dth = double(dt)/1000.0/3600.0

;Compute the start and stop times for the plot (in hours).
   t1 = tmp - width/7200.0
   t2 = tmp + width/7200.0

;Compute the point numbers that correspond to the start and stop times.
; The plot will show the data from i1 to i2.
   i1 = long( (t1-t0) / dth )
   i2 = long( (t2-t0) / dth )

;Some error checking, don't go outside valid data.
   if i1 lt 0 then i1=0
   if i2 ge n_elements(data) then i2=n_elements(data)-1
   if (i1 eq i2) then begin
      print,'OCCPLOT - ERROR. Plot specification includes only one valid point.'
      return
   endif else if (i1 ge n_elements(data)) then begin
      print,'OCCPLOT - ERROR. Plot specification is entirely after valid data.'
      return
   endif else if (i2 lt 0) then begin
      print,'OCCPLOT - ERROR. Plot specification is entirely before valid data.'
      return
   endif

;Compute the xaxis vector that matches the data (in seconds). The xaxis will
; be measured from the nearest second.
   t = dindgen(i2-i1+1)*double(dt)/1000.0d0 + ((double(i1)*dth+t0)-tml)*3600.0d0

;Compute the actual range for the x axis in relative hours.
   xr=[t1-tml,t2-tml]

;Compute the "point" numbers that the x axis range maps to.  Most of the
;  number must be subtracted off to prevent rounding errors in the axis call.
   ir= ([t1,t2] - t0)/dth
   izero=long(ir[0]/100.0d0)*100
   ir=ir-izero

;Convert the xaxis range to seconds.
   xr = xr * 3600.0

;This is a hack because the axis and xyouts windows don't resize with multiple
; plots on a page.  Take the biggest of the nx and ny values (if not zero)
; and scale charsize by 1 over the biggest.
   if !p.multi[1] ne 0 then csz = 1.0/(1.5-1.5/float(max(!p.multi[1:2]))) $
   else csz = 1.0

;Setup the y range on the data.
   if n_elements(yspan) eq 0 then begin
      yr=[min(data[i1:i2]),max(data[i1:i2])]
      if yr[0] eq yr[1] then yr[1]=yr[0]+1
   endif else if n_elements(yspan) eq 1 then begin
      ymid = (min(data[i1:i2])+max(data[i1:i2]))/2.0
      yr = [ymid-yspan/2,ymid+yspan/2]
   endif else begin
      yr=yspan[0:1]
   endelse

;Setup the axis labels.
   fs='(i2.2)'
   xt='Time in seconds relative to ' $
       + string(tcenl[0],form=fs) + ':' $
       + string(tcenl[1],form=fs) + ':' $
       + string(tcenl[2],form=fs)
   yt='Counts'

   if keyword_set(over) then begin
      oplot,t,data[i1:i2],psym=psym,symsize=csz
   endif else begin
      if not keyword_set(title) then title=''
      plot,t,data[i1:i2],xr=xr,yr=yr,subtitle=title, $
              xtit=xt,ytit=yt,psym=psym,xstyle=11,ymargin=[5,3],symsize=csz
      axis,xaxis=1,xstyle=3,xr=ir,xtickform='(i)', $
              xtitle='Point Number -'+string(izero),charsize=csz
   endelse

   if keyword_set(connect) then oplot,t,data[i1:i2],psym=0

end

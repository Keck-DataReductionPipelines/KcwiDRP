;+
; NAME:
;  jitterk
; PURPOSE:   (one line only)
;  Convert pointing jitter data into a convolution kernel.
; DESCRIPTION:
;  This routine is intended to take time-tagged pointing history data and
;     convert this to a convolution kernel that can be used to help model
;     imaging data.  The natural units of this routine is "pixels".  The
;     kernel is generated at this exact scale if SUBSAMPLE=1 (default).
;     The units do NOT change if you set SUBSAMPLE to something other than 1.
;     In that case you will get a kernel with higher spatial resoltion.  Note
;     that the units of dx,dy will be in pixels and will should not change
;     with SUBSAMPLE.
;  You should be able to take a model image at a matching sub-sampling factor,
;     and convolve with the kernel returned from this routine to get a model
;     image that matches the jitter history.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  jitterk,dt,dx,dy,exptime,kernel
; INPUTS:
;  dt - Vector, time from start of exposure.  If first element is not
;         dt=0 then (dt,dx,dy)=0 is assumed for the start.  Units are
;         in seconds.
;  dx - Vector (same length as dt), pointing offset in pixels.
;  dy - Vector (same length as dt), pointing offset in pixels.
;  exptime - Exposure time for kernel, in seconds.  The dt,dx,dy vectors
;              should include exptime but is not enforced.  Times outside
;              dt will be extrapolated.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SUBSAMPLE - sub-sampling factor for kernel.  Default=1
;  DTSAMPLE  - time sampling interval (default=0.1 seconds)
; OUTPUTS:
;  kernel - convolution kernel, will be square and will be normalized.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2007/10/23
;-
pro jitterk,dt,dx,dy,exptime,kernel,SUBSAMPLE=subsample,DTSAMPLE=dtsample, $
               MOVING=moving

   self='jitterk: '
   if badpar(dt,[2,3,4,5],1,caller=self+'(dt) ',npts=ndt) then return
   if badpar(dx,[2,3,4,5],1,caller=self+'(dx) ',npts=ndx) then return
   if badpar(dy,[2,3,4,5],1,caller=self+'(dy) ',npts=ndy) then return
   if badpar(exptime,[2,3,4,5],0,caller=self+'(exptime) ') then return
   if badpar(subsample,[0,2,3],0,caller=self+'(SUMSAMPLE) ',default=1) then return
   if badpar(dtsample,[0,2,3,4,5],0,caller=self+'(DTSAMPLE) ',default=0.1) then return
   if badpar(moving,[0,1,2,3],0,caller=self+'(MOVING) ',default=0) then return

   if ndt ne ndx or ndt ne ndy then begin
      print,self,' Error!  number of points in dt, dx, and dy must match.'
      return
   endif

   if dt[0] gt 0. then begin
      dt = [0.,dt]
      dx = [0.,dx]
      dy = [0.,dy]
   endif

   if moving then begin
      ; Take out a mean object motion rate, do this by fitting a polynomial
      ;  to the tracking data and remove this trend.
      bad=bytarr(n_elements(dx))
      xcoeff = goodpoly(dt,dx,1,100.0,xfit,bad=bad,max_value=1000.0)
      ycoeff = goodpoly(dt,dy,1,100.0,yfit,bad=bad,max_value=1000.0)
      z=where(bad eq 0)
      dt = dt[z]
      dx = dx[z] - xfit[z]
      dy = dy[z] - yfit[z]
   endif else begin
      z = where(dx lt 1000.0 and dy lt 1000.0,count)
      if count ne n_elements(dt) then begin
         dt = dt[z]
         dx = dx[z]
         dy = dy[z]
      endif
   endelse

   ; create master time vector
   npts = long(round(float(exptime)/(dtsample))+1)
   tvec = findgen(npts)*dtsample
   interp,dt,dx,tvec,xvec
   interp,dt,dy,tvec,yvec

   ; apply the sub-sampling factor
   xvec *= subsample
   yvec *= subsample

   ; find the mean position in x and y.  To make a kernel that won't shift
   ;  the images the kernel needs to be centered on the mean
   xmean = mean(xvec)
   ymean = mean(yvec)
   xvec  = round(xvec-xmean)
   yvec  = round(yvec-ymean)

;print,mean(xvec),mean(yvec)
;help,dx,xvec
;print,dx

xr=minmax([xvec,dx*subsample-xmean])
yr=minmax([yvec,dy*subsample-ymean])
plot,xvec,yvec,xr=xr,yr=yr,/iso
oplot,dx*subsample-xmean,dy*subsample-ymean,psym=-4,color='0000ff'xl

   ; create a kernel work space
   xdw=max(abs(xvec)) > 1
   ydw=max(abs(yvec)) > 1
   dw = max([xdw,ydw]) + subsample
;print,'kernel half size ',xdw,ydw
   xwid = 2*dw+1
   ywid = 2*dw+1
   kern1 = fltarr(xwid,ywid)

   ; convert the jitter position into indicies in the array
   loc = (round(yvec)+dw)*round(xwid) + (round(xvec)+dw)
   for i=0L,n_elements(kern1)-1 do begin
      z=where(loc eq i,count)
      if count ne 0 then kern1[i] = count
   endfor

   ; generate a slight gaussian fuzzing kernel to keep the jitter history
   ;   from being an infinitely sharp line.  The fwhm is set to move 5% of
   ;   the flux from the center pixel to the next ring of pixels outward.
   nx = 3 * subsample
   ny = 3 * subsample
   if (subsample/2) * 2 eq subsample then begin
      x0 = ((nx/2)-1 + nx/2)/2.0
      y0 = ((ny/2)-1 + ny/2)/2.0
   endif else begin
      x0 = nx/2
      y0 = ny/2
   endelse
   gauss2d,nx,ny,x0,y0,0.8*subsample,fuzz
   fuzz=fuzz/total(fuzz)

   ; create a larger array to hold kern1 for the convolution
   i0 = 1
   j0 = 1
   kern1b = fltarr(xwid+2,ywid+2)
   kern1b[i0,j0] = kern1

;help,kern1b,fuzz
   if xwid+2 ge nx then begin
;print,'kern1b oxo fuzz'
      kern2 = convol(kern1b,fuzz,/edge_truncate)
   endif else begin
;print,'fuzz oxo kern1b'
      kern2 = convol(fuzz,kern1b,/edge_truncate)
   endelse

   ; normalize and we're done
   kernel=kern2/total(kern2)

;print,'kern1'
;print,rotate(kern1,7)
;print,'kern2'
;print,rotate(kern2,7)
;print,'kernel'
;print,rotate(kernel,7)

;itool,kernel,/block

end

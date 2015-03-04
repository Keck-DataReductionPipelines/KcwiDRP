;+
; NAME:
;	sincfltr
; PURPOSE: (one line)
;	Pass 1-d data through a low-pass filter (damped sinc).
; DESCRIPTION:
;	This procedure will filter an array of data with a low pass filter.
;	The input value, smofac, determines the high-frequency cut-off in the
;	output data.  The new cutoff will be 1/smofac times the old cutoff
;	frequency which is defined to be 1/2 the sampling interval in the
;	original data.
;
;	Thus, a smofac of 1 will return the original array and a smofac greater
;	than one will reduce the resolution of the input by that factor.  Large
;	values of smofac ( > 6 ) should be avoided.  For large values it is
;	much faster to do the filtering in multiple steps (provided you
;	sub-sample the output vector).
;
;	The filter is a damped sinc function and requires 21*smofac points in
;	the convolution kernel.
;
;	Note: The convolution will not be complete for any data point near the
;	edge, so those points cannot be trusted.  The edge effect will be
;	larger for larger values of the smofac.
;
;	If smofac is greater than 2, not all smoothed points are required.
;	Since the filter reduces the band-limit of the data, you can
;	sub-sample the output array with no loss of information.  For instance,
;	a smofac=4 will reduce the resolution of the data by a factor of 4.
;	That means all you need to save is every fourth point to retain all
;	of the information in the smoothed vector, ie., a step_by of 4.
;	In practice, it is probably wise to use a step_by value that is slightly
;	smaller than the value of smofac, eg., smofac=6, step_by=5.
;
;	For a more detailed description of filtering, convolution, sub-sampling,
;	and other related topics, consult the excellent reference: "The Fourier
;	 Transform and Its Applications", by Ronald N. Bracewell, 2nd ed.,
;	 McGraw Hill.
;
; CATEGORY:
;       Numerical
; CALLING SEQUENCE:
;	ans = sincfltr(x,smofac,step_by)
; INPUTS:
;	x       - Input data to be smoothed.
;	smofac  - Smoothing factor relative to current sampling of data.
;	step_by - Sub-sampling factor for output vector (integer).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	return = smoothed and (possibly) sub-sampled data.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	92/11/03 - Ported from an equivalent Zodiac function written in C.
;		Marc W. Buie, Lowell Observatory.
;  94/08/29, MWB, fixed bug for when length of input not evenly divisible
;                 by the sub-sample factor.
;  95/03/29, MWB, fixed bug
;  2003/10/20, MWB, fixed error with convpts, now code forces convpts to
;                     be odd.  Previously, if it came up even then the
;                     output data was shifted by one point.
;-
function sincfltr,x,smofac,step_by

   DAMPFAC = 3.25
   EPS     = 1.0e-5
   MAXPER  = 8
   MINCONV = 21
   MAXCONV = MAXPER*MINCONV

   npts = n_elements(x)
   convpts = (fix(MINCONV * smofac)/2)*2 + 1

; Initialize the sinc (convolution) array
   y = fix(convpts/2)-findgen(convpts)
   arg = !pi * y / smofac
   const = (smofac*DAMPFAC)^2
   sinc = exp(-y*y/const)
   z = where(arg ne 0)
   sinc[z] = sinc[z]*sin(arg[z])/arg[z]
   sinc = sinc/smofac
   sinc = rotate(sinc,2)

; Convolve the sinc array with the input data.  This is the filter.
   xp = fltarr(npts/step_by,/nozero)
   for point=0L,long(npts/step_by)*step_by-1,step_by do begin
      outpt = point/step_by
      lobe = (indgen(convpts) - convpts/2) + point

      if (lobe[0] ge 0 and lobe[convpts-1] lt npts) then begin
         xp[outpt] = total(sinc*x[lobe])
      endif else begin
         vals = fltarr(convpts)
         z = where(lobe lt 0,count)
         if (count ne 0) then vals[z] = x[0]

         z = where(lobe ge 0 and lobe lt npts,count)
         if (count ne 0) then vals[z] = x[lobe[z]]

         z = where(lobe ge npts,count)
         if (count ne 0) then vals[z] = x[npts-1]

         xp[outpt] = total(sinc*vals)
      endelse

   endfor

   return,xp

end

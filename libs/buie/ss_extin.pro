;+
; NAME:
;  ss_extin
; PURPOSE:   (one line only)
;  Simple Spectra - detemine differential extinction
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  ss_extin,root,numlist,outfile
; INPUTS:
;  root    - String, root of file name
;  numlist - String, list of spectrum numbers to load.  For a full description
;               of the syntax allowed, see RANGEPAR.PRO.  A quick example:
;                 '10-15,20-25' would load numbers 10 through 15, inclusive
;                 and 20 through 25, inclusive.
;
;    The file names constructed will be root.NNNN.spec and is expected to
;       be a FITS file with the first column being wavelength and the second
;       being the spectrum.
;  outfile - String, name of output extinction coefficient file.  The
;               file is in FITS format and writes three columns, the first
;               is wavelength (same vector) as from original data.  The
;               second column is extinction in magnitudes per airmass
;               relative to the reference wavelength.  The extinction at
;               the reference wavelength is defined to be zero.  A positive
;               value has a higher extinction than the reference and a
;               negative value indicates lower extinction than the reference
;               wavelength.  The third column is the uncertainty on the
;               extinction.  Note that NaN is used to flag wavelengths
;               that have no valid data.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  REFWAVE - reference wavelength in same units as wavelength vector in
;               the spectrum file.  Default is 0.85 nm.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2004/06/14
;-
pro ss_extin,root,numlist,outfile,REFWAVE=refwave

   if n_params() lt 3 then begin
      print,'Usage: ss_extin,root,numlist,outfile'
      return
   endif

   self='SS_EXTIN: '

   if badpar(root,   7,0,caller=self+'(root) '   ) then return
   if badpar(numlist,7,0,caller=self+'(numlist) ') then return
   if badpar(outfile,7,0,caller=self+'(outfile) ') then return
   if badpar(refwave,[0,2,3,4,5],0,caller=self+'(REFWAVE) ', $
                default=0.85) then return

   rangepar,numlist,fnum
   nspec = n_elements(fnum)

   if nspec le 1 then begin
      print,self,'numlist must contain two or more numbers.'
      return
   endif

   ; Load all the spectra
   for i=0,nspec-1 do begin
      fn = root+'.'+string(fnum[i],format='(i4.4)')+'.spec'
      if not exists(fn) then begin
         print,'File ',fn,' not found. Aborting.'
         return
      endif
      data = readfits(fn,hdr)

      if i eq 0 then begin
         wave = data[*,0]
         npts=n_elements(wave)
         spec = fltarr(npts,nspec)
         am   = fltarr(nspec)
         z=where(abs(wave-refwave) eq min(abs(wave-refwave)))
         refidx = z[0]
      endif
      spec[*,i] = data[*,1]/data[refidx,1]
      am[i]=sxpar(hdr,'AIRMAsS')
      if am[i] lt 1.0 then begin
         read,newam,prompt=fn+': enter airmass '
         am[i] = newam
         sxaddpar,hdr,'AIRMASS',newam
         writefits,fn,data,hdr
      endif
   endfor

   setwin,0
   plot,[0],[1],/nodata,xr=minmax(wave),yr=minmax(spec)
   for i=0,nspec-1 do oplot,wave,spec[*,i]

   extin = fltarr(npts)

   for i=0,npts-1 do begin
      z=where(finite(spec[i,*]),count)
      if count eq 0 then begin
         extin[i] = !values.f_nan
      endif else begin
         mag = -2.5*alog10(spec[i,z])
         coeff = goodpoly(am[z],mag[z],1,3.0,0.5)
         extin[i] = coeff[1]
      endelse
   endfor

   setwin,1
   plot,wave,extin

   data = [[wave],[extin]]
   mkhdr,hdr,data
   sxaddpar,hdr,'ROOT',root,' Root of file names'
   sxaddpar,hdr,'NUMLIST',root,' Files used'
   writefits,outfile,data,hdr

end

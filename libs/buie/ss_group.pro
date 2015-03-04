;+
; NAME:
;  ss_group
; PURPOSE:   (one line only)
;  Simple Spectra: Average a group of spectra, correct to mean airmass
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  ss_group,root,numlist,outfile
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
;  outfile - String, name of output averaged spectrum
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
pro ss_group,root,numlist,outfile,REFWAVE=refwave,EXTIN=fnextin

   if n_params() lt 2 then begin
      print,'Usage: ss_group,root,numlist [,outfile]'
      return
   endif

   self='SS_GROUP: '

   if badpar(root,   7,0,caller=self+'(root) '   ) then return
   if badpar(numlist,7,0,caller=self+'(numlist) ') then return
   if badpar(outfile,[0,7],0,caller=self+'(outfile) ',default='') then return
   if badpar(refwave,[0,2,3,4,5],0,caller=self+'(REFWAVE) ', $
                default=0.85) then return
   if badpar(fnextin,[0,7],0,caller=self+'(EXTIN) ',default='') then return

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
         xidx = indgen(npts)
         spec = fltarr(npts,nspec)
         am   = fltarr(nspec)
         z=where(abs(wave-refwave) eq min(abs(wave-refwave)))
         refidx = z[0]
      endif
      spec[*,i] = data[*,1]
      am[i]=sxpar(hdr,'AIRMAsS')
      if am[i] lt 1.0 then begin
         read,newam,prompt=fn+': enter airmass '
         am[i] = newam
         sxaddpar,hdr,'AIRMASS',newam
         writefits,fn,data,hdr
      endif
   endfor

   if fnextin eq '' then begin
      extin = fltarr(npts)
   endif else begin
      if exists(fnextin) then begin
         data = readfits(fnextin)
         extin = data[*,1]
         extin = extin[*]
      endif else begin
         print,'Extinction file ',fnextin,' not found.  Aborting.'
         return
      endelse
   endelse

   ; Measure the flux level around the data by fitting a robust line to
   ;   a small region around the reference wavelength.
   refval = fltarr(nspec)
   ridx = refidx + (indgen(45)-22)
   for i=0,nspec-1 do begin
      coeff = goodpoly(xidx[ridx]-xidx[refidx],spec[ridx,i],1,2.0,0.5)
      refval[i] = coeff[0]
   endfor
   print,refval
   print,refval/max(refval)
   renormfac = refval/max(refval)
   refspec = where(refval eq max(refval))
   refspec = refspec[0]
   print,'Reference spectrum ', $
      root+'.'+string(fnum[refspec],format='(i4.4)')+'.spec'

   ; Renormalize the spectra
   cspec = spec
   for i=0,nspec-1 do begin
      cspec[*,i] = cspec[*,i]/renormfac[i]
   endfor

   ; Find mean airmass for this group
   meanerr2,am,renormfac,meanam
   print,'Mean airmass of set ',meanam

   ; Apply extinction correction
   for i=0,nspec-1 do begin
      dm = extin*(am[i]-meanam)
      flux = cspec[*,i]
      mag = -2.5*alog10(flux) - dm
      flux = 10.0^(mag/(-2.5))
      cspec[*,i] = flux
      print,i,am[i],meanam,am[i]-meanam
   endfor

   ; Sanity check for slope consistency
   ridx = refidx + (indgen(85)-42)
   for i=0,nspec-1 do begin
      if i ne refspec then begin
         coeff = goodpoly(xidx[ridx]-xidx[refidx], $
                          cspec[ridx,i]/cspec[ridx,refspec],1,2.0,0.5)
         slope = coeff[1]
      endif else begin
         slope = 0.0
      endelse
      print,root+'.'+string(fnum[i],format='(i4.4)')+'.spec ',slope
   endfor

   ; average spectra
   avgspec = fltarr(npts)
   for i=0,npts-1 do begin
      data = cspec[i,*]
      z=where(finite(data),count)
      if count eq 0 then begin
         avgspec[i] = !values.f_nan
      endif else begin
         meanerr2,data,renormfac,meanval
         avgspec[i] = meanval
      endelse
   endfor

   setwin,0
   plot,[0],[1],/nodata,xr=minmax(wave),yr=minmax(spec)
   for i=0,nspec-1 do oplot,wave,spec[*,i]

   setwin,2
   plot,[0],[1],/nodata,xr=minmax(wave),yr=minmax(spec)
   for i=0,nspec-1 do oplot,wave,cspec[*,i]
   oplot,wave,avgspec,color='0000ff'xl

   setwin,3
   plot,[0],[1],/nodata,xr=minmax(wave),yr=[0.5,1.5]
   for i=0,nspec-1 do oplot,wave,cspec[*,i]/spec[*,refspec]

   if outfile ne '' then begin
      writefits,outfile,[[wave],[avgspec]]
   endif

end

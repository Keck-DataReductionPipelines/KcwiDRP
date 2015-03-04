;+
; NAME:
;  osismean
; PURPOSE:
;  Compute mean of a set of OSIRIS XD spectra.
; DESCRIPTION:
;  Given a set of spectra, this will perform a straight average of the
;    provided spectra.  First, the relative signal level (relsig) is
;    computed between all members of the set.  This relative level is
;    normalized to 1.0 for the individual spectrum with the highest
;    signal level.  Then, only those spectra with relsig >= 0.8 will be
;    used for the set mean.  If there aren't 4 spectra in this clase, then
;    the brightest 4 spectra will be used, unless there aren't 4 spectra.
;    In this latter case, all spectra will be averaged.
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  osismean,spec,bad,specmean
; INPUTS:
;  spec  - Input spectrum to be cleaned up.
;  bad   - Array of flags for each point.  0=good, 1=bad.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  REF       - If provided, the relative signal levels are measure relative to
;                 this spectrum out of the stack.
;  ALL       - Flag, if true, forces averaging all spectra regardless of relsig.
;  NORMALIZE - Flag, if true, will renormalize all the spectra to REF prior to
;                 averaging.
;
; OUTPUTS:
;  relsig   - Relative signal levels among set of input spectra.
;  specmean - Average spectrum from the set of spectra.
;  badmean  - Bad flag vector for the average spectrum.
; KEYWORD OUTPUT PARAMETERS:
;  SIGMA    - Uncertainty of final weighted average output points.  Note, any
;                uncertainty of zero means there is no uncertainty.  This will
;                coincide mostly with points that aren't marked bad.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  98/08/13, Written by Marc W. Buie, Lowell Observatory
;  2000/10/1, MWB, added SIGMA keyword (calculation patterned after MEANERR2)
;-
pro osismean,spec,bad,relsig,specmean,badmean,REF=ref,ALL=all,NORMALIZE=norm, $
       WEIGHT=weight,SIGMA=sig

   if badpar(spec,[1,2,3,4,5],2,CALLER='OSISMEAN (spec) ',rank=specrank) then return
   if badpar(bad,[1,2,3],2,CALLER='OSISMEAN (bad) ',rank=badrank) then return
   if badpar(ref,[0,2,3],[0,1],caller='OSISMEAN: (REF) ',default=-1) then return
   if badpar(all,[0,2,3],0,caller='OSISMEAN: (ALL) ',default=0) then return
   if badpar(norm,[0,1,2,3],0,caller='OSISMEAN: (NORMALIZE) ',default=0) then return

   sz=size(spec,/dimensions)
   nspec = sz[1]
   npts  = sz[0]

   if badpar(weight,[0,2,3,4,5],1,caller='OSISMEAN: (WEIGHT) ', $
                                  default=replicate(1.0,nspec)) then return

   ; Protection against NaN values
   z=where(finite(spec) eq 0 and bad eq 0,count)
   if count ne 0 then bad[z]=1

   if norm then begin
;if ref ne -1 then print,'osismean: specific spectrum reference used: ',ref
      sigratio,spec,relsig,noise,bad=bad,REF=ref

      ; Select all spectra with a relsig greater than 0.8 unless ALL are wanted.
      if all then begin
         z=indgen(nspec)
         count=nspec
      endif else if ref ne -1 then begin
         z=where(relsig gt 0.8,count)
;print,'osismean: relsig selection,',count
      endif else begin
         snr=relsig/noise
         z=where(snr gt 0.7*max(snr),count)
;print,'osismean: snr selection,',count
      endelse

      ; If there aren't at least four good ones, then take the four brightest.
      if count lt 4 then begin
         if ref ne -1 then begin
            z=sort(relsig)
            z=z[max([0,nspec-4]):nspec-1]
            count = n_elements(z)
;print,'osismean: relsig, too few, now ',count
         endif else begin
            snr=relsig/noise
            z=sort(snr)
            z=z[max([0,nspec-4]):nspec-1]
            count = n_elements(z)
;print,'osismean: snr, too few, now ',count
         endelse
      endif
   endif else begin
      relsig=replicate(1.0,nspec)
      z=indgen(nspec)
      count=nspec
   endelse

   ; Average these good ones, be mindful of bad flags.
   sum  = fltarr(npts)
   nval = fltarr(npts)
   for i=0,count-1 do begin
      zg = where(bad[*,z[i]] eq 0,countg)
      if countg ne 0 then begin
         sum[zg]  = sum[zg]  + weight[z[i]]/relsig[z[i]]*spec[zg,z[i]]
         nval[zg] = nval[zg] + weight[z[i]]
      endif
   endfor
   badmean=bytarr(npts)
   zb = where(nval eq 0 or sum eq 0,countb)
   if countb ne 0 then badmean[zb]=1

   ; Construct denominator, just ratio points that are good.
   zmg = where(badmean eq 0,countg)
   specmean = replicate(1.0,npts)
   if countg eq 0 then begin
      print,'OSISMEAN: block cleaner.  Fatal error, everything is bad!'
      return
   endif
   specmean[zmg] = sum[zmg]/nval[zmg]

   ; Next, compute chi2 array for spectral average
   sum  = fltarr(npts)
   nval = fltarr(npts)
   for i=0,count-1 do begin
      zg = where(bad[*,z[i]] eq 0,countg)
      if countg ne 0 then begin
         sum[zg]  = sum[zg]  + ((specmean[zg]-spec[zg,z[i]]/relsig[z[i]])/weight[z[i]])^2
         nval[zg] = nval[zg] + 1.0
      endif
   endfor
   chi2=fltarr(npts)
   zok = where(nval gt 1,countok)
   if countok gt 0 then begin
      chi2[zok] = sum[zok]/(nval[zok]-1.0)
   endif
   sf = sqrt(chi2)

   ; Compute the final uncertainties
   sum  = fltarr(npts)
   for i=0,count-1 do begin
      zg = where(bad[*,z[i]] eq 0,countg)
      if countg ne 0 then begin
         sum[zg]  = sum[zg]  + sf[zg]*weight[z[i]]
      endif
   endfor
   sig = fltarr(npts)
   zok = where(sum gt 0.0,countok)
   if countok gt 0 then begin
      sig[zok] = sqrt(1.0/sum[zok])
   endif

end

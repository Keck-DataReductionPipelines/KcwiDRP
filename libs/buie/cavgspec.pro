;+
; NAME: 
;  cavgspec
; PURPOSE: 
;  Robust average of a set of 1-D spectra.
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  cavgspec,specset,bad,avgspec,avgspec_sig,badavg
; INPUTS:
;  specset - a 2-D array of spectra [pixel number,spectrum number]
;  bad     - a 2-D array of badflags [pixel number,spectrum number]
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SCALE - range of pixels to use for scaling all spectra to each other.
;          The default is to use all pixels for scaling.
;  REF   - Spectrum number to use as the scaling reference.  The scale
;          factors are determined relative to this one.
;
; OUTPUTS:
;  avgspec - Final averaged spectrum.
;  avgspec_sig - Uncertainties of the averaged spectrum.
;  badavg  - Bad flags for final average spectrum.
; KEYWORD OUTPUT PARAMETERS:
;  SCFACTOR  - Vector of relative scaling factors for each spectrum.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  98/07/17 - Written by Chris Dalla Piazza, Lycoming College.  Extracted from
;             the original avgspec program
;  98/07/17 - Added denominator filter for the sf part.  Indices were also
;             backwards for the badflags in that section.
;  98/08/17, MWB, added return of bad flags on the average.
;  2000/10/1, MWB, added avgspec_sig return
;-
pro cavgspec,specset,bad,avgspec,avgspec_sig,badavg,SCALE=scale,REF=ref,SCFACTOR=sf, $
       NORMALIZE=norm,WEIGHTED=weighted,WFACTOR=wf

   if badpar(specset,[4,5],2,caller='AVGSPEC: (SPECSET) ') then return
   if badpar(bad,[0,1,2,3],2,caller='AVGSPEC: (BAD) ') then return
   if badpar(scale,[0,2,3],[0,1],caller='AVGSPEC: (scale) ',default=-1) then return
   if badpar(ref,[0,2,3],0,caller='AVGSPEC: (reference) ',default=-1) then return
   if badpar(norm,[0,1,2,3],0,caller='AVGSPEC: (NORMALIZE) ',default=0) then return
   if badpar(weighted,[0,1,2,3],0,caller='AVGSPEC: (WEIGHTED) ',default=0) then return

   info=size(specset)
   npts=info[1]
   nspec=info[2]

   if scale[0] eq -1 then scale = [0,npts-1]

   osismean,specset,bad,relsig,avgspec,badavg,REF=ref,/ALL,NORMALIZE=norm, $
      SIGMA=avgspec_sig

   if weighted then begin

      ; Compute noise against the new mean spectrum
      sigratio,specset,nrelsig,noise,ref=avgspec,scale=scale,bad=bad,rbad=badavg

      ; Normalize the spectra to the same level as REF
      spec=specset
      for i=0,nspec-1 do begin
         spec[*,i]=specset[*,i]/nrelsig[i]
      endfor
      noise = noise/nrelsig

      weight = 1.0/noise^2
      weight = weight/max(weight)

      osismean,spec,bad,nrelsig,avgspec,badavg,weight=weight,SIGMA=avgspec_sig

      wf=weight

   endif

   sf=relsig

end

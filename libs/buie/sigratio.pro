;+
; NAME:
;  sigratio
; PURPOSE:
;  Compute the relative signal level between a set of spectra (1-d vectors).
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  sigratio,stack,relsig,noise,REF=ref
; INPUTS:
;  stack - an array of spectra [npts,nspec]
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  BAD       - Array of flags for each point in stack.  0=good, 1=bad. (default=0).
;  REF       - If provided, the relative signal levels are measure relative to
;                 this spectrum out of the stack.  You can also provide an
;                 external spectrum as a vector with npts.
;  RBAD      - Bad value flags for the reference (only used if REF is a spectrum).
;  SCALE     - range of pixels to use for scaling all spectra to each other.
;                 The default is to use all pixels for scaling.
; OUTPUTS:
;  relsig - Output array, NSPEC long, that has the relative signal levels.
;  noise  - Output array, NSPEC long, that is the noise level in each spectrum
;              relative to the reference.
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
;  98/07/24 - Written by Marc W. Buie, Lowell Observatory
;  98/08/14, MWB, added REF spectrum support, RBAD keyword, and noise output.
;  98/09/10, MWB, cleanup to eliminate illegal floating point operations.
;-
pro sigratio,stack,relsig,noise,REF=in_ref,RBAD=rbad,SCALE=scale,BAD=bad

   if badpar(stack,[2,3,4,5],2,caller='SIGRATIO: (stack) ') then return
   if badpar(scale,[0,2,3],[0,1],caller='SIGRATIO: (SCALE) ', $
                                 default=[-1,-1]) then return
   if badpar(in_ref,[0,2,3,4,5],[0,1],caller='SIGRATIO: (REF) ', $
                               default=-1,rank=refrank) then return
   ref=in_ref

   ; Get the working dimensions.
   sz=size(stack,/dimensions)
   nspec = sz[1]
   npts  = sz[0]

   ; Decide if we need to automatically find the reference.
   autoref = refrank eq 0 and (ref[0] eq -1 or ref[0] ge nspec)
   if autoref then ref=0

   if badpar(bad,[0,1,2,3],2,caller='SIGRATIO: (BAD) ', $
                             default=bytarr(npts,nspec)) then return

   ; Validate bad pixel flags on reference if reference is a spectrum.
   if refrank eq 1 then begin
      if badpar(rbad,[0,1,2,3],[0,1],caller='SIGRATIO: (RBAD) ', $
                                  default=bytarr(npts)) then return
   endif

   ; Protection against NaN values
   z=where(finite(stack) eq 0 and bad eq 0,count)
   if count ne 0 then bad[z]=1

   ; Default scaling region is the entire spectrum
   if scale[0] eq -1 then scale = [0,npts-1]

   ; Set up result array
   relsig = fltarr(nspec)
   noise  = fltarr(nspec)

   ; If autoref, combine all the spectra together for a reference.
   if autoref then begin

      sum=fltarr(scale[1]-scale[0]-1)
      nval=fltarr(scale[1]-scale[0]-1)
      for i=0,nspec-1 do begin
         zg=where(bad[scale[0]:scale[1],i] eq 0,count)
         if count ne 0 then begin
            sum[zg]=sum[zg]+stack[zg+scale[0],i]
            nval[zg]=nval[zg]+1
         endif
      endfor
      zg=where(nval ne 0.0,count)
      if count eq 0 then begin
         print,'SIGRATIO:  ERROR!  all spectral points are bad!'
         return
      endif
      sum[zg]=sum[zg]/nval[zg]
      rspec = sum
      refbad = nval eq 0.0
   
   endif else begin

      if refrank eq 0 then begin
         rspec  = stack[scale[0]:scale[1],ref]
         refbad = bad[scale[0]:scale[1],ref] or rspec eq 0.0
      endif else begin
         rspec  = ref[scale[0]:scale[1]]
         refbad = rbad[scale[0]:scale[1]] or rspec eq 0.0
      endelse

   endelse

   ratio=fltarr(n_elements(rspec))
   for i=0,nspec-1 do begin
      if (refrank eq 0 and i ne ref[0]) or refrank eq 1 or autoref then begin
         newbad=bad[scale[0]:scale[1],i] or refbad
         zg=where(newbad eq 0,count)
         if count ne 0 then begin
            ratio[zg]=stack[zg+scale[0],i]/rspec[zg]
            robomean,ratio,3.0,0.5,avg,dummy,stddev,bad=new
         endif else begin
            avg=1.0
            stddev=0.0
         endelse
      endif else begin
         avg=1.0
         stddev=0.0
      endelse
      relsig[i] = avg
      noise[i]  = stddev
   endfor

   ; Do the auto reference scaling if requested
   if autoref then begin

      idx = reverse(sort(relsig/noise))
      peak = max(relsig[idx[0:nspec/2]])

      relsig = relsig/peak > 1.0e-4
      noise  = noise/peak

;print,'SIGRATIO:'
;for i=0,nspec-1 do begin
;   print,i,relsig[i],noise[i],relsig[i]/noise[i]
;endfor

   endif

end

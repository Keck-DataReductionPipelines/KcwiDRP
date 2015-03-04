;+
; NAME:
;  objratio
;
; PURPOSE:
;  Ratio object spectra to one or more comp star spectra.
;
; DESCRIPTION:
;  Ratio object spectra to comp stars, combined appropriately to
;  match object airmasses.  For a single comp star, obviously you
;  just get a simple ratio - no real airmass correction.  For two
;  or more comp stars, an extinction is derived, and used to correct
;  each comp star spectrum to the object airmass, prior to averaging
;  the comp stars and performing the ratio.  If the /NOEXT keyword
;  is set, no extinction correction is derived or applied.
;
; CATEGORY:
;  Spectroscopy
;
; CALLING SEQUENCE:
;  objratio, calib, object, o_am, o_times, comps, c_am, c_times, ratio
;
; REQUIRED INPUTS:
;   object:  Array of object spectra [spectrum number, pixel number].
;   o_am:    Array of object airmasses.
;   o_times: Array of object mid-point exposure times.
;   comps:   Array of comp star spectra [spectrum number, pixel number].
;   c_am:    Array of comp star airmasses.
;   c_times: Array of comp star mid-point exposure times.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;   O_BAD:  Array of badflags for the object spectra.
;   C_BAD:  Array of badflags for the comp star spectra.
;   NOEXT:  Force simple averaging (disable airmass correction).
;
; OUTPUTS:
;   ratio:  Array of ratioed object spectra, same size as object array.
;
; KEYWORD OUTPUT PARAMETERS:
;   R_BAD:  The resulting badflags in the ratio spectra.
;
; OPTIONAL OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;  None.
;
; SIDE EFFECTS:
;  Extinction plot is plotted to window 0.
;
; RESTRICTIONS:
;
; PROCEDURE:
;  Currently, the times are ignored, but including them gives us the
;  option of later doing an elaborate solution for extinction changing
;  as a function of time, with a whole slew of comp stars, by changing
;  only this single routine.  Otherwise we'd later have to track down
;  and change all the instances where this routine is invoked.
;
; MODIFICATION HISTORY:
;  1998/07/12 WG   Created by Will Grundy, Lowell Observatory
;  1998/07/15 CDP  Changed program to accept the spectra as [pixel number,
;                       spectrum number]
;  1998/07/17 CDP  Added badflag filtering and creation for ratios
;  1998/08, MWB, Rewrote extinction calculation.
;  1998/09/08 WG   Generalized so that more than 2 comp stars can be
;        used and added /NOEXT keyword.
;  1998/09/09, MWB, Rewrote error checking and changed calling sequence.
;-
PRO objratio, calib, object, o_am, o_times, comps, c_am, c_times, ratio, $
      O_BAD=o_bad, C_BAD=c_bad, R_BAD=r_bad, NOEXT=noext

   ; Make sure al 7 input arguments are there
   IF n_params() lt 7 THEN BEGIN
      print,'ERROR: Procedure objratio requires 7 arguments:'
      print,'   calib:      Calibration structure,'
      print,'   object:     Array of object spectra [sp number, pix number],'
      print,'   o_am:       Array of object airmasses,'
      print,'   o_times:    Array of object mid-point exposure times,'
      print,'   comps:      Array of normalized comp star spectra,'
      print,'   c_am:       Array of comp star airmasses,'
      print,'   c_times:    Array of comp star mid-point exposure times,'
      print,'   ratio:      Array of ratio spectra (output).'
      return
   ENDIF

   if badpar(calib,  8,        1,    CALLER='objratio (calib) ')           then return
   if badpar(object, [2,3,4,5],2,    CALLER='objratio (object) ')          then return
   if badpar(o_am,   [4,5],    1,    CALLER='objratio (o_am) ',   npts=n1) then return
   if badpar(o_times,[4,5],    1,    CALLER='objratio (o_times) ',npts=n2) then return
   if badpar(comps,  [2,3,4,5],[1,2],CALLER='objratio (comps) ')           then return
   if badpar(c_am,   [4,5],    1,    CALLER='objratio (c_am) ',   npts=n3) then return
   if badpar(c_times,[4,5],    1,    CALLER='objratio (c_times) ',npts=n4) then return

   sz = size(object)
   nobj = sz[2] ; Number of object spectra
   ndat = sz[1] ; Number of x values in spectra (needs to match calib.npts)
   sz = size(comps)
   ncdat = sz[1]
   if sz[0] eq 1 then ncom=1 else ncom = sz[2] ; Number of comp star spectra

   if badpar(o_bad, [0,1,2,3], 2, CALLER='objratio (O_BAD) ', $
                                  default=bytarr(ndat,nobj)) then return
   if badpar(c_bad, [0,1,2,3], 2, CALLER='objratio (C_BAD) ', $
                                  default=bytarr(ndat,nobj)) then return
   if badpar(noext,[0,1,2,3],0,CALLER='objratio (NOEXT) ',default=0) then return

   sz = size(o_bad)
   nobjb = sz[2]
   ndatb = sz[1]
   sz = size(c_bad)
   ncdatb = sz[1]
   if sz[0] eq 1 then ncomb=1 else ncomb = sz[2]

   ; Spectra must be of a consistent length.
   z=where([ndat,ncdat,ndatb,ncdatb] ne calib.npts,count)
   if count ne 0 then begin
      for i=0,count-1 do begin
         case z[i] OF
            0: print,'OBJRATIO: Error!  length of object spectra must match calib.npts'
            1: print,'OBJRATIO: Error!  length of comp star spectra must match calib.npts'
            2: print,'OBJRATIO: Error!  length of object bad flags must match calib.npts'
            3: print,'OBJRATIO: Error!  length of comp star bad flags must match calib.npts'
         endcase
      endfor
   endif

   if nobj ne nobjb then begin
      print,'OBJRATIO: Error!  Object spectra and object bad flags do not match'
      return
   endif

   if ncom ne ncomb then begin
      print,'OBJRATIO: Error!  Comp spectra and comp bad flags do not match'
      return
   endif

   ; Object information must be consistent
   z=where([n1,n2] ne nobj,count)
   if count ne 0 then begin
      for i=0,count-1 do begin
         case z[i] OF
            0: print,'OBJRATIO: Error!  length of o_am must match number of object spectra'
            1: print,'OBJRATIO: Error!  length of o_times must match number of object spectra'
         endcase
      endfor
   endif

   ; Comp star information must be consistent
   z=where([n3,n4] ne ncom,count)
   if count ne 0 then begin
      for i=0,count-1 do begin
         case z[i] OF
            0: print,'OBJRATIO: Error!  length of c_am must match number of comp spectra'
            1: print,'OBJRATIO: Error!  length of c_times must match number of comp spectra'
         endcase
      endfor
   endif

   ; ---------------- All the arguments must be OK now. ----------------

   ; Setup output arrays
   r_bad = bytarr(ndat,nobj)
   ratio = fltarr(ndat,nobj)

   ; Deal with the case of a single comp star (can't do extinction correction).
   if ncom eq 1 then begin

      for i=0,nobj-1 do begin
         zg=where(finite(comps) eq 1 and comps ne 0)
         ratio[zg,i] = object[zg,i]/comps[zg]
         r_bad[*,i]  = o_bad[*,i] or c_bad[*,i]
         zb=where(finite(comps) eq 0 and comps eq 0 and c_bad eq 0,count)
         if count ne 0 then r_bad[zb,i]=1
      endfor

   ; This takes care of multiple comp stars.
   endif else begin

      kspec   = fltarr(ndat)        ; Extinction mag/airmass
      magspec = fltarr(ndat,ncom)   ; Comp star spectra in magnitudes
      magbad  = replicate(1B,ndat,ncom)   ; Bad flags for the comp magnitude
      tot_xx  = fltarr(ndat)        ; Accumulator for X^2
      tot_xy  = fltarr(ndat)        ; Accumulator for X*Y
      tot_x   = fltarr(ndat)        ; Accumulator for X
      tot_y   = fltarr(ndat)        ; Accumulator for Y
      n       = intarr(ndat)        ;
      kbad    = replicate(1B,ndat)  ; Bad flags for extinction.
      magref  = fltarr(ncom)        ; Reference magnitude for each comp star.

      ; The average magnitude of each comp star spectrum is to be computed
      ;   over a reference window where extinction is expected to be low.
      ref = where(calib.w gt 2.1 and calib.w lt 2.18,countref)
      if countref lt 1 then begin
         print,'objratio: Error!  No data in magnitude reference window.'
         return
      endif

      ; Compute magnitude spectra for comp star, watch for bad values.
      ;   Also accumulate the sums we need for the extinction calculation.
      for i=0,ncom-1 do begin

         ; Bad if flagged as such or if the flux is negative or zero.
         zg = where(comps[*,i] gt 0.0 and c_bad[*,i] eq 0,count)
         magbad[zg,i] = 0B

         ; Only compute magnitude where it's good.
         magspec[zg,i] = -2.5*alog10(comps[zg,i]) + 20.0

         robomean,magspec[ref,i],3.0,0.5,rmag,bad=magbad[ref,i]
         magref[i] = rmag

         magspec[zg,i] = magspec[zg,i] - rmag

         n[zg]      = n[zg] + 1
         tot_xx[zg] = tot_xx[zg] + c_am[i]^2
         tot_xy[zg] = tot_xy[zg] + c_am[i]*magspec[zg,i]
         tot_x[zg]  = tot_x[zg]  + c_am[i]
         tot_y[zg]  = tot_y[zg]  + magspec[zg,i]

      endfor

      print,'magref = ',magref

      zg = where(n ge 2, countg)
      if countg eq 0 then begin
         print,'OBJRATIO: Fatal error, comp stars are all negative or zero'
         return
      endif
      kbad[zg]=0

      ; This is the average comp star spectrum
      cavg      = fltarr(ndat)
      amavg     = fltarr(ndat)
      cavg[zg]  = tot_y[zg]/float(n[zg]) + magref[0]
      amavg[zg] = tot_x[zg]/float(n[zg])

      ; kspec is extinction in mag/airmass.
      if not noext then begin

         ; compute extinction from sums
         kspec[zg] = ( n[zg]*tot_xy[zg] - tot_x[zg]*tot_y[zg] ) / $
             ( n[zg]*tot_xx[zg] - tot_x[zg]*tot_x[zg] )

         ; apply a little bit of smoothing to extinction curve vs. wavelength.
         swidth = 4*max(abs(calib.cof[*,1]))
         lowess,calib.w[zg],kspec[zg],swidth,ysmoo,order=3
         kspec[zg]=ysmoo

         ; Put up a plot of extinction
         setwin,0
         plotspec,calib,kspec,yr=[4.0,-2.0],bad=kbad, $
            ytit='Extinction (mag/airmass)'

      endif

      ; Now, set about correcting all the input spectra given the extinction.
      comp = fltarr(ndat)

      for i=0,nobj-1 do begin ; Loop through object spectra

         ; Join the bad flags between object and extinction.
         r_bad[*,i] = kbad or o_bad[*,i]

         ; Computation done only on good average spec points and finite object
         ;    points, this may include object points that are known to be bad.
         zg = where(kbad eq 0 and finite(object[*,i]) eq 1)

         ; Average comp is corrected to object airmass.
         comp[zg] = (o_am[i] - amavg[zg])*kspec[zg] + cavg[zg]

         ; Divide object spectrum by comp.
         ratio[zg,i] = object[zg,i]/(10.0^((comp[zg]-20.0)/(-2.5)))

      endfor

   endelse ; end of multiple comp star block

end


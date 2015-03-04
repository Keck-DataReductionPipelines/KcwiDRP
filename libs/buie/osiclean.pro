;+
; NAME:
;  osiclean
; PURPOSE:
;  Automatic cleaning of bad and low signal data from OSIRIS XD spectrum
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  osiclean,calib,spec,bad
; INPUTS:
;  calib - Calibration structure (see ldcalir for description).
;  spec  - Input spectrum to be cleaned up.
;  bad   - Array of flags for each point.  0=good, 1=bad.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  DVAL  - Used for cleaning a set of spectra.  This is the threshold relative
;           to unity for clipping the ratio of a spectrum against the set
;           average.  (default=10)
;
;  SILENT - Flag, if set suppresses the conversational printout.
;
; OUTPUTS:
;  bad   - Array of flags for each point.  0=good, 1=bad.
;
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
;  98/07/08, Written by Marc W. Buie, Lowell Observatory
;  98/07/22, MWB, added protection against NaN values (automatically bad).
;  98/07/24, MWB, added set cleaner (when given more than one spectrum).
;
;-
pro osiclean,calib,spec,bad,DVAL=dval,SILENT=silent

   if badpar(calib,8,1,CALLER='OSICLEAN (calib) ') then return
   if badpar(spec,[1,2,3,4,5],[1,2],CALLER='OSICLEAN (spec) ', $
                                    rank=specrank) then return
   if badpar(bad,[1,2,3],[1,2],CALLER='OSICLEAN (bad) ', $
                               rank=badrank) then return
   if badpar(dval,[0,2,3,4,5],0,CALLER='OSICLEAN (DVAL) ', $
                                default=10.0) then return
   if badpar(silent,[0,1,2,3],0,CALLER='OSICLEAN (SILENT) ', $
                                default=0) then return

   if specrank ne badrank then begin
      print,'OSICLEAN: rank of spectra and bad flags must match.'
      return
   endif

   ; Protection against NaN values
   z=where(finite(spec) eq 0 and bad eq 0,count)
   if count ne 0 then bad[z]=1

   ; Clean just one spectrum
   if specrank eq 1 then begin

      for i=0,calib.nor-1 do begin

         i1=calib.o[i,0]
         i2=calib.o[i,1]

         ; Smoothing width for lowess depends on the dispersion
         width = abs(calib.cof[i,1])*4

         ; Make sure not to manipulate bad values.
         zg = where(bad[i1:i2] eq 0,countg)

         ; There must be something to work with at this point.
         if countg gt 2 then begin

            w=replicate(1.0,countg)

            ; First pass on lowess, equal weighting.
            lowess,calib.w[zg+i1],spec[zg+i1],width,sm,order=2,weight=w

            ; Find the mean difference between the original and smoothed curve.
            robomean,spec[zg+i1]-sm,3.0,0.5,avg,avgdev,sig

            ; Recompute weight, broad gaussian relative to difference.
            w = w*exp(-( ((spec[zg+i1]-sm)/(5.0*sig))^2 < 50.0))

            ; Second lowess pass, this time with modified weights.
            lowess,calib.w[zg+i1],spec[zg+i1],width,sm,order=2,weight=w

            ; Bad values are those that deviate unusually from smoothed curve.
            tmpbad=bad[zg+i1]
            robomean,spec[zg+i1]-sm,5.0,0.5,avg,avgdev,tsig,bad=tmpbad
            bad[zg+i1]=tmpbad

         endif

      endfor ; end order loop

   ; Clean a block of related spectra.
   endif else begin

      pass=0

      sz=size(spec,/dimensions)
      nspec = sz[1]
      npts  = sz[0]
      ratio=fltarr(npts,nspec)
      minval = 1.0 - dval
      maxval = 1.0 + dval
      oldcnt=long(total(bad))

      repeat begin
         oldbad=bad

         osismean,spec,bad,relsig,denom,badsum,/norm

         ; Divide input spectra by the average, scaled by relsig.  Force
         ;   the ratio at bad points to be 1.0 for the next step.
         ratio[*] = 1.0
         for i=0,nspec-1 do begin
            zg=where(bad[*,i] eq 0,countg)
            if countg ne 0 then ratio[zg,i] = spec[zg,i]/relsig[i]/denom[zg]
         endfor

         ; At each wavelength (x), find the ratio point furthest from unity.
         maxloc,abs(ratio-1.0),xpos,ypos,/X

         ; Flag those points that fall outside the valid range.
         zb=where(ratio[xpos,ypos] lt minval or ratio[xpos,ypos] gt maxval,countb)
         if countb ne 0 then bad[xpos[zb],ypos[zb]]=1

         ; Some feedback for the screen (if wanted).
         if not silent then begin
            str=strcompress(string('(',countb,':',long(total(bad))-oldcnt, $
                                   ')'),/remove_all)
            print,str,format='($,a)'
         endif

         pass=pass+1

      endrep until countb eq 0

      if not silent then print,'  ',strcompress(pass)

   endelse

end

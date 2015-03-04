;+
; NAME: 
;  avgspec
; PURPOSE: 
;  Robust average of a set of 1-D spectra from FITS files.
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  avgspec,root,outsuf,start,nspec,result
; INPUTS:
;  root   - Root of file name(s) (no . at the end, may include path).
;  outsuf - Suffix for the output file name.  (name will be root+'.'+outsuf)
;  start  - First spectrum number of sequence to average.
;  last   - Last spectrum number of sequence to average. (if negative, this
;             number is interpreted to be the number of spectra to average).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  EXCLUDE   - Scalar or vector of spectrum numbers to exclude from average.
;  SCALE     - range of pixels to use for scaling all spectra to each other.
;              The default is to use all pixels for scaling.
;  REFERENCE - Spectrum number to use as the scaling reference.  The scale
;              factors are determined relative to this one.
;  OUTROOT   - Root of output file name, default=root
;
;  Values for header in output spectrum
;
;  JD        - JD of midtime of output average spectrum (default=none)
;  AIRMASS   - Effective airmass of spectrum (default=none)
;  OBJECT    - Name of object (default=none)
;
; OUTPUTS:
;  result - Final averaged spectrum.  This is also saved to a FITS file.
; KEYWORD OUTPUT PARAMETERS:
;  SCFACTOR  - Vector of relative scaling factors for each spectrum.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/09/18 - Written by Marc W. Buie, Lowell Observatory
;  96/05/29, MWB, changed 4th argument to LAST from NSPEC
;-
pro avgspec,root,outsuf,start,last,result,WEIGHTED=weighted, $
   EXCLUDE=exclude,SCALE=scale,REFERENCE=reference,OUTROOT=outroot, $
   JD=jd,AIRMASS=airmass,OBJECT=object,SCFACTOR=sf

if badpar(root,   7,    0,caller='AVGSPEC: (root) '   ) then return
if badpar(outsuf, 7,    0,caller='AVGSPEC: (outsuf) ' ) then return
if badpar(start,  [2,3],0,caller='AVGSPEC: (start) '  ) then return
if badpar(last,[2,3],0,caller='AVGSPEC: (last) ') then return
if badpar(reference,[0,2,3],0,caller='AVGSPEC: (reference) ',default=-1) then return
if badpar(exclude,[0,2,3],[0,1],caller='AVGSPEC: (exclude) ',default=-1) then return
if badpar(scale,[0,2,3],[0,1],caller='AVGSPEC: (scale) ',default=[-1,-1]) then return
if badpar(outroot,[0,7],0,caller='AVGSPEC: (OUTROOT) ',default=root) then return
if badpar(jd,[0,5],0,caller='AVGSPEC: (JD) ',default=-1.0) then return
if badpar(airmass,[0,2,3,4,5],0,caller='AVGSPEC: (AIRMASS) ',default=-1.0) then return
if badpar(weighted,[0,1,2,3],0,caller='AVGSPEC: (WEIGHTED) ',default=0) then return

nspec = last - start + 1

specs=start+indgen(nspec)
for i=0,nspec-1 do begin
   z=where(specs[i] eq exclude,count)
   if count ne 0 then specs[i]=-1
endfor

; Check to see that each file not excluded exist.  If it doesn't, exclude it.
for i=0,nspec-1 do begin
   if specs[i] ne -1 then begin
      fname = root+'.'+string(specs[i],format='(i3.3)')
      if not exists(fname) then specs[i] = -1
   endif
endfor

z=where(specs ne -1,count)
if count eq 0 then $
   message,'Error ** you have excluded all frames, nothing to do.'

; Get the header from the first spectrum to get size of vector
hdr = headfits(root+'.'+string(specs[z[0]],format='(i3.3)'))
npts=sxpar(hdr,'NAXIS1')

; Now, load all the spectra from the FITS files.
stack=fltarr(npts,count,/nozero)
bad  =bytarr(npts,count)
bad0 =bytarr(npts)
j=0
totalexp = 0.0
for i=0,nspec-1 do begin
   if specs[i] ne -1 then begin
      ; Read FITS file to get spectrum
      fname = root+'.'+string(specs[i],format='(i3.3)')
      tmp = readfits(fname,hdr,/silent)
      exptime=float(sxpar(hdr,'EXPTIME',comment=comment))
      if strmid(comment,0,12) ne ' (GETSTRIP) ' then exptime=exptime+0.9
      avego  =float(sxpar(hdr,'AVEGO'))
      totalexp = totalexp + exptime*avego
      stack[*,j] = tmp/exptime/avego
      ; Look for bad flags file and read if there.
      IF strmid(root,strlen(root)-1,1) eq 's' THEN $
         bname = strmid(root,0,strlen(root)-1) $
      ELSE $
         bname = root
      bname = bname+'b.'+string(specs[i],format='(i3.3)')
      IF exists(bname) THEN BEGIN
         openr,lun,bname,/get_lun
         readu,lun,bad0
         free_lun,lun
         bad[*,j]=bad0
      ENDIF
      j=j+1
   endif
endfor
oldbad=bad

cavgspec,stack,bad,result,badavg,SCALE=scale,REF=reference,SCFACTOR=sf, $
   WEIGHTED=weighted,/NORM

; Save final average spectrum to a FITS file.
fname = outroot+'.'+outsuf
mkhdr,hdr,result
sxdelpar,hdr,'DATE'
if object  ne ''  then sxaddpar,hdr,'OBJECT',object
if jd      gt 0.0 then sxaddpar,hdr,'JD',jd,' Effective mid-time of spectrum',format='f13.5'
sxaddpar,hdr,'EXPTIME',totalexp,' Total integration time of spectrum (seconds)'
if airmass gt 0.0 then sxaddpar,hdr,'AIRMASS',airmass,' Mean airmass of spectrum'
sxaddpar,hdr,'INSTRUME','OSIRIS XD'
writefits,fname,result,hdr

end

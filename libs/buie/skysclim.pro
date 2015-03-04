;+
; NAME:
;  skysclim
; PURPOSE:
;  Compute stretch range for a hard stretch on the background in an image.
; DESCRIPTION:
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  skysclim,image,lowval,hival,meanval,sigma
; INPUTS:
;  image - 2-d image to compute stretch range for.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NPTS - Maximum number of points to grab at random from image for scaling
;           information.  Default=601.
;  LOWCLIP - fraction of random sample to clip at the low end of the signal.
;              If lowclip=.1 and npts=100, then the 10 lowest values in the
;              random sample are excluded BEFORE the robust mean is computed
;              for the stretch range.  This option will probably be just a bit
;              slower if invoked.  This option will likely be more robust against
;              extreme values in the image.  Default=0.0 (no clipping)
;  HICLIP  - fraction of random sample to clip at the high end of the signal.
;              If hiclip=.9 and npts=100, then the 10 highest values in the
;              random sample are excluded BEFORE the robust mean is computed
;              for the stretch range.  This option will probably be just a bit
;              slower if invoked.  This option will likely be more robust against
;              extreme values in the image.  Default=1.0 (no clipping)
;  THRESH  - sigma threshold for excluding points (default=3.0)
;  SEED    - value for the seed used by the random call. Default is undefined
;              causing random to use the system time as the seed
; OUTPUTS:
;  lowval - Low DN value for sky stretch  (meanval-3.0*sigma)
;  hival  - High DN value for sky stretch (meanval+5.0*sigma)
;  meanval - Mean of random sample
;  sigma   - standard deviation of random sample
; KEYWORD OUTPUT PARAMETERS:
;  SAMPLE  - Optional return of the sample of points from the sky that was
;              considered to be representative of the sky.  The length of
;              this array will be less than or equal to NPTS.
;            WARNING!  To preserve a faster execution option if you don't
;              want this sample returned a non-existant variable for this
;              keyword is treated the same as no keyword at all.  If you do
;              pass an undefined variable you will get the sample back but
;              it will contain the entire sky sample, not the sample cleaned
;              of non-sky values.  The input variable can be just about
;              anything to ensure getting back a cleaned sample.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/01/07 - Marc W. Buie
;  2000/02/29, MWB, added NPTS keyword
;  2002/11/17, MWB, added LOWCLIP, HICLIP keywords
;  2002/12/18, MWB, fixed bug causing crash when image is smaller than npts.
;  2005/11/22, MWB, added SAMPLE and THRESH keywords
;  2010/03/12, Florian Scheulen, added SEED keyword
;-
pro skysclim,image,lowval,hival,meanval,sigma, $
   NPTS=npts,LOWCLIP=lowclip,HICLIP=hiclip,SAMPLE=sample,THRESH=thresh, $
   SEED=seed

   self='SKYSCLIM: '
   if badpar(npts,[0,2,3],0,caller=self+'(NPTS) ',default=601) then return
   if badpar(lowclip,[0,4,5],0,caller=self+'(LOWCLIP) ',default=0.0) then return
   if badpar(hiclip,[0,4,5],0,caller=self+'(HICLIP) ',default=1.0) then return
   if badpar(sample,[0,1,2,3,4,5],[0,1,2],caller=self+'(sAMPLE) ',type=sampletype) then return
   if badpar(thresh,[0,4,5],0,caller=self+'(THRESH) ',default=3.0) then return
   if badpar(seed,[0,2,3,4,5],[0,1],caller=self+'(SEED) ') then return

   lowclip=lowclip > 0.0
   hiclip = hiclip < 1.0

   cnpts = min([npts,n_elements(image)])

   ; generate random indicies to get a random sample of the image
   idx=randomu(seed,cnpts)*(n_elements(image)-1)

   ; extract portion
   sample = image[idx]
   bad = bytarr(cnpts)

   if lowclip gt 0.0 or hiclip lt 1.0 then begin

      ; sort
      s = sort( sample )

      ; get index bounds of clipped region
      t1 = long(lowclip * cnpts) > 0L
      t2 = long(hiclip * cnpts) < long(cnpts-1)

      tmpbad = bad[s[t1:t2]]
      bad[*] = 1B
      robomean,sample[s[t1:t2]],thresh,0.5,meanval,dummy,sigma,bad=tmpbad
      bad[s[t1:t2]] = tmpbad

   endif else begin

      robomean,sample,thresh,0.5,meanval,dummy,sigma,bad=bad

   endelse

   if sampletype ne 0 then begin
      z=where(bad eq 0,count)
      if count ne 0 then sample = sample[z]
   endif

   lowval=meanval-3.0*sigma
   hival=meanval+5.0*sigma

end

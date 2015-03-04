;+
; NAME:
;  mmaspli
; PURPOSE:
;  Splice WIYN MMA images back together.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
;
; CALLING SEQUENCE:
;  mmaspli,filelist,outdir
;
; INPUTS:
;  filelist - List of flies to process.
;  outdir  - The directory to write the calibrated files to.  Don't make
;              this the same as the input directory.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;   The calibrated images are written to outdir.  Don't make this the same
;   as the current directory!
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
;  2000/02/28, Written by Marc W. Buie, Lowell Observatory
;  2001/08/19, MWB, modified to work on any even number of groups.
;  2002/03/14, MWB, added special case for NEXTEND from 2 down to 1.
;-
pro mmaspli,filelist,outdir

   if badpar(filelist,7,[0,1],caller='MMASPLI: (fillist) '   ) then return
   if badpar(outdir, 7,    0,caller='MMASPLI: (outdir) ' ) then return

   outdir = addslash(outdir)

   nframes=n_elements(filelist)

   for i=0,nframes-1 do begin

      if exists(filelist[i]) then begin

         ; Check header of image to see if it is a multi-extension image.
         hdr=headfits(filelist[i])
         numext=sxpar(hdr,'NEXTEND')

         if (numext/2)*2 ne numext or numext eq 0 then begin
            print,'MMASPLI: Invalid group fits file. ',filelist[i]
            return
         endif

         print,'Splicing ',filelist[i]

         newext = numext/2
         if newext gt 1 then begin
            sxaddpar,hdr,'NEXTEND',numext/2
            writefits,outdir+filelist[i],0,hdr
         endif

         for j=1,numext,2 do begin
            imagea = 0
            imageb = 0
            imagea = readfits(filelist[i],hdra,exten_no=j,/silent,/noscale)
            imageb = readfits(filelist[i],hdrb,exten_no=j+1,/silent,/noscale)
            sz=size(imagea,/dimen)
            sxaddpar,hdra,'NAXIS2',sz[1]*2
            if newext eq 1 then begin
               sxdelpar,hdr,'EXTEND'
               sxdelpar,hdr,'NEXTEND'
               writefits,outdir+filelist[i],[imagea,imageb],hdr
            endif else begin
               writefits,outdir+filelist[i],[imagea,imageb],hdra,/append
            endelse
         endfor

      endif

   endfor

end

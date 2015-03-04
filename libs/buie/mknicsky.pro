;+
; NAME:
;  mknicsky
; PURPOSE:
;  Create a master sky image from a set of dithered NICMOS images.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  mknicsky,root,PATH=path,OUTPATH=outpath
; INPUTS:
;  root - Root of observation set.  This is usually a 6 character name, the
;            first 4 characters identify the proposal id and the last two
;            identify the visit.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  path    - Directory where the images are read from.  (default = current)
;  outpath - Directory where the final sky image is to be written to,
;               (default=path)
; OUTPUTS:
;
;  A FITS file is written to "outpath" with the name, root_<grismid>.sky
;    where <grismid> is replaced by the grism name as read from the header.
;    Note that you must have at least 3 images for a given grism for the
;    sky image to be created for that grism.
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
;  Written by Marc W. Buie, Lowell Observatory, 1999 July 12
;  2004/9/21, MWB, removed obsolete call to Findfile
;-
pro mknicsky,root,PATH=path,OUTPATH=outpath

   ; Validate arguments
   if badpar(root,7,0,caller='MKNICSKY: (root) ') then return
   if badpar(path,[0,7],0,caller='MKNICSKY: (PATH) ',default='') then return
   if badpar(outpath,[0,7],0,caller='MKNICSKY: (OUTPATH) ', $
                             default=path) then return

   ; Get all the files.
   fnlist=file_search(path+root+'*_cal.fits')
   nf=n_elements(fnlist)

   ; Get a list of the filter used for each image
   filterlist=strarr(nf)
   for i=0,nf-1 do begin
      hdr=headfits(fnlist[i])
      filterlist[i] = strcompress(sxpar(hdr,'FILTER'),/remove_all)
   endfor

   ; Locate all the grism data (starts with G)
   f=strmid(filterlist,0,1)
   z=where(f eq 'G',count)
   if count eq 0 then return

   ; Construct a list of all grisms used.
   tmp = filterlist[z]
   uniq_filt = tmp[uniq(tmp,sort(tmp))]

   ; Loop over the grism list
   for j=0,n_elements(uniq_filt)-1 do begin

      ; Output file name for the sky/background image.
      fnsky = outpath+root+'_'+strlowcase(uniq_filt[j])+'.sky'

      ; Pick the images to combine
      z=where(filterlist eq uniq_filt[j],count)
  
      if count le 2 then begin
         print,'Sorry, this will not work on dither sets that include less than'
         print,'3 images in the visit.  Skipping grism ',uniq_filt[j]

      endif else begin

         ; Read in the raw frames
         cube=fltarr(256,256,count)
         for i=0,count-1 do begin
            fits_read,fnlist[z[i]],a,hdr,exten=1
            cube[*,*,i]=a
         endfor
 
         ; Do the robust average of the sky
         avgclip,cube,sky
   
         ; Save the sky/background image
         print,'Saving average background image to ',fnsky
         writefits,fnsky,sky
      endelse

   endfor

end

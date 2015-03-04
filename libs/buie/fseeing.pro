;+
; NAME:
;  fseeing
; PURPOSE:   (one line only)
;  Collect and maintain seeing information for a group of images
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  fseeing,fnlist
; INPUTS:
;  fnlist - List of image files that should appear in the data file
;             This should not include any directory path information, use
;             the PATH keyword for that.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FWHMGUESS - Initial guess of the FWHM in the image.  Default=3.  If this
;                 is significantly smaller than the actual seeing you will
;                 get an answer that is much too small.  The penalty for
;                 guessing a larger than optimal value appears to be weak.
;  SKYTHRESH - minimum signal to consider when looking for sources, this
;                 number sets the threshold to be this many sigma above the
;                 mean sky level (default=5)
;  MAXSIGNAL - maximum signal to conisder when looking for sources
;                 (default=60000)
;  DISPLAY   - Flag, if set will show plot and image outputs (this is usually
;                 not a good idea)
;  OUTFILE   - Name of the file for saving the data (default is seeing.dat)
;  PATH      - Directory where the images are to be found.  Default is the
;                 current directory.
;  KEYFILE - header keyword correspondence file (see loadkeys.pro)
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;  results - anonymous structure with the data from the seeing.dat file.
;             tags are:
;              fn     - list of file names (alphabetical order)
;              jd     - Julian date mid-time of the image
;              fwhm   - estimated seeing for each image [pixels]
;              objrad - Object radius used
;              nstars - Number of sources used to find fwhm
;              err    - Flag, 1 if there was an error with this entry
;              nfiles - number of files (length of all these vectors)
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2010/09/17 - Written by Marc W. Buie, Southwest Research Institute
;-
pro fseeing,fnlist, $
       SKYTHRESH=skythresh,MAXSIGNAL=maxsignal,DISPLAY=display, $
       FWHMGUESS=fwhmguess,OUTFILE=outfile,PATH=path,KEYFILE=keyfile, $
       RESULTS=results

   self='FSEEING: '
   if badpar(fnlist,7,[0,1],caller=self+'(fnlist) ') then return

   if badpar(path,[0,7],0,caller=self+'(PATH) ',default='') then return
   if badpar(outfile,[0,7],0,caller=self+'(OUTFILE) ', $
                default='seeing.dat') then return
   if badpar(keyfile,[0,7],0,CALLER=self+'(KEYFILE) ', $
                              default='[[DEFAULT]]') then return

   if badpar(skythresh,[0,1,2,3,4,5,12,13,14,15],0, $
             caller=self+'(SKYTHRESH): ',default=5.) then return

   if badpar(maxsignal,[0,1,2,3,4,5,12,13,14,15],0, $
             caller=self+'(MAXSIGNAL): ',default=60000.) then return

   if badpar(fwhmguess,[0,2,3,4,5],0, $
             caller=self+'(FWHMGUESS): ',default=3) then return

   if badpar(display,[0,1,2,3],0, $
             caller=self+'(DISPLAY): ',default=0) then return

   if path ne '' then path=addslash(path)

   nfiles_in = n_elements(fnlist)

   loadkeys,keyfile,keylist

   ; Load any existing data
   if exists(outfile) then begin
      readcol,outfile,fnsee,jdsee,seeing,objrad,nstars,err, $
         format='a,d,f,f,l,i'
   endif else begin
      fnsee=fnlist
      jdsee = dblarr(nfiles_in)
      seeing = replicate(-1.0,nfiles_in)
      objrad = replicate(-1.0,nfiles_in)
      nstars = replicate(0L,nfiles_in)
      err = replicate(2,nfiles_in)
   endelse

   ; sift through the list and make sure all the images we need are in the
   ;   the seeing file.  If it is not found in the list, add it with null
   ;   values and a flag that indicates the missing information needs to
   ;   be filled in.
   for i=0,nfiles_in-1 do begin
      z=where(fnlist[i] eq fnsee,count)
      if count eq 0 then begin
         fnsee = [fnsee,fnlist[i]]
         jdsee  = [jdsee,0.0d0]
         seeing = [seeing,-1.0]
         objrad = [objrad,-1.0]
         nstars = [nstars,0L]
         err    = [err,2]
      endif
   endfor

   nfiles=n_elements(fnsee)

   if max(err) gt 1 then begin
      idx=sort(fnsee)
      jdsee = jdsee[idx]
      fnsee = fnsee[idx]
      seeing = seeing[idx]
      objrad = objrad[idx]
      nstars = nstars[idx]
      err = err[idx]

      ; improve on the fwhmguess if there is already some data
      z=where(err eq 0,count)
      if count gt 0 then begin
         robomean,seeing[z],3.0,0.5,avgseeing
         fwhmguess = fwhmguess > avgseeing
      endif

      z=where(err eq 2,count)
      for i=0,count-1 do begin
         if not exists(path+fnsee[z[i]]) then begin
            print,self,'Image ',path+fnsee[z[i]],' not found.'
            continue
         endif
         im=readfits(path+fnsee[z[i]],hdr)
         parsekey,hdr,keylist,hdrinfo
         jdsee[z[i]] = hdrinfo.jd
         skysclim,im,loval,hival,mval,msig
         seeing,im,fwhm0,objrad0,nstars0,error=err0,nodisplay=not display, $
            minsignal=mval+skythresh*msig,maxsignal=maxsignal, $
            fwhmguess=fwhmguess
         if err0 eq 0 then begin
            seeing[z[i]] = fwhm0
            objrad[z[i]] = objrad0
            nstars[z[i]] = nstars0
            err[z[i]]    = err0
         endif else begin
            err[z[i]] = 1B
         endelse
         print,fnsee[z[i]],seeing[z[i]],objrad[z[i]],nstars[z[i]],err[z[i]]
      endfor
      openw,lunout,outfile,/get_lun
      for i=0,n_elements(fnsee)-1 do begin
         printf,lunout,fnsee[i],jdsee[i],seeing[i],objrad[i],nstars[i],err[i], $
            format='(a,1x,f13.5,2(1x,f6.2),1x,i5,1x,i1)'
      endfor
      free_lun,lunout
   endif

   z=where(err eq 0,ngood)

   results={ $
            fn:    fnsee, $
            jd:    jdsee, $
            fwhm:  seeing, $
            objrad: objrad, $
            nstars: nstars, $
            err:    err, $
            ngood:  ngood, $
            nfiles: nfiles $
            }

end

;+
; NAME:
;   syncalib
; PURPOSE:
;   Create a synthetic suite of calibration images: biases, darks and flats.
; DESCRIPTION:
; Creates a set of randomly generated bias, dark and flat images
; as FITS files suitable for input to mkcalib. The number of
; images of each type may be selected.
; CATEGORY:
;   CCD data processing
; CALLING SEQUENCE:
;   syncalib, root,nbias,ndark,nflat,seed
; INPUTS:
;   root    - prefix of file names for calibration, typically YYMMDD- a
;              '.' is automatically postpended to this name.
;   nbias   - number of bias frames to make.
;   ndark   - number of dark frames to make.
;   nflat   - number of flat frames to make.
;   seed    - random generator seed (which is passed transparently).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;   OVERSCAN -       integer, number of columns for the explicit overscan area.
;                    if <=  0, there is no overscan area generated
;                    and dimensions of the output images match the base images.
;   DDIR     -       String, name of directory where files are to be written. 
;   BIASSTARTFRAME - first fits file number for created biases. File names
;                    are of the form root.FFF where FFF is the file number.
;                    If not specified, file numbers start after the maximum
;                    file number of the current population of DDIR.
;   DARKSTARTFRAME - first fits file number for created darks. File names
;                    are of the form root.FFF where FF is the file number.
;                    If not specified, file numbers start after the maximum
;                    file number of the current population of DDIR.
;   FLATSTARTFRAME - first fits file number for created flats. File names
;                    are of the form root.FFF where FF is the file number.
;                    If not specified, file numbers start after the maximum
;                    file number of the current population of DDIR.
;   DARKEXP        - Adopted exposure time in seconds for dark frames- the 
;                    default is 600 secs.
;   FITSTAG        - Optional tag to add to files names (commonly .fits).  The
;                       default is to NOT add such a tag.
;   FLATMINEXP     - minimum exposure in seconds for flats- for each flat, an
;                    exposure time is adopted as a uniform draw between
;                    FLATMINEXP and FLATMAXEXP. The default for 
;                    FLATMINEXP is 3 secs.
;   FLATMAXEXP     - maximum exposure in seconds for flats- for each flat, an
;                    exposure time is adopted as a uniform draw between
;                    FLATMINEXP and FLATMAXEXP. The default for 
;                    FLATMAXEXP is 25 secs.
;   FLATSIGMIN     - minimum sky level in photons for flats- for each flat, a
;                    signal level is adopted as a uniform draw between
;                    FLATSIGMIN and FLATSIGMAX. The default for 
;                    FLATSIGMIN is 8000 photons.
;   FLATSIGMAX     - maximum sky level in photons for flats- for each flat, a
;                    sky level is adopted as a uniform draw between
;                    FLATSIGMIN and FLATSIGMAX. The default for 
;                    FLATSIGMAX is 26000 photons.
;   GAIN           - gain used for darks and flats. Default is 
;                    2.0 photons or electrons per D/N.
;   RDNOISE        - Read noise for biases, darks and flats. Default is
;                    10 electrons per pixel.
;   BIASLEVEL      - Bias (overscan) level for biases, darks, flats and 
;                    overscan (in D/N).
;   BIASBASE       - Name of FITS file holding the idealized bias image. The
;                    default is syn.bias
;   DARKBASE       - Name of FITS file holding the idealized dark image. The
;                    default is syn.dark. If this does not exist AND 
;                    ndark is 0 AND DARKEXP is at a default value, a
;                    pattern of all zeros is generated on the fly.
;   FLATBASE       - Name of FITS file holding the idealized flat image. The
;                    default is syn.flat
;   CLEANDATA      - Flag, if set, there will be no external blemishes added to
;                    the calibration files. Otherwise, stars are added to 
;                    flats and cosmic rays to all images except overscan areas.
;   COSMICCOUNT    - Mean number of cosmic ray striks added to images.
;   STARCOUNT      - Mean  number of stars added to flat images,
;   VERBOSE  -       Flag, if set, will enable 1 line summary for each frame.
;              
; OUTPUTS:
;  All output is confined to files. Images are int arrays with a minimum
;  FITS header including EXPTIME for darks and flats.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; All frame numbers must be in the range 000-999.
; PROCEDURE:
; MODIFICATION HISTORY:
;   2006/07/10, Written by Peter L. Collins, Lowell Observatory  
;   2006/07/13, PLC, add VERBOSE and fix docunent header.
;   2006/07/14, MWB, added FITSTAG keyword and added to library.
;-

pro  syncalib, root,nbias,ndark,nflat,seed, $
              OVERSCAN=oscsize, DDIR=ddir, BIASSTARTFRAME=startbias, $
              DARKSTARTFRAME=startdark,FITSTAG=fitstag, $
              FLATSTARTFRAME=startflat, $
              DARKEXP=etimedark,FLATMINEXP=etimeflatmin, $
              FLATMAXEXP=etimeflatmax,FLATSIGMIN=flatlvlmin, $
              FLATSIGMAX=flatlvlmax, GAIN=gain,RDNOISE=rdnoise, $
              BIASLEVEL=biaslevel,FLATBASE=flatbase,DARKBASE=darkbase, $
              BIASBASE=biasbase,CLEANDATA=cleandata,COSMICCOUNT=cosmiccount, $
              STARCOUNT=starcount,VERBOSE=verbose

   self='SYNCALIB: '
   if badpar(root,[7],0,caller=self+'(ROOT)') then return
   if badpar(nbias,[1,2,3],0,caller=self+'(nbias) ') then return
   if badpar(ndark,[1,2,3],0,caller=self+'(ndark) ') then return
   if badpar(nflat,[1,2,3],0,caller=self+'(nflat) ') then return
   if badpar(seed,[0,1,2,3,4,5],[0,1],caller=self+'(seed) ') then return
   if badpar(oscsize,[0,2,3],0,caller=self+'(OVERSCAN) ', default=0) then return
   if badpar(ddir,[0,7],0,caller=self+'(DDIR)', default='.') then return
   if badpar(startbias,[0,1,2,3],0,caller=self+'(STARTBIASFRAME) ', $
            default=-1) then return
   if badpar(startdark,[0,1,2,3],0,caller=self+'(STARTDARKFRAME) ', $
             default=-1) then return
   if badpar(startflat,[0,1,2,3],0,caller=self+'(STARTFLATFRAME) ',  $
            default=-1) then return
   if badpar(etimedark,[0,2,3,4,5],0,caller=self+'(ETIMEDARK) ',  $
            default=600.0) then return
   if badpar(etimeflatmin,[0,2,3,4,5],0,caller=self+'(ETIMEFLATMIN) ',  $
             default=3.0) then return
   if badpar(etimeflatmax,[0,2,3,4,5],0,caller=self+'(ETIMEFLATMAX) ',  $
             default=25.0) then return
   if badpar(flatlvlmin,[0,2,3,4,5],0,caller=self+'(FLATLVLMIN) ',  $
             default=8000.0) then return
   if badpar(flatlvlmax,[0,2,3,4,5],0,caller=self+'(FLATLVLMAX) ',  $
             default=26000.0) then return
   if badpar(gain,[0,4,5],0,caller=self+'(GAIN) ', $
             default=2.0) then return
   if badpar(rdnoise,[0,2,3,4,5],0,caller=self+'(RDNOISE) ', $
             default=10.0) then return
   if badpar(biaslevel,[0,2,3,4,5],0,caller=self+'(BIASLEVEL) ', $
             default=2000.0) then return
   if badpar(biasbase,[0,7],0,caller=self+'(BIASBASE) ', $
             default='syn.bias') then return
   if badpar(darkbase,[0,7],0,caller=self+'(DARKBASE) ', $
             default='syn.dark') then return
   if badpar(flatbase,[0,7],0,caller=self+'(FLATBASE) ', $
             default='syn.flat') then return
   if badpar(cleandata,[0,2,3,4,5],0,caller=self+'(CLEANDATA) ', $
             default=0) then return
   if badpar(cosmiccount,[0,2,3,4,5],0,caller=self+'(COSMICCOUNT) ', $
             default=10) then return
   if badpar(starcount,[0,2,3,4,5],0,caller=self+'(STARCOUNT) ', $
             default=7) then return
   if badpar(verbose,[0,2,3,4,5],0,caller=self+'(VERBOSE) ', $
             default=0) then return
   if badpar(fitstag,[0,7],0,caller=self+'(FITSTAG) ', $
             default='') then return

    if cleandata then begin
       cosmiccount = 0
       starcount = 0
    endif

   ; set up the range of files to create.
   framecount= (nbias + ndark + nflat)
   if  framecount eq 0  then begin
      print, self, ' Specified no frames, nothing to do.'
      return
   endif 

 ; Make sure the parent directory exists
   if not exists(ddir) then begin
      print,'The directory [',ddir,']'
            print,'does not exist.  Unable to continue'
      return
   endif

   ddir = addslash(ddir)
   calibname = root + '.'

   ; now make a list of all the current population of the target directory
   ; that matches  root.FFF
   fnlist = file_search(ddir + calibname + '[0-9][0-9][0-9]'+fitstag)
   nfn = n_elements(fnlist)
   if nfn gt 0 then begin
       frameorder = sort(strmid(fnlist, 7, /REVERSE_OFFSET))
       lastfrm = fix( strmid(fnlist[frameorder[nfn-1]],7,3,/REVERSE_OFFSET))
   endif else begin
      lastfrm=0
   endelse

   if nbias gt 0 then begin
      if startbias lt 0 then begin
         startbias = lastfrm + 1
         lastfrm = lastfrm + nbias
      endif
      print, self, ' generating ', strtrim(nbias),  ' bias frames from ', $
               calibname+string(startbias,format='(i3.3)')+fitstag, ' to ',  $
               calibname+string(startbias + nbias - 1,format='(i3.3)')+fitstag

      overwritten = 0
      for j=0,nbias-1 do begin
         if exists(ddir + calibname + $
                   string(startbias+j,format='(i3.3)')+fitstag) then begin
            print, 'file ', calibname + string(startbias+j,format='(i3.3)'), $
                    ' already exists'
            overwritten=overwritten+1
         endif
      endfor

      if overwritten gt 0 then begin
         ans=''
         print, self, 'One or more specified bias frame images already exist'
         read,prompt='They will be overwritten. wish to proceed anyway? (default no) ',ans

         if strlowcase(strmid(ans,0,1))  ne 'y' then return
      endif
   endif

   if ndark gt 0 then begin
      if startdark lt 0 then begin
         startdark = lastfrm + 1
         lastfrm = lastfrm + ndark    
      endif
      print, self, ' generating ', ndark,  ' dark frames from ', $
               calibname+string(startdark,format='(i3.3)')+fitstag, ' to ',  $
               calibname+string(startdark + ndark -1,format='(i3.3)')+fitstag

      overwritten = 0
      for j=0,ndark-1 do begin
         if exists(ddir + calibname + $
                   string(startdark+j,format='(i3.3)')+fitstag) then begin
            print, 'file ', calibname + string(startdark+j,format='(i3.3)'), $
                    ' already exists'
            overwritten=overwritten+1
         endif
      endfor

      if overwritten gt 0 then begin
         print, self, 'One or more specified dark frame images already exist'
         ans=''
         read, $
         prompt='They will be overwritten. wish to proceed anyway? (default no)',ans
         if strlowcase(strmid(ans,0,1))  ne 'y' then return
      endif
   endif

   if nflat gt 0 then begin
      if startflat lt 0 then begin
         startflat = lastfrm + 1
         lastfrm = lastfrm + nflat    
      endif
      print, self, ' generating ', nflat,  ' flat frames from ', $
               calibname+string(startflat,format='(i3.3)')+fitstag, ' to ',  $
               calibname+string(startflat + nflat -1,format='(i3.3)')+fitstag

      overwritten = 0
      for j=0,nflat-1 do begin
         if exists(ddir + calibname + $
                   string(startflat+j,format='(i3.3)')+fitstag) then begin
            print, 'file ', calibname + string(startflat+j,format='(i3.3)'), $
                    ' already exists'
            overwritten=overwritten+1
         endif
      endfor

      if overwritten gt 0 then begin
         ans=''
         print, self, 'One or more specified flat frame images already exist'
         read,prompt='They will be overwritten. wish to proceed anyway? (default no) ',ans
         if strlowcase(strmid(ans,0,1))  ne 'y' then return
      endif
   endif

   ; now get the base images we need.
   if not exists( ddir + biasbase) then begin
      print, 'the file (', ddir + biasbase,  $
             ') for the ideal bias image does not exist'
      print,'Unable to continue'
      return
   endif

   ; need to further check image validity
   biasimg = readfits(ddir + biasbase,/SILENT)
   sz = size(biasimg)
   nx = sz[1]
   ny = sz[2]

   nodark = 0
   if (ndark + nflat) gt  0 and not exists( ddir + darkbase) then begin
      if ndark ne 0 or  etimedark ne 600.0 then begin
         print, 'the file (', ddir + darkbase,  $
                ') for the ideal dark image does not exist'
         print,'Unable to continue'
         return
      endif else begin
         nodark=1
      endelse
   endif

   if (ndark  + nflat) gt 0 and not nodark  then begin
      darkimg = readfits(ddir + darkbase,/SILENT)
      sz = size(darkimg)
      if nx ne  sz[1] or ny ne sz[2] then begin
         print, 'dimension of ideal dark image (', sz[1], ',', sz[2], $
                 ') does not match bias (', nx, ',', ny
         return
      endif
   endif else begin
   ; make an zero array if you think a dark is  unnecessary
      if nodark then darkimg=fltarr(nx,ny)
   endelse

   if nflat gt  0 and not exists( ddir + flatbase) then begin
      print, 'the file (', ddir + flatbase,  $
             ') for the ideal flat image does not exist'
      print,'Unable to continue'
      return
   endif

   if nflat gt 0 then begin
      flatimg = readfits(ddir + flatbase,/SILENT)
      sz = size(flatimg)
      if nx ne  sz[1] or ny ne sz[2] then begin
         print, 'dimension of ideal flat image (', sz[1], ',', sz[2], $
                 ') does not match bias (', nx, ',', ny
         return
      endif
   endif

; all of the above was to check input parameter validity. Now,
; it is a relatively simple matter to generate the images desired.
   for j=0, nbias-1 do begin
      fn = (ddir + calibname + string(startbias+j,format='(i3.3)') + fitstag) 
      synbias, biasimg,biaslevel,gain,rdnoise,seed,scratchimg, $
               OVERSCAN=oscsize, FITSFN=fn,CRSNUM=cosmiccount,CRSOUT=crsout
      sz = size(scratchimg)
      if verbose then $
         print," bias ", fn, $ 
              string(sz[1],sz[2], FORMAT="( ' (',i3,':',i3, ' )')"), $
              string(crsout,FORMAT="( i3, ' crs')")
   endfor

   for j=0, ndark-1 do begin
      fn = (ddir + calibname + string(startdark+j,format='(i3.3)') + fitstag) 
      syndark, darkimg,etimedark,gain,biasimg, biaslevel,rdnoise,seed, $
      scratchimg, OVERSCAN=oscsize, FITSFN=fn, CRSNUM=cosmiccount, CRSOUT=crsout
      sz = size(scratchimg)
      if verbose then $
         print," dark ", fn, $ 
              string(sz[1],sz[2], FORMAT="( ' (',i3,':',i3, ' )')"), $
              string(crsout,FORMAT="( i3, ' crs')"), $
              string(etimedark, FORMAT="(' exptime ', i3, ' secs')")
   endfor

   for j=0, nflat-1 do begin
      fn = (ddir+ calibname + string(startflat+j,format='(i3.3)') + fitstag) 
      etime = randomu(seed)*(etimeflatmax - etimeflatmin) + etimeflatmin
      slvl = randomu(seed)*(flatlvlmax - flatlvlmin) + flatlvlmin
      synflat, flatimg,slvl,darkimg,etime,gain,biasimg,biaslevel, $
               rdnoise,seed,scratchimg,OVERSCAN=oscsize,FITSFN=fn, $
               CRSNUM=cosmiccount,STARRATE=starcount, CRSOUT=crsout, $
               STARSOUT=starsout
      sz = size(scratchimg)
      if verbose then $
         print," flat ", fn, $ 
              string(sz[1],sz[2], FORMAT="( ' (',i3,':',i3, ' )')"), $
              string(crsout,starsout,FORMAT="( i3, ' crs , ', i3, ' stars')"), $
              string(slvl, FORMAT="(i6, ' skyphot')"), $
              string(etime, FORMAT="(' exptime ', i3, ' secs')")
   endfor

   return
end

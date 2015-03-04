;+
; NAME:
;  moscal
; PURPOSE:
;  Apply standard CCD image correction steps to a raw group-FITS image.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
;
; CALLING SEQUENCE:
;  moscal,root,outdir,start,nframes,bias,dark,flat
;
; INPUTS:
;  root    - Root of the file name (you must include . in the name).
;  outdir  - The directory to write the calibrated files to.  Don't make
;              this the same as the input directory.
;  start   - First frame number to read (integer or long).
;               Start can also be a vector of explicit frame numbers to load.
;               In this case, nframes need not be specified and in fact will
;               be ignored.
;  nframes - Number of frames to average.
;  bias    - Bias frame image name to subtract from each raw frame.
;  dark    - Dark frame image name to subtract from each raw frame.
;  flat    - Flat frame image name to subtract from each raw frame.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;   CROP     = region of original image to save, default=no cropping.
;                 [x1,x2,y1,y2]
;
;   EXCLUDE - Optional vector of image numbers that should be excluded from
;                average.  Default is to include all frames.
;
;   EXPKEY = String - FITS keyword to read to get exposure time, default = EXPTIME
;
;   OVERSCAN = column overscan region to use for frame bias level,
;                 default=no overscan subtraction.
;
; OUTPUTS:
;   The calibrated images are written to outdir.  Don't make this the same
;   as the current directory!
;
;   An additional file, 'root'+dgain, is written that contains the mean of
;     each chip before and after the gaincor correction.
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
;  2000/02/03, Written by Marc W. Buie, Lowell Observatory
;  2000/02/28, MWB, added support for frame numbers > 999.
;  2003/03/30 ,MWB, added fix from 0 DN pixels (usually from KPNO)
;  2003/05/30, MWB, cosmetic changes to program internals plus added saving
;                      the differential gain data to a file.
;-
pro moscal,root,outdir,start,nframes,bias,dark,flat, $
   EXCLUDE=exclude,SCALE=in_scale,EXPKEY=expkey, $
   OVERSCAN=in_overscan,CROP=in_crop,GAINCOR=in_gaincor

   self='MOSCAL: '
   if badpar(root,   7,    0,caller=self+'(root) '   ) then return
   if badpar(outdir, 7,    0,caller=self+'(outsuf) ' ) then return
   if badpar(start,[2,3],[0,1],caller=self+'(start) ', $
                               rank=start_rank) then return

   if badpar(exclude,[0,2,3],[0,1],caller=self+'(exclude) ', $
                                   default=-1) then return

   if badpar(in_overscan,[0,2,3],[1,2],caller=self+'(overscan) ', $
                                       rank=o_rank) then return
   if badpar(in_crop,    [0,2,3],[1,2],caller=self+'(crop) ', $
                                       rank=c_rank) then return
   if badpar(expkey,     [0,7],      0,caller=self+'(EXPKEY) ', $
                                       default='EXPTIME') then return
   if badpar(in_gaincor, [0,4,5],  [1],caller=self+'(GAINCOR) ', $
                                       default=1.0) then return

   if badpar(bias,   [0,7],0,caller=self+'(bias) ', $
                                    default='[[none]]') then return
   if badpar(dark,   [0,7],0,caller=self+'(dark) ', $
                                    default='[[none]]') then return
   if badpar(flat,   [0,7],0,caller=self+'(flat) ', $
                                    default='[[none]]') then return

   outdir = addslash(outdir)

   gainfile = root+'dgain'

   ; Check to see if it's a sequential list or random list.
   if start_rank eq 0 then begin
      frames=start+indgen(nframes)
      if badpar(nframes,[2,3],   0,caller='MKBIAS: (nframes) ') then return
   endif else begin
      frames=start
      nframes=n_elements(frames)
   endelse

   ; Setup the file name format string
   digits = fix(ceil(alog10(max(frames)+1))) > 3
   dig = strn(digits)
   fnfmt = '(i'+dig+'.'+dig+')'

   ; Apply the exclusion criteria to the frames list.  Then, find out how
   ;   many frames are being collected and make sure there is something to do.
   for i=0,nframes-1 do begin
      z=where(frames[i] eq exclude,count)
      if count ne 0 then frames[i]=-1
   endfor
   zg=where(frames ne -1,countg)
   if countg eq 0 then $
      message,'Error ** you have excluded all frames, nothing to do.'

   ; Make the first file name
   fname=root+string(frames[0],format=fnfmt)
   if exists(fname+'.fits') then ft='.fits' else ft=''

   ; Check header of image to see if it is a multi-extension image.
   hdr=headfits(fname+ft)
   numext=sxpar(hdr,'NEXTEND')

   if numext le 1 then begin
      print,'MOSCAL: This program only works on group fits files.'
      return
   endif

   if n_elements(in_gaincor) eq 1 then begin
      gaincor = replicate(in_gaincor,numext)
   endif else if n_elements(in_gaincor) ne numext then begin
      print,'MOSCAL: Gain correction vector has the wrong number of elements.'
      return
   endif else begin
      gaincor = in_gaincor
   endelse

   ; Setup the overscan/crop control values
   if numext eq 0 then begin
      extend=0
      numext=1
      if o_rank eq 0 then begin
         do_overscan=0
      endif else begin
         do_overscan=1
         overscan = in_overscan
      endelse
      if c_rank eq 0 then begin
         do_crop=0
      endif else begin
         do_crop=1
         crop = in_crop
      endelse
   endif else begin
      extend=1
      if o_rank eq 0 then begin
         do_overscan=0
      endif else if o_rank eq 1 then begin
         do_overscan=1
         overscan = rebin(in_overscan,n_elements(overscan),numext)
      endif else begin
         do_overscan=1
         overscan = in_overscan
      endelse
      if c_rank eq 0 then begin
         do_crop=0
      endif else if o_rank eq 1 then begin
         do_crop=1
         crop = rebin(in_crop,n_elements(crop),numext)
      endif else begin
         do_crop=1
         crop = in_crop
      endelse
   endelse

   ; Main loop over extension, done just once on "normal" frames
   for ix=1,numext do begin
      ix0=ix-1

      print,'Processing extension ',strn(ix)

      ; Read the bias frame for this image extension
      if bias ne '[[none]]' then begin
         biasim=0
         biasim=readfits(bias,exten_no=ix,/silent)
      endif

      ; Read the dark frame for this image extension
      if dark ne '[[none]]' then begin
         darkim=0
         darkim=readfits(dark,exten_no=ix,/silent)
      endif

      ; Read the flat frame for this image extension
      if flat ne '[[none]]' then begin
         flatim=0
         flatim=readfits(flat,exten_no=ix,/silent)
      endif

      ; Loop over the frames
      for i=0,nframes-1 do begin

         fname=root+string(frames[zg[i]],format=fnfmt)
         if exists(fname+'.fits') then ft='.fits' else ft=''

         ; silently skip the frame if it doesn't exist.
         if exists(fname+ft) then begin

            ; read in the new image, the dummy line before readfits is to dump
            ;   the storage for the array before its used again.
            image = 0
            if ix eq 1 then begin
               hdr=headfits(fname+ft)
               writefits,outdir+fname+ft,0,hdr
            endif

            image = readfits(fname+ft,hdr,exten_no=ix,/silent)

            zz = where(image eq 0,countzz)
            if countzz gt 0 then begin
               image[zz]=max(image)
               print,'ZERO pixel fix applied',countzz,minmax(image)
            endif

            if do_overscan and do_crop then begin
               image = colbias(image,overscan[0,ix0],overscan[1,ix0], $
                        crop[0,ix0],crop[1,ix0],crop[2,ix0],crop[3,ix0],biasval=biasval)
               if extend then $
                  print,fname+ft,' overscan value is ',biasval,' extension ',strn(ix) $
               else $
                  print,fname+ft,' overscan value is ',biasval

            endif else if do_overscan then begin
               image = colbias(image,overscan[0,ix0],overscan[1,ix0],biasval=biasval)
               if extend then $
                  print,fname+ft,' overscan value is ',biasval,' extension ',strn(ix) $
               else $
                  print,fname+ft,' overscan value is ',biasval

            endif else if do_crop then begin
               image = image[crop[0,ix0]:crop[1,ix0],crop[2,ix0]:crop[3,ix0]]
               if extend then $
                  print,fname+ft,' extension ',strn(ix) $
               else $
                  print,fname+ft

            endif else begin
               if extend then $
                  print,fname+ft,' extension ',strn(ix) $
               else $
                  print,fname+ft

            endelse

            if bias ne '[[none]]' then $
               image = temporary(image) - biasim

            if dark ne '[[none]]' then begin
               exptime=float(sxpar(hdr,expkey))
               image = temporary(image) - darkim*exptime
            endif

            if flat ne '[[none]]' then $
               image = temporary(image)/flatim

            ; mean of image before any scaling
            skysclim,image,lowval,hival,meanval1,sigma1,npts=4000

            if gaincor[ix-1] ne 1.0 then $
               image = temporary(image)*gaincor[ix-1]

            ; mean of image after scaling
            skysclim,image,lowval,hival,meanval2,sigma2,npts=4000

            ; post results to a file for possible later use
            tag=fname+' '+strn(ix,length=2)
            info=string(meanval1,gaincor[ix-1],meanval2, $
                           format='(1x,f10.1,1x,f8.6,1x,f10.1)')

            repwrite,gainfile,tag,tag+info

            image = fix(temporary(image < 65535.0) - 32767.5)
            sz=size(image,/dimen)
            sxaddpar,hdr,'NAXIS1',sz[0]
            sxaddpar,hdr,'NAXIS2',sz[1]
            sxaddpar,hdr,'BSCALE',1.0,AFTER='NAXIS2'
            sxaddpar,hdr,'BZERO',32768.0,AFTER='BSCALE'
            sxaddpar,hdr,'BITPIX',16
            writefits,outdir+fname+ft,image,hdr,/append

         endif ; frame processing clause

      endfor ; frame loop

   endfor

end

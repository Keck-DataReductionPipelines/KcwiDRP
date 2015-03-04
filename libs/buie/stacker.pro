;+
; NAME:
;  stacker
; PURPOSE:   (one line only)
;  Stack (co-add) image while registering images.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  stacker,fnlist,xpos,ypos,image
; INPUTS:
;  fnlist - String array of fits files to read, or, 3-d image array, for
;           the raw images to be stacked.
;  xpos   - Array of x-positions, one per frame of registration point.
;  ypos   - Array of y-positions, one per frame of registration point.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ANCHORFRAME - Index into fnlist for the frame to be used as the anchor.
;                  Default is the first frame.
;  ANCHORPOS - [x,y] position to align registration point to, default is
;                the position of the anchor object.
;  BAD -       If specified, String array of fits files to read, or, 3-d image 
;              array, for badmasks applying to the raw images to be stacked.
;  CROP      - [x1,x2,y1,y2] region of image to save.  These values are
;                in the pixel coordinate system of the anchor frame.
;                Default is the full frame.
;  EXTENSION - Image extension to read from image fits files.  This should be
;                left off if the file is not a group fits file.  If it
;                is a group fits file you will need to specify this number
;                to get the right extension.
;  JUSTMEDIAN - Only use median combination in avgclip call.
;  LAYER  -      2-d subframe to read from image fits files. This should be
;                left off if the files are  2-d fits files.  If they
;                are 3-d cubes you will need to specify this number
;                to get the right part of the image. This applies ONLY
;                to IMAGE fits files- BAD mask files are always 2-d.
;  JUSTMEDIAN - Only use median combination in avgclip call (ie if ROBUST
;               keyword in force).
;  ROBUST    - Flag, if set requests that a robust average, via avgclip,
;                  of the image stack be performed. If the number of images is 
;                  2 then this keyword is ignored.
;
;  SILENT    - Flag, if set will suppress all messages to screen.
;  SUPERFRAME- Flag, if set, the images will be stacked onto a
;              'superframe' which is sufficiently large so that all
;              pixels in the individual images will be represented at
;              least once. A special badmask is used to mask off unpopulated
;              elements of the supercube being stacked.
;  DEBUG     - Flag, if set turns on extra debug steps and other information.
;                The debug information in not guaranteed to remain static
;                from one verion of the program or another.
; OUTPUTS:
;  image - Floating point array with stacked image
; KEYWORD OUTPUT PARAMETERS:
;  OUTFILE - Name of file to write the image to.  This is output in FITS format
;              and the header is derived from the first image with some
;              modifications.
;  IMAGESTACK   - use this to get the registered stack of individual frames
;  STACKOFFSET- a 2 x n matrix giving the  X,Y offsets of each individual
;                  frame in the stacked image. That is, stackoffset[*,j]
;                  are the X,Y for the lower left corner of the jth frame
;                  on the stacked image. Depending on the setting of 
;                  SUPERFRAME and the explicit cropping parameters, this
;                  may not be located within the dimensions of the stacked
;                  image.
;  STACKSIZE- a 2 element long vector giving the X,Y dimensions of the
;                output stack and output image. In the absence of CROP and 
;                SUPERFRAME keywords this will be the dimensions of the anchor 
;                frame. 
;  IMAGEMEAN - This is the mean of the output image background signal level
;                as computed and used inside this routine.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2005/07/21
;  2005/11/22, MWB, added DEBUG keyword
;  2006/10/20, MWB, fixed missing keyword problems
;  2007/07/09, MWB, added JUSTMEDIAN and STACK keywords
;  2007/08/30, Peter L. Collins, Lowell Observatory, allow data cube as
;              variant of fnlist, and fits file as variant of BAD. Add
;              SUPERFRAME support and associated keywords.
;  2007/09/07, MWB, incorporate into library to be consistent with newest
;              avgclip and medarr_mwb, STACK keyword renamed to IMAGESTACK
;-
pro stacker,fnlist,xpos,ypos,image, $
       CROP=in_crop,ROBUST=robust,EXTENSION=exten,SILENT=silent,DEBUG=debug, $
       ANCHORFRAME=anchorframe,ANCHORPOS=anchorpos,LAYER=layer, $
       JUSTMEDIAN=justmedian,IMAGESTACK=imagestack,BAD=bad,SUPERFRAME=superframe, $
       STACKOFFSET=stackoffset,STACKSIZE=stacksize,AVGMASK=avgmask, $
       IMAGEMEAN=imagemean

   self='STACKER: '
   if badpar(fnlist,[7,2,3,4,5],[1,3],caller=self+'(fnlist) ', $
             npts=n1, type=timg, dim=dimg, rank=rimg) then return
   if badpar(xpos,[2,3,4,5,6],1,caller=self+'(xpos) ',npts=n2) then return
   if badpar(ypos,[2,3,4,5,6],1,caller=self+'(ypos) ',npts=n3) then return
   if badpar(in_crop,[0,2,3,4,5],1,caller=self+'(CROP) ',npts=n4, $
                default=[-1,-1,-1,-1]) then return
   if badpar(anchorframe,[0,2,3,4,5],0,caller=self+'(ANCHORFRAME) ', $
                default=0) then return
   if badpar(anchorpos,[0,2,3,4,5],1,caller=self+'(ANCHORPOS) ',npts=n5, $
                default=[xpos[anchorframe],ypos[anchorframe]]) then return
   if badpar(robust,[0,1,2,3],0,caller=self+'(ROBUST) ',default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return
   if badpar(exten,[0,1,2,3],0,caller=self+'(EXTENSION) ',default=0) then return
   if badpar(layer,[0,1,2,3],0,caller=self+'(LAYER) ',default=0) then return
   if badpar(justmedian,[0,1,2,3],0,caller=self+'(JUSTMEDIAN) ', $
             default=0) then return
   if badpar(superframe,[0,1,2,3],0,caller=self+'(SUPERFRAME) ', $
             default=0) then return
   if badpar(bad,[0,7,1,2,3],[1,3],caller=self+'(BAD) ', $
             npts=nbad, type=tbad, dim=dbad, rank=rbad) then return

   if (rimg ne 3) ne (timg eq 7) then begin
         print, self, ' fnlist must be a 1 dimensional vector iff STRING type'
         return
   endif
   if (rbad ne 1) ne (tbad ne 7) then begin
         print, self, $
               ' BAD must be a 1 dimensional vector if and only if STRING type'
         return
   endif
   if rbad eq 3 then nbad=dbad[2]
   if rimg eq 3 then n1=dimg[2]

   icntsize = [n1,n2,n3]
   if nbad gt 0 then  icntsize=[icntsize,nbad]

   if min(icntsize) ne max(icntsize) then begin
      print, icntsize
      print,self,' fnlist, xpos, ypos, and (bad) must all have the same length.'
      return
   endif

   if n1 lt 1  then begin
      print, self, ' no images are specified.'
      return
   endif

   if n4 ne 4 then begin
      print,self,' crop must be a 4 element vector'
      return
   endif

   if n5 ne 2 then begin
      print,self,' anchorpos must be a 2 element vector'
      return
   endif

   nz = 1
   if rimg eq 3 then begin
      nx = dimg[0]
      ny = dimg[1]
   endif else begin
      hdr = headfits(fnlist[anchorframe],exten=exten)
      nx = sxpar(hdr,'NAXIS1')
      ny = sxpar(hdr,'NAXIS2')
      nz = sxpar(hdr,'NAXIS3')
   endelse
   
   ; There is something still not quite right here, nz from above is set to 1
   ; if image came in from command line.  But, this may work for now.  All of
   ; this input logic needs a big cleanup.
   if tbad eq 0 then begin
      bad = bytarr(nx,ny,n1)
      rbad = 3
      dbad = size(bad,/dimen)
      tbad = 1
      nbad = n_elements(bad)
   endif

   if rbad eq 3 and ( dbad[0] ne nx  or dbad[1] ne ny) then begin
      print, self, ' dimension of bad mask cube incompatible with image size.'
      return
   endif 
   if rbad eq 1 then begin
      hdr = headfits(bad[anchorframe],exten=exten)
      if sxpar(hdr,'NAXIS1') ne nx or sxpar(hdr,'NAXIS2') ne ny or $
      sxpar(hdr,NAXIS3) gt 1 then begin
         print, self, ' dimensions of bad mask files incompatible with image.'
         return
      endif
   endif

   refpos = anchorpos
   if superframe then  begin
      refpos[0]= max(xpos)
      refpos[1]= max(ypos)
   endif
      
   if min(in_crop) lt 0 then begin
      crop = [0,nx-1,0,ny-1]
      if superframe then begin
         crop[1] += ( refpos[0] - min(xpos))
         crop[3] += ( refpos[1] - min(ypos))
      endif
   endif else begin
      crop = fix(in_crop+0.5)
   endelse
   stackoffset = lonarr(2, n1)
   stackoffset[0,*] = round(refpos[0] - xpos)
   stackoffset[1,*] = round(refpos[1] - ypos)

   outnx = crop[1]-crop[0]+1
   outny = crop[3]-crop[2]+1
   stacksize = [outnx, outny]

   imagestack=fltarr(outnx,outny,n1)
   ; initially the whole bad mask is set.
   if rbad ne 0 or superframe gt 0 then badmask = replicate(1B,outnx,outny,n1)
   
   means = fltarr(n1)
   for i=0,n1-1 do begin
      if rimg ne 3 then raw = readfits(fnlist[i],exten=exten) $
      else raw = fnlist[*,*,i]
      if nz gt 1 then raw=raw[*,*,layer]
      if rbad eq 1 then badimg = readfits(bad[i])
      if rbad eq 2 then badimg = bad[*,*]
      if rbad eq 3 then badimg = bad[*,*,i]
      ; offset from current frame to reference frame
      dx = round(refpos[0]-xpos[i])
      dy = round(refpos[1]-ypos[i])
      print, "stacker image ", strn(i), " offset", dx,dy
      ; coordinates of array boundaries in reference frame system
      rx1 = dx
      rx2 = (nx-1)+dx
      ry1 = dy
      ry2 = (ny-1)+dy
      ; adjust these coordinates to valid region of reference frame
      rx1v = rx1>crop[0] - crop[0]
      rx2v = rx2<crop[1] - crop[0]
      ry1v = ry1>crop[2] - crop[2]
      ry2v = ry2<crop[3] - crop[2]
      ; convert these coordinates back into current frame coordinates
      rx1r = rx1v+crop[0] - dx
      rx2r = rx2v+crop[0] - dx
      ry1r = ry1v+crop[2] - dy
      ry2r = ry2v+crop[2] - dy
      ; find mean sky level and subtract as image gets poked into the stack,
      ; but not for a median image.
      skysclim,raw[rx1r:rx2r,ry1r:ry2r],lowval,hival,meansky,sigval,NPTS=5000
      means[i] = meansky
      ; stuff part of raw that maps onto blank, watch out for edges
      imagestack[rx1v:rx2v,ry1v:ry2v,i] = raw[rx1r:rx2r,ry1r:ry2r]-meansky
      ; also put in the slice of mask. Areas outside are automatically bad.
      badmask[rx1v:rx2v,ry1v:ry2v,i] = badimg[rx1r:rx2r,ry1r:ry2r]
   endfor

   imagemean = mean(means)
   imagestack += imagemean

   if debug then itool,imagestack,/block

   avgclip,imagestack,image,silent=(silent and (debug eq 0)), $
      justmedian=justmedian,nonewbadflags=(robust eq 0),BAD=badmask

end

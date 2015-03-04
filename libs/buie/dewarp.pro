;+
; NAME:
;  dewarp
;
; PURPOSE:
;  Transforms an image from (x,y) to ($\xi$,$\eta$) accounting for rotation and warping.
;
; DESCRIPTION:
;  Transforms an image from the (x,y) coordinate plane to the ($\xi$,$\eta$)
;  coordinate plane. If the image is rotated, the coefficients on the terms
;  of the basis can be adjusted to perform the rotation. Photometric pixel
;  values in the resulting image are obtained from interpolation if necessary.
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  dewarp,ininfo,imgarr,outinfo,imgout,nx,ny
;
; INPUTS:
;  ininfo - Image transformation structure that describes how the input
;              image maps to the plane-of-the-sky.  See astxn2xy.pro for
;              required information.  Note that this routine only uses
;              the FULL transformation option.
;  imgarr - Input image (array)
;  outinfo - Image transformation structure that describes how the output
;              image maps to the plane-of-the-sky.  See astxn2xy.pro for
;              required information.  Note that this routine only uses
;              the FULL transformation option.
;  imgout - Output image (array), may already exist and is added to if /ADD set
;  nx     - xsize of the output image, if imgout exists and ADD is set then
;              this value defaults to the existing size of imgout.  Without
;              /ADD this value is required.
;  ny     - ysize of the output image, if imgout exists and ADD is set then
;              this value defaults to the existing size of imgout.  Without
;              /ADD this value is required.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  ADD - Flag, if set indicates the dewarped image should be added to the
;           destination,
;           Else the dewarped image will be copied to the destination.
;
; OUTPUTS:
;  imgout - The transformed photometric array in (x,y) coordinates based on a
;           cubic convolution resampling of the original photometric array using
;           the transformed coordinates.
;
; KEYWORD OUTPUT PARAMETERS:
;  COUNT - An optional output array with the same dimensions as imgout. Each
;   element of the array indicates how many values have been added to the
;   corresponding pixel. This information then can be used during an averaging
;   process.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;    There is as yet an unresolved issue with flux normalization between
;      input and output images.  At the moment, the input and output should
;      be regarded to be on separate photometric systems.
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2009/11/04, Written by SwRI Clinic Team, Harvey Mudd College
;  2009/11/14, MWB, rework with some new logic
;  2010/02/28, MWB, minor change to reduce memory footprint
;-
pro dewarp,ininfo,imgarr,outinfo,imgout,nx,ny,ADD=ADD,COUNT=count

   self='DEWARP: '
   if badpar(ininfo,8,1,caller=self+'(ininfo) ') then return
   if badpar(imgarr,[2,3,4,5],2,caller=self+'(imgarr) ') then return
   if badpar(outinfo,8,1,caller=self+'(outinfo) ') then return
   if badpar(imgout,[0,2,3,4,5],[0,2],caller=self+'(imgarr) ',type=outtype) then return
   if badpar(nx,[2,3],0,caller=self+'(nx) ') then return
   if badpar(ny,[2,3],0,caller=self+'(ny) ') then return
   if badpar(add,[0,1,2,3],0,caller=self+'(ADD) ',default=0) then return

   ; Get the size of the input array?
   sz = size(imgarr,/dimen)

   ; Create two arrays that carry the native output pixel coordinates
   outy = indgen(long(nx)*long(ny),/LONG)
   outx = outy mod nx
   outy = temporary(outy) / ny

   ; Convert output pixel coordinates to position on sky
   astxy2rd,outx,outy,outinfo,outra,outdec,/FULL

   ; dump outx and outy
   outx=0
   outy=0

   ; Convert sky positions to input pixel coordinates
   astrd2xy,outra,outdec,ininfo,inx,iny,/FULL

   ; dump outra and outdec
   outra=0
   outdec=0

   ; Create output image
   if not add or outtype eq 0 then begin
      imgout = fltarr(nx,ny)
      count=intarr(nx,ny)
   endif
   outsz=size(imgout,/dimen)
   if outsz[0] ne nx or outsz[1] ne ny then begin
      imgout = fltarr(nx,ny)
      count=intarr(nx,ny)
   endif

   ; Does count image exist and match the size of imgout?  If not, create it.
   csz=size(count,/structure)
   if csz.type_name eq 'UNDEFINED' or $
      (csz.type_name ne 'INT' and csz.type_name ne 'LONG') or $
      csz.n_dimensions ne 2 or csz.n_elements ne n_elements(imgout) then $
      count=intarr(nx,ny)

   ; find those output pixels that map onto input array
   zg = where(inx ge 0 and inx lt sz[0] and $
              iny ge 0 and iny lt sz[1], countg)

   if countg gt 0 then begin
      newvals = interpolate(imgarr,inx[zg],iny[zg],cubic=-0.5)
      count[zg] += 1
      if add then imgout[zg] += newvals else imgout[zg] = newvals
   endif else begin
      print,self,'Warning!, no overlap between input and output images.'
   endelse

end

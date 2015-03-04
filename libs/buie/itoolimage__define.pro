;+
; NAME:
;    itoolimage__define
; PURPOSE: (one line)
;    Define the 'itoolimage' object class.
; DESCRIPTION:
;
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    result = obj_new('itoolimage', image)
; INPUTS:
;    image : A 2-D or 3-D image array, for use with the itool GUI.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
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
;    2006/03/15 - Written by Doug Loucks. Renders the itool_init procedure
;              as obsolete. With this object class, all image data,
;              image-data initializations, and image-data cleanup are
;              combined and easily managed, using standard IDL object
;              techniques.
;-

; Cleanup method.
pro itoolimage::cleanup
   compile_opt hidden

   ; Release the memory holding the image data and free the pointer to
   ; those data.
   ptr_free, (*self.pim_parms).imageptr

   ; Release the memory holding the image-parameters structure and free
   ; the pointer to the structure.
   ptr_free, self.pim_parms
end

; Getproperty method.
pro itoolimage::getproperty, PIM_PARMS=pim_parms
   ; Return a coopy of the pointer to the image-parameters structure in
   ; the PIM_PARMS keyword argument.
   if arg_present(pim_parms) then pim_parms = self.pim_parms
end



; Init method.
function itoolimage::init, image
   compile_opt hidden

   ; Make a pointer to the image argument, without copying the data. This
   ; causes the incoming argument to become undefined, but the caller may
   ; extract a copy afterward, by using the pointer variable created here.
   
   pimage = ptr_new(image, /NO_COPY)
   image_size = size(*pimage)
   xsize = image_size[1]
   ysize = image_size[2]
   nframes = 1L
                                                                                
   if image_size[0] eq 3 then begin
      ; We have a cube of images.
      nframes = image_size[3]
   endif

   ; Initialize the image parameters structure.
   im_parms = {$
      airmass:0.0,$
      autophot:0,$      ; Flag, set when auto photometry requested.
      date:'',$
      exptime:1.0,$
      expdelta:0.0,$
      filter:'',$
      imageptr:pimage,$
      imfile:'',$
      jd:0.0D0,$
      lastfwhm:0.0,$        ; Last FWHM
      lastmag:0.0,$         ; Last magnitude
      lastpos:fltarr(2),$   ; Last photometry position
      lasttype:0,$          ; Type of last click 0=none, 1=left, 2=right
      object:'',$
      ut:'',$
      asis:BYTARR(nframes),$
      frame:0L, nframes:nframes,$
      curmin:FLTARR(nframes), curmax:FLTARR(nframes),$
      minvalue:FLTARR(nframes), maxvalue:FLTARR(nframes),$
      ready:BYTARR(nframes),$
      sclmin:FLTARR(nframes), sclmax:FLTARR(nframes),$
      xsize:xsize, ysize:ysize,$
      title:'Itool'}

   ; Create a pointer to the image-parameters structure, without copying
   ; the data.
   self.pim_parms = ptr_new(im_parms, /NO_COPY)

   return, 1
end


pro itoolimage__define
   dummy = {itoolimage, pim_parms:ptr_new()}
end

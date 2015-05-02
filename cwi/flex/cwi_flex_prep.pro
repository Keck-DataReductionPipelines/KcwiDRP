; writes a flexure configuration file for each image that needs to be
; corrected. 
;
; Method: see cwi_flex.readme; defaults to 1.
; 
; Ref: the image number of the reference image that will be used to
; correct flexure affected images to
; 
; imno: the list of images that are affected by flexure and will need
; to be corrected
;
; tmpno: list of template image numbers.  Generally, this will default
; to imno, but there are occasions when the two lists will be
; different.  In particular, ref and tmpno could be numbers of arc
; flat images (say, afternoon and night) and imno will be a night-
; time data image that has no useful sky lines for correction.  This
; may happen when we are looking to correct a standard star, for
; example (though that is rarely necessary).
;
; skipx:  only applies to method 2 -- skips the x cross correlation
;                                     correction
;
; wavemask: a 2xN array of wavelenghts that designate what
; wavelengths should be masked prior to cross correlation.
;
; Note that, additionally, an individual mask can be included for
; frames being corrected using method 1.


pro cwi_flex_prep, ppfname, method=method, ref=ref, imno=imno, tmpno=tmpno, skipx=skipx , wavemask=wvmask
  
  pre="CWI_FLEX_PREP"
; make sure the reference image is specified
  if n_elements(ref) ne 1 then message,"Invalid number of reference images.  Should be 1."
  
; make sure the images are specified
  if n_elements(imno) eq 0 then message,"You need to specify images to correct!"
  
; populate tmpno if not specified. 
  if n_elements(tmpno) eq  0 then begin 
     tmpno = imno
     message, "Using images as templates.",/info
  endif
  if n_elements(tmpno) ne n_elements(imno) then message,"imno and tmpno should have the same number of elements!"
  
; make sure the method is defined
  if not keyword_set(method) then method=1 
  if method ne 1 and method ne 2 then method = 1
  message,"Method: "+string(method),/info
  
; check the skipx keyword.
  if keyword_set(skipx) then skipx=1 else skipx=0
  
  if method eq 2 and skipx eq 1 then message,"Will be skipping x cross correlation.",/info
  
                                ; get default ppar struct
  A = {kcwi_ppar}
  def_ppar = struct_init(A)
  def_ppfname = def_ppar.reddir + def_ppar.ppfname
  def_lnkname = repstr(def_ppfname,'ppar','link')
                                ;
                                ; read ppar file
  if n_params(0) lt 1 then $ 
     ppfname = def_ppfname
  if not file_test(ppfname,/read) then begin
     print,pre+': Error - pipeline parameter file not found: ',ppfname
     return
  endif
  ppar = kcwi_read_ppar(ppfname)
                                ;
                                ; verify ppar
  if kcwi_verify_ppar(ppar,/init) ne 0 then begin
     print,pre+': Error - pipeline parameter file not initialized: ',ppfname
     return
  endif
  
                                ; directories
  if kcwi_verify_dirs(ppar,indir,odir,cdir,ddir,/nocreate) ne 0 then begin
     kcwi_print_info,ppar,pre,'Directory error, returning',/error
     return
  endif
  
; ok. now we are ready to set this thing up.
  
  strc = {  method:0, imno:0, reference:0, template:0, $
           computed:0, applied:0, ns:0, mask:0, skipx:0, $
           kwx_old:fltarr(6,6,24), kwy_old:fltarr(6,6,24), $
           kwx_new:fltarr(6,6,24), kwy_new:fltarr(6,6,24), $
           refmask:0, reference_fname:"", template_fname:"" }

  ; loop over the images.
  for f=0,n_elements(imno)-1 do begin
     strc.method = method
     strc.imno = imno[f]
     strc.reference = ref
     strc.template = tmpno[f]
     strc.computed = 0
     strc.applied = 0
     strc.kwx_old[*,*,*]=0.000
     strc.kwy_old[*,*,*]=0.000
     strc.kwx_new[*,*,*]=0.000
     strc.kwy_new[*,*,*]=0.000
     strc.skipx = skipx
     strc.reference_fname = kcwi_get_imname(ppar, ref,"_flexref",/reduced)
     strc.template_fname = kcwi_get_imname(ppar, tmpno[f],"_flexcor",/reduced)
     dimmask = size(wvmask)
     if dimmask[1] eq 2 then strc.refmask=1.0 else strc.refmask=0.0
     outfname = kcwi_get_imname(ppar, imno[f],"_flexpar",/reduced)
     mwrfits,strc,outfname,/create
     if dimmask[1] eq 2 then begin
        mwrfits,wvmask,outfname
     endif; dimmask
  endfor; f

  ; finally, 

end; cwi_flex_prep

; CWI_FLEX_INJECT_ONE,ppar,kgeom,flexpar
;
; At the moment, this does very little error checking of the
; structures that are passed.
;
; What this code does: 
; if method 2 was chosen, this code fills the the kw?_old structure
;elements, writes out flexpar again, and exits.
; 
; If method 1 was chosen, the code loads the reference and template
;images, checks for the existence of a mask image, and checks if the
;images are nod-and-shuffle.  It then performs a cross-correlation of
;the reference and template images (which might take some time,
;perhaps close to 10 minutes).  
; Once the cross-correlation matrices are found, the program uses them
;to generate a new set of anchor points in the original image, and,
;finally, uses those to generate a new geometry transformation
;matrix.
; When that is done, it flips the .computed flag, which means the
;flexure geometry will be used when kcwi_apply_geom is called. 
;                                                      
pro cwi_flex_inject_one,ppar, kgeom, flexpar, flexparmask
;
; if flexpar is not defined, do absolutely NOTHING.
  if size(flexpar,/type) ne 8 then return;
;

if flexpar.method eq 2 then begin 
   ; fill in kwx and kwy _old structures, write flexpar and exit
   flexpar.kwx_old = kgeom.kwx
   flexpar.kwy_old = kgeom.kwy
   flexpar_fname = kcwi_get_imname(ppar, flexpar.imno,"_flexpar",/reduced)
   mwrfits,flexpar,flexpar_fname,/create
   mwrfits,flexparmask,flexpar_fname
endif else begin
   ; method 1!
   if flexpar.computed eq 0 then begin 
      ; populate old structures
      flexpar.kwx_old = kgeom.kwx
      flexpar.kwy_old = kgeom.kwy
      ; load in the images to be cross correlated.
      template_fname = kcwi_get_imname(ppar,flexpar.template,"_sky",/reduced)
      if file_test(template_fname) then begin
         reference_fname = kcwi_get_imname(ppar,flexpar.reference,"_sky",/reduced)
         img_template = mrdfits(template_fname,0,hdr_template,/silent,/fscale)
         img_reference = mrdfits(reference_fname,0,hdr_reference,/silent,/fscale)
         flex_ns = 1
      endif else begin
         template_fname = kcwi_get_imname(ppar,flexpar.template,"_int",/reduced)
         reference_fname = kcwi_get_imname(ppar,flexpar.reference,"_int",/reduced)
         img_template = mrdfits(template_fname,0,hdr_template,/silent,/fscale)
         img_reference = mrdfits(reference_fname,0,hdr_reference,/silent,/fscale)
         flex_ns = 0 
      endelse ; _sky exist? 
      ;
      ; set up a mask image
      flex_mask = img_reference-img_reference
      ; mask out all but the NS region, if N+S exists.
      if flex_ns then begin
         y0 = sxpar(hdr_reference,"NSOBJR0")
         y1 = sxpar(hdr_reference,"NSOBJR1")
         flex_mask[*,0:y0-1] = 1
         flex_mask[*,y1+1:*] = 1        
      endif; flex_ns
      ; load in the reference mask image, if it exists generate if 
      ; it does not, but is supposed to.
      if flexpar.refmask eq 1 then begin
         refmask_fname = kcwi_get_imname(ppar,flexpar.reference,"_refmask",/reduced);
         if file_test(refmask_fname) then begin
            flex_ref_mask = mrdfits(refmask_fname);
         endif else begin
            ; we need to have a reverse map for this. 
            wavemap_fname = kcwi_get_imname(ppar,kgeom.cbarsimgnum,"_wavemap",/reduced);
            if not file_test(wavemap_fname) then cwi_reverse_geom,kgeom,ppar
            revmap = mrdfits(wavemap_fname);

            ; load in the mask data
            flexpar_fname = kcwi_get_imname(ppar, flexpar.imno,"_flexpar",/reduced)
            mask_data = mrdfits(flexpar_fname,2)
            dimmask = size(mask_data)
            if  dimmask[1] ne 2 then message,"Mask has wrong dimensions."
            tmp_mask = flex_mask-flex_mask
            if dimmask[0] eq 1 then begin
               q = where(revmap ge mask_data[0] and revmap le mask_data[1], nq) 
               if nq gt 0 then tmp_mask[q] = 1
            endif else begin
               for im = 0, dimmask[2]-1 do begin
                  q = where(revmap ge mask_data[0,im] and revmap le mask_data[1,im], nq)
                  if nq gt 0 then tmp_mask[q] = 1
               endfor           ; im
            endelse             ; 
                                ; create a buffer around the actual
                                ; spots.

            tmp_mask = smooth(tmp_mask, 7);
            qm = where(tmp_mask gt 0, nqm)
            if nqm gt 0 then tmp_mask[qm] = 1
            flex_ref_mask = tmp_mask
                                ; write the mask for future runs.
            mwrfits,flex_ref_mask, refmask_fname
            
         endelse; refmask exists.
         flex_mask += flex_ref_mask
      endif; flexpar.refmask eq 1
      ; load in the user mask image, if it exists. 
      mask_fname = kcwi_get_imname(ppar,flexpar.imno,"_flexmask",/reduced)
      if file_test(mask_fname) then begin
         flex_user_mask = mrdfits(mask_fname);
         flex_mask += flex_user_mask;
      endif; mask_fname
      ; construct a mask arraay.
      flex_mask_arr = where(flex_mask gt 0,nq)
      flex_mask[*] = 1
      ; if there is no mask, for simplicty we just mask one pixel
      if nq eq 0 then flex_mask_arr = 1
      flex_mask[flex_mask_arr] = 0 
      window,0
      plotimage,bytscl(flex_mask)
      ;
      ; OK. We have the images, the mask, and so forth, so now
      ; we can set up the cross correlation. 
      ; first we do a grid cross correlation, that *should* make things
      ; a little faster in the full xcorr.
      flex_start_time=systime(1)

      flex_N = 10
      flex_peak = fltarr(2*flex_N+1,2*flex_N+1)
      ; generate median images
      message,"Subtracting median image",/info
      flex_ref_median = cwi_median(img_reference,[1,15])
      flex_tmp_median = cwi_median(img_template,[1,15])
      
;      kern = [[0.0, 0.0, 0.5, 1.0, 0.5, 0.0, 0.0 ], [0.0, 0.0, 1.0, -2.8, 1.0, 0.0, 0.0], $
;              [0.0, 0.0, 1.0, -2.8, 1.0, 0.0, 0.0], [0.0, 0.0, 1.0, -2.8, 1.0, 0.0, 0.0], $
;              [0.0, 0.0, 1.0, -2.8, 1.0, 0.0, 0.0], [0.0, 0.0, 1.0, -2.8, 1.0, 0.0, 0.0], $
;              [0.0, 0.0, 0.5, 1.0, 0.5, 0.0, 0.0] ]

;      kern = [[ 0.0,0.0, 1.0, 0.0, 0.0], [0.0, 1.0, -2.6667, 1.0, 0.0], $
;              [0.0, 1.0, -2.6667, 1.0, 0.0], [0.0, 1.0, -2.6667, 1.0, 0.0],$
;              [0.0, 0.0, 1.0, 0.0, 0.0]]


;kern = [ [ 0.5, 1.0, 0.5 ] , [1.0, -6.0, 1.0],[ 0.5, 1.0, 0.5]]

;kern = [ [ 0.0, 1.0, 0.0 ] , [1.0, -4.0, 1.0],[ 0.0, 1.0, 0.0]]


;      flex_ref_median = smooth(convol(img_reference,kern),3)
;      flex_tmp_median = smooth(convol(img_template,kern),3)

;      mwrfits,smooth(flex_ref_median,3),'tempr.fits',/create
;      mwrfits,smooth(flex_tmp_median,3),'tempt.fits',/create
;      stop
      

      flex_ref_mod =  img_reference-flex_ref_median;
      flex_tmp_mod =  img_template-flex_tmp_median

      

      message,"Computing initial cross-correlation.",/info
      flex_ref_mod = flex_ref_mod*flex_mask
      flex_tmp_mod = flex_tmp_mod*flex_mask
;      for ix=-flex_N,flex_N do begin
;         for iy=-flex_N,flex_N do begin
;            flex_peak[ix+flex_N,iy+flex_N] = $
;               total(shift(flex_ref_mod,ix,iy)*flex_tmp_mod);
;         endfor                 ; iy
;         print,string(ix+flex_N+1)+"/"+string(2*flex_N+1,"(i)")
;      endfor                    ; ix
      

      if flex_ns then correl_optimize,flex_tmp_mod[*,y0:y1], flex_ref_mod[*,y0:y1], xoff, yoff, magnification = 2 $
      else correl_optimize,flex_tmp_mod, flex_ref_mod, xoff, yoff, magnification = 2 
      
      
     ; so, where is the maximum?
;      flex_xcorr_max = max(flex_peak, flex_xcorr_max_idx)
;      flex_xcorr_max_dx = (flex_xcorr_max_idx mod (2*flex_N+1)) - flex_N
;      flex_xcorr_max_dy = (flex_xcorr_max_idx / (2*flex_N+1)) - flex_N
      message,"Preliminary cross-correlation computed:",/info
      message,"           (dx, dy) = ("+string(-xoff,"(f7.3)")+ ", "+ $
              string(-yoff,"(f7.3)")+")",/info
      flex_split_time=systime(1)
      message,"Elapsed: "+string(flex_split_time-flex_start_time,"(i4)"),/info
      message,"Computing full cross-correlation.",/info      
      message,"Reference:  "+string(flexpar.reference),/info
      message,"Template:  "+string(flexpar.template),/info
      message,"IMNO:  "+string(flexpar.imno),/info
      ; prep auto_align_images
      flex_ref_mod = img_reference;-flex_ref_median ;
      flex_tmp_mod = img_template;-flex_tmp_median
      fx_i = fltarr(2,2)
      fx_i = fx_i-fx_i
      fy_i = fx_i
      fx_i[0,1] = 1.0
      fy_i[1,0] = 1.0
;      fx_i[0,0] = -1.0 * flex_xcorr_max_dx
;      fy_i[0,0] = -1.0 * flex_xcorr_max_dy
;      fx_i[0,0] = 1.0 * flex_xcorr_max_dx
;      fy_i[0,0] = 1.0 * flex_xcorr_max_dy
      fx_i[0,0] = 1.0 * xoff
      fy_i[0,0] = 1.0 * yoff
      fx = fx_i
      fy = fy_i
 
      ; quick mask adjustment to take care of some stuff. 
      ; testing as 
      sigcut = 3.0
      if flex_ns then begin
         ref_collapse = median(img_reference[*,y0:y1],dim=2)
         tmp_collapse = median(img_template[*,y0:y1],dim=2)
      endif else begin
         ref_collapse = median(img_reference,dim=2)
         tmp_collapse = median(img_template,dim=2)
      endelse                   ; flex_ns

      ; first cut
         ref_stats = moment(ref_collapse)
         tmp_stats = moment(tmp_collapse)
      q_ref_stats = where(abs(ref_collapse-ref_stats[0]) lt sigcut*sqrt(ref_stats[1]))
      q_tmp_stats = where(abs(tmp_collapse-tmp_stats[0]) lt sigcut*sqrt(ref_stats[1]))
      sub_ref_collapse = ref_collapse[q_ref_stats]
      sub_tmp_collapse = tmp_collapse[q_tmp_stats]

      ; second cut
      sub_ref_stats = moment(sub_ref_collapse)
      sub_tmp_stats = moment(sub_tmp_collapse)
      q_ref_stats = where((ref_collapse-sub_ref_stats[0]) gt sigcut*sqrt(sub_ref_stats[1]),nqrs)
      q_tmp_stats = where((tmp_collapse-sub_tmp_stats[0]) gt sigcut*sqrt(sub_ref_stats[1]),nqts)
      print,"total excess columns: "+string(nqrs+nqts);
      flatmask = img_reference-img_reference;
      flatmask[q_ref_stats] = 1
      flatmask[q_tmp_stats] = 1 
      flatmask = smooth(flatmask,5) ;
      qflatmask = where(flatmask gt 0, nqflatmask)
      print,"total masked columns: "+string(nqflatmask)
      if nqflatmask gt 0 then begin
         flex_mask[qflatmask,*]=0
         plotimage,bytscl(flex_mask) ;
         flex_mask_arr = where(flex_mask eq 0,nqarr)
         if nqarr eq 0 then flex_mask_arr=1
      endif                     ; nqflatmask
;      stop

;      mwrfits,flex_tmp_mod,'temp.fits',/create
;      stop     
; note that the /noscale switch is custom.  It makes sure that only
; shifts and rotations are allowed.
      flex_xcorr = auto_align_images(flex_tmp_mod, flex_ref_mod,$
                                     fx_i, fy_i, fx, fy,$
                                           /double, /noplot, /powell, $
                                           tmask = flex_mask_arr, $
                                           rmask = flex_mask_arr, $
                                    scale = 4.0, ftol=1e-8, /noscale) 
      ; at this point (fx, fy) contain the transformation
      ; that takes the reference into the template
      ; now we need to compute the warp matrices.
      xi = kgeom.xi
      yi = kgeom.yi
      xw = kgeom.xw
      yw = kgeom.yw
      ; apply the flexure warp to the input coefficients.
      cwi_warp_coeffs, xi, yi, fx,fy, xo, yo
      ;                                
      ; also, now let's write out a little image!
      res = poly_2d(img_template,fx,fy,2,cubic=-0.5)

      mwrfits,res,flexpar.template_fname,/create
      message,"To see how well the flexure worked, compare: ",/info
      message,flexpar.template_fname,/info
      message,"with:",/info
      message,template_fname,/info
      ;
         ; There is some padding we will be doing. 
         yo += kgeom.ypad
         ;
         ; loop over slices
         for s = 0, 23 do begin
            ;
            ; select only the points that belong to the slice in question
            
            sli = where(kgeom.slice eq s and kgeom.xo gt 0. and $
		    finite(kgeom.xw) and finite(kgeom.yw), nsli)
            ;
            degree = 3
            ; throw a fit if no such points exist.
            if nsli gt 0 then begin
               ;
               ; now adjust to left edge for relevant xw.
;               xw0 = (min(xw[sli]) - 15. ) > 0.
;               xw_adj = xw[sli] - xw0
               ; 
               ; generate transformation array
               polywarp,xo[sli],yo[sli],xw[sli],yw[sli],3,kwx,kwy,/double
               ;
               ; and fill out the kgeom.kwx/y arrays.
               flexpar.kwx_new[0:degree,0:degree,s] = kwx
               flexpar.kwy_new[0:degree,0:degree,s] = kwy
            endif else message,"No anchor points found."
            
         endfor                 ;s 
         ; flexpar is computed
         flexpar.computed = 1
         ; write out the flexpar. 
         flexpar_fname = kcwi_get_imname(ppar, flexpar.imno,"_flexpar",/reduced)
         mwrfits,flexpar,flexpar_fname,/create
         if flexpar.refmask then mwrfits,flexparmask,flexpar_fname
         flex_end_time = systime(1)
         message,"Total time:"+string(flex_end_time-flex_start_time,"(i5)"),/info
         message,"Cross-correlation and warping coefficients computed.",/info
endif else message,"FLEXPAR already computed.",/info
   
endelse; flexpar.method eq 1


; get out.
end; cwi_flex_inject_one



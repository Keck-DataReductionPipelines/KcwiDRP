; at this point method 1 is done, so this is _all_ about method 2.
; cubes have been written!
pro cwi_flex_inject_three,ppar,kgeom,imgnum
  flex_counter = 0;
  ; loop over all files to be processed.
  for f=0,n_elements(imgnum)-1 do begin
     message,"Image being processed: "+string(imgnum[f]),/info
     flexpar_fname = kcwi_get_imname(ppar, imgnum[f],"_flexpar",/output);
     if file_test(flexpar_fname) then begin
        flexpar = mrdfits(flexpar_fname,1,/silent);
        if flexpar.refmask then flexparmask = mrdfits(flexpar_fname,2,/silent) ;
        if flexpar.method eq 2 then begin   
           if flexpar.computed eq 0 then begin
      ; check if this is a nod-and-shuffle observation
              cub_reference_fname =  kcwi_get_imname(ppar,flexpar.reference,"_scube",/output)
              if file_test(cub_reference_fname) then begin
                 cub_template_fname = kcwi_get_imname(ppar,flexpar.template,"_scube",/output)
                 flex_ns = 1
              endif else begin
                 cub_reference_fname =  kcwi_get_imname(ppar,flexpar.reference,"_icube",/output)
                 cub_template_fname = kcwi_get_imname(ppar,flexpar.template,"_icube",/output)
                 flex_ns = 0 
              endelse                                                ; _scube exists.
                                ; load the two cubes.
              cub_reference = mrdfits(cub_reference_fname,0,ref_hdr) ;
              cub_template = mrdfits(cub_template_fname,0,tmp_hdr)   ;
                                ; how big are they? 
              szcub = size(cub_reference,/dim)
              ; set up a wavelength scale
              dw = sxpar(ref_hdr,'CD3_3')
              w0 = sxpar(ref_hdr,'CRVAL3')
              px0 = sxpar(ref_hdr,'CRPIX3')
              xx = indgen(szcub[2])
              wvs = w0+(xx-px0)*dw
                                ;
              flexpar.kwx_new = flexpar.kwx_old
              flexpar.kwy_new = flexpar.kwy_old
                                ;
                                ; loop over the slices
              yshifts = fltarr(24)
              xshifts = fltarr(24)
              for s=0,23 do begin
                 sli_ref = cwi_median(reform(cub_reference[*,s,*]),[1,20])
                 sli_tmp = cwi_median(reform(cub_template[*,s,*]),[1,20])                 

                 sli_ref_med = cwi_median(sli_ref,[1,15])
                 sli_tmp_med = cwi_median(sli_tmp,[1,15])                
                 sli_ref -= sli_ref_med
                 sli_tmp -= sli_tmp_med
                 cwi_flex_collapse,sli_ref,prof_ref,spec_ref
                 cwi_flex_collapse,sli_tmp,prof_tmp,spec_tmp
                 ; apply mask
                 if flexpar.refmask eq 1 then begin
                    flexpar_fname = kcwi_get_imname(ppar, imgnum[f],"_flexpar",/output) ;
                    refmask = mrdfits(flexpar_fname,2,/silent);
                    masksize = size(refmask)
                    if masksize[1] eq 2 then $
                       cwi_flex_apply_mask,spec_ref,spec_tmp,wvs,refmask
                 endif; there is a mask!

                 
                                ; perform cross correlation
                 ppardisplay = ppar.display
                 ppar.display=1
                 kcwi_xspec, spec_ref, spec_tmp, ppar, spec_offset, spec_max, /plot,/pad,/shift
                 kcwi_xspec, prof_ref, prof_tmp, ppar, prof_offset, prof_max, /plot,/pad,/shift, central=10
                 ppar.display=ppardisplay
                 print,"OFFSETS:"+string(spec_offset)+", "+string(prof_offset)
                                ; update the warp matrices

                 yshifts[s] = spec_offset

                 xshifts[s] = prof_offset
              endfor            ; s
                                ; at this point save the flexpar file and remove the cubes
                                ; also send a message to rerun stage
                                ; 4. 
;              !p. multi = [0,1,2]
;              xvs = findgen(24)
;
;              resy = linfit(xvs,yshifts)
;              plot,xvs,yshifts,psym=4
;              oplot,xvs,resy[0]+resy[1]*xvs,color=fsc_color('blue')
;
;              resx = linfit(xvs,xshifts)
;              plot,xvs,xshifts,psym=4
;              oplot,xvs,resx[0]+resx[1]*xvs,color=fsc_color('blue')

;                 flexpar.kwy_new[0,0,s] -= spec_offset                 
;                 if not flexpar.skipx then flexpar.kwx_new[0,0,s] -= prof_offset
; apply the fit. 
              for s=0,23 do begin
                 flexpar.kwy_new[0,0,s] -=  yshifts[s]; resy[0]+resy[1]*s
                 if not flexpar.skipx then flexpar.kwx_new[0,0,s] -= xshifts[s]; resx[0]+resx[1]*s
              endfor;s 

;              stop
              flexpar.computed = 1
              flex_counter += 1;
              ; write out the flex parameters and remove the previous icube
              mwrfits,flexpar,flexpar_fname,/create
              if flexpar.refmask then mwrfits,flexparmask, flexpar_fname
              cub_fname =  kcwi_get_imname(ppar,flexpar.imno,"_icube",/output)
              if file_test(cub_fname) then file_delete,cub_fname ;
           endif                                                  ; computed eq 0 
        endif; method is 2.                                                     ; 
     endif; flexpar exists

  endfor; files f
     if flex_counter gt 0 then begin
        message,"Flexure computed for:"+string(flex_counter,"(i3)")+" images.",/info
        message,"Re-run kcwi_stage4geom",/info
     endif                      ;
end; cwi_flex_inject_three

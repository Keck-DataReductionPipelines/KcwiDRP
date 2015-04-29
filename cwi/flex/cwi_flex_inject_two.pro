; at this point if the flexure was computed, it has been 
; applied, so we are going to flip that flag. 
; note that the computed flag will only be on for method 2
; if this is the second time through kcwi_stage4geom

pro cwi_flex_inject_two,ppar,kgeom,flexpar,flexparmask
  if size(flexpar,/type) eq 8 then begin
     if flexpar.computed eq 1 and flexpar.applied eq 0 then begin
        flexpar.applied = 1

        flexpar_fname = kcwi_get_imname(ppar, flexpar.imno,"_flexpar",/reduced)
        mwrfits,flexpar,flexpar_fname,/create
        if flexpar.refmask then mwrfits,flexparmask,flexpar_fname
     endif                      ; computed=1, applied=0
  endif                         ; flexpar exists
end                             ; cwi_flex_inject_two

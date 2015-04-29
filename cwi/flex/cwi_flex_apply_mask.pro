pro cwi_flex_apply_mask,spec0,spec1,wvs,mask

  sz = size(mask)
  pad = 0.0
  ys = indgen(n_elements(wvs))
  med0 = median(spec0)
  med1 = median(spec1)
  if sz[0] eq 1 then begin
     q = where( wvs gt mask[0,msk]-pad and wvs lt mask[1,msk]+pad, nq) 
     if nq gt 0 then begin
        spec0[q] = med0
        spec1[q] = med1
     endif                      ; nq gt 0 
  endif else begin
     for msk = 0,sz[2]-1 do begin
        q = where( wvs gt mask[0,msk]-pad and wvs lt mask[1,msk]+pad, nq)
        if nq gt 0 then begin
           spec0[q] = med0
           spec1[q] = med1
        endif                   ; nq gt 0 
        
     endfor                     ; msk
  endelse ;


end; cwi_flex_apply_mask

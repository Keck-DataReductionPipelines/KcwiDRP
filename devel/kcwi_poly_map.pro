; $Id: kcwi_poly_map.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; use matrices kx and ky to map (x_in, y_in) --> (x_out, y_out)
;
; M. Matuszewski 2015/02/09
;
pro kcwi_poly_map,x_in, y_in, kx, ky, x_out, y_out, degree=degree
; what degree is the polynomial
  if n_elements(degree) eq 0 then degree=3
  
  if n_elements(x_in) ne n_elements(y_in) then message,"Element number mismatch."

  nel = n_elements(x_in)

  xs = fltarr(nel,degree+1)
  ys = xs
  
  xs[*,0] = 1.00000D
  ys[*,0] = 1.00000D
  kwx = kx
  kwy = ky
  for deg=1,degree do begin
     xs[*,deg] = xs[*,deg-1]*x_in
     ys[*,deg] = ys[*,deg-1]*y_in
  endfor                        ; deg
  
  ; set up output containers
  x_out = x_in-x_in
  y_out = x_out
  for ix=0,degree do begin
     for iy=0, degree do begin
        x_out += (kx[iy,ix]*xs[*,ix])*ys[*,iy];
        y_out += (ky[iy,ix]*ys[*,iy])*xs[*,ix];
     endfor; iy
  endfor; ix 

  return
end                             ;kcwi_poly_map

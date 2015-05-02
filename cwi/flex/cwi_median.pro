; note -- kern are HALF widths!
function cwi_median,img,kern

sz = size(img,/dim)
if n_elements(sz) ne 2 then message, "Wrong array dimensions"
if n_elements(sz) ne n_elements(kern) then message,"Dimension mismatch."

szx = sz[0]
szy = sz[1]
kx = kern[0]
ky = kern[1]
newimg = img-img


for ix = 0,szx-1 do begin
   for iy = 0, szy-1 do begin
      newimg[ix,iy] = median(img[ix-kx>0:ix+kx<szx-1,iy-ky>0:iy+ky<szy-1])
   endfor; iy
endfor; ix 

return,newimg

end; cwi_median

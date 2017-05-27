function gaussykernel, fwhm, ksize
;+
; gaussykernel	- generate a Gaussian kernel in the y direction
;-
kernel = dblarr(ksize,ksize)
; sigma
sg2 = fwhm * fwhm / (2.35482)^2
; loop over dimensions of kernel
for ix=0,ksize-1 do $
	for iy=0,ksize-1 do begin
		xk = (ksize/2) - ksize + ix + 1
		yk = (ksize/2) - ksize + iy + 1
		kernel[ix,iy] = exp(-0.5 * yk * yk / sg2)
	endfor
; normalize
kernel /= total(kernel)
;
return,kernel
end

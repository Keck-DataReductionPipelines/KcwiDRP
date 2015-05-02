
; at this point, the fs are 2x2 matrices..
pro cwi_warp_coeffs, xi, yi, fx, fy, xo, yo

  
  xo = fx[0] + fx[2]*xi + fx[1]*yi+fx[3]*xi*yi
  yo = fy[0] + fy[2]*xi + fy[1]*yi+fy[3]*xi*yi


  mwrfits,xi,'xys.fits',/create
  mwrfits,yi,'xys.fits'
  mwrfits,xo,'xys.fits'
  mwrfits,yo,'xys.fits'
  


end; cwi_warp_coeffs

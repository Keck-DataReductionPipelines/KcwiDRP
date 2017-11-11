pro apply_flat,flatno,imno,prefix=prefix

  pre='kb171018_'
  if keyword_set(prefix) then pre=prefix
  fim=pre+string(imno,"(i05)")+'_int.fits'
  oim=pre+string(imno,"(i05)")+'_int_orig.fits'
  ofim=pre+string(imno,"(i05)")+'_inth.fits'
  ffl=pre+string(flatno,"(i05)")+'_ratio.fits'

  
  im=mrdfits(fim,0,hdr,/fscale)
  fl=mrdfits(ffl,0,hdr2,/fscale)

  nim=im*fl
;  stop
  mwrfits,im,oim,hdr,/create
  mwrfits,nim,ofim,hdr,/create
  
end

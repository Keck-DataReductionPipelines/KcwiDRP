pro subbg,wvno,imno,prefix=prefix,knots=knots

  pre = 'kb171018_'
  if keyword_set(prefix) then pre=prefix
  fname=pre+string(imno,"(i05)")+'_inth.fits'
  fwave=pre+string(wvno,"(i05)")+'_wavemap.fits'
  fslice=pre+string(wvno,"(i05)")+'_wavemap_sli.fits'
  fpos=pre+string(wvno,"(i05)")+'_wavemap_pos.fits'
  ofname=pre+string(imno,"(i05)")+'_intf.fits'
  ofsky=pre+string(imno,"(i05)")+'_intsky.fits'

  im=mrdfits(fname,0,hdr)

  wavemap=mrdfits(fwave,/fscale)
  slice=mrdfits(fslice)
  pos=mrdfits(fpos)

  print,fname
  print,fwave

  finiteflux=finite(im)
  
  q = where(slice ge 0 and slice le 23 and pos ge 0 and wavemap ge 3500 and wavemap lt 5550 and finiteflux); and wavemap gt 3900 and wavemap le 4300)


  
  fluxes=im
  waves=wavemap
  stop
  fluxes=im[q]
  waves=wavemap[q]

  stop
  owaves=waves
  s=sort(waves)
  waves=waves[s]
  fluxes=fluxes[s]
  
  plot,waves,smooth(fluxes,250),psym=3,yrange=[0,200];,xrange=[4000,4200]
  stop
  n=8000
  if keyword_set(knots) then n=knots
;   n=5000
  bkpt=min(waves)+findgen(n+1)*(max(waves)-min(waves))/n
  print,minmax(bkpt)
  sft0=bspline_iterfit(waves,fluxes,fullbkpt=bkpt,yfit=yfit1,upper=1,lower=1)
  yfit=bspline_valu(owaves,sft0)

  oplot,waves,yfit1,color=fsc_color('orange')

  empty=im-im
  empty[q]=yfit
  mwrfits,empty,ofsky,/create
  mwrfits,im-empty,ofname,hdr,/create
  print,"Done!"
  stop
end

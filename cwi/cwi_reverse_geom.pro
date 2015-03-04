;$Id: cwi_reverse_geom.pro,v 1.2 2015/02/26 07:36:12 neill Exp $
;
; CWI_REVERSE_GEOM
; 
; Once the KCWI pipeline determiens the mapping, this procedure
; can be used to generate an inverse mapping that helps the user
; determine where wavelengths originally fell on the CCD.  Can serve
; as a sanity check 
;
; the procedure outputs a file:
; $OUTDIR/imageNNNNN_wavemap.fits
; where NNNNN is the cbars image number for the geometry 
;  
;
; To do:
; > change how things are loaded? 
; > add some headers to the data.

pro CWI_REVERSE_GEOM, kgeom, ppar, degree=degree

; Check structs
if kcwi_verify_geom(kgeom,/init) ne 0 then return
if kcwi_verify_ppar(ppar) ne 0 then begin
	ppar = {kcwi_ppar}
endif

; read in quantities from the geometry.
; trying to avoid using these inside of the code proper
  imno = kgeom.cbarsimgnum
  nx = kgeom.nx
  ny = kgeom.ny
  ypad = kgeom.ypad
  nasmask = kgeom.nasmask
  trimy0 = kgeom.trimy0
  trimy1 = kgeom.trimy1
  kwx = kgeom.kwx
  kwy = kgeom.kwy
  xi = kgeom.xi
  yi = kgeom.yi
  xw = kgeom.xw
  yw = kgeom.yw
  slice = kgeom.slice
  refoutx = kgeom.refoutx
  x0out = kgeom.x0out
  dwout = kgeom.dwout
  wave0out = kgeom.wave0out
  
  xpad = 3

; process the degree, default to 3. 
  if n_elements(degree) eq 0 then degree = 3 
  if degree lt 2 or degree gt 4 then degree = 3 
  
  outfile = "wavemap"+string(imno,"(i05)")+".fits"

  outfile = kcwi_get_imname(ppar,kgeom.cbarsimgnum,"_wavemap",/reduced);
  print,outfile

  x = dindgen(nx)
  y = dindgen(ny); ypad
  onex = x-x+1.00000d
  oney = y-y+1.000000d
  
  xx = x#oney
  yy = onex#y
  
  reverse_image = xx-xx-10
  
  ; loop over slices
  for s=0, 23 do begin
     qs = where(slice eq s and xi gt 0 and finite(xw) and finite(yw), nqs)
     if nqs eq 0 then message,"Sorry, no points in this slice. Confused. Exiting."
     x0 = xi[qs]
     y0 = yi[qs]+ypad
     x1 = xw[qs]
     y1 = yw[qs]
     x0min = min(x0)
     x0temp = x0-x0min+x0out
;     stop
     ; generate a mapping
     polywarp,x1,y1,x0temp,y0,3,kx,ky,/double
     ; reset the "in" values
     xmm = minmax(x0)
     qin = where(xx gt xmm[0]-xpad and xx lt xmm[1]+xpad and yy ge trimy0 and yy le trimy1, nqxin)
     if nqxin eq 0 then message, "Sorry, no suitable points found. Confused. Exiting."
     xin = xx[qin]-x0min+x0out
     yin = yy[qin]+ypad
     cwi_poly_map,xin,yin,kx,ky,xout,yout

     ; set the pixel values to the wavelengths.
     reverse_image[xin-x0out+x0min,yin-ypad] = yout*dwout+wave0out

  endfor                        ;

  ; write the file
  message,"Writing: "+outfile,/info
  mwrfits, reverse_image,outfile,/create
  message,"Generated reverse map.",/info
end;

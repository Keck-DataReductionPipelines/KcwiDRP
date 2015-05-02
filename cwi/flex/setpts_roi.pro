;+

function setpts_roi,image_trans,image_ref, $
                   plotonly=plotonly, $
                   noscale=noscale, $
                   append=append, $
                   trima=trima, $
                   key=key, $
                   xvi=xvi,yvi=yvi,xvr=xvr,yvr=yvr, $
                   restrict_fov=restrict_fov
;NAME:
;     setpts_roi
;PURPOSE:
;     A front end to the setpts routine which allows a ROI selection on 
;     each image.
;CATEGORY:
;CALLING SEQUENCE:
;     pp=setpts_roi(image,ref_image)
;INPUTS:
;     image = image to be transformed
;     ref_image = reference image
;OPTIONAL INPUT PARAMETERS:
;KEYWORD PARAMETERS
;     Same as setpts.pro
;OUTPUTS:
;     pp vector from setpts
;COMMON BLOCKS:
;SIDE EFFECTS:
;RESTRICTIONS:
;PROCEDURE:
;MODIFICATION HISTORY:
;     T. Metcalf 1996 August 29
;-

   nxi = n_elements(image_trans(*,0))
   nyi = n_elements(image_trans(0,*))
   nxr = n_elements(image_ref(*,0))
   nyr = n_elements(image_ref(0,*))

   bini = 1L+max([nxi,nyi])/640L
   binr = 1L+max([nxr,nyr])/640L

   REPEAT nxi = nxi - 1L UNTIL nxi MOD bini EQ 0
   REPEAT nyi = nyi - 1L UNTIL nyi MOD bini EQ 0
   REPEAT nxr = nxr - 1L UNTIL nxr MOD binr EQ 0
   REPEAT nyr = nyr - 1L UNTIL nyr MOD binr EQ 0

   print,'define ROI for transformed image'
   junk = wdefroi(rebin(image_trans(0:nxi-1,0:nyi-1),nxi/bini,nyi/bini,/sample),xvi,yvi)
   print,'define ROI for reference image'
   junk = wdefroi(rebin(image_ref(0:nxr-1,0:nyr-1),nxr/binr,nyr/binr,/sample),xvr,yvr)

   xvi = [min(xvi),max(xvi)]*bini
   yvi = [min(yvi),max(yvi)]*bini
   xvr = [min(xvr),max(xvr)]*binr
   yvr = [min(yvr),max(yvr)]*binr

   setpts,pp,image_trans(xvi(0):xvi(1),yvi(0):yvi(1)), $
          image_ref(xvr(0):xvr(1),yvr(0):yvr(1)), $
          plotonly=plotonly, $
          noscale=noscale, $
          append=append, $
          trima=trima, $
          key=key

   if NOT keyword_set(restrict_fov) then begin
      pp(0,0,*) = pp(0,0,*) +  xvr(0)  ; x-coord of ref image
      pp(1,0,*) = pp(1,0,*) +  yvr(0)  ; y-coord of ref image
      pp(0,1,*) = pp(0,1,*) +  xvi(0)  ; x-coord of transformed image
      pp(1,1,*) = pp(1,1,*) +  yvi(0)  ; y-coord of transformed image
   endif

   return,pp

end


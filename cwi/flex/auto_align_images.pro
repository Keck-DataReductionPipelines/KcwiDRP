

function auto_align_images_func,pqin,init=init,noplot=keynoplot

   ; This routine calculates the cross correlation between the reference
   ; image and the transformed image after applying the pq transformation.
   ; It is used in the minimization routine to find the best transformation
   ; and should not be called directly.

   common auto_align_images_private,rimage,timage, $
                                    npq,rnx,rny,nxt,nyt, $
                                    restricted,quiet,xmask,ymask,rmask, $
                                    bestpq,bestcorr,noplot,levels, $
                                    ngoodtest,marray,noscale
   common auto_align_images_dfpmin, use_dfp, dfpscale

   if use_dfp then pq = pqin*dfpscale else pq = pqin
   
   if restricted then begin
      ;pq is [exscl,eyscl,exshft,eyshft,erot]
      
      tr = rss2pq(nxt,nyt,xshift=pq[2],yshift=pq[3],rot12=pq[4], $
                  xscale=pq[0],yscale=pq[1],/center,p=p,q=q)
   endif else begin
      p = pq[0L:npq-1L]
      q = pq[npq:*]
   endelse
   ; There is a bug in some versions of IDL in which the missing value is
   ; always set to 0.0 after a call to poly_2d, even if the missing keyword
   ; is set to something other than 0.0.  Hence to find the good part of the
   ; transformation, I have to run poly_2d twice since the image array may 
   ; have arbitraty value and I can't just check for zeroes in itest (which 
   ; may have real zeroes in it).
   itest = poly_2d(timage,p,q,2,rnx,rny,cubic=-0.5,missing=0.0)
   imiss = poly_2d(marray,p,q,2,rnx,rny,cubic=-0.5,missing=0)
   good = where(imiss NE 0.0,ngood)

   if keyword_set(init) then ngoodtest = long(ngood/10.0 + 0.5)>0L
   ; Make sure that the transformation is not wiping out more than 90% 
   ; of the points which were good in the inital guess.
   if ngood GT ngoodtest then begin

      ; Make the arrays as small as possible to increase speed
      x = good MOD rnx
      y = good / rnx
      minx = min(x) & maxx = max(x)
      miny = min(y) & maxy = max(y)
      snx = maxx-minx+1L
      sny = maxy-miny+1L
      ; Cut off zero areas to save computation time
      srimage = rimage[minx:maxx,miny:maxy]
      sitest  =  itest[minx:maxx,miny:maxy]
      simiss  =  imiss[minx:maxx,miny:maxy]

      mask = fltarr(snx,sny)  ; Mask out all the "bad" points
      mask[*] = 1.0
      bad = where(simiss EQ 0,nbad)  ; Could be some if images have 
                                     ; relative rotation
      if nbad GT 0 then begin
         mask[bad] = 0.0 ; Zero out missing areas
         ;if NOT keyword_set(quiet) then $
         ;   srimage[bad] = 0.0
      endif
      if xmask[0] GE 0 then begin  ; Apply transformed mask
         tforward = dblarr(4,2)
         ; The transformation of the mask to the new coordinate system is
         ; restricted to a general linear transform.  To get the linear
         ; part, make the array square, extract the lowest portion, and
         ; then make it a 1-d array again.  This looks complex, but just
         ; keeps the indices straight.
         tforward[*,0] = reform((reform(p,long(sqrt(n_elements(p))+0.5), $
                               long(sqrt(n_elements(p))+0.5)))[0:1,0:1],4)
         tforward[*,1] = reform((reform(q,long(sqrt(n_elements(q))+0.5), $
                               long(sqrt(n_elements(q))+0.5)))[0:1,0:1],4)
         ; The inversion is only valid when the transformation is strictly
         ; rotation, shift, and scale.  Significant warping will invalidate
         ; the inversion.  However, this is much faster than using pqinvert
         ; so we'll live with the possibility that the mask is off a bit in
         ; some unusual situations.
         treverse=caltrans(ppinvert(pq2pp(tforward))) ; Invert p&q
         p = reform(treverse[*,0],2,2)  ; inverted p & q
         q = reform(treverse[*,1],2,2)
         ; Get the transformed coordinates
         txmask = p(0,0) + p(0,1)*xmask + p(1,0)*ymask + p(1,1)*xmask*ymask - minx
         tymask = q(0,0) + q(0,1)*xmask + q(1,0)*ymask + q(1,1)*xmask*ymask - miny
         txmask = long(txmask + 0.5)  ; Nearest integer
         tymask = long(tymask + 0.5)
         mgood = where(txmask GE 0 AND txmask LE (snx-1) AND $
                       tymask GE 0 AND tymask LE (sny-1),nmgood)
         if nmgood GT 0 then begin
            mask[txmask[mgood],tymask[mgood]] = 0.0
            if NOT keyword_set(quiet) then $
               srimage[txmask[mgood],tymask[mgood]] = 0.0
         endif
      endif
      if rmask[0] GE 0 then begin  ; Apply the reference mask
         xrmask = (rmask MOD rnx) - minx
         yrmask = (rmask / rnx) - miny
         mgood = where(xrmask GE 0 AND xrmask LE (snx-1) AND $
                       yrmask GE 0 AND yrmask LE (sny-1),nmgood)
         if nmgood GT 0 then begin
            mask[xrmask[mgood],yrmask[mgood]] = 0.0
            if NOT keyword_set(quiet) then $
               srimage[xrmask[mgood],yrmask[mgood]] = 0.0
         endif
      endif

      ; Compute the cross correlation at the good points
      cgood = where(mask NE 0.0,ncgood)
      if ncgood EQ 0 then crosscorr = 0. $
      else begin
         ravg = total(srimage[cgood])/ncgood
         tavg = total(sitest[cgood]) /ncgood

         ; The cross correlation
         ;crosscorr = ((srimage-ravg))*((sitest-tavg))*mask
         ;crosscorr = total(crosscorr)/ncgood

         ; The normalized cross correlation (in range [-1,+1])
         rr = (srimage-ravg)*mask
         tt = (sitest-tavg)*mask
         crosscorr = total(rr*tt)/sqrt(total(rr^2)*total(tt^2))
      endelse

      ; Optionally show the progress
      if NOT keyword_set(quiet) AND NOT keyword_set(noplot) AND $
         NOT keyword_set(keynoplot) then begin
         if size(sitest,/n_dimensions) EQ 2 AND $
            (size(sitest,/dimensions))[0] GT 1 then begin
            binx = fix(!d.x_size/snx)>1
            biny = fix(!d.y_size/sny)>1
            bin = binx < biny
            if !d.x_size LT snx*bin OR !d.y_size LT sny*bin then begin
               if !d.window GE 0 then wdelete,!d.window
               wdef,ws,snx*bin,sny*bin
            endif
            erase
            tv,bytscl(rebin(srimage,snx*bin,sny*bin,/sample), $
                      top=!d.table_size-2)
            clevels = min(sitest) + (max(sitest)-min(sitest))*levels
            ocontour2,sitest,level=clevels,bin=bin,c_color=!d.table_size-1
            xyouts,/normal,0.0,0.0,string(crosscorr[0,0]),align=0.0, $
                   charsize=1.5
            empty
         endif
      endif

      ;print,[pq,crosscorr[0]]  ; debug statement

      if NOT finite(crosscorr[0,0]) then return,0.0

      if -crosscorr[0,0] LT bestcorr then begin
         bestpq=pq
         bestcorr = -crosscorr[0,0]
      endif

      return,-crosscorr[0,0]  ; negative since powel minimizes, 
                              ; but we want to maximize the cross corr
   endif else return,0.0  ; Not enough good points so must be WAY off.

end

function auto_align_images_grad,pq

   common auto_align_images_grad_dfpmin,delta

   ; Compute the gradient of the cross correlation wrt the transformation

   gradient = pq
   testpq = double(pq)
   for i=0,n_elements(gradient)-1 do begin
      dpqi = double(pq[i])
      d = dpqi*delta
      ;if d EQ 0 then d = sqrt((machar(/double)).eps)
      if d EQ 0 then d = delta
      testpq[i] = dpqi + d     & cplu1 = auto_align_images_func(testpq,/noplot)
      testpq[i] = dpqi - d     & cmin1 = auto_align_images_func(testpq,/noplot)
      testpq[i] = dpqi + 2.0*d & cplu2 = auto_align_images_func(testpq,/noplot)
      testpq[i] = dpqi - 2.0*d & cmin2 = auto_align_images_func(testpq,/noplot)
      testpq[i] = dpqi  ; restore it
      ; really do need the 5 point symmetric derivative here
      gradient[i] = (cmin2 - 8.0d0*cmin1 + 8.0d0*cplu1 - cplu2)/(12.0d0*d)
      ;gradient[i] = (cplu1-cmin1)/(2.0d0*d)  ; symmetric 3 point derivative
   endfor
   return,gradient

end

function auto_align_images_conj_grad,pq,pder

   r = auto_align_images_func(pq)
   if n_params() GT 1 then pder = auto_align_images_grad(pq)
   return,r

end

;+

function auto_align_images,timagein,rimagein,pin,qin,pout,qout, $
                           unrestricted=unrestrictedin,quiet=quietin, $
                           missing=missval,itmax=itmaxin,double=double, $
                           tmask=tmask,rmask=rmaskin,amoeba=amoebain, $
                           powell=powellin,scale=sscalein,dfpmin=dfpminin, $
                           conjgrad=conjgradin, $
                           noplot=noplotin,levels=levelsin,ftol=ftol, $
                           noscale=noscalein

;NAME:
;     AUTO_ALIGN_IMAGES
;PURPOSE:
;     Align arbitrary images using a cross correlation.  This routine is
;     particularly useful for images from different instruments in which
;     a very general transformation between the two images is required.  
;     Don't use this routine if you only need to shift the two images on
;     top of one another.  It'll work, but it will be much slower than
;     you need.
;CATEGORY:
;CALLING SEQUENCE:
;     inew=auto_align_images(transformed_image,reference_image, $
;                            pin,qin,pout,qout)
;INPUTS:
;     transformed_image = image to be transformed onto the reference image.
;     refererence_image = reference image. 
;     pin,qin = Initial guess for the p and q passed to poly_2d.  They must
;               be 4-element vectors unless /unrestriced is set, in which 
;               case you can use a higher order transformation (see poly_2d).
;               In genral they must each have (N+1)^2 elements where N is the 
;               order.  You must get pin and qin pretty close!! 
;               e.g. you can use the mouse to click on corresponding points 
;               like this:
;           pp = setpts_roi(transformed_image,reference_image)
;           tt = caltrans(pp)
;           pin = tt[*,0]
;           qin = tt[*,1]
;           Note: It is a good idea to use the type 's' transtype *structure*
;           in the call to caltrans if you do not set /unrestricted.
;OPTIONAL INPUT PARAMETERS:
;KEYWORD PARAMETERS
;     missing = Value to use where the transformed image is out of the 
;               field of view of the initial image to be transformed.
;     /unrestricted = Do not restrict the transformation to rotation, shift, 
;                     and scale, i.e. allow a very general warping.
;                     Chances are you do not want to set this keyword in most 
;                     circumstances since it is slower.  You might use it 
;                     if the images have blurring or distortion.  If you
;                     are not satisfied with the rotation, shift, scale 
;                     result, try using this keyword as there may be some
;                     subtle warping in one of your images.
;     tmask = index vector into the transformed image giving pixels which are
;             to be ignored in the transformed image when computing the cross
;             correlation.  Use to wipe out off-limb ponts, blemishes, etc.
;             For example, if your image has points off the limb and these
;             points have a value less than, say, 50, then you would use
;             tmask = where(transformed_image LT 50.0).
;     rmask = index vector into the reference image giving pixels which are
;             to be ignored in the reference image when computing the cross
;             correlation.  Use to wipe out off-limb points, blemishes, etc.
;     /powell, /amoeba, /dfpmin = Select the minimization algorithm 
;                       (powell, amoeba, or dfpmin).  /dfpmin is
;                       a bit faster, but /powell is the most
;                       robust.  /powell is the default and is the
;                       best choice.  /amoeba does a more 
;                       global search but does not always find the
;                       maximum correlation with sufficient accuracy.
;                       dfpmin is more likely to get stuck in a local
;                       minimum and so is more senstive to the initial
;                       guess than powell or amoeba.
;     itmax = maximum number of iterations.  For the Powell and dfpmin
;             minimizations, this is the number of iterations (default
;             = 200 for Powell, 500 for dfpmin).  For the amoeba
;             minimization this is the maximum number of function
;             calls (default =10000).
;     ftol = tolerance.  Def = 1.e-4 for /powell and 1.e-6 for
;            /amoeba and 1.e-7 for dfpmin.
;     scale = sets the scale of the search space in the minimization.  If 
;             your initial guess is not very good you might want to set this
;             to a larger value.  Accuracy and speed will suffer if you need
;             to increase this, however.  Default = 1.0.  If your initial
;             guess is really good, you might want to set this to something
;             in the range from 0.1 to 0.5.  If your initial guess is really
;             bad, you might want to set this to something in the range from
;             2.0 to 10.0.
;     levels = Contour levels on display as a fraction of the distance
;              between the minimum and maximum values in the image.
;              Default = [0.1,0.25,0.50,0.75,0.9].  Not used if /noplot or
;              /quiet is set.
;     /noscale = does not allow the image to be scaled, rotation and
;     shift only.  Added MKM 150212
;     /double = use double precision in the minimization.
;     /quiet = work quietly
;     /noplot = No graphics output.  Implied when /quiet is set.
;OUTPUTS:
;     inew = new transformed image overlaid on reference image.
;     pout,qout = Output transformation.  
;COMMON BLOCKS:
;SIDE EFFECTS:
;RESTRICTIONS:
;     The initial guess (pin,qin) must be close.  This routine only refines
;     that guess.  How close is close?  I'm not sure.  It seems pretty robust,
;     but be careful if your initial guess isn't very good.  If you
;     use the dfpmin algorithm, the solution is particularly sensitive
;     to the initial guess, much more so than powell or amoeba.
;
;     *Very slow*
;
;     When tmask is set, the tmask coordinates must be transformed to the 
;     reference coordinate system.  This transformation is limited to a 
;     general linear transformation.  So, if you are using a higher order
;     transformation, the tmask may not be transformed quite right.  It is
;     very unlikely this will ever be a problem, but there it is.
;PROCEDURE:
;
;     The initial warping is tweaked until a maximum in the cross correlation
;     is found using the powell minimization procedure.
;
;     The transformed image overlaid on the reference image is:
;
;     new_image = poly_2d(transformed_image,pout,qout,2,rnx,rny,cubic=-0.5)
;     tvscl,reference_image
;     ocontour2,new_image
;
;        where rnx,rny is the size of the reference image
;
;     If unrestricted=0 and quiet=0 then the best rotation, shift, and 
;     scale are listed.  These define how to transform transformed_image to
;     reference_image with the rotation defined at positive if the rotation
;     is CCW.  These values can also be derived from pout and qout as follows:
;
;     pq2rss,pout,qout,erot,exscl,eyscl,exshft,eyshft,enrss,nxt,nyt,/center
;
;     Here's a trick to verify accuracy.  After the minimization, you can
;     restart again with a small scale value to check that the algorithm
;     converges to the same point again:
;        inew=auto_align_images(timage,rimage,pin,qin,pint,qint)
;        inew=auto_align_images(timage,rimage,pint,qint,pout,qout,scale=0.1)
;
;     Since /amoeba does a more global search, it is useful for
;     improving an initial guess with a quick call that does not fully
;     converge, e.g., 
;     inew=auto_align_images(timage,rimage,pin,qin,pint,qint,/amoeba,/quiet,itmax=250)
;     inew=auto_align_images(timage,rimage,pint,qint,pout,qout,/powell)
;
;MODIFICATION HISTORY:
;     T. Metcalf 2001-09-25 Written.
;     T. Metcalf 2001-09-26 Improved the way the display windows work.  If the
;                           window is too small, it will appear the that 
;                           alignment is wrong even though it is right.  So,
;                           make sure that the windows are always large
;                           enough.
;     T. Metcalf 2001-09-28 Added tmask and rmask keywords.
;     T. Metcalf 2001-09-29 Reduced the initial size of the scale variations
;                           in the minimization procedure.  This should
;                           prevent the minimization from getting confused.
;                           Changed cross correlation to normalized cross 
;                           correlation.  This is important since the 
;                           normalized cross correlation is to some extent
;                           independent of shifts and scales.
;     T. Metcalf 2001-10-01 Use cubic convolution interpolation in the calls
;                           to poly_2d instead of bilinear interpolation.
;                           It's a bit more robust and the routine is already
;                           slow anyway, so what the heck.
;     T. Metcalf 2001-10-02 Added a test for a warped initial guess.
;     T. Metcalf 2001-10-04 Allow higher order transformations when
;                           /unrestricted is set.
;     T. Metcalf 2001-10-10 Added a check for NaN in the cross-correlation.
;                           Added option to use the amoeba minimization 
;                           algorithm rather than the powell algortithm and
;                           made amoeba the default since it is thought to be
;                           more robust. 
;     T. Metcalf 2001-10-11 Check that amoeba converged.
;     T. Metcalf 2001-11-08 Changed the contours drawn so that they will scale
;                           between the minimum and maximum values rather than
;                           between 0 and the maximum.  Added the "levels"
;                           keyword.  These are minor changes.
;     T. Metcalf 2001-11-14 Added a check to make sure the transformation does
;                           not get too far from the initial guess.  This 
;                           prevents the maximization of the cross correlation
;                           from converging to an artifically inflated (and
;                           incorrect) maximum which includes only a few 
;                           points from the transformed image.
;     T. Metcalf 2001-11-19 Changed the default minimization algorithm to
;                           Powell.   It is considerably slower, but some
;                           tests on TRACE WL data show it to give a better
;                           answer than amoeba.  Added ftol keyword.  Improved
;                           the weighting scheme.
;     T. Metcalf 2002-03-30 There is a bug in some versions of IDL in which
;                           the missing keyword passed to poly_2d does not
;                           work correctly.  Added a work around for this.
;                           Also forced the images to be float or
;                           double.
;     T. Metcalf 2005-03-14 Added dfpmin option. Also added the
;                           conjugate gradient minimization, but it is
;                           very slow so not too useful. 
;     T. Metcalf 2005-04-12 Use amoebax instead of amoeba.
;-

common auto_align_images_private,rimage,timage, $
                                 npq,rnx,rny,nxt,nyt, $
                                 restricted,quiet,xmask,ymask,rmask, $
                                 bestpq,bestcorr,noplot,levels, $
                                 ngoodtest,marray,noscale
common auto_align_images_dfpmin, use_dfp, dfpscale
common auto_align_images_grad_dfpmin, delta

if keyword_set(unrestrictedin) then restricted=0 else restricted=1
if keyword_set(quietin) then quiet=1 else quiet=0
if keyword_set(noplotin) then noplot=1 else noplot=0
if keyword_set(rmaskin) then rmask = rmaskin else rmask = -1
if keyword_set(sscalein) then sscale = float(abs(sscalein)) else sscale = 1.0
if keyword_set(levelsin) then levels = float(levelsin) $
else levels=[0.1,0.25,0.50,0.75,0.9]
if keyword_set(noscalein) then noscale=1 else noscale=0


use_amoeba = 0
use_powell = 0
use_dfpmin = 0
use_conjgr = 0
if NOT keyword_set(powellin) AND NOT keyword_set(amoebain) then $
   use_powell = 1
if keyword_set(powellin) AND keyword_set(amoebain) then $
   use_powell = 1
if NOT keyword_set(powellin) AND keyword_set(amoebain) then $
   use_amoeba = 1
if keyword_set(powellin) AND NOT keyword_set(amoebain) then $
   use_powell = 1
if keyword_set(dfpminin) then begin
   use_dfpmin = 1
   use_powell = 0 
   use_amoeba = 0
   use_conjgr = 0
endif
if keyword_set(conjgradin) then begin
   use_dfpmin = 0
   use_powell = 0 
   use_amoeba = 0
   use_conjgr = 1
endif
if use_amoeba EQ 0 AND use_powell EQ 0 and use_dfpmin EQ 0 and use_conjgr EQ 0 then $
   message,'Stupid programming error'
if NOT quiet then begin
   if use_amoeba then message,/info,'Using the amoeba algorithm.'
   if use_powell then message,/info,'Using the Powell algorithm.'
   if use_dfpmin then message,/info,'Using the dfpmin algorithm.'
   if use_conjgr then message,/info,'Using the conj grad  algorithm.'
endif

if keyword_set(itmaxin) then begin
   itmax=long(itmaxin)
endif else begin
   if use_powell then itmax=200L
   if use_amoeba then itmax=10000L
   if use_dfpmin then itmax = 400L
endelse
use_dfp = 0  ; passed in common block to turn on/off dfp scaling

torderp = sqrt(float(n_elements(pin)))-1.0  ; Transformation order
torderq = sqrt(float(n_elements(qin)))-1.0
if torderp LT 1.0 OR $
   torderq LT 1.0 OR $
   torderp MOD 1.0 NE 0.0 OR $
   torderq MOD 1.0 NE 0.0 OR $
   torderp NE torderq then $
   message,'ERROR: pin and/or qin has an invalid dimension.  They must have'+$
           ' dimension (N+1)^2, where N is the order of the transformation'+$
           ' (N >= 1), and they must both have the same dimension.'
torder = torderp

if NOT quiet AND NOT noplot then begin
   tvlct,/get,r,g,b
   rsave = r & gsave=g & bsave=b
   r[!d.table_size-1] = 255
   g[!d.table_size-1] = 0
   b[!d.table_size-1] = 0
   tvlct,r,g,b
endif

rnx = n_elements(rimagein[*,0])
rny = n_elements(rimagein[0,*])
nxt = n_elements(timagein[*,0])
nyt = n_elements(timagein[0,*])

marray = make_array(nxt,nyt,/int,value=1)

; In case xmask & ymask are left in the common block from a previous call
xmask = -1 & ymask = -1

if keyword_set(tmask) then begin  ;set up the transformed image mask
   if tmask[0] GE 0 then begin
      xmask = tmask MOD nxt
      ymask = tmask / nxt
   endif
endif

if keyword_set(double) OR $
   strupcase(size(rimagein,/tname)) EQ 'DOUBLE' OR $
   strupcase(size(timagein,/tname)) EQ 'DOUBLE' then begin
   rimage = double(rimagein)
   timage = double(timagein)
endif else begin
   rimage = float(rimagein)
   timage = float(timagein)
endelse

; The restricted transformation includes only rotation, scale, and shift.
; The unrestricted transformation also includes warping.
if restricted then begin
   if n_elements(pin) NE 4 or n_elements(qin) NE 4 then $
      message,/info,'You passed in an initital transformation with order '+$
                    'higher than 1 but did not set the /unrestricted '+$
                    'keyword.  The transformation is being limited to linear.'
   if NOT quiet then begin
      print
      print,'Initial guess:'
   endif
   ; Get the initial guess for rotation, shift, and scale.
   ; enrss gives the "leftover" warping in the initial guess
   ; that is neglected.
   ;
   ; If the input is not a 4-element vector we have to extract the part of the
   ; input which corresponds to the linear transformation: spin,sqin.
   ; To get the linear part, make the array square, extract the lowest 
   ; portion, and then make it a 1-d array again.  This looks complex, but it
   ; just keeps the indices straight.
   spin = reform((reform(pin,long(sqrt(n_elements(pin))+0.5), $
                            long(sqrt(n_elements(pin))+0.5)))[0:1,0:1],4)
   sqin = reform((reform(qin,long(sqrt(n_elements(qin))+0.5), $
                            long(sqrt(n_elements(qin))+0.5)))[0:1,0:1],4)
   ; Now get the rotation, shift, and scale for the initial guess
   pq2rss,spin,sqin,erot,exscl,eyscl,exshft,eyshft,enrss,nxt,nyt, $
          /center, $
          quiet=quiet
   if max(abs((pq2pp(enrss))[*,1,*])) GE 0.1 then begin
      message,/info,'WARNING: There is significant warping in your initial '+ $
                    'guess.  The warping is being ignored since you have ' + $
                    'not selected the /unrestriced keyword.  The alignment' + $
                    ' may be off.  If the warping is real, use the ' + $
                    '/unrestricted keyword.  If the warping is just an ' + $
                    'artifact of your initial guess, and if you used ' + $
                    'caltrans to get p and q, pass a type "s" transtype ' + $
                    '*structure* to caltrans (see caltrans documentaiton).'
   endif
   pq = [exscl,eyscl,exshft,eyshft,erot]
   npq = -1
endif else begin
   npq = n_elements(pin)
   if n_elements(qin) NE npq then $
      message,'p and q must have the same dimension'
   pq = [reform(pin,npq),reform(qin,npq)]
endelse
npq2 = n_elements(pq)

bestcorr = 0.0
bestcorr = auto_align_images_func(pq,/init)
bestpq = pq

if NOT quiet then begin
   print
   print,'Initial correlation coefficient: ',-bestcorr
endif

; weight sets the scales such that higher order terms have a smaller
; range.

weight = 5^(float(lindgen(torder+1,torder+1) MOD long(torder+1)) + $ ;# of x-terms
            float(lindgen(torder+1,torder+1) / long(torder+1)))      ;# of y-terms

if use_dfpmin OR use_conjgr then use_dfp = 1 else use_dfp = 0 ; set up common block

if keyword_set(use_conjgr) then begin
   ;if NOT quiet then message,/info,'ConjGrad'

   if keyword_set(double) then ctol = 1.0e-8 > ((machar(/double)).eps*2.0) $
   else ctol = 1.0e-6 > ((machar()).eps*2.0)
   if keyword_set(ftol) then ctol = ftol > ((machar(double=double)).eps*2.0)

   pqdfp = pq
   dfpscale = 1.0
   rnew = 0.0
   irepeat = 0L
   REPEAT begin
      pqdfp = pqdfp * dfpscale
      dfpscale = abs(pqdfp*sscale)

      bad = where(dfpscale eq 0)
      if bad[0] GE 0 then dfpscale[bad] = 1.0

      pqdfp = pqdfp / dfpscale

      delta = (0.5d0 / ((double(irepeat)+1.0d0)^2.0d0)) > sqrt((machar(/double)).eps)
      rlast = rnew
      i = 0
      REPEAT begin
         if i eq 0 then initialize = 1 else initialize = 0
         minf_conj_grad,pqdfp,mincorr,conv,func_name='auto_align_images_conj_grad', $
                        initialize=initialize,use_deriv=1,tolerance=ctol
         converged = conv LE 0.0
         i = i + 1
      endrep UNTIL converged or i GT 1000
      rnew = mincorr
      irepeat = irepeat + 1L
      ;if NOT keyword_set(quiet) then print,'conj grad',irepeat,rnew,delta,!stime
      done = (abs(rnew-rlast) LE ctol AND irepeat GT 2) OR irepeat GE 10
   endrep UNTIL done
   pq = pqdfp*dfpscale
endif

if keyword_set(use_dfpmin) then begin
   ;if NOT quiet then message,/info,'DFPmin'

   if keyword_set(double) then dtol = 1.0e-9 > ((machar(/double)).eps*2.0) $
   else dtol = 1.0e-7 > ((machar()).eps*2.0)
   if keyword_set(ftol) then dtol = ftol > ((machar(double=double)).eps*2.0)

   pqdfp = pq
   dfpscale = 1.0
   rnew = 0.0
   i = 0L
   REPEAT begin
      pqdfp = pqdfp * dfpscale
      dfpscale = abs(pqdfp*sscale)

      bad = where(dfpscale eq 0)
      if bad[0] GE 0 then dfpscale[bad] = 1.0

      pqdfp = pqdfp / dfpscale

      rlast = rnew
      ; delta is the gradient scale, must be double precision.
      ; start delta large to pick up large scale features and
      ; then reduce the scale of the gradient as we go along.
      delta = (0.5d0 / ((double(i)+1.0d0)^2.0d0)) > sqrt((machar(/double)).eps) 
      ; Sometimes DFPMIN crashes so we need to trap this.
      catch,error_status
      if error_status NE 0 then begin
         if NOT keyword_set(quiet) then $
            message,/info,'WARNING: Error in DFPMIN, skipping this iteration: '+ $
                          !error_state.msg
      endif else begin
         dfpmin,pqdfp,dtol,mincorr, $
                'auto_align_images_func', $
                'auto_align_images_grad',double=double, $
                eps=2.0*(machar(double=double)).eps, $
                iter=niter,itmax=itmax,tolx=dtol,stepmax=stepmax
      endelse
      catch,/cancel
      rnew = mincorr
      i = i + 1L
      ;if NOT keyword_set(quiet) then print,'dfpmin',i,rnew,delta,' ',!stime
      done = (abs(rnew-rlast) LE dtol) OR (i GE 15)
   endrep UNTIL done
   pq = pqdfp*dfpscale
   if niter GE itmax then $
      message,/info,'WARNING: DFPmin failed to fully converge'
endif

if keyword_set(use_powell) then begin
   ;if NOT quiet then message,/info,'Powell'

   xi = fltarr(npq2,npq2)
   for i=0L,npq2-1L do xi[i,i]=sscale   ; unit vectors for Powell minimization

   ; The initial changes in the parameters are OK for shift and rotation
   ; since a 1 pixel shift or a 1 degree rotation is a good starting change.
   ; But changing the scaling by 100% is not good.  So, reduce the size of
   ; this initial change.

   if restricted then begin
      xi[0,0] = xi[0,0]*0.1  ; Size of x scale changes
      xi[1,1] = xi[1,1]*0.1  ; Size of y scale changes
      ; added MKM 150212
      ; if we are asking for no scaling, then we do not want to scale!
      if noscale then begin
         xi[0,0] = 0.000
         xi[1,1] = 0.000
         message,"Not scaling!",/info
      endif; noscale
   endif else begin
      for i=0L,npq2-1L do xi[i,i]=xi[i,i]/weight[i MOD (torder+1)^2]
   endelse

   ; Now do the minimization to find the best alignment

   if keyword_set(double) then ptol = 1.0e-8 > ((machar(/double)).eps*2.0) $
   else ptol = 1.0e-4 > ((machar()).eps*2.0)
   if keyword_set(ftol) then ptol = ftol > ((machar(double=double)).eps*2.0)
   powell,pq,xi,ptol,mincorr,'auto_align_images_func', $
          iter=niter,itmax=itmax,double=double
   if niter GE itmax then $
      message,/info,'WARNING: Powell failed to fully converge'
endif

if keyword_set(use_amoeba) then begin
   ;if NOT quiet then message,/info,'Amoeba'
   scale =  fltarr(npq2) + sscale
   if restricted then begin
      scale[0] = scale[0]*0.1  ; Size of x scale changes
      scale[1] = scale[1]*0.1  ; Size of y scale changes
   endif else begin
      for i=0L,npq2-1L do scale[i]=scale[i]/weight[i MOD (torder+1)^2]
   endelse
   if keyword_set(double) then begin
      pq = double(pq)
      scale = double(scale)
   endif
   if keyword_set(double) then atol = 1.0e-8 > ((machar(/double)).eps*2.0) $
   else atol = 1.0e-6 > ((machar()).eps*2.0)
   if keyword_set(ftol) then atol = ftol > ((machar(double=double)).eps*2.0)
   pq = amoebax(atol,atol,function_name='auto_align_images_func', $
                function_value=mincorr, $
                ncalls=ncalls, $
                nmax=itmax, $
                p0=pq, $
                scale=scale)
   if n_elements(pq) EQ 1 OR ncalls GE itmax then begin
      if NOT quiet then message,/info,'WARNING: AMOEBA failed to fully converge.'
      pq = bestpq
      mincorr = bestcorr
   endif
   mincorr = mincorr[0]
endif

if NOT quiet then begin
   print,'Final correlation coefficient: ',-mincorr
   print
endif

; Get the final transformation for output
print,pq
if restricted then begin
   ;pq is [exscl,eyscl,exshft,eyshft,erot]
   tr = rss2pq(nxt,nyt,xshift=pq[2],yshift=pq[3],rot12=pq[4], $
               xscale=pq[0],yscale=pq[1],/center,p=p,q=q)
endif else begin
   p = pq[0L:npq-1L]
   q = pq[npq:*]
endelse

; The transformed image
itest = poly_2d(timage,p,q,2,rnx,rny,cubic=-0.5,missing=missval)

if NOT quiet AND NOT noplot then begin  ; Optionally display the alignment
   binx = fix(!d.x_size/rnx)>1
   biny = fix(!d.y_size/rny)>1
   bin = binx < biny
   if !d.x_size LT rnx*bin OR !d.y_size LT rny*bin then begin
      wdef,ws,rnx*bin,rny*bin
   endif
   erase
   tv,bytscl(rebin(rimage,rnx*bin,rny*bin,/sample),top=!d.table_size-2)
   clevels = min(itest)+(max(itest)-min(itest))*levels
   ocontour2,itest,level=clevels,c_color=!d.table_size-1,bin=bin
   tvlct,rsave,gsave,bsave
endif

; Optionally display rotation, shift, and scale
if restricted and NOT quiet then begin 
   print
   print,'Final parameters:'
   pq2rss,p,q,erot,exscl,eyscl,exshft,eyshft,enrss,nxt,nyt,/center
   print
end

pout = p
qout = q
return,itest

end

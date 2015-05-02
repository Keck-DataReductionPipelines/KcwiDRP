
function rss2pq,nx1,ny1,xscale=inxscale,yscale=inyscale, $
               xshift=inxshift,yshift=inyshift,rot12=inrot12, $
               center=center,p=p,q=q,anchor=anchor,rotfirst=rotfirst, $
               axshift=inxshifta,ayshift=inyshifta

;+
;NAME:
;     RSS2PQ
;PURPOSE:
;     Given a rotation, shift and scale change, returns the p and q 
;     variables used in poly_2d to warp images.
;CATEGORY:
;CALLING SEQUENCE:
;     transformation = rss2pq(xshift=xshift,yshift=yshift,rot12=rot12,
;                             xscale=xscale,yscale=yscale)
;INPUTS:
;OPTIONAL INPUT PARAMETERS:
;     nx = size of image to be transformed (used only if /center is set)
;     ny = size of image to be transformed (used only if /center is set)
;KEYWORD PARAMETERS
;     rot12 = rotation CCW in degrees (default = 0)
;     xshift = x shift in pixels (default = 0), units=reference pixels
;     yshift = y shift in pixels (default = 0), units=reference pixels
;     xscale = x scale relative to reference scale (default = 1)
;     yscale = y scale relative to reference scale (default = 1)
;     anchor = [x0,y0] defines the center of rotation (units=reference pixels)
;     /center = rotate about the center of the image instead of the lower left
;               corner (only affects xhift and yshift).
;               Same as anchor=[(nx-1)/2.,(ny-1)/2.]
;     /rotfirst = rotate,shift,scale rather than the default
;                   shift,rotate,scale.  This is the behavior the routine
;                   originally had.
;     axshift = shift applied after rotation (rotfirst=0) or before
;               rotation (rotfirst=1), in addition to the shift
;               applied on the other side of the rotation with xshift.
;     ayshift = shift applied after rotation (rotfirst=0) or before
;               rotation (rotfirst=1), in addition to the shift
;               applied on the other side of the rotation with yshift.
;     p = output variable for P
;     q = output variable for Q
;OUTPUTS:
;     transformation = matrix with the same format as the output from 
;                      caltrans.pro
;                      t(*,0) = p
;                      t(*,1) = q
;COMMON BLOCKS:
;SIDE EFFECTS:
;RESTRICTIONS:
;     Using /shiftfirst will make your life a lot easier.
;PROCEDURE:
;     The rotation anchor is in 
;     the original coordinate system and the shifts are in
;     units of the original image.
;
;     First shifts, then rotates, then scales.  Pass the output from RSS2PQ
;     through pq2pp.pro if you want output that mimics setpts rather than 
;     output that mimics caltrans.
;MODIFICATION HISTORY:
;     Written by T. Metcalf 12-Aug-93
;     1994-11-10 TRM Added anchor keyword.
;     1995-05-19 TRM Changed default from rotate-shift-scale to 
;                    shift-rotate-scale.  Use the /rotfirst keyword to get
;                    the old behavior.
;     2005-01-24 TRM Added axshift and ayshift keywords.  The anchor
;                    for /center was defined as [nx/2,ny/2].  Now it
;                    is [(nx-1)/2,(ny-1)/2].
;-

;message,/info,'WARNING: Default behavior of rss2pq changed on 1995-05-19'

; Rotation is CW
; scale is scale1/scale2

if n_elements(inxscale) LE 0 then xscale=1.d0 else xscale = double(inxscale)
if n_elements(inyscale) LE 0 then yscale=1.d0 else yscale = double(inyscale)
if n_elements(inxshift) LE 0 then xshift=0.d0 else xshift = double(inxshift)
if n_elements(inyshift) LE 0 then yshift=0.d0 else yshift = double(inyshift)
if n_elements(inxshifta) LE 0 then xshifta=0.d0 else xshifta = double(inxshifta)
if n_elements(inyshifta) LE 0 then yshifta=0.d0 else yshifta = double(inyshifta)
if n_elements(inrot12) LE 0 then rot12=0.d0 else rot12 = double(inrot12)

nanchor = n_elements(anchor)
if nanchor EQ 2 OR keyword_set(center) then begin
  if nanchor EQ 2 then begin
     nx12 = double(anchor(0))
     ny12 = double(anchor(1))
  endif else begin
     nx12 = (nx1-1.)/2.d0
     ny12 = (ny1-1.)/2.d0
  endelse
  halfshft1 = [ [1.d0,0.d0,-nx12], $
                [0.d0,1.d0,-ny12], $
                [0.d0,0.d0,1.d0]]
  halfshft2 = [ [1.d0,0.d0,nx12], $
                [0.d0,1.d0,ny12], $
                [0.d0,0.d0,1.d0]]
endif

dtor = !dpi/180

ct = cos(-rot12*dtor)
st = sin(-rot12*dtor)

if keyword_set(center) OR nanchor EQ 2 then $
   rtn = halfshft1 # [ [ct,-st,0.d0], $
                       [st,ct,0.d0], $
                       [0.d0,0.d0,1.d0]] # halfshft2 $
else $
   rtn = [ [ct,-st,0.d0], $
           [st,ct,0.d0], $
           [0.d0,0.d0,1.d0]]

shft = [ [1.d0,0.d0,-xshift], $
         [0.d0,1.d0,-yshift], $
         [0.d0,0.d0,1.d0]]

scl = [ [1.d0/xscale, 0.d0, 0.d0], $
        [0.d0, 1.d0/yscale, 0], $
        [0.d0, 0.d0, 1.d0 ]]

shfta = [ [1.d0,0.d0,-xshifta], $
          [0.d0,1.d0,-yshifta], $
          [0.d0,0.d0,1.d0]]

if keyword_set(rotfirst) then tran = shfta # rtn # shft # scl $
else tran = shft # rtn # shfta # scl

;tran = scl # rtn # shft
;tran = (rtn # scl) # shft
;tran = shft # scl # rtn
;tran = rtn # (scl # shft)

p = [tran(2,0)*xscale,tran(1,0),tran(0,0),0.]
q = [tran(2,1)*yscale,tran(1,1),tran(0,1),0.]

answer = dblarr(4,2)
answer(*,0)=p
answer(*,1)=q

return,answer

end

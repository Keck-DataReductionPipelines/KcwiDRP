
;+
; NAME:
;        CALTRANS
; PURPOSE:
;         Calculates a linear transform to map one image onto another.
; CATEGORY:
; CALLING SEQUENCE:
;        m = caltrans(p)
; INPUTS:
;        p = float(2,2,n) Set of reference point coordinates in both images.
;            n : number of points, n >= 3 (n>=1 for transtype = s).
;            p(0,0,*), p(1,0,*) : x and y coord. of points in reference image.
;            p(0,1,*), p(1,1,*) : x and y coord. of points in image to be
;                                 transformed.
;            p can be created conveniently with the setpts procedure.
; KEYWORDS (INPUT):
;        transtype : can be either string, float element, or structure:
;        transtype = string.  Type of transformation, default = 'g'.
;                    'g' = general linear transformation.
;                    'i' = isotropic expansion, rotation, and shift.
;                          (no transposition, and image and reference
;                          must have square pixels)
;                    's' = shift only.
;        transtype = float.  pixel aspect ratio of image (ypixsize/xpixsize).
;                            Equivalent to transtype='i', but with non-square
;                            image pixels (ref. pixels must still be square).
;                            Use -ypix/xpix if the image is transposed.
;        transtype = structure.  Allows very flexible constrained transf.
;                                The structure must have the following tags:
;                 .type = string ('g','i',or 's').  Type of transformation.
;                 .pix_x = float. image pixel size in x
;                 .pix_y = float. image pixel size in y
;                 .ref_x = float. reference pixel size in x
;                 .ref_y = float. reference pixel size in y
;                 .transpose = int.  0 or 1, if 1, image is transposed.
;                 .phi   = float. angle of image_y measured counterclockwise
;                          from ref_y (c.c.wise = pos. towards -ref_x)
;                 Note: if .type = 'g' then all other tags are ignored.
;                       if .type = 'i' then only the ratios .pix_y/.pix_x,
;                                      .ref_y/.ref_x and .transpose are used.
;                       if .type = 's' then all tags are used.  The pixel
;                                      sizes can be in any (linear) system.
;        residuals = int. If present and equal to 1: residuals are printed.
; OUTPUTS:
;        m = float(4,2).  Matrix elements of the linear transform.
;            m(*,0) and m(*,1) can directly be used in poly_2d. Refer to
;            poly_2d for details.
; COMMON BLOCKS:
;        None.
; SIDE EFFECTS:
;        None.
; RESTRICTIONS:
; PROCEDURE:
;        Caltrans performs a constrained linear transform. It allows for
;        image translation, rotation and stretching in x and y.  Because
;        it is a constrained transform, the cross terms m(3,*) are always 0.
;        The program makes a least square fit if more than 3 points are given.   ; MODIFICATION HISTORY:
;        JPW, Nov. 1989
;        JPW, Nov. 1994 implemented some of ANM's upgrades:
;                       uses svbksb, and implements transtype='i'
;        JPW, Nov. 1994 added structure and float options for transtype.
;-

function caltrans,p,transtype=key0,residuals=rflg

sp = size(p)
if (sp(0) lt 2) or (sp(0) gt 3) or (sp(1) ne 2) or (sp(2) ne 2) then  begin
   print,'point array has invalid dimensions '
   goto,done
endif
if sp(0) eq 2 then npp = 1 else npp = sp(3)

if n_elements(key0) eq 0 then key0 = 'g'
keytyp = size(key0)
case keytyp(keytyp(0)+1) of
  7 : begin		; string
       key = key0
       ; default parameters for constrained transformations
       par1 = 1.0	; pixel aspect ratio of image (ysize/xsize)
       par0 = 1.0	; pixel aspect ratio of reference (ysize/xsize)
       rpix = 1.0	; ratio of image scales in y (im_ysize/ref_ysize)
       phi  = 0.0	; rotation angle: y-direction of image measured
                        ; counterclockwise from y-direction of reference
                        ; (in radian).  (c.c.wise = from y_ref towards -x_ref).
       ; Note1: transposition is forced with negative values of par1.
       ; Note2: Only par0,par1 are used for transtype 'i',
       ; none are used for type 'g'.
      end
  8 : begin		; structure
       key = key0.type
       if  key ne 'g' then begin
          par1 = float(key0.pix_y/key0.pix_x)
          par0 = float(key0.ref_y/key0.ref_x)
          if keyword_set(key0.transpose) then par1 = -par1
          rpix = float(key0.pix_y/key0.ref_y)
          phi = key0.phi
       endif
      end
  else : begin		; type 'i' with non-square image pixels
       key = 'i'
       par1 = float(key0)
       par0 = 1.0
      end
endcase

case key of
 'g': begin             ; general linear transformation

     ; x_tr = c(0) + c(1)*y_ref + c(2)*x_ref
     ; y_tr = d(0) + d(1)*y_ref + d(2)*x_ref

      if npp lt 3 then begin
         print,'not enough data points'
         goto,done
      endif
      z = fltarr(npp,3)
      z(*,0) = 1.0
      z(*,1) = p(1,0,*)
      z(*,2) = p(0,0,*)
      u = reform(p(0,1,*))
      v = reform(p(1,1,*))
; use singular value decomposition to solve system
      svd,z,zww,zu,zv
      svbksb,zu,zww,zv,u,c
      svbksb,zu,zww,zv,v,d
; calculate residuals
      resu = u - (z # c)
      resv = v - (z # d)
     end

 'i': begin             ; isotropic linear transformation

     ; x_tr = cd(0) +             cd(1)*y_ref +                 cd(2)*x_ref
     ; y_tr = cd(3) + (par0/par1)*cd(2)*y_ref - (1/(par0*par1))*cd(1)*x_ref

      if npp lt 2 then begin
         print,'not enough data points'
         goto,done
      endif
      z = fltarr(npp*2,4)
      z(0:npp-1,0) = 1.0
      z(0:npp-1,1) = p(1,0,*)
      z(0:npp-1,2) = p(0,0,*)
      z(npp:npp*2-1,1) = (-1.0/(par0*par1)) * p(0,0,*)
      z(npp:npp*2-1,2) = (par0/par1) * p(1,0,*)
      z(npp:npp*2-1,3) = 1.0
      u = [reform(p(0,1,*)),reform(p(1,1,*))]
     ; use singular value decomposition to solve system
      svd,z,zww,zu,zv
      svbksb,zu,zww,zv,u,cd
      c = cd(0:2)
      d = [cd(3),(par0/par1)*cd(2),(-1.0/(par0*par1))*cd(1)]
     ; calculate residuals
      res = u - (z # cd)
      resu = res(0:npp-1)
      resv = res(npp:npp*2-1)
     end

 's': begin             ; shift only

     ; x_tr = cc + sin(phi)*(par1/rpix)        * y_ref
     ;           + cos(phi)*(par1/(par0*rpix)) * x_ref
     ; y_tr = dd + cos(phi)*(1.0/rpix)         * y_ref
     ;           - sin(phi)*(1.0/(par0*rpix))  * x_ref

      u = reform(p(0,1,*))
      v = reform(p(1,1,*))
      zu =   sin(phi)*(par1/rpix)       *reform(p(1,0,*)) $
           + cos(phi)*(par1/(par0*rpix))*reform(p(0,0,*))
      zv =   cos(phi)*(1.0/rpix)        *reform(p(1,0,*)) $
           - sin(phi)*(1.0/(par0*rpix)) *reform(p(0,0,*))
      cc = total(u - zu)/npp
      dd = total(v - zv)/npp
      c = [cc, sin(phi)*par1/rpix, cos(phi)*par1/(par0*rpix)]
      d = [dd, cos(phi)/rpix, -sin(phi)/(par0*rpix)]
     ; calculate residuals
      resu = u - (zu + cc)
      resv = v - (zv + dd)
     end

 else: print,'invalid calculation type requested '
endcase

m = [[c,0.0],[d,0.0]]

; print residuals if residuals flag present = 1
if n_elements(rflg) eq 1 then if rflg eq 1 then begin
   print,'the residuals are:
   print,'   in x: '
   print,resu
   print,'   in y: '
   print,resv
endif

done: return,m
end

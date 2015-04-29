
function pq2pp,pin,qin,x=xin,y=yin

;+
;NAME:
;     PQ2PP
;PURPOSE:
;     Converts the P and Q variables used in poly2d into a pp matrix which
;     simulates the output from setpts.pro.  This is the inverse of caltrans.
;CATEGORY:
;CALLING SEQUENCE:
;     pp = pq2pp(p,q)  OR
;     pp = pq2pp(t)
;INPUTS:
;     p = p matrix used by poly2d
;     q = q matrix used by poly2d
;     t = transformation matrix from caltrans.pro
;OPTIONAL INPUT PARAMETERS:
;KEYWORD PARAMETERS
;     x = x values to use in the transformation (optional)
;     y = y values to use in the transformation (optional)
;OUTPUTS:
;     pp = fltarr(2,2,*)  (see setpts.pro)
;COMMON BLOCKS:
;SIDE EFFECTS:
;RESTRICTIONS:
;     Only works for linear transformations.
;
;     If x and y are specified they must have the same dimension.
;PROCEDURE:
;MODIFICATION HISTORY:
;     Written by T. Metcalf 12-Aug-93
;-


   if n_params() EQ 1 then begin
      q = pin(*,1)
      p = pin(*,0)
   endif $
   else begin
      p = pin
      q = qin
   endelse

   if n_elements(p) NE 4 or n_elements(q) NE 4 then $
      message,'Input has wrong dimension'

   if n_elements(xin) LT 1 or n_elements(yin) LT 1 or $
      n_elements(xin) NE n_elements(yin) then begin
      np = 4
      x = findgen(np)
      y = x^2
   endif $
   else begin
      x = float(xin)
      y = float(yin)
      np = n_elements(x)
   endelse

   pp = fltarr(2,2,np)

   xy = fltarr(np,4)
   xy(*,0) = 1.
   xy(*,1) = y
   xy(*,2) = x
   xy(*,3) = x*y

   pp(0,0,*) = x    ; x in reference image
   pp(1,0,*) = y    ; y in reference image

   pp(0,1,*) = xy#p   ; x in transformed image
   pp(1,1,*) = xy#q   ; y in transformed image

   return,pp

end

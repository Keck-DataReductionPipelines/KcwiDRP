;+
; NAME:
;  fulljoin
; PURPOSE: 
;  Create the full join, or Cartesian  product of two sets.
; DESCRIPTION:
; a and b are two vectors of identical type.
; a matrix c is returned where c[0,*] is values of a in order
;                              c[1,*] is values of b in order
; 

; CATEGORY:
;  Set Manipulation
; CALLING SEQUENCE:
;
;  fulljoin( a,b)
;
; INPUTS:
;  a -       1-D array
;  b -       1-D array
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; NOUNIQ -   Flag, if set, does a straight join across a and b as is.
;            Otherwise the a and b arrays are sorted into uniq values.
; PRESERVE-  Flag, if set, and NOUNIQ not set, unique values for a and b
;            will be used but they will be in their original order, ie,
;            for each unique value, the order of its first appearance.
; OUTPUTS:
; Returns a product matrix  of dimension 2 X (m * n).
; of the form  [ [a0,b0], [a0, b1], [a0,b2]... [a1,b0],... [an,bn-1], [an.bn]]
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;       2006/08/16, Written by Peter L. Collins, Lowell Observatory
;-

function fulljoin, a,b, NOUNIQ=nouniq,PRESERVE=preserve

   if not keyword_set(nouniq) then begin
      if not keyword_set(preserve) then begin
         a_set = a[uniq(a, sort(a))]
         b_set = b[uniq(b, sort(b))]
      endif else begin
         ai_set = uniq(a, sort(a))
         a_set = a[ ai_set[sort(ai_set)]]
         bi_set = uniq(b, sort(b))
         b_set = b[ bi_set[sort(bi_set)]]
      endelse
   endif else begin
      a_set = a
      b_set = b
   endelse

   c_set = replicate(a[0], 2,n_elements(a_set)*n_elements(b_set))

   aindex = 0
   for i=0, n_elements(a_set)-1 do begin
      c_set[0,aindex:aindex+n_elements(b_set)-1] = a_set[i]
      c_set[1,aindex:aindex+n_elements(b_set)-1] = b_set[*]
      aindex += n_elements(b_set)
   endfor
   
   return,c_set
end


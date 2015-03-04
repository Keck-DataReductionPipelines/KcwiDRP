;+
; NAME: 
;  intrsect
; PURPOSE: 
;  Find the intersection or its inverse between two arrays.
; DESCRIPTION:
; CATEGORY:
;  Set Manipulation
; CALLING SEQUENCE:
;  intrsect,a,b,c,nfound, [NNOT]
; INPUTS:
;     a - First set.
;     b - Second set.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     NNOT - Flag, if true will invert the intersection operations, that is,
;            the entries that do NOT appear in both sets will be returned.
; OUTPUTS:
;     c      - Intersection (or NOT) of sets A and B.
;     nfound - Number of items in the intersectoin (or NOT).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  93/12/16 - Marc W. Buie, Lowell Observatory, original version.
;-
pro intrsect,a,b,c,nfound,NNOT=nnot

   ; Extract a unique list of entries in each set.
   uniq_a = a[uniq(a,sort(a))]
   uniq_b = b[uniq(b,sort(b))]

   num_a = n_elements(uniq_a)
   num_b = n_elements(uniq_b)

   ; This handles the case of a direct intersection
   if not keyword_set(nnot) then begin
      na = intarr(num_a)

      ; Count the number of times each element in a is seen in b, answer
      ;   will either be the index of its position in b or -1 if not found.
      for i=0,num_a-1 do na[i] = where(uniq_b eq uniq_a[i])

      ; Index where A had a match in B
      insect_a = where(na ne -1,count_a)

      if count_a gt 0 then begin
         c = uniq_a[insect_a]
         nfound = count_a
      endif else begin
         nfound = 0
      endelse

   ; This handles the opposite of an intersection.
   endif else begin
      na = intarr(num_a)
      nb = intarr(num_b)
      for i=0,num_a-1 do na[i] = where(uniq_b eq uniq_a[i])
      for i=0,num_b-1 do nb[i] = where(uniq_a eq uniq_b[i])
      insect_a = where(na eq -1,count_a)
      insect_b = where(nb eq -1,count_b)
      if count_a gt 0 and count_b gt 0 then begin
         c = [uniq_a[insect_a],uniq_b[insect_b]]
         c = c[uniq(c,sort(c))]
         nfound = n_elements(c)
      endif else if count_a gt 0 and count_b le 0 then begin
         c = uniq_a[insect_a]
         nfound = count_a
      endif else if count_a le 0 and count_b gt 0 then begin
         c = uniq_b[insect_b]
         nfound = count_b
      endif else begin
         nfound = 0
      endelse
   endelse

end

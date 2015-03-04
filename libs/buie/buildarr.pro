;+
; NAME:
;  buildarr
; PURPOSE:   (one line only)
;  Build up a master array by concatenation
; DESCRIPTION:
;  This routine does very little, really.  It just allows you to
;    concatenate vectors, one at a time while also handling the
;    first time when the output array doesn't yet exist.  This was
;    written to help clean up a common code construct that would look
;    this this:
;
;       nval=0
;       for i=0,nother do begin
;          generate some vectors, say a,b,c
;          if nval eq 0 then begin
;             aa=a
;             bb=b
;             cc=c
;          endif else begin
;             aa=[aa,a]
;             bb=[bb,b]
;             cc=[cc,c]
;          endelse
;          nval += n_elements(a) ; assuming a,b,c same length
;       endfor
;
;  With this routine this same action looks like:
;       nval=0
;       for i=0,nother do begin
;          generate some vectors, say a,b,c
;          buildarr,aa,a,nval eq 0
;          buildarr,bb,b,nval eq 0
;          buildarr,cc,c,nval eq 0
;          nval += n_elements(a) ; assuming a,b,c same length
;       endfor
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  buildarr,array,newarray,nval
; INPUTS:
;  array    - The is the master array that is to be built.  On input it
;               will either be empty or contain values already placed in
;               the array.
;  newarray - Value or values to add to the master array.
;  nval     - Number of valid values already in the master array.  All that
;               really matters here is that nval be set to zero on the first
;               time adding to the array and after that it's not zero.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  array - Is modified by adding new array to end.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2010/03/02, Written by Marc W. Buie, Southwest Research Institute
;-
pro buildarr,array,newarr,nval

   self='buildarr: '
   if badpar(array,[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],[0,1], $
              caller=self+'(array) ') then return
   if badpar(newarr,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],[0,1], $
              caller=self+'(newarray) ') then return
   if badpar(nval,[1,2,3],0,caller=self+'(nval) ') then return

   if nval eq 0 then begin
      array = newarr
   endif else begin
      array = [array,newarr]
   endelse

end

;+
; NAME:
;  trimrank
; PURPOSE:   (one line only)
;  Remove dimensions of 1 thus returning the lowest possible rank for input
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  retval = trimrank(input)
; INPUTS:
;  input = variable of any type or rank
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWRITE - Change is made in place (see reform.pro documentation)
; OUTPUTS:
;  return value is the input with all dimensions of no size removed.  This
;    is nearly identical to the IDL routine, reform when called with no
;    arguments.  The one difference this routine offers is it will convert a
;    one element vector to a scalar.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2009/11/12
;-
function trimrank,input,OVERWRITE=overwrite

   if keyword_set(overwrite) then begin
      input = reform(input,/overwrite)
      if n_elements(input) eq 1 then return,input[0] else return,input
   endif else begin
      outval = reform(input)
      if n_elements(outval) eq 1 then return,outval[0] else return,outval
   endelse

end

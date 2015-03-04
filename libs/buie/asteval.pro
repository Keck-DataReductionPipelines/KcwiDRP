;+
; NAME:
;  asteval
; PURPOSE:
;  Evaulate an astrometric polynomial function.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  val=asteval,ind,coeff,terms
; INPUTS:
;  x     - X - coordinate (scalar or vector)
;  y     - Y - coordinate (scalar or vector)
;  coeff - Coefficients of the transformation, this must be a vector that has
;              the terms to be used, the length must be equal to total(terms)
;  terms - Which terms to use, see documentation for ASTTERMS.PRO for the
;             expected content of this input variable.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return value - Dependent value(s), if x,y was 1-d then this will be scalar.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  1997/06/17, Written by Marc W. Buie, Lowell Observatory
;  2009/06/22, MWB, minor modification to preserve rank of the result.
;  2009/11/12, MWB, rework logic for returning data with proper rank
;-
function asteval,x,y,coeff,terms

   ind = astterms(x,y,terms)
   sz=size(ind)
   if sz[0] eq 1 then begin
      val = total(ind*coeff,/double)
   endif else begin
      val = ind##coeff
   endelse
   return,trimrank(val,/overwrite)

end


function ppinvert,pfpp

;+
;NAME:
;     PPINVERT
;PURPOSE:
;     Invert a "pp" matrix obtained from setpts.pro.  This converts the 
;     pp variable representing the transformation from A to B into a pp
;     variable representing the transformation from B to A.
;CATEGORY:
;CALLING SEQUENCE:
;     inverse = invertpp(pp)
;INPUTS:
;     pp = pp variable obtained from setpts.pro
;OPTIONAL INPUT PARAMETERS:
;KEYWORD PARAMETERS
;OUTPUTS:
;     inverse = inverted pp variable
;COMMON BLOCKS:
;SIDE EFFECTS:
;RESTRICTIONS:
;PROCEDURE:
;     Very straightforward but hard to remember so I put the 3 lines into
;     a function.
;MODIFICATION HISTORY:
;     29-Sep-93 Written by T. Metcalf
;-

invpfpp = make_array(size=size(pfpp))
invpfpp(*,0,*) = pfpp(*,1,*)
invpfpp(*,1,*) = pfpp(*,0,*)

return, invpfpp
 
end

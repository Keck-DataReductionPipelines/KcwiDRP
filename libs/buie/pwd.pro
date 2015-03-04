;+
; NAME:
;  pwd
; PURPOSE:
;  Print current working directory.
; DESCRIPTION:
;
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  pwd
; INPUTS:
;  None.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
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
;  1999 May 13, Written by Marc W. Buie, Lowell Observatory
;
;-
pro pwd
   cd,current=current
   print,current
end

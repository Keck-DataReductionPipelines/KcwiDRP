;+
; NAME:
;	negative
; PURPOSE: (one line)
;	Invert the current display lookup table.
; DESCRIPTION:
;	The current lookup table is fetched, rotated end-for-end, and put
;	re-loaded.
; CATEGORY:
;       Image display
; CALLING SEQUENCE:
;	negative
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	Modifies current lookup table.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1991 Oct 29.
;-
pro negative
   tvlct,r,g,b,/get
   r=rotate(r,2)
   g=rotate(g,2)
   b=rotate(b,2)
   tvlct,r,g,b
end

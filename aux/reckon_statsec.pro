;------------------------------------------------------------------------------
; $Id: reckon_statsec.pro,v 1.1 2013/05/16 23:48:07 neill Exp $
;
;+
; NAME:
;    reckon_statsec
;
; PURPOSE:
;   parse the statsec string in la_cosmic and return the indices
;   which bracket the section to be exaimed.
;
;
; CALLING SEQUENCE:
;     reckon_statsec,  statstring,arrsize
;
; INPUTS:
;   statstring:  The string such as "[23:34,*]"
;   arrsize   :  Array with the x and y sizes of the image
;
; OUTPUTS:
;   returns the indices of the statsec in the form [x1,x2,y1,y2]
;
;
; PROCEDURES CALLED:
;
; COMMENTS:
;   A good deal of error checking is done to ensure that
;   a statsection will be valid.
;
; NOTES:
; BUGS:
;
; REVISION HISTORY:
;   20-May-2001  Written by Joshua Bloom, Caltech (jsb@astro.caltech.edu)
;-
;------------------------------------------------------------------------------
function reckon_statsec, statstring,arrsize
;; must be of the form
;; X1:X2,Y1:Y2

tmp = size(statstring)

if (tmp[1] ne 7) then begin
    print, 'RECKON_STATSEC: Warning, statsec not valid, using full image'
    return, [0,arrsize[0]-1,0,arrsize[1]-1]
endif
tmp = strsplit(statstring,'[',/extract)
tmp = strsplit(tmp[0],']',/extract)

;; break up the string by the comma and evaluate
str = strsplit(tmp[0],',',/extract)
nstr = n_elements(str)
if (nstr ne 2) then begin
    print, 'RECKON_STATSEC: Warning, statsec not valid, using full image'
    return, [0,arrsize[0]-1,0,arrsize[1]-1]
endif
retarr = lonarr(4)
for i=0,1 do begin
    ; now look at each string and parse
    str1 = strsplit(str[i],':',/extract)
    nstr1 = n_elements(str1)
    if (nstr1 gt 2) then begin
        ;; malformed strsep
        retarr[i*2] = 0
        retarr[i*2 + 1] = arrsize[i] - 1
    endif
    if (nstr1 eq 1) then begin
        
        if (stregex(str1[0],'[*]',/boolean)) then begin
            ;; the user wants the entire image
            retarr[i*2] = 0
            retarr[i*2 + 1] = arrsize[i] - 1
        endif else begin
            ;; it's a number, so convert it 
            retarr[i*2] = long(str1[0])
            retarr[i*2 + 1] = long(str1[0])
        endelse
    endif else begin
        retarr[i*2] = long(str1[0])
        retarr[i*2 + 1] = long(str1[1])
    endelse
endfor

return, retarr
end

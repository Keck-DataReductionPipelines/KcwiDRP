;------------------------------------------------------------------------------
; $Id: lacos_replace.pro,v 1.1 2013/05/16 23:48:07 neill Exp $
;
;+
; NAME:
;   lacos_replace, arr, repval, low, high 
;
; PURPOSE:
;  replace pixels whose value are between low and high with value = repval
;  modelled after IRAF (!) IMREPLACE
;
; CALLING SEQUENCE:
;     lacos_replace, arr, repval, low, high
;
; INPUTS:
;   arr:       number array of any size or dimension.
;   repval     valid replacment value
;   low,high   bracket values to replace (can be 'INDEF') to replace all
;
; OUTPUTS:
;   returns the array = arr but with the replaced pixels
;
; PROCEDURES CALLED:
;
; COMMENTS:

; NOTES:
; BUGS:
;
; REVISION HISTORY:
;   20-May-2001  Written by Joshua Bloom, Caltech (jsb@astro.caltech.edu)
;-
;------------------------------------------------------------------------------
function lacos_replace, arr, repval, low, high
extr = 1d40
slow  = size(low)
shigh = size(high)

if (shigh[1] eq 7) then begin
    if (high ne 'INDEF') then begin
        print, 'LACOS_REPLACE: Sorry must call with INDEF not'
        print, high
        return, arr
    endif
    high = extr
endif

if (slow[1] eq 7) then begin
    if (low ne 'INDEF') then begin
        print, 'LACOS_REPLACE: Sorry must call with INDEF not'
        print, slow
        return, arr
    endif
    low = -1d0 * extr
endif

high = double(high)
low  = double(low)
bads = where((arr le high) and (arr ge low), n)
if (n ge 1) then arr[bads] = repval
return, arr
end

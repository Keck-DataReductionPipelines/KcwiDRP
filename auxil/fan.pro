;+
; NAME: 
;           FAN
;
; PURPOSE:
;           Take the outer product of the input ARRAY and a
;           UNIT_VECTOR to "fan out" a 1D vector into an array 
;           comprised of the vector repeated row-wise NFAN times.
;           Useful for array-wise mathematics (Look Ma, no FOR loops!)
;
; CALLING SEQUENCE:
;           result = fan(array [,nfan, /transpose])
;
; INPUTS:
;           ARRAY - 1D array, input vector
;           NFAN  - number of times to repeat the input vector,
;                   default is N_ELEMENTS(ARRAY)
;
; KEYWORD PARAMETERS:
;                     
;           TRANSPOSE - Repeat the input vector column-wise
;
; OUTPUTS:
;           A 2D array with N_ELEMENTS(ARRAY) columns and NFAN
;           rows.
;
; EXAMPLE:
;           Fan a FINDGEN of 3 elements, twice.
;
;           IDL> a = findgen(3)
;           IDL> print,fan(a,2)
;                 0.00000      1.00000      2.00000
;                 0.00000      1.00000      2.00000
;
; MODIFICATION HISTORY:
;           Created sometime in ought-2 by JohnJohn
; 06 Dec 2002 JohnJohn- Added some error handling at the beginning
;-
function fan,array,nfan,transpose=transpose
   on_error,2  ;if broke then return to sender
   if n_params() lt 1 then begin 
       message,'Syntax: f = fan(array [,nfan, /transpose])',/info
       return,-1
   endif

   if n_elements(nfan) eq 0 then nfan = n_elements(array)
   unit_vector = replicate(1d,nfan)   ;dblarr(nfan)+1.
   if keyword_set(transpose) then new = array##unit_vector $
     else new = unit_vector##array
   return,new
end


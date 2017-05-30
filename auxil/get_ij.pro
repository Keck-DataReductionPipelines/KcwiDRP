;+
; Project     : SOHO - CDS     
;                   
; Name        : GET_IJ()
;               
; Purpose     : Determine (i,j) indices from a 1-d index of values   
;               
; Explanation : 
;               
; Use         : ij=get_ij(v,nx)
;    
; Inputs      : v  = index to convert
;               nx = first dimension of 2-d array from which v was derived
;               
; Opt. Inputs : None
;               
; Outputs     : ij = [i,j] ; i,j indices of v. 
;               
; Opt. Outputs: None
;               
; Keywords    : None
;
; Calls       : MOD
;
; Common      : None
;               
; Restrictions: None
;               
; Side effects: None
;               
; Category    : Util, numerical
;               
; Prev. Hist. : None
;
; Written     : D M Zarro (ARC) Nov 1992  
;               
; Modified    : 
;
; Version     : Version 1, Nov-92
;-            

function get_ij,v,nx

on_error,1

if n_params() lt 2 then message,'syntax --> ij=get_ij(v,nx)'

return,transpose([[v mod long(nx)], [v/long(nx)]]) & end


      FUNCTION tenv,dd,mm,ss
;+
; NAME:
;	TENV()
; PURPOSE:
;	Converts sexigesimal number or vector to decimal.  
; EXPLANATION:
;	Like TEN() but allows vector input.
;
; CALLING SEQUENCES:
;	Result = TENV( dd, mm )           ; result = dd + mm/60.
;	Result = TENV( dd, mm, ss)        ; result = dd + mm/60. + ss/3600.
;
; INPUTS:
;	dd - Sexigesimal element(s) corresponding to hours or degrees
;	mm - Sexigesimal element(s) corresponding to minutes
;	ss - Sexigesimal element(s) corresponding to seconds (optional)
;		The input parameters can be scalars or vectors.   However, the
;		number of elements in each parameter must be the same.
;
; OUTPUTS:
;	Result -  double, decimal equivalent of input sexigesimal 
;		quantities.  Same number of elements as the input parameters.
;		If the nth element in any of the input parameters is negative 
;		then the nth element in Result will also be negative.
;
; EXAMPLE:
;	If dd = [60,60,0], and mm = [30,-30,-30], then
;
;	IDL> Result = TENV(dd,mm)  ====>   Result =  [60.5,-60.5,-0.5]
;
; WARNING: 
;       TENV() will recognize values of -0.0 as negative numbers.    However,
;        there is no distinction in the binary representation of -0 and 0 
;        (integer values), and so TENV will treat both values as positive.
; PROCEDURE:
;	Mostly involves checking arguments and setting the sign.
;
;   MODIFICATION HISTORY:
;	Written by W.B. Landsman           April, 1991
;       Recognize -0.0   W. Landsman/B. Stecklum   Dec 2005
;
;-
 compile_opt idl2
 On_error,2                                 ;Return to caller

 npts = N_elements(dd)
 npar = N_params()
 if npts EQ 0 then begin
     print,'Syntax -  RESULT = TENV( dd, mm, ss)'
     return, 0.0d
 endif

 if ( npar EQ 1 ) then return,double( dd )   ;No need to check for neg values.

 value = double( abs(dd) ) 

 if ( npar GT 1 ) then begin               ;Add minutes/60., check for <0

      if N_elements(mm) NE npts then $
           message,'ERROR - Number of elements in each parameter must be equal'
      nd=(strpos(string(dd),'-') ge 0)
      nm=(strpos(string(mm),'-') ge 0)
      neg =  nd OR nm
      value = value + abs(mm)/60.0d

 endif

 if ( npar GT 2 ) then begin               ;Add sec/3600., check for <0

      if N_elements(ss) NE npts then $
           message,'ERROR - Number of elements in each parameter must be equal'
      ns=(strpos(string(ss),'-') ge 0)
      neg = neg OR ns
      value = value + abs(ss)/3600.0d

 endif

 neg = where( neg, Nfound )                  ;Account for negative values
 if ( Nfound GT 0 ) then value[neg] = -value[neg]

 return,value      
 end

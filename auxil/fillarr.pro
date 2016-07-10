;+
; NAME: 
;       FILLARR 
;
;
; PURPOSE:
;       This function generates an array from MIN to MAX with
;       step size DEL. If an integer number of steps cannot be
;       fit between MIN and MAX, then MAX will be adjusted to
;       be as close as the specified maximum as possible.
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;       f = fillarr(n, min, max [,fan=, transfan=, /double])
;
;
; INPUTS:
;       DEL:  The desired step size
;       MIN:  The value of the first array element in F
;       MAX:  The value of the last array element in F if
;             (MAX-MIN)/DEL is an integer. Adjusted otherwise.
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
;       FANNNED:    Number of times the array is to be repeated.
;                   The final dimensions of F  will be 
;                   fix((MAX-MIN)/DEL) + 1 columns by FANNED ows.
;
;       /TRANSPOSE  Final dimensions of F wil be FAN columns by 
;                   fix((MAX-MIN)/DEL) + 1 rows if FAN is specified. 
;
; OUTPUTS:
;
;       F:    Final array. If input parameters are double precision,
;             then F will be double as well. F is float otherwise.
;
; RESTRICTIONS:
;
;       You'll need FAN.PRO to use the fan= keyword. 
;       http://astron.berkeley.edu/~johnjohn/idl.html#FAN
;
; EXAMPLE:
;
;         For an array that runs from 2 to 5 in steps of .7
;
;         IDL> f = fillarr(.7,2,5)
;         IDL> print, f
;            2.00000      2.70000      3.40000     4.10000    4.80000
;         
; MODIFICATION HISTORY:
; Written by John "JohnJohn" Johnson 21-Feb-2002
; 22-Feb-2002 JohnJohn- Fixed precision bug
; 23-Feb-2002 JohnJohn- Calculations performed in double precision. 
;                       Output in double precision if input is 
;                       double.
; 01-Mar-2002 JohnJohn- Much props to Tim Robishaw (Tdogg) for helping
;                       me understand machine precision and finally fixing
;                       the precision bug.
; 23 Apr 2002 JohnJohn- Modified the /FAN operation to match my new
;                       FAN procedure. Default direction of the
;                       fanning process is in the column direction,
;                       i.e. a 5-element array with FAN=2 will yeild a
;                       5x2 array rather than the other way around.
; 06 Dec 2002 JohnJohn- Modified the /FAN operation again to run using
;                       the actuall FAN proceedure which is faster
;                       than doing two separate operations for fanning
;                       and taking the transpose. duh.
; 14 Apr 2005 JohnJohn- Fixed bug where if n_params() eq 2, then MIN
;                       was being modified. Input variable now
;                       protected by renaming MIN as MININ.
;-
function fillarr,del,minin,max,fanned=fanned,transpose=transpose
;DEAL WITH HUMANS
on_error,2		;Return to caller if an error occurs
if n_params() lt 2 then message,'INCORRECT NUMBER OF INPUTS. Syntax: f = fillarr(del,min,max)',/ioerror

if n_params() eq 2 then begin
    max = minin[1]
    min = minin[0]
endif else min = minin

if max lt min then message,'MIN must be less than MAX',/ioerror
if del eq 0 then message,'DEL cannot equal 0',/ioerror

;if all of the input parameters are double, the return the answer in
;double precision.
doub = (size(del,/type) eq 5) and (size(min,/type) eq 5) and (size(max,/type) eq 5) or keyword_set(double)
del = double(del)
min = double(min)
max = double(max)
;ARG will go into A later. These are the only real calculations performed.
arg = (max-min)/del
;test for and correct rounding errors
rnd = round(arg)
eps = (machar(/double)).eps
if abs(rnd-arg) lt rnd*eps then arg = rnd else arg = fix(arg,type=3)

a = dindgen(arg+1)*del+min      ;can you believe there's all this code just to do this?

if n_elements(fanned) ne 0 then begin
    nfan = fanned
    if keyword_set(transpose) then a = fan(a,nfan,/transpose) $
       else a = fan(a,nfan)
endif

if not doub then a = float(a)

return,a 
end

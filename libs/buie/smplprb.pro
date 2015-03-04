;+
; NAME:
;  smplprb
; PURPOSE:   (one line only)
;  Sample a probability function to facilitate drawing random numbers from it.
; DESCRIPTION:
;  This program is intended to take a probability function, and return
;    an array that if plotted as a histogram will look like the probability
;    function.  The array that is produced is meant to be used to draw
;    random numbers that will be characterized by the probility function.
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  xval=SMPLPRB(func,x1,x2,n)
; INPUTS:
;  func   :String. Function name. ex: 'sqrt' or 'alog'
;              Any positive definite function can be used and must take
;               one and only one argument.
;  x1     : Min of x range.
;  x2     : Max of x range. 
; OPTIONAL INPUT PARAMETERS:
;  n      : Size of output array.  Determines sampling size.
;           DEFAULT=10000.  As this number is increased the resolution of
;           the sampled probability function is improved.  Successful usage
;           of this routine will depend on tuning this value.
; KEYWORD INPUT PARAMETERS:
; plot    : Flag, if set will cause a plots to be generated to show
;             a histogram derived from xval with the function overplotted.
; OUTPUTS:
;  returns an array of n x-values between x1 and x2.  Each discrete value of
;     X appears in the output array as many times as is needed to represent
;     the probability of that value.  This is a relative number since
;     increasing N will increase the number of times that every value of X
;     will appear.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Can only take probability functions, i.e. the function cannot be
;  less than zero.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/13, Written by Maureen Teyssier, REU Lowell Observatory
;  2004/07/15, MWB, incorporated into library.
;-
function smplprb,func,x1,x2,n,PLOT=plot

   self='SMPLPRB: '
   if badpar(func,[7],0,CALLER=self+'(func) ') then return,-1
   if badpar(x1,[1,2,3,4,5],0,CALLER=self+'(x1) ') then return,-1
   if badpar(x2,[1,2,3,4,5],0,CALLER=self+'(x2) ') then return,-1

   if badpar(n,[0,1,2,3,4,5],0,CALLER=self+'(n) ',DEF=10000) then return,-1

   if badpar(plot,[0,1,2,3],0,CALLER=self+'(PLOT) ',DEF=0) then return,-1

   ;m is used to sample x.  Making m a large number will sample x finely.  
   ;It is necessary to make m large enough so that you get a representative 
   ;sample, (this prevents skipping over values). However, if m is overly 
   ;large, there are a lot of repeated values-you're not gaining any more 
   ;information. Sqrt(n) is a heuristic guess for m that seems to work in 
   ;most cases.
                                
   m=sqrt(n)

   ; Divide the given range into M discrete values
   dx=(x2-x1)/m

   ;Creating an array of discrete x values over the given range from x1 to x2.  
   x=x1+indgen(m)*dx

   ;evaluate the function at every x 
   fxn=call_function(func,x)

   ;normalize the resulting sampled function 
   fcn=fxn/total(fxn)

   ;when you divide the range into m pieces of size dx, it isn't divided
   ;exactly. So, when you multiply m pieces by dx, and add x1, you don't
   ;get x2 exactly. 

   ;Due to the rounding necessary to make "count"(below) an integer, you 
   ;can obtain more than N elements.  To prevent the program from throwing an 
   ;error, 10 extra elements were added.  These are removed later, as they 
   ;do not affect the curve.
   xval=fltarr(n+10)
   j=0L

   ;for each value of x that is evaluated for the normalized function, 
   ;find out how many times x should appear in the output array
   for i=0L,m-1 do begin
      ;Assigning the probablility, of the function at x, an integer.  This
      ;integer is also the number of times that x will be placed in xval.
      count=fix(fcn[i]*n+.5)
      if count gt 0 then begin
         ;Placing the replicated value of x in xval
         xval[j:j+count-1]=replicate(x[i],count)
         j+=count 
      endif
   endfor

   ;Truncate the output array back to the right length
   xval=xval[0:n-1]

   ;check the histogram against the plot
   if keyword_set(plot) then begin
      h=histogram(xval,nbins=m,min=x1,max=x2)
      plot,x,h[1:*],psym=10
      oplot,x,fxn*n
   endif

   return,xval

end

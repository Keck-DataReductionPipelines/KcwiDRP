pro oplerr, x, y, err, $
    DIR = kdir, PSYM = lsym, SYMSIZE = ssize, COLOR=pcolor, NOCLIP=noclip
;
;+
; NAME:
;	oplerr
; PURPOSE:
;	Overplot data points with accompanying x or y error bars.
; CATEGORY:
;  2-D plotting
; CALLING SEQUENCE:
;	OPLERR, [ X ,]  Y , ERR [, DIR= ] [, PSYM= ] [, SYMSIZE= ] [, COLOR= ]
; INPUTS:
;	Y = array of Y values.
;	ERR = array of error bar values.
; OPTIONAL INPUT PARAMETERS:
;	X = optional array of X values.  The procedure checks whether or not
;	    the third parameter passed is a vector to decide if X was passed.
;	    If not passed, then INDGEN(Y) is assumed.
;	DIR = direction to put error bar (default = 6)
;	PSYM = plotting symbol (default = +7)
;	SYMSIZE = symbol size (default = 1.0)
;	COLOR = color for the symbols (default = !P.COLOR)
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Arrays must not be of type string.  There must be enough points to
;	plot.
; PROCEDURE:
;	A plot of X versus Y with error bars drawn from the point in the
;	direction specified by DIR.  Recognized values for DIR are:
;
;	1	X + ERR
;	2	Y + ERR
;	3	X - ERR
;	4	Y - ERR
;	5	X - ERR and X + ERR
;	6	Y - ERR and Y + ERR     (default)
;
;	Plot is written to the output device over any plot already there.
; MODIFICATION HISTORY:
;	William Thompson	Applied Research Corporation
;	July, 1986		8201 Corporate Drive
;				Landover, MD  20785
;
;	Marc Buie		Lowell Observatory
;	1991 October 11
;	Modified to allow specifying the direction for the error bars.
;	Change PSYM to a keyword parameter
;	1992 June 24, Add SYMSIZE as a keyword parameter
;  94/09/14, MWB, added NOCLIP keyword parameter
;-
;
on_error,2              ; Return to caller if an error occurs

;
; Decode acutal parameters
;
np = n_params(0)
if np lt 2 then $
   message, 'Need 2 or 3 acutal parameters: [ X ,]  Y , ERR [, DIR= ] [, PSYM= ]' $
else if np eq 2 then begin			;Only Y and ERR passed.
	derr = y
	yy = x
	xx = indgen(n_elements(x))
end else if np eq 3 then begin
	derr = err
	yy = y
	xx = x
endif

; Make sure all vectors are the same length
if n_elements(xx) ne n_elements(yy) then $
	message,'X and Y vectors are not the same length.'
if n_elements(xx) ne n_elements(derr) then $
	message,'ERR vector not the same length as data.'

; Decode keyword parameters
if n_elements(kdir) eq 0 then kdir = 6
if kdir lt 1 and kdir gt 6 then $
	message,'DIR is out of range.'
if n_elements(lsym) eq 0 then lsym = 7
if n_elements(ssize) eq 0 then ssize = 1
if n_elements(pcolor) eq 0 then pcolor = !p.color
if n_elements(noclip) eq 0 then noclip = 0

;
;  Some extra checking to handle a 1 point plot (why oh why?)
;
n = n_elements(xx)
if n lt 1 then message, 'No points to plot.' $
else if n eq 1 then begin		;Double XX and YY arrays to allow
	xx = [xx[0],xx[0]]		;	plotting of single point.
	yy = [yy[0],yy[0]]
	derr = [derr[0],derr[0]]
endif

oplot,xx,yy,psym=lsym,symsize=ssize,color=pcolor,noclip=noclip	;Plot data points.

for i = 0,n-1 do begin			;Plot error bars.
   if kdir eq 1 then begin
	xxx = [xx[i],xx[i]+derr[i]]
	yyy = [yy[i],yy[i]]
   end else if kdir eq 2 then begin
	xxx = [xx[i],xx[i]]
	yyy = [yy[i],yy[i]+derr[i]]
   end else if kdir eq 3 then begin
	xxx = [xx[i]-derr[i],xx[i]]
	yyy = [yy[i],yy[i]]
   end else if kdir eq 4 then begin
	xxx = [xx[i],xx[i]]
	yyy = [yy[i]-derr[i],yy[i]]
   end else if kdir eq 5 then begin
	xxx = [xx[i]-derr[i],xx[i]+derr[i]]
	yyy = [yy[i],yy[i]]
   end else if kdir eq 6 then begin
	xxx = [xx[i],xx[i]]
	yyy = [yy[i]-derr[i],yy[i]+derr[i]]
   end
   oplot,xxx,yyy,psym=0,linestyle=0,color=pcolor,noclip=noclip
endfor

return

end


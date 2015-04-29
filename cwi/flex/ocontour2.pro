pro ocontour2, img, x0, y0, x1, y1,bin=bin, qstop=qstop, $
	levels=levels, max_value=max_value, color=color, spline=spline, $
	c_thick=c_thick, c_linestyle=c_linestyle, nlevels=nlevels, $
	c_labels=c_labels, c_color=c_color, c_charsize=c_charsize, $
	c_annotation=c_annotation, normal=normal,  $
        c_orientation=c_orientation, c_spacing=c_spacing, fill=fill
;
;+
;NAME:
;	ocontour
;PURPOSE:
;	To overlay a contour on top of an image
;CALLING SEQUENCE:
;	ocontour, img
;	ocontour, data(*,*,5), bin=6
;	ocontour, img, 100, 100
;INPUT:
;	img	- the image to be contoured on top of the
;		  image aleady displayed with "TV"
;OPTIONAL INPUT:
;	x0	- If the origin of the image is not (0,0) then it is
;		  necessary to pass the left corner
;	y0	- see "x0"
;	bin	- If the image was displayed using REBIN or CONGRID
;		  then the routine needs to know that so that it scales
;		  properly.  This is the rebinning factor that was used
;		  to display the first image.
;OPTIONAL INPUT:
;	levels	- IDL contour command
;	max_value- IDL contour command
;	color	- IDL contour command
;	c_thick	- IDL contour command
;	c_linestyle - IDL contour command
;	nlevels	- IDL contour command
;	c_labels- IDL contour command
;	c_color	- IDL contour command
;	c_charsize - IDL contour command
;	c_annotation - IDL contour command
;       c_orientation - IDL contour command
;       c_spacing - IDL contour command

;HISTORY:
;	Written 28-Oct-91 by M.Morrison
;-
;
siz = size(img)
nx = siz(1)
ny = siz(2)
;
if (n_elements(bin) eq 0) then bin = 1
;
if (n_elements(x0) eq 0) then x0 = 0
if (n_elements(y0) eq 0) then y0 = 0
if (n_elements(x1) eq 0) then x1 = x0+nx*bin
if (n_elements(y1) eq 0) then y1 = y0+ny*bin

;
cmd = 'contour, img, xstyle=1+4, ystyle=1+4, /noerase, position = [x0,y0,x1,y1]'
if not keyword_set(normal) then cmd = cmd + ',/dev' else cmd = cmd + ',/normal'
if (keyword_set(c_labels)) then cmd = cmd + ', c_labels=c_labels
if (keyword_set(levels)) then cmd = cmd + ', levels=levels'
if (keyword_set(max_value)) then cmd = cmd + ', max_value=max_value'
if (keyword_set(color)) then cmd = cmd + ', color=color  '
if (keyword_set(c_thick)) then cmd = cmd + ', c_thick=c_thick'
if (keyword_set(c_linestyle)) then cmd = cmd + ', c_linestyle=c_linestyle'
if (keyword_set(spline)) then cmd = cmd + ', spline=spline'
if (keyword_set(nlevels)) then cmd = cmd + ', nlevels=nlevels'
if (keyword_set(c_color)) then cmd = cmd + ', c_color=c_color'
if (keyword_set(c_charsize)) then cmd = cmd + ', c_charsize=c_charsize
if (keyword_set(c_annotation)) then cmd = cmd + ', c_annotation=c_annotation
if (keyword_set(c_orientation)) then cmd = cmd + ', c_orientation=c_orientation'
if (keyword_set(c_spacing))     then cmd = cmd + ', c_spacing=c_spacing'
if (keyword_set(fill))     then cmd = cmd + ', fill=fill'

;


stat = execute(cmd)
;
if (keyword_set(qstop)) then stop
end


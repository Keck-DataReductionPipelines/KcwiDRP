;-
; $Id: kcwi_geom_resid.pro,v 1.1 2014/10/01 19:14:37 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_GEOM_RESID
;
; PURPOSE:
;	Calculates the residuals of the geometric transformation
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_GEOM_RESID,Xi,Yi,Xw,Yw,Degree,Kwx,Kwy,Xrsd,Yrsd
;
; INPUTS:
;	Xi,Yi	- Initial control points
;	Xw,Yw	- Wavelength corrected control points
;	Degree	- Degree passed to POLYWARP
;	Kwx,Kwy	- Output coeffs from POLYWARP
;
; OUTPUTS:
;	Xrsd,Yrsd	- Differences between Xi,Yi inputs and calculated
;				points from coeffs
;
; SIDE EFFECTS:
;	None
;
; EXAMPLE:
;	polywarp,xi,yi,xw,yw,3,kwx,kwy,/double,status=status
;	kcwi_geom_resid,xi,yi,xw,yw,3,kwx,kwy,xrsd,yrsd
;	!p.multi=[0,1,2]
;	plot,xi,xrsd,psym=4
;	plot,yi,yrsd,psym=4
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-OCT-01	Initial Revision
;-
;
pro kcwi_geom_resid,xi,yi,xw,yw,degree,kwx,kwy,xrsd,yrsd
;
; setup
pre = 'KCWI_GEOM_RESID'
version = repstr('$Revision: 1.1 $ $Date: 2014/10/01 19:14:37 $','$','')
xrsd = -1.
yrsd = -1.
;
np = n_elements(xi)
if n_elements(yi) ne np or n_elements(xw) ne np or $
	n_elements(yw) ne np then begin
	print,pre,': ERROR - control point vectors must all have the same number of elements'
	return
endif
;
kwxsz = size(kwx,/dim)
kwysz = size(kwy,/dim)
if kwxsz[0] ne degree+1 or kwxsz[1] ne degree+1 or $
   kwysz[0] ne degree+1 or kwysz[1] ne degree+1 then begin
   print,pre,': ERROR - 2D coeff array dimension error'
   return
endif
;
xrsd = fltarr(np)
yrsd = fltarr(np)
;
xp=0
yp=0
for i=0,degree do for j=0,degree do begin
	xp = xp + kwx[i,j]*xw^j*yw^i
	yp = yp + kwy[i,j]*xw^j*yw^i
endfor
xrsd = xi - xp
yrsd = yi - yp
;
return
end

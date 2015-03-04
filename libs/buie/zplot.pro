;+
; NAME: 
;  zplot
; PURPOSE:
;  Plot differential refraction as a function of wavelength.
; DESCRIPTION:
; CATEGORY:
;  2-D plotting
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  1991 - Written by Marc W. Buie, Lowell Observatory
;-
pro zplot,pressure,temp,water

z=[75.,10.,0.,20.,30.,40.,50.,60.,70.]/!radeg
sz = 1./cos(z)
am = sz - 0.0018167*(sz-1.0) - 0.002875*(sz-1.0)^2 - 0.0008083*(sz-1.0)^3
wave=indgen(200.)/200.*.57+.5
nw = n_elements(wave)
nz = n_elements(z)

dr = difref(wave,pressure,temp,water,z[0])
ytitle='Differential Refraction (arc-seconds)'
xtitle='Wavelength (Angstroms)'
title=string('T=',temp,'!9%!3C  P=',fix(pressure),' mm Hg  H!d2!n0=',water,' mm Hg',format='(a,f5.1,a,i4,a,i4,a)')
plot,wave*10000.0,dr,xrange=[4900.0,11000.0],xtitle=xtitle,ytitle=ytitle,title=title
xyouts,wave[nw-1]*10000.0,dr[nw-1],string(am[0],format='(f5.2)')

for i=1,nz-1 do begin
   dr = difref(wave,pressure,temp,water,z[i])
   oplot,wave*10000.0,dr
   xyouts,wave[nw-1]*10000.0,dr[nw-1],string(am[i],format='(f5.2)')
end

return
end

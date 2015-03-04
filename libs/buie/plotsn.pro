pro plotsn,i1,i2,x,y,label
;+
;NAME:
;	plotsn
;PURPOSE:
;	Analyze a vector and retrieve the signal-to-noise ratio.
;DESCRIPTION:
;	First, fit a polynomial to the data, subtract the polynomial and
;	add the mean of the polynomial to flatten the data.
;	Compute the mean and standard deviation in the flattened data.
;CATEGORY
;       Spectroscopy
;CALLING SEQUENCE:
;	plotsn,point1,point2,x,y,label
;INPUTS:
;	point1   = First data point to use from x and y.
;	point2   = Last data point to use from x and y.
;	x        = 1-d vector containing independent variable.
;	y        = 1-d vector containing dependent variable.
;	label    = String title for plot.
;OUTPUTS:
;	None
;REVISION HISTORY:
;	Written by Marc Buie on 2/27/91
;COMMENTS:
;	A single number for the SNR is somewhat dubious if the data spans
;	a wide numeric value.
;-

tmp = !p.multi

!p.multi=[0,1,3]
arlen=float(n_elements(x[i1:i2]))
order=4
sigwide=3.5
thresh=3.3

for order=2,6 do begin
   xx=x[i1:i2]
   yy=y[i1:i2]
   coeff=goodpoly(xx,yy,order,thresh,yfit,newx,newy)
   xx=newx
   yy=newy
   arlen=float(n_elements(xx))
   chisq=total((yy-yfit)^2)/(arlen-order)
   flat = (yy-yfit)+(total(yfit)/arlen)
   sigma = stdev(flat,mean)
   snr   = mean/sigma
   ;print,'Order ',order,'chisq=',chisq,'   S/N=',snr,$
   ;   format='(a,i1,2x,a,f6.1,a,f6.1)'
   if order eq 2 then begin
      minchi = chisq
      minorder = 2
   end else begin
      if chisq lt minchi then begin
         minchi = chisq
         minorder = order
      end
   end
end

order = minorder
xx=x[i1:i2]
yy=y[i1:i2]
coeff=goodpoly(xx,yy,order,thresh,yfit,newx,newy)
;print,coeff
xx=newx
yy=newy
arlen=float(n_elements(xx))
chisq=total((yy-yfit)^2)/(arlen-order)
flat   = (yy-yfit)+(total(yfit)/arlen)
sigma  = stdev(flat,mean)
snr    = mean/sigma
weight = mean/(sigma*sigma)
;print,'    Best chisq=',chisq,'   S/N=',snr,'for order',order,$
;   format='(a,f6.1,a,f6.1,2x,a,1x,i1)'

linslope=slope(6000.,8300.,100.,x,y)
;print,linslope

; Top plot on the page
title='Original data and the polynomial fit'
xtitle='Wavelength (Angstrons)'
ytitle='Counts'
plot,x[i1:i2],y[i1:i2],title=title,xtitle=xtitle,ytitle=ytitle
oplot,xx,yfit
;plot,xx,yfit

;Middle plot on page
title='Flattened data (data divided by fit)'
plot,xx,flat,title=title,xtitle=xtitle,ytitle=ytitle

;Lower right hand corner plot
!p.multi=[1,2,3]
histr=histogram(flat,min=-sigwide*sigma+mean,max=sigwide*sigma+mean,binsize=0.2*sigma)
hlen = float(n_elements(histr))
idx=(2.0*indgen(hlen)/hlen-1.0)*sigwide
coverage=fix(total(histr)/n_elements(x[i1:i2])*100+0.5)
xtitle='O-C (sigma)'
ytitle='Percent of distribution'
title='Histogram of flat data'
plot,idx,histr*100.0/n_elements(x[i1:i2]),xtitle=xtitle,ytitle=ytitle,title=title

lab0='Signal-to-noise analysis'
lab1=string('Point range',i1,'to',i2,format='(a,1x,i3,1x,a,1x,i3)')
lab2=string('Poly_fit: order=',order,format='(a,i1)')
lab3=string('Histogram plot -',coverage,'% of distribution',format='(a,1x,i3,a)')
lab4=string('Median',median(flat),'counts',format='(a,1x,f7.1,2x,a)')
lab5=string('Mean  ',mean,'counts',format='(a,1x,f7.1,2x,a)')
lab6=string('Sigma ',sigma,'counts',format='(a,1x,f7.1,2x,a)')
lab7=string('S/N   ',snr,format='(a,1x,f7.2)')
lab8=string(i2-i1+1-n_elements(xx),'outliers removed',format='(i3,1x,a)')
lab9=string('Outlier threshold=',thresh,'sigma',format='(a,1x,f3.1,1x,a)')
laba=string(linslope[0],'+/-',linslope[1],' per 1000 !s!a!e !20o!r!n!3A',format='(f7.4,1x,a,1x,f6.4,1x,a)')
labb='Linear slope between 6000 and 8300'
labc=string('weight (S/N^2)   ',weight,format='(a,1x,f7.2)')

left = 0.17
right = left + 0.10
line = 0.33

xyouts,right,line,lab0,/normal,charsize=1.5,alignment=0.5
line = line - 0.03

xyouts,right,line,label,/normal,charsize=1.5,alignment=0.5
line = line - 0.03

xyouts,right,line,lab1,/normal,alignment=0.5
line = line - 0.02
xyouts,right,line,lab2,/normal,alignment=0.5
line = line - 0.02
xyouts,right,line,labb,/normal,alignment=0.5
line = line - 0.02
xyouts,right,line,laba,/normal,alignment=0.5

line = line - 0.03
xyouts,right,line,lab9,/normal,alignment=0.5
line = line - 0.02
xyouts,right,line,lab8,/normal,alignment=0.5
line = line - 0.03

xyouts,right,line,lab3,/normal,alignment=0.5
line = line - 0.03

xyouts,left,line,lab4,/normal
line = line - 0.02
xyouts,left,line,lab5,/normal
line = line - 0.02
xyouts,left,line,lab6,/normal
line = line - 0.03

xyouts,left,line,lab7,/normal
line = line - 0.02
xyouts,left,line,labc,/normal
line = line - 0.02

xyouts,1.0,0.98,systime(),/normal,alignment=1.0

!p.multi=tmp

return
end

;$Id$
pro kcwi_cflat_anal,imgno
;+
;	kcwi_cflat_anal - analyze continuum flat images for charge transfer
;-
pre = 'KCWI_CFLAT_ANAL'
version = repstr('$Revision$ $Date$','$','')
;
img = mrdfits('image'+strn(imgno)+'.fits',0,hdr,/fscale)
kcwi_map_ccd,hdr,asec,bsec,csec,tsec,namps=namps,trimmed_size=tsz
osx0 = bsec[0,0,0] + 20
osx1 = bsec[0,0,1] - 20
print,'overscan pixels: ',osx0,osx1
prescan = sxpar(hdr,'prescan')
postscan = sxpar(hdr,'postscan')
overscan = sxpar(hdr,'overscan')
binstr = sxpar(hdr,'ccdsum')
xbin = fix(gettok(binstr,' '))
lastpix = 4096 / xbin + prescan
msklo = sxpar(hdr,'nsobjr0')
mskhi = sxpar(hdr,'nsobjr1')
sz = size(img,/dim)
npt = sz[0]*xbin
;
oscan = 0.145 * total(img[2097:2257,*],1)/161.0
mscan = 0.145 * total(img[35:2066,*],1)/2032.0
cscan = 0.145 * total(img[*,700:1360],2)/661.0
;
obias = [oscan[10:670],oscan[1420:2055]]
ims,obias,obmn
oscan = oscan - obmn
print,'oscan bias = ',obmn
;
mbias = [mscan[10:600],mscan[1500:2055]]
ims,mbias,mbmn
mscan = mscan - mbmn
print,'mscan bias = ',mbmn
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=2
si=1.5
;
!p.multi=[0,2,2]
;
; overscan row averaged plot
plot,oscan,/xs,/ys,xtitle='Y px',ytitle='e-', $
	title='CFLAT Image number: '+strn(imgno)
oplot,!x.crange,[0,0]
oplot,[670,670],!y.crange,color=colordex('green')
oplot,[1420,1420],!y.crange,color=colordex('green')
oplot,[msklo,msklo],!y.crange,color=colordex('red')
oplot,[mskhi,mskhi],!y.crange,color=colordex('red')
legend,['OSCAN','BIAS = '+string(obmn,form='(f7.2)')+' e-'], $
	charthi=th,/clear,clr_color=!p.background
yrng = !y.crange
;
; overscan column averaged within N&S mask region
blev = 115.2
resid = total(cscan[lastpix:osx1]-blev)
cterr = 1. - (resid/cscan[lastpix-1]/npt)
plot,cscan,xran=[2060,sz[0]],/xs,yran=[110,120],/ys, $
	title='CTE!DEPER!N = '+string(cterr,form='(f9.7)'), $
	xtitle='X px', ytitle='e-'
oplot,[lastpix-1,lastpix-1],!y.crange,color=colordex('green')
oplot,[osx0,osx0],!y.crange,color=colordex('red')
oplot,[osx1,osx1],!y.crange,color=colordex('red')
oplot,!x.crange,[blev,blev],color=colordex('blue')
legend,['LASTPIX = '+string(cscan[lastpix-1],format='(f9.3)')+' e-', $
	'RESIDUAL = '+string(resid,form='(f9.3)')+' e-', $
	'NPIXTRAN = '+strn(npt)], $
	charthi=th,/clear,clr_color=!p.background,/bottom
;
; image row averaged plot
plot,mscan,yran=yrng,/ys,/xs,xtitle='Y px',ytitle='e-'
oplot,!x.crange,[0,0]
oplot,[600,600],!y.crange,color=colordex('green')
oplot,[1500,1500],!y.crange,color=colordex('green')
oplot,[msklo,msklo],!y.crange,color=colordex('red')
oplot,[mskhi,mskhi],!y.crange,color=colordex('red')
legend,['IMAGE','BIAS = '+string(mbmn,form='(f7.2)')+' e-'], $
	charthi=th,/clear,clr_color=!p.background
;
; ratio of image to overscan within N&S mask region
rat = oscan/mscan
plot,rat,xran=[698,1371],/xs,yran=[-0.001,0.001],/ys,xtitle='Y px',ytitle='RAT'
;
ratg = rat[698:1371]
ims,ratg,gmn,gsg
oplot,!x.crange,[gmn,gmn],linesty=2
print,gmn,gsg
legend,['RATIO','AVG = '+string(gmn,form='(f7.5)')], $
	charthi=th,box=0
;
return
end

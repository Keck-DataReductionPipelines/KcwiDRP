pro kcwi_make_bspline_flat,wmimno,flatno,internal=internal,prefix=prefix,binning=binning

; order of operations
; 1. Run stage 1 [easy]
; 2. Run stage 4 to generate reverse mapping (wavelength and
; positions) [doable, easy; right now run it and remove all cubes, etc.]
; 3. Generate a flat [that's what this does]
;    a. if using continuum flat, correct for the vignetting
; 4. "intf" becomes "int" divided by the flat generated above
; 5. Can generate another sky spectrum here now and do a sky
; subtraction!
; 6. remove all cubes, re-run stage 4 with the replaced intf images



xbin=2
if keyword_set(binning) then xbin=binning
fitl=4/xbin
fitr=24/xbin
flatl=34/xbin
flatr=72/xbin
buffer=5.0/xbin
refslice=11

pre='kb171018_'
if keyword_set(prefix) then pre=prefix


allidx=findgen(140/xbin)
str=string(wmimno,"(i05)")
wavemap=mrdfits(pre+str+'_wavemap.fits',0,hdr,/fscale)
slice=mrdfits(pre+str+'_slicemap.fits',0,hdr)
pos=mrdfits(pre+str+'_posmap.fits',0,hdr)
strf=string(flatno,"(i05)")
flat=mrdfits(pre+strf+'_int.fits',0,hdr)
outfile=pre+strf+'_ratio.fits'




if keyword_set(internal) then begin
   
   q=where(wavemap gt 0)
   waves=minmax(wavemap[q])
   dw=(waves[1]-waves[0])/30.0
   wavemin=(waves[0]+waves[1])/2.0-dw
   wavemax=(waves[0]+waves[1])/2.0+dw
   print,wavemin,wavemax
   q=where(slice eq refslice and wavemap ge wavemin and wavemap le wavemax)
   plot,pos[q],flat[q],psym=3,/xs
   
; select the points we will fit for the vignetting 
   xfit=pos[q]  
   yfit=flat[q]
   qfit=where(xfit ge fitl and xfit le fitr)
   xfit=xfit[qfit]
   yfit=yfit[qfit]
   s=sort(xfit)
   xfit=xfit[s]
   yfit=yfit[s]
   
   resfit=linfit(xfit,yfit)
   
   oplot,allidx,resfit[0]+resfit[1]*allidx,color=fsc_color('purple')
   
; select the template region
   xflat=pos[q] 
   yflat=flat[q]
   qflat=where(xflat ge flatl and xflat le flatr)
   xflat=xflat[qflat]
   yflat=yflat[qflat]
   s=sort(xflat)
   xflat=xflat[s]
   yflat=yflat[s]
   
   resflat=linfit(xflat,yflat)
   oplot,allidx,resflat[0]+resflat[1]*allidx,color=fsc_color('red')
   
; compute the intersection
   xinter=-(resflat[0]-resfit[0])/(resflat[1]-resfit[1])
   
; figure out where the correction applies
   qinter=where(pos ge 0 and pos lt xinter-buffer)
; apply the corection!
   newflat=flat
   newflat[qinter]= (resflat[0]+resflat[1]*pos[qinter])/(resfit[0]+resfit[1]*pos[qinter])*flat[qinter]
   
; now deal with the the intermediate (buffer) region
   qspline=where(pos ge xinter-buffer and pos le xinter+buffer)
   posmin=min(pos[qspline])
   posmax=max(pos[qspline])
   valuemin=(resflat[0]+resflat[1]*posmin)/(resfit[0]+resfit[1]*posmin)
   valuemax=1
   
   slopeleft=resflat[1]/(resfit[1]*posmin+resfit[0])-(resflat[1]*posmin+resflat[0])*resfit[1]/((resfit[1]*posmin+resfit[0])*(resfit[1]*posmin+resfit[0]))
   
   sloperight=resflat[1]/(resfit[1]*posmax+resfit[0])-(resflat[1]*posmax+resflat[0])*resfit[1]/((resfit[1]*posmax+resfit[0])*(resfit[1]*posmax+resfit[0]))
   
;slopemid=resflat[1]/(resfit[1]*xinter+resfit[0])-(resflat[1]*xinter+resflat[0])*resfit[1]/((resfit[1]*xinter+resfit[0])*(resfit[1]*xinter+resfit[0]))
   
;print,slopeleft,slopemid,sloperight
   
   spline_p,[posmin,posmax],[valuemin,valuemax],xr,yr,interval=0.1,tan0=[-slopeleft,0],tan1=[-sloperight,0]
   plot,xr,yr,/ys               ;,psym=3
   yvals=interpol(yr,xr,pos[qspline])*flat[qspline]
   newflat[qspline]=yvals
   
   print,xinter
endif else begin
   newflat=flat
endelse 

; this is the corrected flatfield
mwrfits,newflat,'newflat.fits',hdr,/create

print,'newflat written.'



;;;; now we try to generate a spectrum to flat field to.
ffleft=10/xbin
ffright=70/xbin
ffslice=11
ffslice2=11
sm=25

; Generate a reference slice spectrum bspline fit 
refslice=11
qref=where(slice eq refslice and pos ge ffleft and pos le ffright)
xfr=wavemap[qref]
yfr=newflat[qref]
s=sort(xfr)
xfr=xfr[s]
yfr=yfr[s]
invvar=1/(1+abs(yfr))
n=100.0
bkpt=min(wavemap[qref])+findgen(n+1)*(max(wavemap[qref])-min(wavemap[qref]))/n
sftr=bspline_iterfit(xfr,yfr,fullbkpt=bkpt,yfit=yfitr)

; Generate a blue slice spectrum bspline fit 
blueslice=12
blueleft=60/xbin
blueright=80/xbin
qblue=where(slice eq blueslice and pos ge blueleft and pos le blueright)
xfb=wavemap[qblue]
yfb=newflat[qblue]
s=sort(xfb)
xfb=xfb[s]
yfb=yfb[s]
invvar=1/(1+abs(yfb))
n=100.0
bkpt=min(wavemap[qblue])+findgen(n+1)*(max(wavemap[qblue])-min(wavemap[qblue]))/n
sftb=bspline_iterfit(xfb,yfb,fullbkpt=bkpt,yfit=yfitb)
; 

; Generate a red slice spectrum bspline fit
redslice=23
redleft=60/xbin
redright=80/xbin
qred=where(slice eq redslice and pos ge redleft and pos le redright)
xfd=wavemap[qred]
yfd=newflat[qred]
s=sort(xfd)
xfd=xfd[s]
yfd=yfd[s]
invvar=1/(1+abs(yfd))
n=100.0
bkpt=min(wavemap[qred])+findgen(n+1)*(max(wavemap[qred])-min(wavemap[qred]))/n
sftd=bspline_iterfit(xfd,yfd,fullbkpt=bkpt,yfit=yfitd)

; waves.
minwave=min(xfb)
maxwave=max(xfd)
nwaves=1000.0
waves=minwave+(maxwave-minwave)*findgen(nwaves+1)/nwaves


plot,xfr,yfitr,xrange=[3000,6000]
oplot,xfb,yfitb,color=fsc_color('blue')
oplot,xfd,yfitd,color=fsc_color('red')


stop
wavebuffer=0.1
minrwave=min(xfr)
maxrwave=max(xfr)
wavebuffer2=0.05

qbluefit=where(waves lt minrwave+(maxrwave-minrwave)*wavebuffer and waves gt minrwave+(maxrwave-minrwave)*wavebuffer2)
qredfit=where(waves ge minrwave+(maxrwave-minrwave)*(1-wavebuffer) and waves lt minrwave+(maxrwave-minrwave)*(1-wavebuffer2))

bluefit=bspline_valu(waves[qbluefit],sftb)
refbluefit=bspline_valu(waves[qbluefit],sftr)
redfit=bspline_valu(waves[qredfit],sftd)
refredfit=bspline_valu(waves[qredfit],sftr)

plot,waves[qbluefit],refbluefit
oplot,waves[qbluefit],bluefit,color=fsc_color('blue')
stop
plot,waves[qbluefit],refbluefit/bluefit
bluelinfit=linfit(waves[qbluefit],refbluefit/bluefit)
oplot,waves[qbluefit],bluelinfit[0]+bluelinfit[1]*waves[qbluefit],color=fsc_color('blue')
stop
plot,waves[qredfit],refredfit
oplot,waves[qredfit],redfit,color=fsc_color('red')
stop
plot,waves[qredfit],refredfit/redfit,/ys
redlinfit=linfit(waves[qredfit],refredfit/redfit)
oplot,waves[qredfit],redlinfit[0]+redlinfit[1]*waves[qredfit],color=fsc_color('red')
stop

;; at this point we are going to try to merge the points

qselred=where(xfd ge maxrwave)
qselblue=where(xfb le minrwave)

redfluxes=yfd[qselred]*(redlinfit[0]+redlinfit[1]*xfd[qselred])
bluefluxes=yfb[qselblue]*(bluelinfit[0]+bluelinfit[1]*xfb[qselblue])

allx=[xfb[qselblue],xfr,xfd[qselred]]
ally=[bluefluxes,yfr,redfluxes]

s=sort(allx)
allx=allx[s]
ally=ally[s]


invvar=1/(1+abs(yfb))
n=100.0
bkpt=min(allx)+findgen(n+1)*(max(allx)-min(allx))/n
sftall=bspline_iterfit(allx,ally,fullbkpt=bkpt,yfit=yfitall)

plot,xfr,yfr,psym=3,xrange=[3000,6000],/xs
oplot,xfr,yfitr,color=fsc_color('green'),thick=2
oplot,allx,ally,psym=3,color=fsc_color('purple')
oplot,allx,yfitall,color=fsc_color('red'),thick=2
stop

; OK. Now we have extended to the full range... so... we are going to
; make a ratio flat!

comflat=flat-flat
qz= where(wavemap ge 0 );and slice ge 0 and slice le 23)

comvals=bspline_valu(wavemap[qz],sftall)

comflat[qz]=comvals
ratio=flat-flat
qzer = where(newflat ne 0)
ratio[qzer]=comflat[qzer]/newflat[qzer]

qq=where(ratio lt 0)
ratio[qq]=0.0
qq=where(ratio ge 2)
ratio[qq]=2

mwrfits,ratio,'ratio_f.fits',/create
print,"I am done!"
stop


; select the pixels in the reference slice and correct positions
qff0=where(slice eq ffslice and pos ge ffleft and pos le ffright)
; plot a little more than the KCWI blue range
plot,wavemap[qff0],smooth(newflat[qff0],sm),psym=3,/xs,xrange=[3400,6000]

qff=where(slice eq ffslice2 and pos ge ffleft and pos le ffright)
oplot,wavemap[qff],smooth(newflat[qff],sm),psym=3,color=fsc_color('orange')

; generate the bspline fit.
xf=wavemap[qff0]
yf=newflat[qff0]
s=sort(xf)
xf=xf[s]
yf=yf[s]
res=1/(1+abs(yf))
n=100.0
bkpt=min(wavemap[qff0])+findgen(n+1)*(max(wavemap[qff0])-min(wavemap[qff0]))/n
sft=bspline_iterfit(xf,yf,fullbkpt=bkpt,yfit=yfit)

xf1=wavemap[qff]
yf1=newflat[qff]
s=sort(xf1)
xf1=xf1[s]
yf1=yf1[s]
bkpt=min(wavemap[qff])+findgen(n+1)*(max(wavemap[qff])-min(wavemap[qff]))/n
sft0=bspline_iterfit(xf1,yf1,fullbkpt=bkpt,yfit=yfit1)
yfita=bspline_valu(xf,sft0)

; so the spline works pretty good, but we need to stitch it together
; for the full wavelenth range... 

;print,ec
oplot,wavemap[qff0],yfit,color=fsc_color('blue'),psym=3
stop
plot,wavemap[qff0],yfit-newflat[qff0],psym=3,/xs
stop
plot,wavemap[qff0],smooth(newflat[qff0],sm),psym=3,/xs,xrange=[3600,5000]
oplot,wavemap[qff0],yfit,color=fsc_color('blue'),psym=3
oplot,wavemap[qff],smooth(newflat[qff],sm),psym=3,color=fsc_color('green')
oplot,wavemap[qff0],yfita,color=fsc_color('red'),psym=3

qz = where(pos ge 0)

xp=wavemap[qz]
newvals=bspline_valu(xp,sft)
help,newvals
plot,xp,newvals,psym=3,title='newvals'
stop
comflat=flat-flat
comflat[qz]=newvals
mwrfits,comflat,'comflat.fits',/create

qnz = where(newflat ne 0)
ratio=flat-flat
ratio[qnz]=comflat[qnz]/newflat[qnz]
mwrfits,ratio,outfile,/create
print,"Done!"


  end

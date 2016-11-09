pro arc_scan,arc_imno,bar_imno,obj_imno,bspec,plot=plot
;+
;-
arc = mrdfits('kcwi'+strn(arc_imno)+'_int.fits',0,ahdr)
bar = mrdfits('kcwi'+strn(bar_imno)+'_int.fits',0,bhdr)
obj = mrdfits('kcwi'+strn(obj_imno)+'_int.fits',0,ohdr)
out = 'kcwi'+strn(obj_imno)+'_img.fits'
pout = 'kcwi'+strn(obj_imno)+'_pimg.fits'
;
do_plot = keyword_set(plot)
;
sz = size(arc,/dim)
;
nx = sz[0]
ny = sz[1]
;
namp = sxpar(ahdr,'nvidinp')
rn = 0.
switch namp of
	4: rn = rn + sxpar(ahdr,'biasrn4')
	3: rn = rn + sxpar(ahdr,'biasrn3')
	2: rn = rn + sxpar(ahdr,'baisrn2')
	1: rn = rn + sxpar(ahdr,'biasrn1')
endswitch
rn /= float(namp)
;
bstr = sxpar(ahdr,'binning')
bin = bstr
xbin = fix(gettok(bstr,','))
ybin = fix(bstr)
;
ifu = sxpar(ahdr,'ifunum')
ifunam = sxpar(ahdr,'ifunam')
q = ''
;
; set IFU-specific params
case ifu of
	1: begin	; Large slicer
		do_gauss = (1 eq 0)
		minpix = 6 / ybin
	end
	2: begin	; Medium slicer
		if ybin eq 2 then begin	; 2x2 binning
			do_gauss = (1 eq 1)
			minpix = 3
		endif else begin	; 1x1 binning
			do_gauss = (1 eq 0)
			minpix = 6
		endelse
	end
	3: begin	; Small slicer
		do_gauss = (1 eq 1)
		if ybin eq 2 then $
			minpix = 1 $
		else	minpix = 2
	end
	else: begin
		print,'ERROR - Unknown slicer ID: ',ifu
		return
	end
endcase
;
; output image
onx = 180 / xbin
ony = 300 / ybin
oimg = fltarr(onx,ony)
pimg = fltarr(180,700)
yp = 0L
;
data = fltarr(3,24,1000) - 1.
sp = -1
last = 0
maxnx = 0
maximx = 0
maximy = 0
got_range = (0 eq 1)
dx = 2 / xbin
bspec = fltarr(1000,24)
angles = fltarr(24)
coords = intarr(5,24)
;
;
for i=4,nx-4 do begin
	;
	; extract column
	col = reform(median(arc[(i-dx):(i+dx),*],dimen=1))
	;
	; find start of slice
	t=where(col gt rn*10., nt)
	if nt gt minpix then begin
		;
		; new slice
		if last le minpix then begin
			sp += 1
			nxp = 0
			if sp gt 23 then begin
				print,'Slice overflow: ',sp
				break
			endif
			if sp gt 0 then begin
				xf = reform(data[0,sp-1,*])
				yf = reform(data[1,sp-1,*])
				wf = reform(data[2,sp-1,*])
				good = where(yf gt 0.); and wf gt 1.)
				xf = xf[good]
				yf = yf[good]
				wf = wf[good]
				c = poly_fit(xf,yf,1)
				angle = atan(c[1]) / !DTOR
				angles[sp-1] = angle
				ims,wf,w,wstd
				w = fix(w/2.)>1
				;yrng = get_plotlims([yf,poly(xf,c)])
				xrng = get_plotlims(xf)
				;
				; extract trace from bar image
				xx = lindgen( fix(max(xf)-min(xf))+20 ) + $
					long(min(xf)-14)
				nxx = n_elements(xx)
				x0 = xx[0]
				x1 = xx[nxx-1]
				y0 = fix(poly(x0,c)+0.5)
				y1 = fix(poly(x1,c)+0.5)
				if y0 lt y1 then begin
					y0 = y0 - w*2.
					y1 = y1 + w*2.
				endif else begin
					yt = y0
					y0 = y1 - w*2.
					y1 = yt + w*2.
				endelse
				coords[0,sp-1] = x0
				coords[1,sp-1] = x1
				coords[2,sp-1] = y0
				coords[3,sp-1] = y1
				coords[4,sp-1] = w
				yc = fix( ( (y1-y0)+1 ) / 2 )
				;
				; extract subimg
				sub = bar[x0:x1,y0:y1]
				subr = rot(sub,angle,cubic=-0.5)
				bs = median(subr[0:nxx-1,yc-w:yc+w],dimen=2)
				bspec[0:nxx-1,sp-1] = bs
				if nxx gt maximx then $
					maximx = nxx
				yp += w*2
				;yrng = get_plotlims(yf)
				yrng = get_plotlims(bs)
				yrng[0] = 0.
				if do_plot then begin
					plot,xx,bs,psym=-4,/xs,xran=xrng,/ys,yran=yrng,$
					;plot,xf,yf,psym=4,/ys,yran=yrng,/xs,xran=xrng,$
						title=ifunam+' Slice: '+strn(sp-1)
					;oplot,xf,poly(xf,c)
					print,'xrange, np: ',minmax(xx), $
						n_elements(xx),$
						format='(a,2f9.2,2x,i7)'
					q=''
					read,'next: ',q
				endif
				got_range = (1 eq 0)
			endif
			print,'Slice, i, yp',sp,i,yp
		endif
		;
		; get range domains
		rangepar,t,tstr
		ran = strsplit(tstr,',',/extract,count=n)
		;
		; do we have more than one?
		if n gt 1 then begin
			for j=0,n-1 do begin
				rangepar,ran[j],ys
				;
				; if large, this is the one we want
				if n_elements(ys) gt 5 then begin
					ran = ran[j]
					break
				endif
			endfor
		endif
		;
		; get centroid
		rangepar,ran[0],iy
		cnt = cntrd1d(iy,col[iy])
		if not got_range then begin
			xrng = [cnt-(25./float(ybin)),cnt+(25./float(ybin))]
			got_range = (1 eq 1)
		endif
		err = fltarr(n_elements(iy)) + rn
		if do_gauss then begin
			while n_elements(iy) lt 5 do begin
				iy = [min(iy)-1, iy, max(iy)+1]
				err = [rn, err, rn]
			endwhile
			startp = [max(col[iy]),cnt,2.]
			yfit = gaussfit(iy,col[iy],pars,nterm=3, $
				estimates=startp,measure_err=err,sigma=perr)
			print,i,pars,format='(i8,2x,4f10.3)'
			if n_elements(perr) eq 3 then begin
				print,n_elements(iy),perr, $
					format='(i8,2x,4f10.3)'
				fcnt = pars[1]
				fwid = pars[2]
			endif else begin
				perr = [1.e9,1.e9]
				fcnt = 1.e9
				fwid = 1
			endelse
			ferr = perr[1]
		endif else begin
			startp = [cnt,(n_elements(iy)-5)>5,2.,2.,max(col[iy]),0.]
			pars = mpfitfun('erfcfit',iy,col[iy],err,startp, $
				yfit=yfit,perror=perr,/quiet)
			;print,i,pars,format='(i8,2x,6f10.3)'
			if n_elements(perr) eq 6 then begin
				;print,n_elements(iy),perr, $
				;	format='(i8,2x,6f10.3)'
				fcnt = pars[0]
				fwid = pars[1]
			endif else begin
				perr = 1.e9
				fcnt = 1.e9
				fwid = 1
			endelse
			ferr = perr[0]
		endelse
		;
		; get data
		if ferr le 0.001 and abs(cnt-fcnt) le 1. then begin
			data[0,sp,nxp] = float(i)
			data[1,sp,nxp] = fcnt
			data[2,sp,nxp] = fwid
			if do_plot then begin
			    plot,iy,col[iy],psym=4,/xs,xran=xrng, $
				title='col: '+strn(i)+', '+bin+', '+ifunam+' Slice: '+strn(sp)
			    oplot,iy,yfit
			    oplot,[fcnt,fcnt],!y.crange
			    oplot,[cnt,cnt],!y.crange,linesty=2
			    wait,0.01
			    if strupcase(strtrim(q,2)) ne 'Q' then $
				read,'next: ',q
			endif
			;
			; store max number of x points
			if nxp gt maxnx then maxnx = nxp
			nxp += 1
		endif else begin
			;print,'rejected: ',ferr,(cnt-fcnt)
		endelse
	endif	; nt gt minpix
	last = nt
endfor
if sp le 23 then begin
	xf = reform(data[0,sp,*])
	yf = reform(data[1,sp,*])
	wf = reform(data[2,sp,*])
	good = where(yf gt 0.) ; and wf gt 1.)
	xf = xf[good]
	yf = yf[good]
	wf = wf[good]
	ims,wf,w,wstd
	w = fix(w/2.)>1
	c = poly_fit(xf,yf,1)
	angle = atan(c[1]) / !DTOR
	angles[sp] = angle
	yrng = get_plotlims([yf,poly(xf,c)])
	xrng = get_plotlims(xf)
	;xx = lindgen( fix(max(xf)-min(xf)) ) + long(min(xf))
	xx = lindgen( fix(max(xf)-min(xf))+20 ) + long(min(xf)-14)
	nxx = n_elements(xx)
	x0 = xx[0]
	x1 = xx[nxx-1]
	y0 = fix(poly(x0,c)+0.5)
	y1 = fix(poly(x1,c)+0.5)
	if y0 lt y1 then begin
		y0 = y0 - w*2.
		y1 = y1 + w*2.
	endif else begin
		yt = y0
		y0 = y1 - w*2.
		y1 = yt + w*2.
	endelse
	coords[0,sp] = x0
	coords[1,sp] = x1
	coords[2,sp] = y0
	coords[3,sp] = y1
	coords[4,sp] = w
	yc = fix( ( (y1-y0)+1 ) / 2 )
	;
	; extract subimg
	sub = bar[x0:x1,y0:y1]
	subr = rot(sub,angle,cubic=-0.5)
	;
	; extract trace from bar image
	bs = median(subr[0:nxx-1,yc-w:yc+w],dimen=2)
	bspec[0:nxx-1,sp] = bs
	if nxx gt maximx then $
		maximx = nxx
	yrng = get_plotlims(bs)
	yrng[0] = 0.
	if do_plot then begin
		plot,xx,bs,psym=-4,/xs,xran=xrng,/ys,yran=yrng, $
		;plot,xf,yf,psym=4,/ys,yran=yrng,/xs,xran=xrng, $
			title=ifunam+' Slice: '+strn(sp)
		;oplot,xf,poly(xf,c)
		print,'xrange, np: ',minmax(xx),nxx,$
			format='(a,2f9.2,2x,i7)'
	endif
	maximy = yp + w*2
endif
;
; get offsets
bspec = bspec[0:maximx,*]
for i=0,23 do begin
	off = ccpeak(bspec[*,i],bspec[*,11],20)
	x0 = coords[0,i]
	x1 = coords[1,i]
	y0 = coords[2,i]
	y1 = coords[3,i]
	w  = coords[4,i]; + fix(2/ybin)
	nxx = (x1-x0) + 1
	sub = obj[x0:x1,y0:y1]
	subr = rot(sub,angles[i],cubic=-0.5)
	si = size(subr)
	x = lindgen(si[1],si[2])
	y = x/si[1]
	x = x MOD si[1]
	res = interpolate(subr,x-off,y)
	yc = si[2]/2
	if i gt 0 then begin
		yp0 = yp1; - fix(2/ybin)
		yp1 = yp0 + w*2
		yp2 = yp3
		yp3 = yp2 + (si[2]-1)
	endif else begin
		yp0 = 0
		yp1 = w*2
		yp2 = 0
		yp3 = si[2]-1
	endelse
	oimg[0:nxx-1,yp0:yp1] += res[0:nxx-1,yc-w:yc+w]
	pimg[0:nxx-1,yp2:yp3] = subr[0:nxx-1,*]
	plot,res[*,yc],/xs
	wait,0.1
endfor
maximy = yp1
;
oimg = oimg[0:maximx,0:maximy]
mwrfits,oimg,out,bhdr,/create
mwrfits,pimg,pout,bhdr,/create
print,'Wrote: ',out
;
return
end

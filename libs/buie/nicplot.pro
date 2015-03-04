;+
; NAME:
;  nicplot
; PURPOSE:
;  Create a summary plot of the contents of a NICMOS Grism data structure.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  nicplot,obs
; INPUTS:
;  obs - anonymous structure containing the NICMOS data.  This structure
;           is initially created by NICINIT.PRO
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  PS - Flag, if set causes a postscript plot to be generated without changing
;        the current display device.  Note, that if the device is already
;        PS (postscript) then a postscript file will be created regardless of
;        the setting of this flag.
;
;  QUEUE - Print queue to send the postscript file to
;            (default=don't send to printer).
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1999 July 16
;-
pro nicplot,obs,PS=ps,QUEUE=queue,FILE=file

   if badpar(ps,[0,1,2,3],0,CALLER='NICPLOT: (PS) ',default=0) then return
   if badpar(queue,[0,7],0,CALLER='NICPLOT: (QUEUE) ',default='') then return
   if badpar(file,[0,7],0,CALLER='NICPLOT: (FILE) ',default='idl.ps') then return

   mt = strtrim(sxpar(obs.hdr,'TARGNAME'),2) + ' (' + obs.root + obs.obsid + ')'
   blanks=replicate(' ',20)
   x1l = obs.x1>0

   if ps then begin
      d_name = !d.name
      set_plot,'PS'
   endif

   if !d.name eq 'PS' then begin
      p_font = !p.font
      !p.font = 0
      device,/Helvetica,file=file
      xsz = 18.0
      ysz = 25.0
      setpage,/portrait,xsize=xsz,ysize=ysz

      cs=0.7

      px1 = 0.03
      px2 = 0.33
      px3 = 0.67
      px4 = 1.0
      py1 = 0.0
      py2 = 0.4
      py3 = 0.7
      py4 = 1.0
      gap = 0.04

      plot,obs.x,obs.yc,xr=[0,obs.nx],yr=[0,obs.ny],charsize=cs, $
         title='Extraction map',xtitle='X pixel position', $
         ytitle='Y pixel position',/isotropic, $
         position=[px3+gap,py2+gap,px4-gap,py3-gap]
      oplot,[x1l,obs.x2,obs.x2,x1l,x1l], $
            [obs.y1,obs.y1,obs.y2,obs.y2,obs.y1]
      oplot,obs.xg[0:obs.ngood-1],obs.yg[0:obs.ngood-1],psym=4,symsize=0.5

      plot,obs.xg[0:obs.ngood-1],obs.yg[0:obs.ngood-1]-obs.yc[obs.xg[0:obs.ngood-1]],psym=5,symsize=0.75,charsize=cs, $
         title='Residuals from centerline fit',yr=[-0.5,0.5], $
         xtitle='X pixel position',ytitle='O-C of object location (pixels)', $
         position=[px2+gap,py3+gap,px4-gap,py4-gap],/noerase

      if obs.pass eq 0 or obs.nobj eq 1 then begin
         plot,obs.xp[0:obs.pnp-1],obs.yp[0:obs.pnp-1],psym=3,yr=[-0.1,1.0],charsize=cs, $
            title='Phased spatial profile', $
            xtitle='Y position relative to centerline (pixels)', $
            ytitle='Relative signal',/noerase, $
            position=[px1+gap,py3+gap,px2-gap,py4-gap]
         oplot,obs.xgrid,obs.yps+0.2

         interp,obs.xgrid,obs.yps,obs.xp[0:obs.pnp-1],ypfit
         plot,obs.xp[0:obs.pnp-1],obs.yp[0:obs.pnp-1]-ypfit,psym=3,charsize=cs, $
            title='Spatial profile residuals', $
            xtitle='Y position relative to centerline (pixels)', $
            ytitle='O-C of profile signal and numerical profile', $
            /noerase,position=[px1+gap,py2+gap,px2-gap,py3-gap]
      endif else begin
         for pick=x1l,obs.x2 do begin
            i=pick-x1l

            yv = obs.y-obs.yc[pick]
            z=where(yv ge -obs.hwid and yv le obs.hwid)
            yv=yv[z]
            dat = obs.image[pick,z]
            dat=dat[*]

            cprof=nic2src_fn(yv,2)

            colprof1 = (dat-cprof[*,1]*obs.ospec[i,1])/obs.ospec[i,0]
            colprof2 = (dat-cprof[*,0]*obs.ospec[i,0])/obs.ospec[i,1]

            if i eq 0 then begin
               xp1 = yv
               yp1 = colprof1
               xp2 = yv+obs.yo[0]-obs.yo[1]
               yp2 = colprof2
            endif else begin
               xp1 = [xp1,yv]
               yp1 = [yp1,colprof1]
               xp2 = [xp2,yv+obs.yo[0]-obs.yo[1]]
               yp2 = [yp2,colprof2]
            endelse
         endfor
         plot,xp1,yp1,psym=3,yr=[-0.1,1.0],charsize=cs, $
            title='Phased spatial profile 1', $
            xtitle='Y position relative to centerline (pixels)', $
            ytitle='Relative signal',/noerase, $
            position=[px1+gap,py3+gap,px2-gap,py4-gap]
         oplot,obs.xgrid,obs.ypsi+0.2
         plot,xp2,yp2,psym=3,yr=[-0.1,1.0],charsize=cs, $
            title='Phased spatial profile 2', $
            xtitle='Y position relative to centerline (pixels)', $
            ytitle='Relative signal',/noerase, $
            position=[px1+gap,py2+gap,px2-gap,py3-gap]
      endelse

      skysclim,obs.image[x1l:obs.x2,obs.y1:obs.y2],lowval,hival
      bim=bytscl(obs.image,min=lowval,max=hival,top=!d.n_colors-41)
      bim[x1l:obs.x2,obs.y1:obs.y2]=bim[x1l:obs.x2,obs.y1:obs.y2]+40B
      tv,bim,(px2+gap)*xsz,(py2+gap)*ysz,xsize=0.25*xsz,ysize=0.25*xsz,/centimeters

      if obs.pass eq 0 or obs.nobj eq 1 then begin
         plot,obs.wave,obs.spec,/noerase,charsize=cs, $
            title='Raw spectrum', $
            xtitle='Wavelength (microns)',ytitle='Integrated signal (DN)', $
            position=[px1+gap,py1+gap,px3-gap,py2-gap]
      endif else begin
         plot,obs.wspec[*,0],obs.ospec[*,0],/noerase,charsize=cs, $
            title='Raw spectrum',xr=minmax(obs.wspec), $
            ytitle='Integrated signal (DN)', $
            position=[px1+gap,(py1+py2)/2.0,px3-gap,py2-gap], $
            xtickname=blanks
         plot,obs.wspec[*,1],obs.ospec[*,1],/noerase,charsize=cs, $
            xtitle='Wavelength (microns)',ytitle='Integrated signal (DN)', $
            position=[px1+gap,py1+gap,px3-gap,(py1+py2)/2.0], $
            xr=minmax(obs.wspec)
      endelse

      pxl = px3+gap/2.0
      pxr = px4-gap
      pxm = (pxl+pxr)/2.0
      py  = py2-gap
      dy  = -0.02

      ts = 1.0
      str = strtrim(sxpar(obs.hdr,'TARGNAME'),2)
      device,/Bold
      xyouts,pxl,py,'Target:'+str,charsize=ts,/normal
      device,Bold=0

      py=py+dy
      xyouts,pxl,py,'Visit: '+obs.root+'   ObsID: '+obs.obsid,charsize=ts,/normal

      py=py+dy
      xyouts,pxl,py,'     background: '+obs.refid,charsize=ts,/normal

      py=py+dy
      str = 'Dither position '+strn(sxpar(obs.hdr,'PATT_POS'))+' of '+ $
                           strn(sxpar(obs.hdr,'NUMPOS'))
      xyouts,pxl,py,str,charsize=ts,/normal
      py=py+dy*0.5

      ts = 0.8
      dy = dy * 0.8

      py=py+dy
      str = strtrim(sxpar(obs.hdr,'TELESCOP'),2)+'/'+ $
            strtrim(sxpar(obs.hdr,'INSTRUME'),2)+'    P'+ $
            strn(sxpar(obs.hdr,'PROPOSID'))
      xyouts,pxl,py,str,charsize=ts,/normal

      py=py+dy
      str = strtrim(sxpar(obs.hdr,'APERTURE'),2)+' / '+ $
            strtrim(sxpar(obs.hdr,'OBSMODE'),2)+' / '+ $
            strtrim(sxpar(obs.hdr,'FILTER'),2)
      xyouts,pxl,py,str,charsize=ts,/normal

      py=py+dy
      str = 'Focused for '+strtrim(sxpar(obs.hdr,'FOCUS'),2)
      xyouts,pxl,py,str,charsize=ts,/normal

      py=py+dy*0.5

      py=py+dy
      expmid = double(sxpar(obs.hdr,'EXPSTART')+sxpar(obs.hdr,'EXPEND'))/2.0
      expmid = expmid + 2400000.5d0
      jdstr,expmid,0,str
      xyouts,pxl,py,'MidTime: '+str+' UT',charsize=ts,/normal

      py=py+dy
      expt = sxpar(obs.hdr,'EXPTIME')
      str='ExpTime: '+strtrim(string(expt,format='(f10.1)'),2)+' sec'
      xyouts,pxl,py,str,charsize=ts,/normal

      py=py+dy*0.5

      py=py+dy
      if obs.doflat then begin
         str='Flat: polynomial order '+strn(obs.forder)
      endif else begin
         str='No flat fielding'
      endelse
      xyouts,pxl,py,str,charsize=ts,/normal

      if obs.pass gt 0 then begin
         py=py+dy
         str='Profile iteration '+strn(obs.pass)
         xyouts,pxl,py,str,charsize=ts,/normal
      endif

      jdstr,systime(/julian),0,str
      xyouts,1.0,0.0,str+' UT',align=1.0,charsize=0.5,/normal

      device,/close

      if queue ne '' then begin
         spawn,'lp -c -d '+queue+' idl.ps'
      endif

      !p.font = p_font

   endif else begin

      setwin,0,xsize=400,ysize=400
      plot,obs.x,obs.yc,xr=[0,obs.nx],yr=[0,obs.ny], $
         title=mt+'   Extraction map',xtitle='X pixel position', $
         ytitle='Y pixel position',/isotropic
      oplot,[x1l,obs.x2,obs.x2,x1l,x1l], $
            [obs.y1,obs.y1,obs.y2,obs.y2,obs.y1]
      oplot,obs.xg[0:obs.ngood-1],obs.yg[0:obs.ngood-1],psym=4,symsize=0.5

      setwin,1,xsize=400,ysize=300
      plot,obs.xg[0:obs.ngood-1],obs.yg[0:obs.ngood-1]-obs.yc[obs.xg[0:obs.ngood-1]],psym=5,symsize=0.75,yr=[-0.5,0.5], $
         title=mt+'   Residuals from centerline fit', $
         xtitle='X pixel position',ytitle='O-C of object location (pixels)'

      setwin,2,xsize=400,ysize=400
      plot,obs.xp[0:obs.pnp-1],obs.yp[0:obs.pnp-1],psym=3,yr=[-0.1,1.0], $
         title=mt+'   Phased spatial profile', $
         xtitle='Y position relative to centerline (pixels)', $
         ytitle='Relative signal'
      oplot,obs.xgrid,obs.yps+0.2

      setwin,3,xsize=400,ysize=300
      interp,obs.xgrid,obs.yps,obs.xp[0:obs.pnp-1],ypfit
      plot,obs.xp[0:obs.pnp-1],obs.yp[0:obs.pnp-1]-ypfit,psym=3, $
         title=mt+'   Spatial profile residuals', $
         xtitle='Y position relative to centerline (pixels)', $
         ytitle='O-C of profile signal and numerical profile'

      setwin,4,xsize=obs.nx,ysize=obs.ny
      skysclim,obs.image[x1l:obs.x2,obs.y1:obs.y2],lowval,hival
      bim=bytscl(obs.image,min=lowval,max=hival,top=!d.n_colors-21)
      bim[x1l:obs.x2,obs.y1:obs.y2]=bim[x1l:obs.x2,obs.y1:obs.y2]+20B
      tv,bim

      if obs.pass eq 0 or obs.nobj eq 1 then begin
         setwin,5,xsize=400,ysize=300
         plot,obs.wave,obs.spec, $
            title=mt+'   '+obs.grism+' spectrum', $
            xtitle='Wavelength (microns)',ytitle='Integrated signal (DN)'
      endif else begin
         setwin,5,xsize=400,ysize=300
         plot,obs.wspec[*,0],obs.ospec[*,0], $
            title=mt+'   '+obs.grism+' spectrum 1', $
            xtitle='Wavelength (microns)',ytitle='Integrated signal (DN)'
         setwin,6,xsize=400,ysize=300
         plot,obs.wspec[*,1],obs.ospec[*,1], $
            title=mt+'   '+obs.grism+' spectrum 2', $
            xtitle='Wavelength (microns)',ytitle='Integrated signal (DN)'
      endelse


   endelse

   if ps then begin
      set_plot,d_name
   endif

end

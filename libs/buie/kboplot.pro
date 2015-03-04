;+
; NAME:
;  kboplot
; PURPOSE:   (one line only)
;  Interactive plotting tool for the DES mySQL database contents.
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  kboplot
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
;  Written by Marc W. Buie, Lowell Observatory, 2003/06/03
;  2003/08/31, MWB, added snap to jpeg tool.
;  2003/09/29, MWB, added postscript plot save feature.
;-
pro kboplot_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state


   free_lun,(*state).dblun

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).jd
   ptr_free,(*state).x
   ptr_free,(*state).y
   ptr_free,(*state).orbtype
   ptr_free,(*state).quality
   ptr_free,(*state).objectid

   ; Free up the state structure itself.
   ptr_free, state

end

pro kboplot_choosedata, state, axisname


   choice = picker((*state).choices[1,*],index=index)

   if choice eq '[[[CANCEL]]]' then return

   if axisname eq 'Abscissa' then begin
      (*state).xaxis = (*state).choices[0,index]
      (*state).xname = (*state).choices[1,index]
      (*state).xflip = (*state).choices[2,index]
      (*state).xiso  = (*state).choices[3,index]
   endif else begin
      (*state).yaxis = (*state).choices[0,index]
      (*state).yname = (*state).choices[1,index]
      (*state).yflip = (*state).choices[2,index]
      (*state).yiso  = (*state).choices[3,index]
   endelse

   (*state).dirty=1

end

; hardcopy =  0  send plot to display window
;          =  1  generate postscript file (idl.ps) and send to default printer
;          = -1  generate postscript file (default = idl.ps)
;          = -2  generate encapsulated file (default = idl.eps)
pro kboplot_plot, state, hardcopy, FILE=file

   deffile = 'idl.ps'
   if hardcopy eq -2 then deffile = 'idl.eps'
   if badpar(file,[0,7],0,caller='KBOPLOT_PLOT: (FILE) ', $
                default=deffile) then return

   if (*state).dirty then begin

      cmd = ['select '+(*state).xaxis+','+(*state).yaxis+',', $
             'dclass.orbtype,quality,dclass.objectid,pos.jd', $
             'from dclass,elem,pos', $
             'where dclass.objectid=elem.objectid ', $
             '  and dclass.objectid=pos.objectid ', $
             (*state).qual_select+';']

      mysqlquery,(*state).dblun,cmd,x,y,orbtype,quality,objectid,jd, $
         format='(f,f,a,i,a,d)'
      ptr_free,(*state).x
      ptr_free,(*state).y
      ptr_free,(*state).orbtype
      ptr_free,(*state).quality
      ptr_free,(*state).objectid
      ptr_free,(*state).jd
      (*state).x = ptr_new(x)
      (*state).y = ptr_new(y)
      (*state).orbtype  = ptr_new(orbtype)
      (*state).quality  = ptr_new(quality)
      (*state).objectid = ptr_new(objectid)
      (*state).jd       = ptr_new(jd)
      (*state).dirty=0
   endif

   iso = (*state).xiso eq (*state).yiso and (*state).xiso ne ''


   if hardcopy eq 0 then begin
      widget_control,(*state).drawwin, get_value=winnum
      wset,winnum
      cs=2.0
      cst=1.5
      ss=0.5
      colorarr = ['000000'xl, $
                  '707070'xl, $
                  '0000FF'xl, $
                  '00FF00'xl, $
                  'FF00FF'xl, $
                  '00FFFF'xl, $
                  'FFFF00'xl, $
                  'FFFFFF'xl ]
   endif else begin
      ; hardcopy/file setup
      d_name = !d.name
      p_font = !p.font
      !p.font = 0
      set_plot,'PS'
      encap = hardcopy eq -2
      device,encapsulated=encap,/Helvetica,file=file,/color
      if encap then begin
         if iso then begin
            device,/portrait,xsize=7.0,ysize=7.0
            xmargin=[7.5,1]
            ymargin=[3.2,1]
         endif else begin
            device,/portrait,xsize=10.5,ysize=7.0
            xmargin=[7.8,1]
            ymargin=[3.2,2]
         endelse
         cs=0.8
         cst=0.5
         ss=0.25
      endif else begin
         if iso then $
            setpage,/portrait,ysize=19.0,xsize=19.0 $
         else $
            setpage,/landscape,ysize=19.0,xsize=25.0
         cs=1.0
         cst=0.5
         ss=0.5
         xmargin=[7.5,1]
         ymargin=[3.2,1]
      endelse
      colors = ['000000'xl, $
                'C0C0C0'xl, $ ; bad
                '0808D3'xl, $ ; resonant
                '4EAA5A'xl, $ ; scatnear
                '0080FF'xl, $ ; scatextd
                'FF9090'xl, $ ; cen
                '000000'xl, $ ; class
                'FFFFFF'xl ]
      b = (colors and 'FF0000'xl)/'10000'xl
      g = (colors and '00FF00'xl)/'100'xl
      r = (colors and '0000FF'xl)
      colorarr=indgen(n_elements(colors))
      tvlct,r,g,b
   endelse

   if (*state).autoscale then begin
      if (*state).xflip eq 'norm' then $
         (*state).xr = minmax((*(*state).x)) $
      else $
         (*state).xr = maxmin((*(*state).x))

      if (*state).yflip eq 'norm' then $
         (*state).yr = minmax((*(*state).y)) $
      else $
         (*state).yr = maxmin((*(*state).y))
   endif

   title=strmid((*state).qual_select,4,99)
   if title eq '' then title='All objects'

   plot,[0],[1],/nodata,xr=(*state).xr,yr=(*state).yr, $
      xtitle=(*state).xname,ytitle=(*state).yname,charsize=cs, $
      title=title,iso=iso,xmargin=xmargin,ymargin=ymargin

   oplot,(*(*state).x),(*(*state).y),psym=8,symsize=ss

   jdstr,(*(*state).jd)[0],0,jds
   jds = 'Epoch:'+jds
   xyouts,0.01,0.01,jds,charsize=cst,/normal

   ; bad error objects
   pos = strpos((*(*state).orbtype),'ERR2LARGE')
   z=where(pos ge 0,count)
   if count ne 0 then $
      oplot,(*(*state).x)[z],(*(*state).y)[z], $
         psym=8,symsize=ss,color=colorarr[1]

   ; Resonant objects
   pos = strpos((*(*state).orbtype),':')
   z=where(pos ge 0,count)
   if count ne 0 then $
      oplot,(*(*state).x)[z],(*(*state).y)[z], $
         psym=8,symsize=ss,color=colorarr[2]

   ; Scattered objects
   pos = strpos((*(*state).orbtype),'SCATNEAR')
   z=where(pos ge 0,count)
   if count ne 0 then $
      oplot,(*(*state).x)[z],(*(*state).y)[z], $
         psym=8,symsize=ss,color=colorarr[3]

   ; Scattered objects
   pos = strpos((*(*state).orbtype),'SCATEXTD')
   z=where(pos ge 0,count)
   if count ne 0 then $
      oplot,(*(*state).x)[z],(*(*state).y)[z], $
         psym=8,symsize=ss,color=colorarr[4]

   ; Centaur objects
   pos = strpos((*(*state).orbtype),'CEN')
   z=where(pos ge 0,count)
   if count ne 0 then $
      oplot,(*(*state).x)[z],(*(*state).y)[z], $
         psym=8,symsize=ss,color=colorarr[5]

   ; Classical objects
   pos = strpos((*(*state).orbtype),'CLASS')
   z=where(pos ge 0,count)
   if count ne 0 then $
      oplot,(*(*state).x)[z],(*(*state).y)[z], $
         psym=8,symsize=ss,color=colorarr[6]

   if hardcopy eq 1 then begin
      hardcopy
      set_plot,d_name
      !p.font = p_font
   endif else if hardcopy eq -1 then begin
      device,/close
      set_plot,d_name
      !p.font = p_font
   endif else if hardcopy eq -2 then begin
      device,/close
      set_plot,d_name
      !p.font = p_font
   endif

end

pro kboplot_postinfo, state, idx
   if idx lt 0 or idx ge n_elements( (*(*state).x) ) then return

   ; simple stuff
   str = 'Object name '+(*(*state).objectid)[idx]
   str = [str,'']
   newstr = (*state).xname+' = ' + string((*(*state).x)[idx])
   newstr = newstr + ', ' + (*state).yname+' = ' + string((*(*state).y)[idx])
   newstr = strcompress(newstr)
   str = [str,newstr]

   ; Current position information
   tab = string(byte(9))
   cmd = "select * from pos where objectid='"+(*(*state).objectid)[idx]+"';"
   mysqlcmd,(*state).dblun,cmd,answer,nlines
   heads = strsplit(answer[0],tab,/extract)
   words = strsplit(answer[1],tab,/extract)

   str = [str,'']

   z=where(heads eq 'jd')
   jdstr,double(words[z[0]]),0,jds
   newstr = 'Information determined for '+jds+' UT'
   z=where(heads eq 'hv')
   newstr = newstr+'       Hv='+words[z[0]]
   z=where(heads eq 'gv')
   newstr = newstr+', Gv='+words[z[0]]
   str = [str,newstr]
   str = [str,'']

   z=where(heads eq 'ra')
   rastr,double(words[z[0]]),1,ras
   newstr = '  RA = '+ras
   z=where(heads eq 'decl')
   decstr,double(words[z[0]]),0,decs
   newstr = newstr+', Dec = '+decs+'  J2000'
   z=where(heads eq 'vmag')
   newstr = newstr+'        V=' + $
      strcompress(string(float(words[z[0]]),format='(f10.1)'),/remove_all)
   str = [str,newstr]
   str = [str,'']

   z=where(heads eq 'sun')
   newstr = '  r=' + $
      strcompress(string(float(words[z[0]]),format='(f10.2)'),/remove_all)+ $
      ' AU'
   z=where(heads eq 'earth')
   newstr = newstr+',  delta=' + $
      strcompress(string(float(words[z[0]]),format='(f10.2)'),/remove_all)+ $
      ' AU'
   z=where(heads eq 'elong')
   newstr = newstr+',   Sel=' + $
      strcompress(string(float(words[z[0]]),format='(f10.1)'),/remove_all)+ $
      ' deg'
   z=where(heads eq 'phang')
   newstr = newstr+',   phase=' + $
      strcompress(string(float(words[z[0]]),format='(f10.1)'),/remove_all)+ $
      ' deg'
   str = [str,newstr]
   str = [str,'']

   z=where(heads eq 'x')
   newstr = '  x = ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=9)
   z=where(heads eq 'x_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=6)+ $
      ' AU'
   z=where(heads eq 'xv')
   newstr = newstr+'    xv = ' + $
      strn(float(words[z[0]]),format='(e15.4)',length=11)
   z=where(heads eq 'xv_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(e15.4)',length=10)+ $
      ' AU/day'
   str = [str,newstr]

   z=where(heads eq 'y')
   newstr = '  y = ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=9)
   z=where(heads eq 'y_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=6)+ $
      ' AU'
   z=where(heads eq 'yv')
   newstr = newstr+'    yv = ' + $
      strn(float(words[z[0]]),format='(e15.4)',length=11)
   z=where(heads eq 'yv_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(e15.4)',length=10)+ $
      ' AU/day'
   str = [str,newstr]

   z=where(heads eq 'z')
   newstr = '  z = ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=9)
   z=where(heads eq 'z_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=6)+ $
      ' AU'
   z=where(heads eq 'zv')
   newstr = newstr+'    zv = ' + $
      strn(float(words[z[0]]),format='(e15.4)',length=11)
   z=where(heads eq 'zv_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(e15.4)',length=10)+ $
      ' AU/day'
   str = [str,newstr]
   str = [str,'']

   ; Orbital elements
   cmd = "select * from elem where objectid='"+(*(*state).objectid)[idx]+"';"
   mysqlcmd,(*state).dblun,cmd,answer,nlines
   heads = strsplit(answer[0],tab,/extract)
   words = strsplit(answer[1],tab,/extract)

   z=where(heads eq 'semi')
   newstr = '  a    = ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)
   z=where(heads eq 'semi_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)+' AU '
   z=where(heads eq 'manom')
   newstr = newstr+'    M    = ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)
   z=where(heads eq 'manom_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)+' deg'
   str = [str,newstr]

   z=where(heads eq 'ecc')
   newstr = '  ecc  = ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=6)
   z=where(heads eq 'ecc_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=6)+'    '
   z=where(heads eq 'arg')
   newstr = newstr+'    arg  = ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=6)
   z=where(heads eq 'arg_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.4)',length=6)+' deg'
   str = [str,newstr]

   z=where(heads eq 'inc')
   newstr = '  inc  = ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)
   z=where(heads eq 'inc_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)+' deg'
   z=where(heads eq 'node')
   newstr = newstr+'    node = ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)
   z=where(heads eq 'node_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)+' deg'
   str = [str,newstr]
   str = [str,'']

   z=where(heads eq 'peri')
   newstr = '    q  = ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=7)
   z=where(heads eq 'peri_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)+' AU'
   z=where(heads eq 'period')
   newstr = newstr+'      P  = ' + $
      strn(float(words[z[0]]),format='(f10.1)',length=8)
   z=where(heads eq 'period_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.1)',length=6)+' years'
   str = [str,newstr]

   z=where(heads eq 'aphelion')
   newstr = '    Q  = ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=7)
   z=where(heads eq 'aphelion_s')
   newstr = newstr+' +- ' + $
      strn(float(words[z[0]]),format='(f10.2)',length=6)+' AU'
   z=where(heads eq 'epoch')
   jdstr,double(words[z[0]]),0,jds
   newstr = newstr+'      Epoch '+jds
   str = [str,newstr]
   str = [str,'']

   z=where(heads eq 'erra')
   newstr = ' Positional uncertainty  erra = ' + $
      strn(float(words[z[0]]),format='(f12.1)')
   z=where(heads eq 'errb')
   newstr = newstr+',  errb = ' + $
      strn(float(words[z[0]]),format='(f12.1)')+' arcsec'
   str = [str,newstr]

   z=where(heads eq 'lastobs')
   jdlast=double(words[z[0]])
   jdstr,jdlast,100,jds
   newstr = ' Last observed ' + jds
   curjd = systime(/julian,/utc)
   diff = curjd - jdlast
   if diff lt 270 then begin
      newstr = newstr+' (' + $
         strn(fix(diff))+' days ago)'
   endif else begin
      newstr = newstr+' (' + $
         strn(float(diff)/365.25,format='(f10.1)')+' years ago)'
   endelse
   z=where(heads eq 'arc')
   arc=float(words[z[0]])
   if arc lt 45 then begin
      newstr = newstr+'  '+ $
         strn(float(arc),format='(f10.1)')+' day arc'
   endif else if arc lt 270 then begin
      newstr = newstr+'  '+ $
         strn(float(arc)/30.0,format='(f10.1)')+' month arc'
   endif else begin
      newstr = newstr+'  '+ $
         strn(float(arc)/365.25,format='(f10.1)')+' year arc'
   endelse
   z=where(heads eq 'nobs')
   newstr = newstr + ', '+strn(long(words[z[0]]))+' observations'
   str = [str,newstr]
   str = [str,'']

   ; Orbital elements
   cmd = "select * from dclass where objectid='"+(*(*state).objectid)[idx]+"';"
   mysqlcmd,(*state).dblun,cmd,answer,nlines
   heads = strsplit(answer[0],tab,/extract)
   words = strsplit(answer[1],tab,/extract)

   z=where(heads eq 'quality')
   newstr = 'Quality flag = '+strn(long(words[z[0]]))
   str = [str,newstr]
   str = [str,'']

   fmt='(a,3(1x,a9))'
   labl='Type:     '
   z1=where(heads eq 'orbtype')
   z2=where(heads eq 'orbtype_p')
   z3=where(heads eq 'orbtype_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='axisobj   '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='ecceobj   '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='qmin      '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='amean     '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='amin      '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='emean     '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='emax      '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='imean     '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='fracstop  '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='cjmean    '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   str = [str,'']

   labl='libcent0  '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libamp0   '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libcent1  '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libamp1   '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libcent2  '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libamp2   '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libcent3  '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libamp3   '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libcent4  '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='libamp4   '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   str = [str,'']

   labl='kozaimean '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]
   labl='kozaiamp  '
   z1=where(heads eq strtrim(labl,2))
   z2=where(heads eq strtrim(labl,2)+'_p')
   z3=where(heads eq strtrim(labl,2)+'_m')
   newstr = string(labl,words[z1[0]],words[z2[0]],words[z3[0]],format=fmt)
   str = [str,newstr]

   widget_control, (*state).infoid, set_value=str
end

pro kboplot_scalename, range, name
   diff = range[1] - range[0]
   if diff le 1.0 then begin
      name = strcompress(string(range[0],format='(f10.3)'),/remove_all)
      name += '-'
      name += strcompress(string(range[1],format='(f10.3)'),/remove_all)
   endif else if diff le 1000.0 then begin
      name = strcompress(string(fix(range[0]),format='(i10)'),/remove_all)
      name += '-'
      name += strcompress(string(fix(ceil(range[1])),format='(i10)'),/remove_all)
   endif else begin
      name = 'other'
   endelse
end

pro kboplot_eve, event

   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   case event_name of

      'THE_MENU': begin
         case event.value of

            'Absicca': begin
               kboplot_choosedata, state, 'Abscissa'
               (*state).autoscale=1
            end

            'EPS Plot': begin
               file = (*state).xaxis+':'+(*state).yaxis
               if (*state).qs ne '' then file += ':'+(*state).qs
               if not (*state).autoscale then begin
                  kboplot_scalename,(*state).xr,name
                  file += ':'+name + ','
                  kboplot_scalename,(*state).yr,name
                  file += name
               endif
               file += '.eps'
               print,'plot saved to ',file
               kboplot_plot,state,-2,file=file
               return
            end

            'Ordinate': begin
               kboplot_choosedata, state, 'Ordinate'
               (*state).autoscale=1
            end

            'Print': begin
               kboplot_plot,state,1
               return
            end

            'PS Plot': begin
               file = (*state).xaxis+':'+(*state).yaxis
               if (*state).qs ne '' then file += ':'+(*state).qs
               if not (*state).autoscale then begin
                  kboplot_scalename,(*state).xr,name
                  file += ':'+name + ','
                  kboplot_scalename,(*state).yr,name
                  file += name
               endif
               file += '.ps'
               print,'plot saved to ',file
               kboplot_plot,state,-1,file=file
               return
            end

         ; Quality submenu
            'Q=3': begin
               (*state).qual_select = 'and quality=3 ';
               (*state).qs = 'q_eq_3';
               (*state).dirty=1
            end

            'Q>1': begin
               (*state).qual_select = 'and quality>1 ';
               (*state).qs = 'q_gt_1';
               (*state).dirty=1
            end

            'Q>0': begin
               (*state).qual_select = 'and quality>0 ';
               (*state).qs = 'q_gt_0';
               (*state).dirty=1
            end

            'Q=2': begin
               (*state).qual_select = 'and quality=2 ';
               (*state).qs = 'q_eq_2';
               (*state).dirty=1
            end

            'Q=1': begin
               (*state).qual_select = 'and quality=1 ';
               (*state).qs = 'q_eq_1';
               (*state).dirty=1
            end

            'Q=0': begin
               (*state).qual_select = 'and quality=0 ';
               (*state).qs = 'q_eq_0';
               (*state).dirty=1
            end

            'Show All': begin
               (*state).qual_select = '';
               (*state).qs = '';
               (*state).dirty=1
            end

            'Reset Scaling': begin
               (*state).autoscale=1
            end

            'Snap to JPEG': begin
               widget_control,(*state).drawwin, get_value=winnum
               wset,winnum
               img=tvrd(/true)
               write_jpeg,'kboplot.jpg',img,true=1,quality=100
               print,'current plot saved to kboplot.jpg'
               return
            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               RETURN
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

         kboplot_plot, state, 0

      end ; THE_MENU

      'Mainbase': begin

         ; Use if you have other widgets on screen, need to take off their
         ;   size from the event x,y size.
;         info=widget_info((*state).colbaseid,/geometry)
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

         ; Use if draw window is only thing in the tool.
         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

;        refresh window here
         kboplot_plot, state, 0
      end

      'Window': begin
         if event.type le 1 then begin
            if event.press then begin
               (*state).xpress = event.x
               (*state).ypress = event.y
            endif else begin
               if event.x ne (*state).xpress and event.y ne (*state).ypress then begin
                  res1=convert_coord((*state).xpress,(*state).ypress,/device,/to_data)
                  res2=convert_coord(event.x,event.y,/device,/to_data)
                  if (*state).xflip eq 'norm' then $
                     (*state).xr= [res1[0]>(*state).xr[0],res2[0]<(*state).xr[1]] $
                  else $
                     (*state).xr= [res1[0]<(*state).xr[0],res2[0]>(*state).xr[1]]
                  if (*state).yflip eq 'norm' then $
                     (*state).yr= [res1[1]>(*state).yr[0],res2[1]<(*state).yr[1]] $
                  else $
                     (*state).yr= [res1[1]<(*state).yr[0],res2[1]>(*state).yr[1]]
                  (*state).autoscale=0
                  kboplot_plot, state, 0
               endif else begin
                  res=convert_coord(event.x,event.y,/device,/to_data)
                  diff = sqrt( ((*(*state).x) - res[0])^2 + $
                               ((*(*state).y) - res[1])^2 ) 
                  idx = where(diff eq min(diff))
                  kboplot_postinfo,state,idx[0]
               endelse
            endelse
         endif else if event.type eq 5 then begin
            print,'ASCII: ',event.ch
         endif else if event.type eq 6 then begin
            print,'non-ASCII: ',event.key
         endif else begin
            help, event, /STRUCTURE
         endelse
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro kboplot

   ; optional
   if xregistered('kboplot') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. KBOPLOT cannot be started.'
      return
   endif

   ;Define the main base.
   mainbase = widget_base( TITLE='KBOPLOT: KBO database plotter', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Print',$
                     '0\EPS Plot',$
                     '0\PS Plot',$
                     '2\Exit',$
                     '1\Setup',$
                     '0\Absicca',$
                     '2\Ordinate',$
                     '1\Quality',$
                     '0\Q=3', $
                     '0\Q>1', $
                     '0\Q>0', $
                     '0\Q=2', $
                     '0\Q=1', $
                     '0\Q=0', $
                     '2\Show All', $
                     '1\Tools',$
                     '0\Snap to JPEG', $
                     '0\Reset Scaling', $
                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase)

   win1 = widget_draw( base, XSIZE=1024, YSIZE=768, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Window', KEYBOARD_EVENTS=2 )

   openmysql,dblun,'des'

   choices = [['elem.semi', 'semi-major axis',      'norm',''], $
              ['elem.ecc',  'eccentricity',         'norm',''], $
              ['elem.inc',  'inclination',          'norm',''], $
              ['elem.peri', 'Perihelion Distance',  'norm',''], $
              ['elem.aphelion', 'Aphelion Distance', 'norm',''], $
              ['elem.period', 'Period',             'norm',''], $
              ['elem.erra', 'semi-major axis of error ellipse', 'norm',''], $
              ['elem.errb', 'semi-minor axis of error ellipse', 'norm',''], $
              ['elem.arc',  'Astrometric arc (days)', 'norm',''], $
              ['elem.nobs', 'Number of observations', 'norm',''], $
              ['dclass.cj', 'Tisserand parameter',  'norm',''], $
              ['pos.vmag',  'Vmag',                 'flip',''], $
              ['pos.hv',    'HV',                   'flip',''], $
              ['pos.sun',   'Heliocentric Distance','norm','iso'], $
              ['pos.earth', 'Geocentric Distance',  'norm','iso'], $
              ['pos.elong', 'Solar Elongation',     'norm',''], $
              ['pos.ra',    'Right Ascension (J2000)', 'norm',''], $
              ['pos.dec',   'Declination (J2000)',  'norm',''], $
              ['pos.x',     'X',                    'norm','iso'], $
              ['pos.y',     'Y',                    'norm','iso'], $
              ['pos.z',     'Z',                    'norm','iso'], $
              ['pos.xv',    'X velocity',           'norm','iso2'], $
              ['pos.yv',    'Y velocity',           'norm','iso2'], $
              ['pos.zv',    'Z velocity',           'norm','iso2'] ]

   infobase = widget_base(group_leader=mainbase,/column, $
                           title='Object Information Window')

   infoid   = widget_text(infobase, xsize=80, ysize=43, /scroll)


   state = ptr_new({ $

      ; Data and information in the widget
      choices: choices, $
      dblun: dblun, $
      dirty: 1, $                ; if set, need to rerun query
      qual_select: '', $         ; Orbit quality select string
      qs: '', $                  ; quality select flag label
      autoscale: 1, $            ; flag, if set, auto-scale plot
      xpress: -1L, $
      xr: [0.,1.], $
      ypress: -1L, $
      yr: [0.,1.], $

      jd:    ptr_new(), $
      x:     ptr_new(), $
      xaxis: choices[0,0], $
      xname: choices[1,0], $
      xflip: choices[2,0], $
      xiso:  choices[3,0], $
      y:     ptr_new(), $
      yaxis: choices[0,1], $
      yname: choices[1,1], $
      yflip: choices[2,1], $
      yiso:  choices[3,1], $
      objectid: ptr_new(), $
      orbtype: ptr_new(), $
      quality: ptr_new(), $

      ; Widget ids
      drawwin: win1, $           ; ID of main draw window
      infoid:  infoid, $         ; ID of information window

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ;Realize the info base.
   widget_control, infobase, /REALIZE

   ;Generate the initial default plot
   kboplot_plot, state, 0

   ; Give control to the XMANAGER.
   xmanager, 'kboplot', mainbase, $
             EVENT_HANDLER='kboplot_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='kboplot_cleanup'

end

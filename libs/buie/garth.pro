;+
; NAME:
;  garth
; PURPOSE:
;  Visual inspector of moving target triplet detections.
; DESCRIPTION:
;
;  First, some vocabulary:
;
;  Object - Putative detection of a moving source.  Consists of three positions
;              measured from three images taken at different times.
;
;  Field  - A single location in the sky that has been imaged at three
;              different times.  Each field has a name and three files names
;              for the three images.  Each field also has an assocated list
;              of objects.
;
;  Triplet List - This is a list of fields, ie., a list of field names and
;                    the file names for the images.
;
;  To begin processing the object list(s), there must be a triplet list file.
;  These files end with a .match suffix.  For every entry in the .match file
;  there must be an object list file.  These are named FLDNAM.obj where FLDNAM
;  is the name of the field found in the first column of the .match file.
;   ex:
;        field_1  980115.001 980115.051 980115.101
;        field_2  980115.002 980115.052 980115.102
;
;  could be two list from a .match file, perhaps named 980115.match.  There
;  would then be two related files, field_1.obj and field_2.obj that would
;  have the object list.  The object list files are created by the program
;  LINKOBJ and are not intended to be directly edited (though with care, this
;  is possible).
;
; CATEGORY:
;  Widgets
; CALLING SEQUENCE:
;  garth
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;    KEYLIST=      : Name of a file containing a correspondence list. This list
;                    associates a set of standard names with the actual keyword
;                    names found in a FITS file header. If this keyword is
;                    omitted, a default list is used, as if a file with the
;                    following contents had been supplied:
;                     AIRMASS   K  AIRMASS
;                     DATE      K  DATE-OBS
;                     DATETMPL  T  DD-MM-YYYY
;                     EXPDELTA  V  0.0
;                     EXPTIME   K  EXPTIME
;                     FILTER    K  FILTERS
;                     FILENAME  K  CCDFNAME
;                     OBJECT    K  OBJECT
;                     UT        K  UT 
;                    The middle column is a flag. It may be K, for Keyword,
;                    T, for Template, or V, for Value. If it is V, the contents
;                    of the third field on that line should make sense for the
;                    name in the first field.
;
;  PATH      - Optional path for original image directory.
;                If not specified, the current directory is used.
;
;  SUBSIZE   - Size of the sub-images to show (default = 249 pixels)
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
;  This widget tool must be run on a 24-bit color display.
;
; PROCEDURE:
;
;------------------------------------------------------------------------------
;  Internal Support routines.
;
;  garth_hardcopy  : Hardcopy of current field
;  garth_load      : Load a new match file
;  garth_newfield  : Setup information for new field
;  garth_newobject : Setup information for new object
;  garth_objprint  : Hardcopy of current object
;  garth_svobjlist : Save validation flags for current field
;
;  garth_eve       : Event handler
;
;------------------------------------------------------------------------------
; MODIFICATION HISTORY:
;  1998/03/11, Written by Marc W. Buie, Lowell Observatory
;  98/06/23, MWB, added hardcopy functions.
;  98/09/31, MWB, added support for multi-extension group FITS files
;  98/10/22, MWB, added SUBSIZE keyword.
;  98/10/24, MWB, added pick triplet button function
;  2004/9/21, MWB, removed obsolete call to Findfile
;
;-
pro garth_objprint, state, COLOR=color

   IF state.matchfile EQ '' THEN RETURN
   IF state.objnum EQ -1 THEN RETURN

   ; Retrieve the current list of objects
   WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
   WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY
   WIDGET_CONTROL, state.wina,   GET_UVALUE=objdat, /NO_COPY
   old_dev = !d.name
   p_font = !p.font
   widget_control, state.queueid, get_value=queue
   queue=queue[0]

   set_plot,'PS'
   setpage,/portrait
   device,/Helvetica,bits=8
   if keyword_set(color) then device,/color
   !p.font=0

   if keyword_set(color) then begin
      ccolor0='00FF80'XL
      ccolor1='0080FF'XL
   endif else begin
      color0=0
      color1=0
   endelse

   allcirc = oblist.crate[state.objnum]*oblist.dtc gt 10.0

   yoff = 1.5
   ss = 1.5

   w = 4.60
   x = 6.50

   xyouts,(x+w*0.5)*1000,23.0*1000,'LONEOS Discovery Images of 1998 MQ',align=0.5,/device

   y = 3.45*w + yoff
   tv,255-objdat.suba,x*1000,y*1000,xsize=w*1000,ysize=w*1000,/device
   pos=[x,y,x+w,y+w]*1000
   plot,[0],position=pos,xr=[0,state.wid-1],xstyle=5,yr=[0,state.wid-1],ystyle=5, $
      /nodata,/noerase,/device
   oplot,[objdat.xa1],[objdat.ya1],psym=8,color=color0,symsize=ss
   if allcirc then oplot,[objdat.xb1],[objdat.yb1],psym=8,color=color1,symsize=ss
   if allcirc then oplot,[objdat.xc1],[objdat.yc1],psym=8,color=color1,symsize=ss
   jdstr,oblist.infoa.jd,0,date
   xyouts,(x+w*0.5)*1000,(y+w+0.2)*1000,date,/device,align=0.5

   y = 2.3*w + yoff
   tv,255-objdat.subb,x*1000,y*1000,xsize=w*1000,ysize=w*1000,/device
   pos=[x,y,x+w,y+w]*1000
   plot,[0],position=pos,xr=[0,state.wid-1],xstyle=5,yr=[0,state.wid-1],ystyle=5, $
      /nodata,/noerase,/device
   if allcirc then oplot,[objdat.xa1],[objdat.ya1],psym=8,color=color1,symsize=ss
   oplot,[objdat.xb1],[objdat.yb1],psym=8,color=color0,symsize=ss
   if allcirc then oplot,[objdat.xc1],[objdat.yc1],psym=8,color=color1,symsize=ss
   jdstr,oblist.infob.jd,0,date
   xyouts,(x+w*0.5)*1000,(y+w+0.2)*1000,date,/device,align=0.5

   y = 1.15*w + yoff
   tv,255-objdat.subc,x*1000,y*1000,xsize=w*1000,ysize=w*1000,/device
   pos=[x,y,x+w,y+w]*1000
   plot,[0],position=pos,xr=[0,state.wid-1],xstyle=5,yr=[0,state.wid-1],ystyle=5, $
      /nodata,/noerase,/device
   if allcirc then oplot,[objdat.xa1],[objdat.ya1],psym=8,color=color1,symsize=ss
   if allcirc then oplot,[objdat.xb1],[objdat.yb1],psym=8,color=color1,symsize=ss
   oplot,[objdat.xc1],[objdat.yc1],psym=8,color=color0,symsize=ss
   jdstr,oblist.infoc.jd,0,date
   xyouts,(x+w*0.5)*1000,(y+w+0.2)*1000,date,/device,align=0.5

   if keyword_set(color) then begin
      y = 0.0 + yoff
      tv,[[[objdat.suba]],[[objdat.subc]],[[objdat.subc]]],true=3, $
         x*1000,y*1000,xsize=w*1000,ysize=w*1000,/device
;      if allcirc then oplot,[objdat.xa1],[objdat.ya1],psym=8,color=color1,symsize=ss
;      if allcirc then oplot,[objdat.xb1],[objdat.yb1],psym=8,color=color1,symsize=ss
;      if allcirc then oplot,[objdat.xc1],[objdat.yc1],psym=8,color=color1,symsize=ss
   endif

   device,/close
   file='idl.ps'
   if queue ne '<default>' then begin
      cmd = 'lpr -P'+queue+' '+file
   endif else begin
      cmd = 'lpr '+file
   endelse
;   spawn,cmd
   set_plot,old_dev
   !p.font = p_font
   WIDGET_CONTROL, state.wina,   SET_UVALUE=objdat, /NO_COPY
   WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
   WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
   
end

pro garth_hardcopy, state, fieldnum, exten

   IF state.matchfile EQ '' THEN RETURN

   WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY
   matchnum = match.num
   WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY

   if fieldnum ge 0 and fieldnum lt matchnum and exten le state.numext then begin

      if fieldnum ne state.fieldnum or exten ne state.curext then begin
         garth_newfield,state,fieldnum,exten
         garth_newobject,state,0
      endif

      WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY
      WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY

      widget_control, state.queueid, get_value=queue
      queue=queue[0]

      print,'hardcopy of field (',match.obj[state.fieldnum], $
             ')  ',match.fna[state.fieldnum],' to print queue ',queue

      if oblist.nobj eq -1  then begin
         ima=readfits(state.path+match.fna[state.fieldnum],hdra,/silent)
         hdrb=headfits(state.path+match.fnb[state.fieldnum])
         hdrc=headfits(state.path+match.fnc[state.fieldnum])
         parsekey,hdra,state.hdrlist,infoa
         parsekey,hdrb,state.hdrlist,infob
         parsekey,hdrc,state.hdrlist,infoc
         sz=size(ima)
         nx = sz[1]
         ny = sz[2]
         oblist = { nobj:-1, ima:temporary(ima), nx:nx, $
                    ny:ny, infoa:infoa, infob:infob, infoc:infoc }
      endif

      old_dev = !d.name
      p_font = !p.font

      skysclim,oblist.ima,lowval,hival,meanval,sigma
      WIDGET_CONTROL, state.minsigid, GET_VALUE=lowsig
      WIDGET_CONTROL, state.maxsigid, GET_VALUE=hisig

      lowval = meanval+lowsig*sigma
      hival  = meanval+hisig*sigma
      hardim,oblist.ima,lowval,hival,autosize=2,/negative,/noclose,/noprint, $
         position=[1.2,5.0],title=match.obj[state.fieldnum],width=19.7,/silent

      ; overplot object positions
      if oblist.nobj ne -1 then begin
         z=where(oblist.flag eq 'y',count)
         if count ne 0 then begin
            oplot,[oblist.xa1[z]],[oblist.ya1[z]],psym=8
;            oplot,[oblist.xb1[z]],[oblist.yb1[z]],psym=8
            oplot,[oblist.xc1[z]],[oblist.yc1[z]],psym=8
            for i=0,count-1 do begin
               xyouts,oblist.xa1[z[i]],oblist.ya1[z[i]]+10, $
                  strcompress(z[i]),align=0.5
            endfor
         endif
      endif

      ; Annotate bottom of page with field information
      xleft = 2.5*1000.0
      ypos  = 3.5*1000.0
      dy    = -0.5*1000.0
      dx    =  0.5*1000.0
      xyouts,xleft,ypos,match.obj[state.fieldnum],/device
      jdstr,oblist.infoa.jd,100,date
      xyouts,xleft+dx*6,ypos,date,/device

      ypos=ypos+dy*1.2
      xyouts,xleft+dx,ypos,match.fna[state.fieldnum],/device,charsize=0.8
      jdstr,oblist.infoa.jd,0,time
      xyouts,xleft+dx*6,ypos,strmid(time,11,99),/device,charsize=0.8
      ypos=ypos+dy*0.8
      xyouts,xleft+dx,ypos,match.fnb[state.fieldnum],/device,charsize=0.8
      jdstr,oblist.infob.jd,0,time
      xyouts,xleft+dx*6,ypos,strmid(time,11,99),/device,charsize=0.8
      ypos=ypos+dy*0.8
      xyouts,xleft+dx,ypos,match.fnc[state.fieldnum],/device,charsize=0.8
      jdstr,oblist.infoc.jd,0,time
      xyouts,xleft+dx*6,ypos,strmid(time,11,99),/device,charsize=0.8

      ypos=ypos+dy*1.5
      ofstr = string(state.fieldnum+1,match.num,format='(i10," of ",i10)')
      ofstr = strcompress(strtrim(ofstr,2))
      xyouts,xleft,ypos,/device,'Field '+ofstr

      ypos=ypos+dy*1.5
      if oblist.nobj eq -1 then begin
         xyouts,xleft,ypos,'No objects found automatically.',/device
      endif else begin
         xyouts,xleft,ypos,/device, $
            string(oblist.nobj,format='(i3)')+' total objects found'
         ypos=ypos+dy
         xyouts,xleft,ypos,/device, $
            string(oblist.nyes,format='(i3)')+' verified objects'
         ypos=ypos+dy
         xyouts,xleft,ypos,/device, $
            string(oblist.nno,format='(i3)')+' false positives'
         if oblist.nunk ne 0 then begin
            ypos=ypos+dy
            xyouts,xleft,ypos,/device, $
               string(oblist.nunk,format='(i3)')+' unknown (not yet validated)'
         endif
      endelse

      device,/close
      file='idl.ps'
      if queue ne '<default>' then begin
         cmd = 'lpr -P'+queue+' '+file
      endif else begin
         cmd = 'lpr '+file
      endelse
      spawn,cmd
      set_plot,old_dev
      !p.font=p_font

      WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
      WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
   endif

end

PRO garth_svobjlist, state
   IF state.matchfile EQ '' THEN RETURN
   IF state.objnum EQ -1 THEN RETURN

   ; Retrieve the triplet list information
   WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY

   IF oblist.nobj EQ -1 THEN BEGIN
      WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
      return
   ENDIF

   WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY

   IF oblist.dirtyflags THEN BEGIN
      fnobj = strlowcase(match.obj[state.fieldnum])+state.exttag+'.obj'
      print,'Updating validation flags for ',fnobj
      openw,lun,fnobj,/get_lun
      printf,lun,match.fna[state.fieldnum],' ', $
                 match.fnb[state.fieldnum],' ', $
                 match.fnc[state.fieldnum]
      printf,lun,oblist.xoffb,oblist.yoffb,oblist.xoffc,oblist.yoffc, $
             format='(4(1x,f8.2))'
      FOR i=0,oblist.nobj-1 DO BEGIN
         printf,lun,oblist.xa[i],oblist.ya[i], $
                    oblist.xb[i],oblist.yb[i], $
                    oblist.xc[i],oblist.yc[i], $
                    oblist.flag[i],format='(6(1x,f8.2),1x,a1)'
      ENDFOR
      free_lun,lun
      oblist.dirtyflags = 0
   ENDIF

   WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
   WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
END

PRO garth_newobject, state, newobject
   bel = STRING( 7B )

   ; Retrieve the current list of objects
   WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
   WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY
   WIDGET_CONTROL, state.wina,   GET_UVALUE=objdat, /NO_COPY

   ; Make sure there are some objects to deal with (this really shouldn't fail).
   IF oblist.nobj le 0 THEN BEGIN
      print,'Object list is empty',bel
      WIDGET_CONTROL, state.wina, GET_VALUE=winnum
      WSET, winnum
      erase
      WIDGET_CONTROL, state.winb, GET_VALUE=winnum
      WSET, winnum
      erase
      WIDGET_CONTROL, state.winc, GET_VALUE=winnum
      WSET, winnum
      erase
      WIDGET_CONTROL, state.winrgb, GET_VALUE=winnum
      WSET, winnum
      erase
      WIDGET_CONTROL, state.mainbase, UPDATE=0
      WIDGET_CONTROL, state.objectof,  SET_VALUE='No objects'
      WIDGET_CONTROL, state.barateid,  SET_VALUE=''
      WIDGET_CONTROL, state.carateid,  SET_VALUE=''
      WIDGET_CONTROL, state.mainbase, UPDATE=1

      widget_control, state.prevtrip, $
         sensitive = state.fieldnum ne 0 or state.curext gt 1

      widget_control, state.nexttrip, $
         sensitive = state.fieldnum ne match.num-1 or $
                     state.curext ne state.numext

      widget_control, state.hardobjid, sensitive=0
      widget_control, state.chardobjid, sensitive=0
      WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
      WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
      state.objnum = -1
      return
   ENDIF

   ; if object number is negative or too big, set to last object
   IF newobject lt 0 or newobject ge oblist.nobj THEN newobject = oblist.nobj-1

   state.objnum = newobject
   widget_control, state.prevtrip, $
      sensitive = state.fieldnum ne 0 or $
                  state.objnum   ne 0 or $
                  state.curext   gt 1
   widget_control, state.nexttrip, $
      sensitive = state.fieldnum ne match.num-1   or $
                  state.objnum   ne oblist.nobj-1 or $
                  state.curext   ne state.numext
   widget_control, state.hardobjid, sensitive=1
   widget_control, state.chardobjid, sensitive=1
   widget_control, state.picktrip, sensitive=1

   ; Put the new object information on the screen.
   WIDGET_CONTROL, state.mainbase, UPDATE=0
   str = string(newobject+1,oblist.nobj,format='(i10," of ",i10)')
   str = strcompress(strtrim(str,2))
   WIDGET_CONTROL, state.objectof,  SET_VALUE=str
   str = string(oblist.brate[newobject],oblist.bdir[newobject], $
                  format='(f10.2," pix/hr, ",f10.1,"deg")')
   str = strcompress(strtrim(str,2))
   WIDGET_CONTROL, state.barateid,  SET_VALUE=str
   str = string(oblist.crate[newobject],oblist.cdir[newobject], $
                  format='(f10.2," pix/hr, ",f10.1,"deg")')
   str = strcompress(strtrim(str,2))
   WIDGET_CONTROL, state.carateid,  SET_VALUE=str
   WIDGET_CONTROL, state.mainbase, UPDATE=1

   IF oblist.flag[state.objnum] eq '?' THEN nval=0
   IF oblist.flag[state.objnum] eq 'y' THEN nval=1
   IF oblist.flag[state.objnum] eq 'n' THEN nval=2
   WIDGET_CONTROL, state.stateid, SET_VALUE=nval

   ; grab sub-images centered on the B position
   xc = fix(oblist.xb1[newobject]+0.5)
   yc = fix(oblist.yb1[newobject]+0.5)
   suba=fltarr(state.wid,state.wid)
   subb=fltarr(state.wid,state.wid)
   subc=fltarr(state.wid,state.wid)
   i2a = 0
   i3a = state.wid-1
   j2a = 0
   j3a = state.wid-1
   i2b = 0
   i3b = state.wid-1
   j2b = 0
   j3b = state.wid-1
   i2c = 0
   i3c = state.wid-1
   j2c = 0
   j3c = state.wid-1

   ; extraction section for A
   i0a = max([xc - state.dw,0])
   i1a = min([xc + state.dw,oblist.nx-1])
   j0a = max([yc - state.dw,0])
   j1a = min([yc + state.dw,oblist.ny-1])

   IF i0a eq 0 THEN i2a=i3a-(i1a-i0a)
   IF j0a eq 0 THEN j2a=j3a-(j1a-j0a)
   IF i1a eq oblist.nx-1 THEN i3a=i1a-i0a
   IF j1a eq oblist.ny-1 THEN j3a=j1a-j0a

   ; extraction section for B
   i0b = max([xc - state.dw + fix(oblist.xoffb+0.5),0])
   i1b = min([xc + state.dw + fix(oblist.xoffb+0.5),oblist.nx-1])
   j0b = max([yc - state.dw + fix(oblist.yoffb+0.5),0])
   j1b = min([yc + state.dw + fix(oblist.yoffb+0.5),oblist.ny-1])

   IF i0b eq 0 THEN i2b=i3b-(i1b-i0b)
   IF j0b eq 0 THEN j2b=j3b-(j1b-j0b)
   IF i1b eq oblist.nx-1 THEN i3b=i1b-i0b
   IF j1b eq oblist.ny-1 THEN j3b=j1b-j0b

   ; extraction section for C
   i0c = max([xc - state.dw + fix(oblist.xoffc+0.5),0])
   i1c = min([xc + state.dw + fix(oblist.xoffc+0.5),oblist.nx-1])
   j0c = max([yc - state.dw + fix(oblist.yoffc+0.5),0])
   j1c = min([yc + state.dw + fix(oblist.yoffc+0.5),oblist.ny-1])

   IF i0c eq 0 THEN i2c=i3c-(i1c-i0c)
   IF j0c eq 0 THEN j2c=j3c-(j1c-j0c)
   IF i1c eq oblist.nx-1 THEN i3c=i1c-i0c
   IF j1c eq oblist.ny-1 THEN j3c=j1c-j0c

   suba[i2a:i3a,j2a:j3a]=oblist.ima[i0a:i1a,j0a:j1a]
   subb[i2b:i3b,j2b:j3b]=oblist.imb[i0b:i1b,j0b:j1b]
   subc[i2c:i3c,j2c:j3c]=oblist.imc[i0c:i1c,j0c:j1c]

   ; Deterine the sky scaling for the subimages.
   WIDGET_CONTROL, state.minsigid, GET_VALUE=dm
   WIDGET_CONTROL, state.maxsigid, GET_VALUE=dp
   dm = float(dm[0])
   dp = float(dp[0])

   z=where(suba ne 0.0)
   skysclim,suba[z],lowval,hival,amean,asig
   alowval=amean + dm*asig
   ahival =amean + dp*asig
   suba=bytscl(temporary(suba),min=alowval,max=ahival,top=255)

   z=where(subb ne 0.0)
   skysclim,subb[z],lowval,hival,bmean,bsig
   blowval=bmean + dm*bsig
   bhival =bmean + dp*bsig
   subb=bytscl(temporary(subb),min=blowval,max=bhival,top=255)

   z=where(subc ne 0.0)
   skysclim,subc[z],lowval,hival,cmean,csig
   clowval=cmean + dm*csig
   chival =cmean + dp*csig
   subc=bytscl(temporary(subc),min=clowval,max=chival,top=255)

   allcirc = oblist.crate[newobject]*oblist.dtc gt 10.0
   xa1=oblist.xa1[newobject]-xc+state.dw
   ya1=oblist.ya1[newobject]-yc+state.dw
   xb1=oblist.xb1[newobject]-xc+state.dw
   yb1=oblist.yb1[newobject]-yc+state.dw
   xc1=oblist.xc1[newobject]-xc+state.dw
   yc1=oblist.yc1[newobject]-yc+state.dw
   ccolor0='00FF80'XL
   ccolor1='0080FF'XL

   WIDGET_CONTROL, state.wina, GET_VALUE=winnum
   WSET, winnum
   if state.bf eq 1 then tv,suba $
   else tv,rebin(suba,state.wid*state.bf,state.wid*state.bf,/sample)
   plots,xa1*state.bf,ya1*state.bf,psym=8,/device,symsize=2.5,color=ccolor0
   if allcirc then plots,xb1*state.bf,yb1*state.bf,psym=8,/device,symsize=2.5,color=ccolor1
   if allcirc then plots,xc1*state.bf,yc1*state.bf,psym=8,/device,symsize=2.5,color=ccolor1

   WIDGET_CONTROL, state.winb, GET_VALUE=winnum
   WSET, winnum
   if state.bf eq 1 then tv,subb $
   else tv,rebin(subb,state.wid*state.bf,state.wid*state.bf,/sample)
   if allcirc then plots,xa1*state.bf,ya1*state.bf,psym=8,/device,symsize=2.5,color=ccolor1
   plots,xb1*state.bf,yb1*state.bf,psym=8,/device,symsize=2.5,color=ccolor0
   if allcirc then plots,xc1*state.bf,yc1*state.bf,psym=8,/device,symsize=2.5,color=ccolor1

   WIDGET_CONTROL, state.winc, GET_VALUE=winnum
   WSET, winnum
   if state.bf eq 1 then tv,subc $
   else tv,rebin(subc,state.wid*state.bf,state.wid*state.bf,/sample)
   if allcirc then plots,xa1*state.bf,ya1*state.bf,psym=8,/device,symsize=2.5,color=ccolor1
   if allcirc then plots,xb1*state.bf,yb1*state.bf,psym=8,/device,symsize=2.5,color=ccolor1
   plots,xc1*state.bf,yc1*state.bf,psym=8,/device,symsize=2.5,color=ccolor0

   WIDGET_CONTROL, state.winrgb, GET_VALUE=winnum
   WSET, winnum
   if state.bf eq 1 then $
      tv,[[[suba]],[[subc]],[[subc]]],true=3 $
   else $
      tv,[[[rebin(suba,state.wid*state.bf,state.wid*state.bf,/sample)]], $
          [[rebin(subc,state.wid*state.bf,state.wid*state.bf,/sample)]], $
          [[rebin(subc,state.wid*state.bf,state.wid*state.bf,/sample)]]],true=3
   IF allcirc THEN BEGIN
      plots,xa1*state.bf,ya1*state.bf,psym=8,/device,symsize=2.5,color=ccolor0
      plots,xb1*state.bf,yb1*state.bf,psym=8,/device,symsize=2.5,color=ccolor1
      plots,xc1*state.bf,yc1*state.bf,psym=8,/device,symsize=2.5,color=ccolor0
   ENDIF

   objdat = 0.0   ; release memory before recreating the structure.
   objdat = { $
      suba:    suba, $
      subb:    subb, $
      subc:    subc, $
      xa1:     xa1, $
      ya1:     ya1, $
      xb1:     xb1, $
      yb1:     yb1, $
      xc1:     xc1, $
      yc1:     yc1 $
      }

   WIDGET_CONTROL, state.wina,   SET_UVALUE=objdat, /NO_COPY
   WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
   WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY

END

PRO garth_newfield, state, newfield, newexten

   bel = STRING( 7B )

   IF state.matchfile EQ '' THEN RETURN

   IF newfield ne state.fieldnum or newexten ne state.curext THEN BEGIN

      garth_svobjlist, state

      ; Retrieve the triplet list information
      WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY

      ; Check for out of range field number
      IF newfield lt 0 THEN BEGIN
         print,'You are already on the first field, you cannot backup from here.',bel
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         RETURN
      ENDIF
      IF newfield ge match.num THEN BEGIN
         print,'No more fields, you are on the last one already.',bel
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         RETURN
      ENDIF

      state.fieldnum = newfield

      IF not exists(state.path+match.fna[newfield]) THEN BEGIN
         print,'File ',state.path+match.fna[newfield],' was not found.',bel
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         return
      ENDIF
      IF not exists(state.path+match.fnb[newfield]) THEN BEGIN
         print,'File ',state.path+match.fnb[newfield],' was not found.',bel
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         return
      ENDIF
      IF not exists(state.path+match.fnc[newfield]) THEN BEGIN
         print,'File ',state.path+match.fnc[newfield],' was not found.',bel
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         return
      ENDIF

      ; Check header of image to see if it is a multi-extension image.
      hdr=headfits(state.path+match.fna[newfield])
      state.numext=sxpar(hdr,'NEXTEND') > 1
      if state.numext eq 1 then state.curext=0 else state.curext=newexten > 1
      widget_control, state.pickexten, sensitive=state.numext gt 1

      widget_control, state.prevfield, sensitive=state.fieldnum ne 0
      widget_control, state.nextfield, sensitive=state.fieldnum ne match.num-1

      WIDGET_CONTROL, state.mainbase, UPDATE=0
      ofstr = string(newfield+1,match.num,format='(i10," of ",i10)')
      ofstr = strcompress(strtrim(ofstr,2))
      WIDGET_CONTROL, state.fieldof,  SET_VALUE=ofstr
      WIDGET_CONTROL, state.fieldid,  SET_VALUE=match.obj[newfield]
      if state.curext eq 0 then $
         ofstr = 'n/a' $
      else $
         ofstr = string(state.curext,state.numext,format='(i10," of ",i10)')
      ofstr = strcompress(strtrim(ofstr,2))
      WIDGET_CONTROL, state.extenid,  SET_VALUE=ofstr
      WIDGET_CONTROL, state.frameaid, SET_VALUE=match.fna[newfield]
      WIDGET_CONTROL, state.framebid, SET_VALUE=match.fnb[newfield]
      WIDGET_CONTROL, state.framecid, SET_VALUE=match.fnc[newfield]
      WIDGET_CONTROL, state.mainbase, UPDATE=1

      widget_control, state.prevext, $
         sensitive=state.numext ne 1 and state.curext ne 1
      widget_control, state.nextext, $
         sensitive=state.numext ne 1 and state.curext ne state.numext

      IF state.curext eq 0 THEN BEGIN
         state.exttag = ''
      ENDIF ELSE BEGIN
         state.exttag = 'x'+strb36(state.curext)
      ENDELSE

      ; Construct file name where the object list should be found.
      fnobj = strlowcase(match.obj[newfield])+state.exttag+'.obj'
      IF not exists(fnobj) THEN BEGIN
         print,'Object list file ',fnobj,' could not be found.',bel
         WIDGET_CONTROL, state.mainbase, UPDATE=0
         WIDGET_CONTROL, state.unkid, SET_VALUE='0'
         WIDGET_CONTROL, state.yesid, SET_VALUE='0'
         WIDGET_CONTROL, state.noid,  SET_VALUE='0'
         WIDGET_CONTROL, state.mainbase, UPDATE=1
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         WIDGET_CONTROL, state.imbase, SET_UVALUE={nobj:-1}, /NO_COPY
         RETURN
      ENDIF

      ; Read the object list
      rdoblist,fnobj,nobj,fntrip,dt,offvals,xyvals,flags

      IF nobj le 0 THEN begin
         print,'No objects in list file ',fnobj,bel
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         widget_control, state.picktrip, sensitive=0
         return
      ENDIF

      IF match.fna[newfield] ne fntrip[0] or $
         match.fnb[newfield] ne fntrip[1] or $
         match.fnc[newfield] ne fntrip[2] THEN BEGIN
         print,'Triplet frame list in ',fnobj,' does not match original list.',bel
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         return
      ENDIF

      ; Free memory
      WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
      oblist=0.0

      print,'Load image ',match.fna[newfield],' ',state.exttag
      ima=readfits(state.path+match.fna[newfield],exten=state.curext,hdra,/silent)
      print,'Load image ',match.fnb[newfield],' ',state.exttag
      imb=readfits(state.path+match.fnb[newfield],exten=state.curext,hdrb,/silent)
      print,'Load image ',match.fnc[newfield],' ',state.exttag
      imc=readfits(state.path+match.fnc[newfield],exten=state.curext,hdrc,/silent)

      parsekey,hdra,state.hdrlist,infoa
      parsekey,hdrb,state.hdrlist,infob
      parsekey,hdrc,state.hdrlist,infoc

      sz=size(ima)
      nx = sz[1]
      ny = sz[2]

      xa1 = xyvals[0,*]
      ya1 = xyvals[1,*]
      xb1 = xyvals[2,*]-offvals[0]
      yb1 = xyvals[3,*]-offvals[1]
      xc1 = xyvals[4,*]-offvals[2]
      yc1 = xyvals[5,*]-offvals[3]
      dtb = float(infob.jd-infoa.jd)*24.0
      dtc = float(infoc.jd-infoa.jd)*24.0
      brate=sqrt((xb1-xa1)^2+(yb1-ya1)^2)/dtb
      bdir =atan(yb1-ya1,xb1-xa1)*!radeg
      crate=sqrt((xc1-xa1)^2+(yc1-ya1)^2)/dtc
      cdir =atan(yc1-ya1,xc1-xa1)*!radeg

      z=where(flags eq '?',countunk)
      z=where(flags eq 'y',countyes)
      z=where(flags eq 'n',countno)

      oblist = { $
         flag: flags, $
         dirtyflags: 0, $
         nunk: countunk, $
         nyes: countyes, $
         nno: countno, $
         ima: temporary(ima), $
         imb: temporary(imb), $
         imc: temporary(imc), $
         nx: nx, $
         ny: ny, $
         infoa: infoa, $
         infob: infob, $
         infoc: infoc, $
         xa: xyvals[0,*], $
         ya: xyvals[1,*], $
         xb: xyvals[2,*], $
         yb: xyvals[3,*], $
         xc: xyvals[4,*], $
         yc: xyvals[5,*], $
         xa1: xa1, $
         ya1: ya1, $
         xb1: xb1, $
         yb1: yb1, $
         xc1: xc1, $
         yc1: yc1, $
         dtb: dtb, $
         dtc: dtc, $
         brate: brate, $
         bdir: bdir, $
         crate: crate, $
         cdir: cdir, $
         nobj: nobj, $
         xoffb: offvals[0], $
         yoffb: offvals[1], $
         xoffc: offvals[2], $
         yoffc: offvals[3] $
         }

      ; Now that we have all the information, update the screen.
      WIDGET_CONTROL, state.mainbase, UPDATE=0
      WIDGET_CONTROL, state.unkid, SET_VALUE=strtrim(string(oblist.nunk),2)
      WIDGET_CONTROL, state.yesid, SET_VALUE=strtrim(string(oblist.nyes),2)
      WIDGET_CONTROL, state.noid,  SET_VALUE=strtrim(string(oblist.nno),2)
      WIDGET_CONTROL, state.mainbase, UPDATE=1

      WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
      WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
   ENDIF
END

pro garth_nexttrip, state
   IF state.fieldnum ne -1 THEN BEGIN
      WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
      IF state.objnum+1 lt oblist.nobj THEN BEGIN
         WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
         garth_newobject,state,state.objnum+1
      ENDIF ELSE IF state.curext lt state.numext THEN BEGIN
         WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
         garth_newfield,state,state.fieldnum,state.curext+1
         garth_newobject,state,0
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
         garth_newfield,state,state.fieldnum+1,0
         garth_newobject,state,0
      ENDELSE
   ENDIF
end

PRO garth_load, state
   bel = STRING( 7B )

   IF state.matchfile EQ '' THEN RETURN

   IF NOT exists( state.matchfile ) THEN BEGIN
      print, 'Match file ',state.matchfile,' not found.',bel
      WIDGET_CONTROL, state.textid, SET_VALUE=''
      WIDGET_CONTROL, state.textid, SET_UVALUE={num:0}
      state.matchfile=''
      widget_control, state.pickfield, sensitive=0
      widget_control, state.pickexten, sensitive=0
      widget_control, state.picktrip, sensitive=0
      RETURN
   ENDIF

   ; Load the information from the match list file.
   readcol,state.matchfile,obj,fna,fnb,fnc,format='a,a,a,a',/silent
   match = { $
      obj: obj, $
      fna: fna, $
      fnb: fnb, $
      fnc: fnc, $
      num: n_elements(obj) $
      }
   WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
   widget_control, state.pickfield, sensitive=1
   widget_control, state.hardid, sensitive=1
   widget_control, state.hardallid, sensitive=1

END

PRO garth_eve, event
   WIDGET_CONTROL, event.id, /HOURGLASS

   stash = WIDGET_INFO( event.handler, /CHILD )
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

   CASE event.id OF
      state.exitid: BEGIN
         garth_svobjlist, state
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      END

      state.chardobjid: BEGIN
         garth_objprint,state,/color
      END

      state.hardobjid: BEGIN
         garth_objprint,state
      END

      state.hardallid: BEGIN
         currentfield = state.fieldnum
         currentexten = state.curext
         WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY
         matchnum = match.num
         WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
         for i=0,matchnum-1 do $
            for j=1,state.numext do $
               garth_hardcopy,state,i,j
         garth_newfield,state,currentfield,currentexten
         garth_newobject,state,0
      END

      state.hardid: BEGIN
         garth_hardcopy,state,state.fieldnum
      END

      state.maxsigid: BEGIN
         WIDGET_CONTROL, event.id, GET_VALUE=hivalue
         hivalue = float(fix(hivalue[0]*10.0))/10.0
         WIDGET_CONTROL, state.minsigid, GET_VALUE=lovalue
         lovalue=float(lovalue[0])
         if lovalue eq hivalue then hivalue=hivalue+0.1
         hivalue = strcompress(string(hivalue,format='(f10.1)'))
         WIDGET_CONTROL, state.maxsigid, SET_VALUE=hivalue
         garth_newobject,state,state.objnum
      END

      state.minsigid: BEGIN
         WIDGET_CONTROL, event.id, GET_VALUE=lovalue
         lovalue = float(fix(lovalue[0]*10.0))/10.0
         WIDGET_CONTROL, state.maxsigid, GET_VALUE=hivalue
         hivalue=float(hivalue[0])
         if lovalue eq hivalue then lovalue=lovalue-0.1
         lovalue = strcompress(string(lovalue,format='(f10.1)'))
         WIDGET_CONTROL, state.minsigid, SET_VALUE=lovalue
         garth_newobject,state,state.objnum
      END

      state.nexttrip: BEGIN
         garth_nexttrip,state
      END

      state.nextext: BEGIN
         IF state.fieldnum ne -1 THEN BEGIN
            garth_newfield,state,state.fieldnum,state.curext+1
            garth_newobject,state,0
         ENDIF
      END

      state.nextfield: BEGIN
         IF state.fieldnum ne -1 THEN BEGIN
            garth_newfield,state,state.fieldnum+1,0
            garth_newobject,state,0
         ENDIF
      END

      state.pickid: BEGIN
         fn = DIALOG_PICKFILE( GROUP=event.top, TITLE='Select Triplet List', $
                               FILTER='*.match')
         IF fn ne '' THEN BEGIN
            print,fn
            state.matchfile = fn
            WIDGET_CONTROL, state.textid, SET_VALUE=state.matchfile
            garth_load,state
            garth_newfield,state,0,0
            garth_newobject,state,0
         ENDIF
      END

      state.pickexten: BEGIN
         IF state.matchfile ne '' and state.numext gt 1 THEN BEGIN
            WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY
            WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
            picklist=strarr(state.numext)
            for i=1,state.numext do begin
               exttag = 'x'+strb36(i)
               if state.curext eq i then begin
                  if oblist.nobj ne -1 then begin
                     nobj=oblist.nobj
                     countunk=oblist.nunk
                     countyes=oblist.nyes
                     countno =oblist.nno
                  endif else begin
                     nobj=0
                  endelse
               endif else begin
                  objnam = strlowcase(match.obj[state.fieldnum])+exttag+'.obj'
                  if exists(objnam) then begin
                     rdoblist,objnam,nobj,fntrip,dt,offset,pos,flags
                     z=where(flags eq '?',countunk)
                     z=where(flags eq 'y',countyes)
                     z=where(flags eq 'n',countno)
                  endif else begin
                     nobj=0
                  endelse
               endelse
               str=exttag+' N:'+strcompress(nobj,/remove_all)
               if nobj gt 0 then begin
                  str=str+' '+ $
                         'y:'+strcompress(countyes,/remove_all)+' '+ $
                         'n:'+strcompress(countno,/remove_all)
                  if countunk gt 0 then $
                     str=str+' '+ $
                            '?:'+strcompress(countunk,/remove_all)
               endif
               picklist[i-1]=str
            endfor
            newstr=picker(picklist,group=event.top,title='Pick Extension',INDEX=idx)
            WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
            WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
            IF newstr ne '[[[CANCEL]]]' THEN BEGIN
               garth_newfield,state,state.fieldnum,idx+1
               garth_newobject,state,0
            ENDIF
         ENDIF
      END

      state.pickfield: BEGIN
         IF state.matchfile ne '' THEN BEGIN
            WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY
            WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
            picklist=strarr(match.num)
            FOR i=0,match.num-1 DO BEGIN
               picklist[i]=string(i+1,match.obj[i]+'         ', $
                               match.fna[i],match.fnb[i],match.fnc[i], $
                               format='(i3,1x,a10,1x,a,1x,a,1x,a)')
               nunk=0
               nyes=0
               nno=0
               for j=1,state.numext do begin
                  if state.fieldnum eq i and state.curext eq j then begin
                     if oblist.nobj ne -1 then begin
                        nobj=oblist.nobj
                        nunk=oblist.nunk
                        nyes=oblist.nyes
                        nno =oblist.nno
                     endif
                  endif else begin
                     exttag = 'x'+strb36(j)
                     objnam = strlowcase(match.obj[i])+exttag+'.obj'
                     if exists(objnam) then begin
                        rdoblist,objnam,nobj,fntrip,dt,offset,pos,flags
                        z=where(flags eq '?',countunk)
                        z=where(flags eq 'y',countyes)
                        z=where(flags eq 'n',countno)
                        nunk=nunk+countunk
                        nyes=nyes+countyes
                        nno =nno +countno
                     endif
                  endelse
               endfor
               nobj=nunk+nyes+nno
               picklist[i]=picklist[i]+'  N:'+strcompress(nobj,/remove_all)
               if nobj gt 0 then begin
                  picklist[i]=picklist[i]+' '+ $
                                 'y:'+strcompress(nyes,/remove_all)+' '+ $
                                 'n:'+strcompress(nno,/remove_all)
                  if nunk gt 0 then $
                     picklist[i]=picklist[i]+' '+ $
                                    '?:'+strcompress(nunk,/remove_all)
               endif
            endfor

            newfieldstr=picker(picklist,group=event.top,title='Pick Field',INDEX=idx)

            WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
            WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY

            IF newfieldstr ne '[[[CANCEL]]]' THEN BEGIN
               garth_newfield,state,idx,0
               garth_newobject,state,0
            ENDIF
         ENDIF
      END

      state.picktrip: BEGIN
         IF state.matchfile ne '' and state.numext gt 1 THEN BEGIN
            WIDGET_CONTROL, state.textid, GET_UVALUE=match, /NO_COPY
            WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
            picklist=strarr(oblist.nobj)
            for i=0,oblist.nobj-1 do begin
               picklist[i] = string(strb36(i),oblist.flag[i],oblist.xa[i],oblist.ya[i], $
                                    oblist.brate[i],oblist.bdir[i],oblist.crate[i], $
                                    oblist.cdir[i], $
                                    format='(a2,1x,a,2x,"[",f6.1,",",f6.1,"]",' + $
                                           '2(1x,"{",f6.2,";",f6.1,"}"))')
            endfor
            newstr=picker(picklist,group=event.top,title='Pick Triplet',INDEX=idx)
            WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
            WIDGET_CONTROL, state.textid, SET_UVALUE=match, /NO_COPY
            IF newstr ne '[[[CANCEL]]]' THEN BEGIN
               garth_newobject,state,idx
            ENDIF
         ENDIF
      END

      state.prevext: BEGIN
         IF state.fieldnum ne -1 THEN BEGIN
            garth_newfield,state,state.fieldnum,state.curext-1
            garth_newobject,state,0
         ENDIF
      END

      state.prevtrip: BEGIN
         IF state.fieldnum ne -1 THEN BEGIN
            WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
            IF state.objnum-1 ge 0 THEN BEGIN
               WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
               garth_newobject,state,state.objnum-1
            ENDIF ELSE IF state.curext gt 1 THEN BEGIN
               WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
               garth_newfield,state,state.fieldnum,state.curext-1
               garth_newobject,state,-1
            ENDIF ELSE BEGIN
               WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
               garth_newfield,state,state.fieldnum-1,state.numext
               garth_newobject,state,-1
            ENDELSE
         ENDIF
      END

      state.prevfield: BEGIN
         IF state.fieldnum gt 0 THEN BEGIN
            garth_newfield,state,state.fieldnum-1,0
            garth_newobject,state,0
         ENDIF
      END

      state.queueid: BEGIN
         WIDGET_CONTROL, event.id, GET_VALUE=value
         fn = STRTRIM( value[0], 2 )
         if fn eq '' then $
            WIDGET_CONTROL, state.queueid, SET_VALUE='<default>'
      END

      state.resetid: BEGIN
         WIDGET_CONTROL, state.maxsigid, SET_VALUE='5.0'
         WIDGET_CONTROL, state.minsigid, SET_VALUE='-3.0'
         garth_newobject,state,state.objnum
      END

      state.stateid: BEGIN
         WIDGET_CONTROL, state.imbase, GET_UVALUE=oblist, /NO_COPY
         autonext = 0
         IF oblist.nobj ne -1 THEN BEGIN
            oblist.flag[state.objnum] = event.value
            if event.value ne '?' then autonext = 1
            z=where(oblist.flag eq '?',count)
            oblist.nunk = count
            z=where(oblist.flag eq 'y',count)
            oblist.nyes = count
            z=where(oblist.flag eq 'n',count)
            oblist.nno = count
            oblist.dirtyflags = 1
            WIDGET_CONTROL, state.mainbase, UPDATE=0
            WIDGET_CONTROL, state.unkid, SET_VALUE=strtrim(string(oblist.nunk),2)
            WIDGET_CONTROL, state.yesid, SET_VALUE=strtrim(string(oblist.nyes),2)
            WIDGET_CONTROL, state.noid,  SET_VALUE=strtrim(string(oblist.nno),2)
            WIDGET_CONTROL, state.mainbase, UPDATE=1
         ENDIF
         WIDGET_CONTROL, state.imbase, SET_UVALUE=oblist, /NO_COPY
         if autonext then garth_nexttrip,state
      END

      state.textid: BEGIN
         WIDGET_CONTROL, event.id, GET_VALUE=value
         fn = STRTRIM( value[0], 2 )
         IF fn ne '' THEN BEGIN
            print,fn
            state.matchfile = fn
            WIDGET_CONTROL, state.textid, SET_VALUE=state.matchfile
            garth_load,state
            garth_newfield,state,0,0
            garth_newobject,state,0
         ENDIF
      END

      ELSE : BEGIN
         MESSAGE, 'Unknown event:', /INFO
         HELP, event, /STRUCTURE
      END
   ENDCASE

   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

END

PRO garth,KEYLIST=in_keylist,PATH=path,SUBSIZE=subsize,BINFAC=binfac

   IF xregistered('garth') THEN return

   IF (!d.flags and 256) eq 0 THEN BEGIN
      print, 'Error. No windowing device. GARTH cannot be started.'
      return
   ENDIF

   IF !d.n_colors ne 16777216 THEN BEGIN
      print,'Error.  24-bit display device is required.'
      return
   ENDIF

   IF badpar(path,[0,7],0, $
         CALLER='GARTH: (PATH) ',DEFAULT='') THEN RETURN
   if path ne '' then path=addslash(path)

   IF badpar(subsize,[0,2,3],0, $
         CALLER='GARTH: (SUBSIZE) ',DEFAULT=249) THEN RETURN
   IF badpar(binfac,[0,2,3],0, $
         CALLER='GARTH: (BINFAC) ',DEFAULT=1) THEN RETURN

   IF KEYWORD_SET( in_keylist ) THEN $
      loadkeys,in_keylist,hdrlist $
   ELSE $
      loadkeys,'[[DEFAULT]]',hdrlist

   dw = subsize/2

   state = { $
      path: path, $
      dw:   dw, $                ; half width of sub-image for display
      wid:  dw*2+1, $            ; full width of sub-image for display
      bf:   binfac, $            ; binning factor for display
      prevext: 0L, $
      nextext: 0L, $
      hdrlist: hdrlist, $
      fieldnum: -1, $
      objnum: -1, $
      numext: 1, $
      curext: 0, $
      exttag: '', $
      frameaid: 0L, $
      framebid: 0L, $
      framecid: 0L, $
      unkid: 0L, $
      yesid: 0L, $
      noid: 0L, $
      objectof: 0L, $
      barateid: 0L, $
      carateid: 0L, $
      fieldof: 0L, $
      fieldid: 0L, $
      extenid: 0L, $
      stateid: 0L, $
      wina: 0L, $
      winb: 0L, $
      winc: 0L, $
      winrgb: 0L, $
      matchfile:'', $
      pickid: 0L, $
      textid: 0L, $
      imbase: 0L, $
      mainbase:0L, $
      exitid:0L, $
      picktrip: 0L, $
      pickobject: 0L, $
      pickexten: 0L, $
      pickfield: 0L, $
      prevtrip: 0L, $
      nexttrip: 0L, $
      minsigid: 0L, $
      maxsigid: 0L, $
      resetid: 0L, $
      hardid: 0L, $
      hardallid: 0L, $
      hardobjid: 0L, $
      chardobjid: 0L, $
      queueid: 0L, $
      prevfield:0L, $
      nextfield:0L $
      }

   setusym,-1

   ;Define the main base.
   mainbase = WIDGET_BASE( TITLE='GARTH', /COLUMN, UVALUE=0 )
   state.mainbase = mainbase

   ; File select base.
   filebase = WIDGET_BASE( mainbase, /ROW, /FRAME )
   state.exitid = WIDGET_BUTTON( filebase, VALUE='Exit' )
   w1 = WIDGET_LABEL( filebase, VALUE='TRIPLET FILE LIST:' )
   state.pickid = WIDGET_BUTTON( filebase, VALUE='Select File' )
   state.textid = WIDGET_TEXT( filebase, VALUE=state.matchfile, /EDITABLE, $
                    XSIZE=60, UVALUE={num:0} )

   b1=WIDGET_BASE( mainbase, /ROW, /FRAME )

   ; Image display area.
   state.imbase = WIDGET_BASE( b1, /COLUMN, UVALUE={nobj:-1} )
   r1 = WIDGET_BASE( state.imbase, /ROW )
   sz = state.wid ; * state.bf
   state.wina   = WIDGET_DRAW( r1, XSIZE=sz, YSIZE=sz, RETAIN=2, UVALUE=0 )
   state.winb   = WIDGET_DRAW( r1, XSIZE=sz, YSIZE=sz, RETAIN=2 )
   r2 = WIDGET_BASE( state.imbase, /ROW )
   state.winc   = WIDGET_DRAW( r2, XSIZE=sz, YSIZE=sz, RETAIN=2 )
   state.winrgb = WIDGET_DRAW( r2, XSIZE=sz, YSIZE=sz, RETAIN=2 )
   r2 = WIDGET_BASE( state.imbase, /ROW )

;   tt1 = WIDGET_BASE( b2, /ROW, /FRAME )
;   t1 = WIDGET_BASE( tt1, /COLUMN )
   t2 = WIDGET_BASE( r2, /ROW, /FRAME )
   t3 = WIDGET_LABEL( t2, VALUE='Min Display (sigma)' )
   state.minsigid = WIDGET_TEXT( t2, VALUE='-3', /EDITABLE, XSIZE=5 )
;   t2 = WIDGET_BASE( t1, /ROW )
   t2 = WIDGET_BASE( r2, /ROW, /FRAME )
   t3 = WIDGET_LABEL( t2, VALUE='Max Display (sigma)' )
   state.maxsigid = WIDGET_TEXT( t2, VALUE='5', /EDITABLE, XSIZE=5 )
   state.resetid  = WIDGET_BUTTON( r2, VALUE='Reset Min/Max' )
   ; Field summary information
   b2=WIDGET_BASE( b1, /COLUMN )
   tt1 = WIDGET_BASE( b2, /ROW, /FRAME )
   t1 = WIDGET_BASE( tt1, /COLUMN )
   t2 = WIDGET_LABEL( t1, VALUE='Current Field:', /ALIGN_RIGHT )
   t3 = WIDGET_LABEL( t1, VALUE='Field Name:',/ALIGN_RIGHT )
   t3 = WIDGET_LABEL( t1, VALUE='Extension:',/ALIGN_RIGHT )
   t4 = WIDGET_LABEL( t1, VALUE='Image A:', /ALIGN_RIGHT )
   t5 = WIDGET_LABEL( t1, VALUE='Image B:', /ALIGN_RIGHT )
   t6 = WIDGET_LABEL( t1, VALUE='Image C:', /ALIGN_RIGHT )
   t6 = WIDGET_LABEL( t1, VALUE='Not Validated:', /ALIGN_RIGHT )
   t6 = WIDGET_LABEL( t1, VALUE='Verified:', /ALIGN_RIGHT )
   t6 = WIDGET_LABEL( t1, VALUE='Spurious:', /ALIGN_RIGHT )

   t7 = WIDGET_BASE( tt1, /COLUMN )
   state.fieldof  = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.fieldid  = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.extenid  = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.frameaid = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.framebid = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.framecid = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.unkid    = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.yesid    = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.noid     = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )

   tt1 = WIDGET_BASE( b2, /ROW, /FRAME )
   t1 = WIDGET_BASE( tt1, /COLUMN )
   t8 = WIDGET_LABEL( t1, VALUE='Current Object:', /ALIGN_RIGHT )
   t9 = WIDGET_LABEL( t1, VALUE='B-A rate,dir:', /ALIGN_RIGHT )
   ta = WIDGET_LABEL( t1, VALUE='C-A rate,dir:', /ALIGN_RIGHT )

   t7 = WIDGET_BASE( tt1, /COLUMN )
   state.objectof = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.barateid = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )
   state.carateid = WIDGET_LABEL( t7, VALUE='', /ALIGN_LEFT, /DYNAMIC_RESIZE )

   tt1 = WIDGET_BASE( b2, /COLUMN, /FRAME )
   t1 = WIDGET_BASE( tt1, /ROW )
   t2 = WIDGET_LABEL( t1, VALUE='    Valid:', /align_right )
   state.stateid = cw_bgroup( t1, ['?','y','n'], $
                              /return_name, /exclusive, /row )
   t1 = WIDGET_BASE( tt1, /ROW )
   t2 = WIDGET_LABEL( t1, VALUE='  Triplet:', /align_right )
   state.prevtrip= WIDGET_BUTTON( t1, VALUE='Previous' )
   state.nexttrip= WIDGET_BUTTON( t1, VALUE='Next' )
   state.picktrip= WIDGET_BUTTON( t1, VALUE='Pick' )
   t1 = WIDGET_BASE( tt1, /ROW )
   t2 = WIDGET_LABEL( t1, VALUE='Extension:', /align_right )
   state.prevext= WIDGET_BUTTON( t1, VALUE='Previous' )
   state.nextext= WIDGET_BUTTON( t1, VALUE='Next' )
   state.pickexten= WIDGET_BUTTON( t1, VALUE='Pick' )
   t1 = WIDGET_BASE( tt1, /ROW )
   t2 = WIDGET_LABEL( t1, VALUE='    Field:', /align_right )
   state.prevfield = WIDGET_BUTTON( t1, VALUE='Previous' )
   state.nextfield = WIDGET_BUTTON( t1, VALUE='Next' )
   state.pickfield= WIDGET_BUTTON( t1, VALUE='Pick' )
   widget_control, state.pickfield, sensitive=0
   widget_control, state.pickexten, sensitive=0
   widget_control, state.picktrip, sensitive=0
   widget_control, state.prevtrip, sensitive=0
   widget_control, state.nexttrip, sensitive=0
   widget_control, state.prevext, sensitive=0
   widget_control, state.nextext, sensitive=0
   widget_control, state.prevfield, sensitive=0
   widget_control, state.nextfield, sensitive=0

   tt1 = WIDGET_BASE( b2, /ROW, /FRAME )
   t1 = WIDGET_BASE( tt1, /COLUMN )
   t2 = WIDGET_BASE( t1, /ROW )
   t3 = WIDGET_LABEL( t2, VALUE='Printer' )
   state.queueid   = WIDGET_TEXT( t2, VALUE='<default>', /EDITABLE, XSIZE=15 )
   state.hardobjid = WIDGET_BUTTON( t1, VALUE='B/W Hardcopy of this object' )
   state.chardobjid = WIDGET_BUTTON( t1, VALUE='Color Hardcopy of this object' )
   state.hardid    = WIDGET_BUTTON( t1, VALUE='Hardcopy of this field' )
   state.hardallid = WIDGET_BUTTON( t1, VALUE='Hardcopy of all fields' )

   widget_control, state.hardid, sensitive=0
   widget_control, state.hardallid, sensitive=0
   widget_control, state.hardobjid, sensitive=0
   widget_control, state.chardobjid, sensitive=0

   ;Stash area for the state structure.
   stash = WIDGET_INFO( mainbase, /CHILD )

   ;Stash the state structure.
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

   ;Realize the main base.
   WIDGET_CONTROL, mainbase, /REALIZE

   ;
   res=file_search('*.match')
   if n_elements(res) eq 1 and res[0] ne '' then begin
      WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
      state.matchfile = res[0]
      WIDGET_CONTROL, state.textid, SET_VALUE=state.matchfile
      garth_load,state
      garth_newfield,state,0,0
      garth_newobject,state,0
      WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
   endif

   ; Give control to the XMANAGER.
   XMANAGER, 'garth', mainbase, $
             EVENT_HANDLER='garth_eve', $
             GROUP_LEADER=mainbase

   setusym,1

END

;+
; CLASS_NAME:
;    itoolwapixed
;
; PURPOSE (one line):
;    To edit individual pixels in an itool image.
;
; CATEGORY:
;    Widgets
;
; SUPERCLASSES:
;
; SUBCLASSES:
;
; CREATION:
;    See itoolwapixed::init
;
; METHODS:
;    itoolwapixed::cleanup
;    itoolwapixed::getproperty
;    itoolwapixed::update
;    itoolwapixed::realize
;    itoolwapixed::init
;
; MODIFICATION HISTORY:
;    2004/04/15, Written by Doug Loucks, Consultant for Lowell Observatory.
;    (See cw_pixed.pro)
;    Removed all remnants of compound-widget code; replaced with
; stand-alone code that is compatible with the new object-oriented version
; of itool.
;    Modified the usage of the state-structure variable. A pointer to
; the state structure is stored in the object instance of this tool and
; this tool's object reference is stored in its top-level base.
;    Modified the incoming argument. Now, it is the object reference of the
; host instance of itool, which is stored in this tool's object instance.
;
;    2006/03/15 - DWL, Minor modifications to use the methods of the new
;                   'itoolimage' object class that was added to the itool GUI.
;                   See itoolimage__define.pro and itool__define.pro for
;                   details.
;
;-

; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwapixed::cleanup
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    obj_destroy, oref
;
; INPUTS:
;    oref : An itoolwapixed object reference.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwapixed::getproperty
;
; PURPOSE:
;   To retrieve "properties" defined for the itoolwapixed object
; class. Properties are specified as keyword arguments.
;
; CALLING SEQUENCE:
;    oref->getproperty
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    PSTATE : Set this keyword to a named variable into which will be
;             placed a pointer to the state structure.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwapixed::update
;
; PURPOSE:
;
; CALLING SEQUENCE:
;    oref->update, newvalue
;
; INPUTS:
;    newvalue : A new value for a pixel, or for a set of pixels. This
;               parameter is a structure with the following definition:
;               {count:1, x:x, y:y, value:value} where
;               count is the number of elements in the remaining three tags
;               x and y are arrays of coordinates of changed pixels.
;               value is an array of values for changed pixels.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwapixed::realize
;
; PURPOSE:
;    To realize a new, managed instance of the itoolwapixed object class.
;
; CALLING SEQUENCE:
;    oref->realize
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    NO_BLOCK : Set this keyword to cause the widget application to run
;               in non-blocked mode.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwapixed::init
;
; PURPOSE:
;    To initialize a new instance of the itoolwapixed object class.
;
; CALLING SEQUENCE:
;    oref = obj_new('itoolwapixed', oitool, pimage, xpos, ypos)
;
; INPUTS:
;    oitool      : The object reference of an instance of itool.
;    pimage      : A pointer to the array to be edited.
;    xpos, ypos  : The pixel at this location in the array, along with a
;                  collection of its neighbors, will be displayed as a grid of
;                  editable text widgets.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    GROUP_LEADER : The group leader for this tool.
;    GRIDSIZE : The size of the grid of surrounding pixels.
;               Default is 7 pixels (7X7 grid). Note: In order to maintain
;               symmetry about the center pixel, the grid size needs to be odd.
;               Therefore, even values will be increased by 1 pixel.
;
; OUTPUTS:
;    oref = The object reference of the new instance of the itoolwapixed
;           object class.
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    After creating a new instance of this object-oriented widget application,
; it must be realized.
;
;    This widget may be used to edit a grid of pixels in an array.
; Unless near the edge of the image, the grid is centered on (xpos, ypos).
; The left column and the bottom row display the y and x coordinates,
; respectively, for the grid. xpos and ypos are enclosed in brackets,
; for identification.
;
; EXAMPLE:
;    oref = obj_new('itoolwapixed', oitool, pimage, xpos, ypos,$
;       GROUP_LEADER=group_leader)
;
;    oref->realize
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; Procedure itoolwapixed::cleanup
; -----------------------------------------------------------------------------
pro itoolwapixed::cleanup
   compile_opt hidden

   ptr_free, self.pstate
end



; -----------------------------------------------------------------------------
; Procedure itoolwapixed::getproperty
; -----------------------------------------------------------------------------
pro itoolwapixed::getproperty, PSTATE=pstate
   compile_opt hidden

   if arg_present(pstate) then pstate = self.pstate
end



; -----------------------------------------------------------------------------
; Procedure itoolwapixed::update
; -----------------------------------------------------------------------------
pro itoolwapixed::update, newvalue
   compile_opt hidden

   ; Get a pointer to itool's state structure and a pointer to the image-
   ; parameters structure..
   self.oitool->getproperty, PSTATE=pstate_itool, OIMAGE=oimage
   oimage->getproperty, PIM_PARMS=pim_parms

   pworkstate = (*pstate_itool).pworkstate
   pzoomstate = (*pstate_itool).pzoomstate

   pimage = (*pim_parms).imageptr
   widget_control, (*pstate_itool).work_view, get_value=work_win
   widget_control, (*pstate_itool).zoom_view, get_value=zoom_win
   curmin = (*pim_parms).curmin[(*pim_parms).frame]
   curmax = (*pim_parms).curmax[(*pim_parms).frame]

   ; Save current window number.
   curwin = !d.window

   for j=0, newvalue.count-1 do begin
      (*pimage)[newvalue.x[j], newvalue.y[j], (*pim_parms).frame] =$
      newvalue.value[j]

      t = rebin([newvalue.value[j]], (*pzoomstate).zfact,$
         (*pzoomstate).zfact, /SAMPLE)

      t = bytscl(t, min=curmin, max=curmax, TOP=(*pstate_itool).ncolors-1)
      dx = newvalue.x[j] - (*pzoomstate).spx
      dy = newvalue.y[j] - (*pzoomstate).spy
      wset, zoom_win

      tv, t, (*pzoomstate).xoff+dx*(*pzoomstate).zfact, $
             (*pzoomstate).yoff+dy*(*pzoomstate).zfact

      t = rebin([newvalue.value[j]], (*pworkstate).zfact,$
      (*pworkstate).zfact,$
      /SAMPLE)

      t = bytscl(t, min=curmin, max=curmax, TOP=(*pstate_itool).ncolors-1)
      dx = newvalue.x[j] * (*pworkstate).zfact
      dy = newvalue.y[j] * (*pworkstate).zfact
      wset, work_win
      tv, t, dx, dy
   endfor

   ; Restore the window number.
   wset, curwin
end



; -----------------------------------------------------------------------------
; Procedure itoolwapixed::realize
; -----------------------------------------------------------------------------
pro itoolwapixed::realize, NO_BLOCK=no_block
   compile_opt hidden

   widget_control, self.tlb, /REALIZE

   xmanager, 'itoolwapixed', self.tlb, NO_BLOCK=no_block,$
      CLEANUP='itoolwapixed_cleanup'
end


; -----------------------------------------------------------------------------
; Function itoolwapixed::init
; Init method for the itool Pixel Editor.
; -----------------------------------------------------------------------------
function itoolwapixed::init, oitool, pimage, xpos, ypos,$
   GRIDSIZE=in_gridsize, GROUP_LEADER=group_leader

   tlb = widget_base(TITLE='Itool Zoom-Window Pixel Editor', COLUMN=1,$
      GROUP_LEADER=group_leader, MBAR=mbar)

   stat = size(*pimage)
   xsize = stat[1]
   ysize = stat[2]
   typecode = stat[3]

   case typecode OF
      1 : begin
         width = 4
         fmt   = '(I4)'
         tval  = 0B
      end

      2 : begin
         width = 6
         fmt   = '(I6)'
         tval  = 0
      end

      3 : begin
         width = 12
         fmt   = '(I12)'
         tval   = 0L
      end

      4 : begin
         width = 12
         fmt   = '(G0.0)'
         tval  = 0.0
      end

      5 : begin
         width = 14
         fmt   = '(G0.0)'
         tval  = 0.0
      end

      else : begin
         print, 'Error. Typecode ' + string(typecode,FORMAT='(G0.0)') +$
                  ' not supported'
         width = 12
         fmt   = '(G0.0)'
         tval  = 0.0
         return, 0
      end
   endcase

   if keyword_set(in_gridsize) then gridsize=in_gridsize else gridsize=7

   if (gridsize mod 2) eq 0 then gridsize=gridsize+1

   hw = gridsize / 2
   xset = xpos - hw
   yset = ypos - hw
   if xset LT 0 then xset = 0
   if yset LT 0 then yset = 0
   if xset+gridsize GE xsize then xset=xsize-gridsize
   if yset+gridsize GE ysize then yset=ysize-gridsize

   if !order eq 0 then begin
      y1 = gridsize - 1
      y2 = 0
      dy = -1
   endif else begin
      y1 = 0
      y2 = gridsize - 1
      dy = 1
   endelse

   ; Extract grid to be edited, and make a copy that won't be touched.
   grid    = (*pimage)[xset:xset+gridsize-1, yset:yset+gridsize-1 ]
   savgrid = grid

   ; File Menu.
   filemenuid = widget_button(mbar, VALUE='File', /MENU)

   dummy = widget_button(filemenuid, VALUE='Done',$
      EVENT_PRO='itoolwapixed_done_event')

   ; Action Menu.
   actionmenuid = widget_button(mbar, VALUE='Action', /MENU)

   dummy = widget_button(actionmenuid,VALUE='Restore Initial Values',$
      EVENT_PRO='itoolwapixed_restore_event')

   cba = widget_base(tlb, COLUMN=gridsize+1, FRAME=1)

   xids    = lonarr(gridsize,/nozero)
   yids    = lonarr(gridsize,/nozero)
   gridids = lonarr(gridsize, gridsize,/nozero)

   f = '(G0.0)'

   ; Create the y label ids
   for j=y1,y2,dy do begin
      text = string(yset+j,format=f)
      mark = (yset+j) eq ypos
      if mark then text = '[' + text + ']'
      yids[j] = widget_label(cba,value=text)
   endfor

   w0 = widget_label(cba, value='    ')

   ; Create the x label and grid ids
   for i=0,gridsize-1 do begin
      for j=y1,y2,dy do begin
         text = string((*pimage)[xset+i,yset+j],FORMAT=f)
         w0   = widget_text(cba,VALUE=text,UVALUE={x:i,y:j},Xsize=width,/EDIT)
         gridids[i,j] = w0
      endfor

      text = string(xset+i,FORMAT=f)
      mark = (xset+i) eq xpos
      if mark then text = '[' + text + ']'
      xids[i] = widget_label(cba,VALUE=text)
   endfor

   ; Scan through all the items in this base and find the max x and y sizes.
   max_xs=0
   max_ys=0
   t_b = widget_info(cba,/child)  ; gets first child

   while (t_b ne 0L) do begin
      geo = widget_info(t_b,/geometry)
      max_xs=max([max_xs,geo.scr_xsize])
      max_ys=max([max_ys,geo.scr_ysize])
      t_b = widget_info(t_b,/sibling) ; gets next child (0=no more)
   endwhile

   ; Now, go back through all widget items and set them to the same size.
   t_b = widget_info(cba,/child)

   while (t_b ne 0L) do begin
      widget_control,t_b,SCR_XSIZE=max_xs,SCR_YSIZE=max_ys
      t_b = widget_info(t_b,/sibling)
   endwhile

   m = median(grid)

   xp = xpos - xset
   yp = ypos - yset

   x = indgen(gridsize)
   ya = grid[ xp, * ]
   yb = grid[ *, yp ]
   w = where(x ne xp, count)
   coeff1 = goodpoly(x[w]+xpos, ya[w], 1, 3.0, yfit)
   coeff2 = goodpoly(x[w]+ypos, yb[w], 1, 3.0, yfit)

   ny1 = poly(xpos, coeff1)
   ny2 = poly(ypos, coeff2)

   text = string('Grid Median: ',m,'   Linear Interpolation, Row:  ',ny2,$
                 '   Column: ',ny1,format='(A,G0.0,A,G0.0,A,G0.0)')

   w0 = widget_label(tlb,VALUE=text)

   state = {$
      chgdids:lonarr(gridsize,gridsize),$  ; flags, set if cell modified
      fmt:fmt,$
      grid:grid,$
      gridids:gridids,$
      gridsize:gridsize,$
      pimage:pimage,$
      savgrid:savgrid,$
      tval:tval,$
      xids:xids,$
      xpos:xpos,$
      xset:xset,$
      yids:yids,$
      ypos:ypos,$
      yset:yset $
   }

   self.tlb = tlb
   self.oitool = oitool

   ; Create a pointer to the state structure and store it in the object
   ; instance of this application.
   self.pstate = ptr_new(state, /no_copy)

   ; Store the object reference to this application in the UVALUE of the
   ; top-level base
   widget_control, tlb, SET_UVALUE=self
                                                                                
   ; Return successful initialization.
   return, 1
end


; -----------------------------------------------------------------------------
; Procedure itoolwapixed_cleanup
; -----------------------------------------------------------------------------
pro itoolwapixed_cleanup, tlb
   compile_opt hidden

   widget_control, tlb, GET_UVALUE=oitoolwapixed
   obj_destroy, oitoolwapixed
end



; -----------------------------------------------------------------------------
; Procedure itoolwapixed_done_event
; -----------------------------------------------------------------------------
pro itoolwapixed_done_event, event
   compile_opt hidden

   widget_control, event.top, /DESTROY
end


; -----------------------------------------------------------------------------
; Procedure itoolwapixed_restore_event
; -----------------------------------------------------------------------------
pro itoolwapixed_restore_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwapixed
   oitoolwapixed->getproperty, PSTATE=pstate

   w = where((*pstate).chgdids gt 0, count)

   if count gt 0 then begin
      x = intarr(count)
      y = intarr(count)
      v = replicate((*pstate).tval, count)
      widget_control, event.top, UPDATE=0

      for j=0, count-1 DO begin
         widget_control, (*pstate).chgdids[w[j]], GET_UVALUE=uv
         x[j] = uv.x
         y[j] = uv.y
         v[j] = (*pstate).savgrid[ uv.x, uv.y ]
         text = string(v[j], FORMAT=(*pstate).fmt)
         widget_control, (*pstate).chgdids[w[j]], SET_VALUE=text
         (*pstate).chgdids[ w[j] ] = 0
      endfor

      widget_control, event.top, UPDATE=1

      oitoolwapixed->update, {count:count, x:(*pstate).xset+x,$
         y:(*pstate).yset+y, value:v}
   endif
end



; -----------------------------------------------------------------------------
; Procedure itoolwapixed_event
; Default event handler. Handles events from the text widgets (cells).
; -----------------------------------------------------------------------------
pro itoolwapixed_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwapixed
   oitoolwapixed->getproperty, PSTATE=pstate

   widget_control, event.id, GET_VALUE=value, GET_UVALUE=uvalue
   x = uvalue.x
   y = uvalue.y
   (*pstate).grid[x, y] = value[0]

   newvalue = {count:1, x:[(*pstate).xset+x], y:[(*pstate).yset+y],$
      value:[(*pstate).tval]}

   (*pstate).chgdids[x, y] = event.id
   newvalue.value = value[0]

   oitoolwapixed->update, newvalue
end



; -----------------------------------------------------------------------------
; Procedure itoolwapixed__define
; Define the itoolwapixed object class.
;
; Attributes:
;   tlb    : top-level base.
;   pstate : pointer to state structure.
;   oitool : object reference of the host instance of itool.
; -----------------------------------------------------------------------------
pro itoolwapixed__define
   dummy = {itoolwapixed, tlb:0L, pstate:ptr_new(), oitool:obj_new()}
end

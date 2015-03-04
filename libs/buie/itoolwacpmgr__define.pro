;+
; CLASS_NAME:
;    itoolwacpmgr
;
; PURPOSE (one line):
;    To perform interactive sky selection for faint-comet photometry.
;
; CATEGORY:
;    Widgets
;
; SUPERCLASSES:
;
; SUBCLASSES:
;
; CREATION:
;    See itoolwacpmgr::init
;
; METHODS:
;    itoolwacpmgr::cleanup
;    itoolwacpmgr::ac1
;    itoolwacpmgr::display
;    itoolwacpmgr::addvalue
;    itoolwacpmgr::getproperty
;    itoolwacpmgr::realize
;    itoolwacpmgr::init
;
; MODIFICATION HISTORY:
;    2004/04, Written by Doug Loucks, Consultant for Lowell Observatory.
;    (See cw_cpmgr.pro)
;    Removed all remnants of compound-widget code; replaced with
; stand-alone code that is compatible with the new object-oriented version
; of itool.
;    Modified the usage of the state-structure variable. A pointer to
; the state structure is stored in the object instance of this tool and
; this tool's object reference is stored in its top-level base.
;    Modified the storage of the three variables pidx, pidxcum, and pidxall.
; Now, pointers to these are allocated and copies of the pointers are stored
; in the state structure and in the object instance, providing quick access
; from event handlers and from method routines.
;    Modified the incoming argument. Now, it is the object reference of the
; host instance of itool, which is stored in this tool's object instance.
;
;    2006/03/15 - DWL, Minor modifications to use the methods of the new
;                   'itoolimage' object class that was added to the itool GUI.
;                   See itoolimage__define.pro and itool__define.pro for
;                   details.
;    2006/5/22  - Peter L. Collins, Lowell Observatory
;                   added processing for CCD readout noise ("rdnoise") in
;                   the photometry parameters- specifically, in calls to
;                   basphote.
;
;-

; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwacpmgr::cleanup
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    obj_destroy, oref
;
; INPUTS:
;    oref : An itoolwacpmgr object reference.
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
;    itoolwacpmgr::ac1
;
; PURPOSE:
;    To compute the sky values for each of the sets of pixels
; (accepted : pidxcum, new set : pidx, and the sum of these : pidxall).
; Pointers to these three variables are stored in the state structure and
; the object instance, allowing quick access from event handlers and
; from method routines.
;
; CALLING SEQUENCE:
;    oref->ac1
;
; INPUTS:
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
;    itoolwacpmgr::display
;
; PURPOSE:
;    To display photometric results in a text widget.
;
; CALLING SEQUENCE:
;    oref->display
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    INIT : Set this keyword to force initial values of area and pcounts
;           to be set to zero.
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
;    itoolwacpmgr::addvalue
;
; PURPOSE:
;    To add a new pixel value to the list of pixels.
;
; CALLING SEQUENCE:
;    oref->addvalue, value
;
; INPUTS:
;    value : A new pixel value, defined by a structure {x:x, y:y}.
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
;    itoolwacpmgr::getproperty
;
; PURPOSE:
;   To retrieve "properties" defined for the itoolwacpmgr object
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
;    OITOOL : Set this keyword to a named variable into which will be
;             placed a copy of the object reference of a host instance
;             of the "itool" object class.
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
;    itoolwacpmgr::realize
;
; PURPOSE:
;   To realize a new, managed instance of the itoolwacpmgr object class.
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
;    itoolwacpmgr::init
;
; PURPOSE:
;    To initialize a new instance of the itoolwacpmgr object class.
;
; CALLING SEQUENCE:
;    oref = obj_new('itoolwacpmgr', oitool)
;
; INPUTS:
;    oitool : An object reference of a host instance of the "itool" object
;             class.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    GROUP_LEADER = The group leader for this tool.
;
; OUTPUTS:
;    oref = The object reference of the new instance of the itoolwacpmgr
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
; EXAMPLE:
;    oref = obj_new('itoolwacpmgr', oitool, GROUP_LEADER=group_leader)
;    oref->realize
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------


; ----------------------- Method Routines -------------------------------------

; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr::cleanup
; -----------------------------------------------------------------------------
pro itoolwacpmgr::cleanup
   compile_opt hidden

   ptr_free, self.pstate, self.pidx_p, self.pidxall_p, self.pidxcum_p
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr::ac1
; -----------------------------------------------------------------------------
pro itoolwacpmgr::ac1
   compile_opt hidden

   self.oitool->getproperty, OIMAGE=oimage, PPH_PARMS=pph_parms
   oimage->getproperty, PIM_PARMS=pim_parms
   pimage = (*pim_parms).imageptr
   timage = (*pimage)[*,*,(*pim_parms).frame]

   ; Check for accepted pixels.
   if n_elements(*self.pidxcum_p) gt 1 then begin
      ; Compute sky average and sky sigma.

      robomean, timage[*self.pidxcum_p], 3.0, 0.5, skavcum, avgdev,$
      stddev, var, skew, kurt, nsky, pcum, STDMEAN=sksicum
      (*self.pstate).skavcum   = skavcum
      (*self.pstate).sksicum   = sksicum

      ; Compute object flux and magnitude.
      basphote, (*pph_parms).gain, timage, (*pim_parms).exptime,$
      (*self.pstate).objx, (*self.pstate).objy, (*pph_parms).radius,$
      (*self.pstate).skavcum, -(*self.pstate).sksicum, /NOLOG, /SILENT,$
      RDNOISE=(*pph_parms).rdnoise, $
      FLUX=flux, FLERR=fluxerr, MAG=mag, ERR=magerr, XCEN=xcen, YCEN=ycen

      (*self.pstate).pnskcum   = flux
      (*self.pstate).pnskercum = fluxerr
      (*self.pstate).magcum    = mag
      (*self.pstate).magercum  = magerr
   endif else begin
      (*self.pstate).skavcum   = 0.0
      (*self.pstate).sksicum   = 0.0
      (*self.pstate).pnskcum   = 0.0
      (*self.pstate).pnskercum = 0.0
      (*self.pstate).magcum    = 99.9999
      (*self.pstate).magercum  = 0.0
   endelse

   ; Check for new batch of pixels.
   if n_elements(*self.pidx_p) gt 1 then begin
      ; Compute sky average and sky sigma.
      robomean, timage[*self.pidx_p], 3.0, 0.5, skav, avgdev,$
      stddev, var, skew, kurt, nsky, pix, STDMEAN=sksi

      (*self.pstate).skav = skav  ; New set.
      (*self.pstate).sksi = sksi  ; New set.
   endif else begin
      (*self.pstate).skav    = 0.0
      (*self.pstate).sksi    = 0.0
      (*self.pstate).pnsk    = 0.0
      (*self.pstate).pnsker  = 0.0
   endelse

   ; Check for combined set of pixels (pidxcum plus pidx).
   if n_elements(*self.pidxall_p) gt 1 then begin
      (*self.pstate).skpxall = n_elements(*self.pidxall_p)

      ; Compute sky average and sky sigma.
      robomean, timage[*self.pidxall_p], 3.0, 0.5, skavall, avgdev,$
      stddev, var, skew, kurt, nsky, pall, STDMEAN=sksiall

      (*self.pstate).skavall = skavall
      (*self.pstate).sksiall = sksiall
      (*self.pstate).skpxuall = nsky

      ; Compute object flux and magnitude.
      basphote, (*pph_parms).gain, timage, (*pim_parms).exptime,$
      (*self.pstate).objx, (*self.pstate).objy, (*pph_parms).radius,$
      (*self.pstate).skavall, -(*self.pstate).sksiall, /NOLOG, /SILENT,$
      RDNOISE=(*pph_parms).rdnoise, $
      FLUX=flux, FLERR=fluxerr, MAG=mag, ERR=magerr, XCEN=xcen, YCEN=ycen
   
      (*self.pstate).objx      = xcen
      (*self.pstate).objy      = ycen
      (*self.pstate).pnskall   = flux
      (*self.pstate).pnskerall = fluxerr
      (*self.pstate).magall    = mag
      (*self.pstate).magerall  = magerr

      ; Plot a histogram from the combined set of pixels.
      mindat = min(pall, MAX=maxdat)
      binsiz = (maxdat - mindat) / 40.0

      if binsiz gt 0 then begin
         h = histogram(float(pall), BINSIZE=binsiz, MIN=mindat, MAX=maxdat)
         idx = findgen(n_elements(h)) * binsiz + mindat
         xtrm = [0.,max(h)]
         curwin = !d.window
         widget_control, (*self.pstate).drawid, GET_VALUE=dwin
         wset, dwin

         plot, idx, h, PSYM=10, TITLE='(Clipped) HISTOGRAM',$
         XTITLE='SIGNAL', YSTYLE=11, XMARGIN=[4,3], YMARGIN=[4,3],$
         YRANGE=xtrm

         axis, YAXIS=1, YSTYLE=3, YRANGE=xtrm/total(h)*100.0, YTITLE='N'
         wset, curwin
      endif
   endif else begin
      (*self.pstate).skpxall   = 0
      (*self.pstate).skpxuall  = 0
      (*self.pstate).skavall   = 0.0
      (*self.pstate).sksiall   = 0.0
      (*self.pstate).pnskall   = 0.0
      (*self.pstate).pnskerall = 0.0
      (*self.pstate).magall    = 99.9999
      (*self.pstate).magerall  = 0.0
      curwin = !d.window
      widget_control, (*self.pstate).drawid, GET_VALUE=dwin
      wset, dwin
      erase
      wset, curwin
   endelse
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr::display
; -----------------------------------------------------------------------------
pro itoolwacpmgr::display, INIT=init
   compile_opt hidden

   ; Get some itool pointers.
   self.oitool->getproperty, OIMAGE=oimage, PPH_PARMS=pph_parms
   oimage->getproperty, PIM_PARMS=pim_parms

   pimage = (*pim_parms).imageptr
   timage = (*pimage)[*,*,(*pim_parms).frame]

   if keyword_set(init) then begin
      area = 0.0
      pcounts = 0.0
   endif else begin
      area = !pi * (*pph_parms).radius * (*pph_parms).radius
      boxm, timage, (*self.pstate).objx, (*self.pstate).objy, 5, 5, xmax, ymax
      pcounts = timage[xmax, ymax]
   endelse

   ; Compute the sky noise ratio.
   sigphotsky = area * (*self.pstate).sksiall * (*pph_parms).gain
   photons = (*self.pstate).pnskall * (*pim_parms).exptime
   varphotobj = photons + sigphotsky * sigphotsky

   if varphotobj ne 0 then begin
      sknfrac = sigphotsky * sigphotsky / varphotobj
   endif else begin
      sknfrac = 0.0
   endelse

   if (*self.pstate).ncli lt 0 then ncli=0 else ncli=(*self.pstate).ncli

   ; Build the text array used for the numeric display.
   text =[string('Num spots', ncli, '      ', 'Sky pix  total',$
                (*self.pstate).skpxall, FORMAT='(A,I5,A,A,I7)'),$
        string('Obj pos x', (*self.pstate).objx, '  ',     '          used',$
                (*self.pstate).skpxuall, FORMAT='(A,F9.3,A,A,I7)'),$
        string('        y', (*self.pstate).objy, '  ',     'Sky noise ratio',$
                sknfrac, FORMAT='(A,F9.3,A,A,F6.3)'),$
        '',$
        string('Peak counts', pcounts, ' (', pcounts-(*self.pstate).skavcum,$
                ' above sky)', FORMAT='(A,I7,A,I5,A)'),$
        string('Object+sky ',$
                ((*self.pstate).pnskall+(*self.pstate).skavall)/$
                (*pph_parms).gain,$
                ' +- ',$
                ((*self.pstate).pnskerall+(*self.pstate).sksiall)/$
                (*pph_parms).gain,$
                ' cts/s', FORMAT='(A,F10.2,A,F8.2,A)'),$
        '',$
        string('Object   all',$
           (*self.pstate).pnskall/(*pph_parms).gain, ' +- ',$
           (*self.pstate).pnskerall/(*pph_parms).gain, ' cts/s',$
           FORMAT='(A,F9.2,A,F8.2,A)'),$
        string('    w/o last',$
           (*self.pstate).pnskcum/(*pph_parms).gain, ' +- ',$
           (*self.pstate).pnskercum/(*pph_parms).gain, ' cts/s',$
           FORMAT='(A,F9.2,A,F8.2,A)'),$
        '',$
        string('Sky w/o last', (*self.pstate).skavcum, ' +- ',$
                (*self.pstate).sksicum, ' cts',$
                FORMAT='(A,F9.2,A,F8.2,A)'),$
        string('     new set', (*self.pstate).skav, ' +- ',$
                (*self.pstate).sksi, ' cts/pix',$
                FORMAT='(A,F9.2,A,F8.2,A)'),$
        '',$
        string('Mag      all', (*self.pstate).magall, ' +- ',$
                (*self.pstate).magerall, FORMAT='(A,F9.4,A,F7.4)'),$
        string('    w/o last', (*self.pstate).magcum, ' +- ',$
                (*self.pstate).magercum, FORMAT='(A,F9.4,A,F7.4)')]

   widget_control, (*self.pstate).statsid, SET_VALUE=text
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr::addvalue
; Adds a new pixel to the batch being collected.
; -----------------------------------------------------------------------------
pro itoolwacpmgr::addvalue, value
   compile_opt hidden

   ; 'value' is a structure: {x:x, y:y}

   self.oitool->getproperty, OIMAGE=oimage, PPH_PARMS=pph_parms
   oimage->getproperty, PIM_PARMS=pim_parms
   pimage = (*pim_parms).imageptr

   frame = (*pim_parms).frame

   (*self.pstate).ncli = (*self.pstate).ncli + 1

   case (*self.pstate).ncli of
      0 : begin
         ; This should be the object.

         basphote, (*pph_parms).gain, (*pimage)[*,*,frame],$
         (*pim_parms).exptime,$
         value.x, value.y, (*pph_parms).radius, 0.0, 0.0,$
         /NOLOG, /SILENT, FLUX=flux, FLERR=fluxerr, MAG=mag, ERR=magerr,$
         RDNOISE=(*pph_parms).rdnoise, $
         XCEN=xcen, YCEN=ycen

         (*self.pstate).pnskall   = flux
         (*self.pstate).pnskerall = fluxerr
         (*self.pstate).magall    = mag
         (*self.pstate).magerall  = magerr
         (*self.pstate).objx = xcen
         (*self.pstate).objy = ycen

         self->display
         widget_control, (*self.pstate).newid, SENSITIVE=1
         widget_control, (*self.pstate).doneid, SENSITIVE=1
      end

      1: begin
         ; This is the first sky batch.
         (*self.pstate).skyx = value.x
         (*self.pstate).skyy = value.y

         getannul,(*pimage)[*,*,frame],value.x,value.y,0,(*pph_parms).radius,$
         px, *self.pidx_p

         *self.pidxcum_p = 0
         *self.pidxall_p = *self.pidx_p

         self->ac1
         self->display
      end

      else : begin
         ; Additional sky spots.
         if n_elements(*self.pidx_p) gt 1 then begin
            t = string((*self.pstate).ncli-1, (*self.pstate).skyx,$
            (*self.pstate).skyy,$
            (*self.pstate).skav, (*self.pstate).sksi, (*self.pstate).skavall,$
            (*self.pstate).sksiall,$
            FORMAT='(I5,I5,I5,1X,G10.6,G10.6,G12.7,G12.7)')

            widget_control, (*self.pstate).dlistid, SET_VALUE=t, /APPEND
            *self.pidxcum_p = *self.pidxall_p
         endif

         (*self.pstate).skyx = value.x
         (*self.pstate).skyy = value.y
         
         getannul, (*pimage)[*,*,frame],value.x,value.y,0,(*pph_parms).radius,$
         px, *self.pidx_p

         a = [*self.pidxcum_p, *self.pidx_p]
         *self.pidxall_p = a[uniq(a, sort(a))]
;        pidx = pidx

         self->ac1
         self->display
         widget_control, (*self.pstate).dlastid, SENSITIVE=1
      end
   endcase

   ; Sensitize some buttons.
   widget_control, (*self.pstate).newid, SENSITIVE=1
   widget_control, (*self.pstate).clearid, SENSITIVE=1
   widget_control, (*self.pstate).dlastid, SENSITIVE=1
   widget_control, (*self.pstate).doneid, SENSITIVE=1
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr::getproperty
; -----------------------------------------------------------------------------
pro itoolwacpmgr::getproperty, OITOOL=oitool, PSTATE=pstate
   compile_opt hidden

   if arg_present(oitool) then oitool = self.oitool
   if arg_present(pstate) then pstate = self.pstate
end



; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr::realize
; -----------------------------------------------------------------------------
pro itoolwacpmgr::realize, NO_BLOCK=no_block
   compile_opt hidden

   widget_control, self.tlb, /REALIZE

   xmanager, 'itoolwacpmgr', self.tlb, NO_BLOCK=no_block,$
      CLEANUP='itoolwacpmgr_cleanup'

   self->display, /INIT
end


; -----------------------------------------------------------------------------
; Function itoolwacpmgr::init
; -----------------------------------------------------------------------------
function itoolwacpmgr::init, oitool, GROUP_LEADER=group_leader
   compile_opt hidden

   tlb = widget_base(TITLE='Itool Comet Photometry Manager', COLUMN=1,$
         GROUP_LEADER=group_leader, MBAR=mbar)

   ; Define the state control structure.
   state = { $
      clearid:0L,$
      dlastid:0L,$
      dlistid:0L,$
      doneid:0L,$
      drawid:0L,$
      magall:99.9999,$
      magcum:99.9999,$
      magerall:0.0,$
      magercum:0.0,$
      newid:0L,$
      ncli:-1L,$
      objx:0.0,$
      objy:0.0,$
      pidx_p:ptr_new(/ALLOCATE_HEAP),$
      pidxall_p:ptr_new(/ALLOCATE_HEAP),$
      pidxcum_p:ptr_new(/ALLOCATE_HEAP),$
      pnsk:0.0,   pnskcum:0.0,   pnskall:0.0,$
      pnsker:0.0, pnskercum:0.0, pnskerall:0.0,$
      skyx:0, skyy:0,$
      skpxcum:0L, skpxall:0L,$
      skpxuall:0L,$
      skav:0.0, skavcum:0.0, skavall:0.0,$
      sksi:0.0, sksicum:0.0, sksiall:0.0,$
      statsid:0L$
   }

   ; File Menu.
   filemenuid = widget_button(mbar, VALUE='File', /MENU)

   dummy = widget_button(filemenuid, VALUE='Dismiss',$
      EVENT_PRO='itoolwacpmgr_dismiss_event')

   state.doneid = widget_button(filemenuid, VALUE='Done/Save',$
      EVENT_PRO='itoolwacpmgr_done_event', SENSITIVE=0)

   ; Action Menu.
   actionmenuid = widget_button(mbar, VALUE='Action', /MENU)

   state.newid = widget_button(actionmenuid, VALUE='Start Over',$
      EVENT_PRO='itoolwacpmgr_new_event', SENSITIVE=0)

   state.clearid = widget_button(actionmenuid, VALUE='Clear All Skys',$
      EVENT_PRO='itoolwacpmgr_clear_event', SENSITIVE=0)


   state.dlastid = widget_button(actionmenuid, VALUE='Delete Last Sky',$
      EVENT_PRO='itoolwacpmgr_dlast_event', SENSITIVE=0)



   ; Stats and histogram section.
   rb  = widget_base(tlb, ROW=1, FRAME=1, UVALUE=0)
   cb1 = widget_base(rb, COLUMN=1, UVALUE=0)
   cb2 = widget_base(rb, COLUMN=1, UVALUE=0)

   tb  = cb1
   state.statsid = widget_text(tb, VALUE='', XSIZE=42, YSIZE=15)

   tb  = cb2
   state.drawid  = widget_draw(tb, XSIZE=200, YSIZE=200)

   ; Scrollable text widget for display of accepted data.
   ; First, do the label for the columns.
   rb  = widget_base(tlb, /ROW)
   ;                    1         2         3         4         5         6
   ;           12345678901234567890123456789012345678901234567890123456789012345
   displist = ' spot    x    y    average     sigma     cum avg     cum sig'
   l1  = widget_label(rb, VALUE=displist)
   state.dlistid = widget_text(tlb, VALUE='', /SCROLL, XSIZE=60, YSIZE=4)


   self.tlb = tlb
   self.oitool = oitool

   ; Copies of these pointers are stored in the state structure and in the
   ; object instance, so that their data are easily available to method
   ; routines and event handlers. Note that the pointers are allocated on
   ; the heap in the state structure.
   self.pidx_p = state.pidx_p
   self.pidxcum_p = state.pidxcum_p
   self.pidxall_p = state.pidxall_p

   ; Create a pointer to the state structure and store it in the object
   ; instance of this application.
   self.pstate = ptr_new(state, /no_copy)

   ; Store the object reference to this application in the UVALUE of the
   ; top-level base
   widget_control, tlb, SET_UVALUE=self

   ; Return successful initialization.
   return, 1
end


; ----------------------- Event Handlers and Cleanup --------------------------

; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr_cleanup
; -----------------------------------------------------------------------------
pro itoolwacpmgr_cleanup, tlb
   compile_opt hidden

   widget_control, tlb, GET_UVALUE=oitoolwacpmgr
   obj_destroy, oitoolwacpmgr
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr_dismiss_event
; Event handler for the 'Dismiss' button.
; -----------------------------------------------------------------------------
pro itoolwacpmgr_dismiss_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwacpmgr
   oitoolwacpmgr->getproperty, PSTATE=pstate

   if (*pstate).ncli ne -1 then begin
      t = [$
        'You have a pending photometric extraction.  If you choose to',$
        'proceed, this pending operation will be cancelled.  If you do',$
        'not want to lose the pending measurement, then select cancel',$
        'and finish the measurement by clicking Done/Save. Once you',$
        'have done this, then click Dismiss again.']

      con = qannounc(t, TITLE='Comet Photometry Dismiss Confirmation',$
         FALSE='Cancel this request',$
         TRUE='Ok, delete pending measurement',$
         XSIZE=max(strlen(t)), YSIZE=n_elements(t), GROUP_LEADER=event.top)
   endif else begin
      con=1
   endelse

   ; Proceed if okay
   if con then begin
      widget_control, event.top, /destroy
   endif
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr_new_event
; Event handler for the 'Start Over' button.
; -----------------------------------------------------------------------------
pro itoolwacpmgr_new_event, event
   compile_opt hidden

   widget_control, event.id, SENSITIVE=0
   widget_control, event.top, GET_UVALUE=oitoolwacpmgr

   oitoolwacpmgr->getproperty, PSTATE=pstate

   (*pstate).ncli = -1
   (*pstate).objx = 0.0
   (*pstate).objy = 0.0

   pidx_p = (*pstate).pidx_p
   pidxcum_p = (*pstate).pidxcum_p
   pidxall_p = (*pstate).pidxall_p

   *pidx_p    = 0
   *pidxcum_p = 0
   *pidxall_p = 0

   oitoolwacpmgr->ac1

   oitoolwacpmgr->display, /INIT

   widget_control, (*pstate).clearid, SENSITIVE=0
   widget_control, (*pstate).doneid, SENSITIVE=0
   widget_control, (*pstate).dlastid, SENSITIVE=0
   widget_control, (*pstate).dlistid, SET_VALUE=''
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr_clear_event
; Event handler of the 'Clear All Skys' button.
; -----------------------------------------------------------------------------
pro itoolwacpmgr_clear_event, event
   compile_opt hidden

   widget_control, event.id, SENSITIVE=0

   ; Get the object reference of this application.
   widget_control, event.top, GET_UVALUE=oitoolwacpmgr

   ; Get the itool object reference and the pointer to this application's
   ; state structure.
   oitoolwacpmgr->getproperty, OITOOL=oitool, PSTATE=pstate

   ; Now, get some pointers from itool.
   oitool->getproperty, OIMAGE=oimage, PPH_PARMS=pph_parms
   oimage->getproperty, PIM_PARMS=pim_parms
   pimage = (*pim_parms).imageptr

   (*pstate).ncli = 0

   pidx_p = (*pstate).pidx_p
   pidxcum_p = (*pstate).pidxcum_p
   pidxall_p = (*pstate).pidxall_p

   *pidx_p    = 0
   *pidxcum_p = 0
   *pidxall_p = 0

   oitoolwacpmgr->ac1

   basphote,(*pph_parms).gain,(*pimage)[*,*,(*pim_parms).frame],$
   (*pim_parms).exptime,$
   (*pstate).objx, (*pstate).objy, (*pph_parms).radius, 0.0, 0.0,$
   /NOLOG, /SILENT, FLUX=flux, FLERR=fluxerr, MAG=mag, ERR=magerr,$
   RDNOISE=(*pph_parms).rdnoise, $
   XCEN=xcen, YCEN=ycen

   (*pstate).pnskall   = flux
   (*pstate).pnskerall = fluxerr
   (*pstate).magall    = mag
   (*pstate).magerall  = magerr
   (*pstate).objx = xcen
   (*pstate).objy = ycen

   oitoolwacpmgr->display, /INIT
   widget_control, (*pstate).dlastid, SENSITIVE=0
   widget_control, (*pstate).dlistid, SET_VALUE=''
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr_dlast_event
; Event handler for the 'Delete Last Sky' button.
; -----------------------------------------------------------------------------
pro itoolwacpmgr_dlast_event, event
   compile_opt hidden

   widget_control, event.id, SENSITIVE=0

   ; Get the object reference of this application.
   widget_control, event.top, GET_UVALUE=oitoolwacpmgr

   ; Get some data in this application's object instance: the itool
   ; object reference and the pointer to this application's state structure.
   oitoolwacpmgr->getproperty, OITOOL=oitool, PSTATE=pstate

   ; Now, get some pointers from itool.
   oitool->getproperty, OIMAGE=oimage, PPH_PARMS=pph_parms
   oimage->getproperty, PIM_PARMS=pim_parms
   pimage = (*pim_parms).imageptr

   (*pstate).ncli = (*pstate).ncli - 1
   pidx_p = (*pstate).pidx_p
   pidxcum_p = (*pstate).pidxcum_p
   pidxall_p = (*pstate).pidxall_p

   *pidx_p = 0

   if (*pstate).ncli eq 0 then begin
      *pidxcum_p = 0
      *pidxall_p = 0
      oitoolwacpmgr->ac1

      basphote,(*pph_parms).gain,(*pimage)[*,*,(*pim_parms).frame],$
      (*pim_parms).exptime,$
      (*pstate).objx, (*pstate).objy, (*pph_parms).radius, 0.0, 0.0,$
      /NOLOG, /SILENT, FLUX=flux, FLERR=fluxerr, MAG=mag, ERR=magerr,$
      RDNOISE=(*pph_parms).rdnoise, $
      XCEN=xcen, YCEN=ycen

      (*pstate).pnskall   = flux
      (*pstate).pnskerall = fluxerr
      (*pstate).magall    = mag
      (*pstate).magerall  = magerr
      (*pstate).objx = xcen
      (*pstate).objy = ycen
      oitoolwacpmgr->disp, /INIT
      widget_control, (*pstate).clearid, SENSITIVE=0
   endif else begin
      *pidxall_p = *pidxcum_p
      oitoolwacpmgr->ac1
      oitoolwacpmgr->display
   endelse
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr_done_event
; Event handler for the 'Done/Save' button.
; -----------------------------------------------------------------------------
pro itoolwacpmgr_done_event, event
   compile_opt hidden

   ; Get the object reference of this application.
   widget_control, event.top, GET_UVALUE=oitoolwacpmgr

   ; Get some data in this application's object instance: the itool
   ; object reference and the pointer to this application's state structure.
   oitoolwacpmgr->getproperty, OITOOL=oitool, PSTATE=pstate

   ; Now, get some pointers from itool.
   oitool->getproperty, OIMAGE=oimage, PPH_PARMS=pph_parms
   oimage->getproperty, PIM_PARMS=pim_parms
   pimage = (*pim_parms).imageptr

   ; Do the photometry.
   objnum = (*pph_parms).objnum

   basphote, (*pph_parms).gain, (*pimage)[*,*,(*pim_parms).frame],$
   (*pim_parms).exptime,$
   (*pstate).objx, (*pstate).objy, (*pph_parms).radius,$
   (*pstate).skavall,$
   -(*pstate).sksiall, (*pph_parms).logfile, objnum,$
   AIRMASS=(*pim_parms).airmass, /ALTLOG,$
   BOXMRAD=(*pph_parms).boxmrad, EXACT=(*pph_parms).exact,$
   NAME=(*pim_parms).object, NOMEXT=(*pph_parms).nomext,$
   FILTER=(*pim_parms).filter,$
   FNAME=(*pim_parms).imfile, JD=(*pim_parms).jd,$
   PSCALE=(*pph_parms).platescale, RDNOISE=(*pph_parms).rdnoise, $
   ZPOINT=(*pph_parms).zpoint

   (*pph_parms).objnum = objnum

   (*pstate).ncli = -1
   (*pstate).objx = 0.0
   (*pstate).objy = 0.0

   pidx_p = (*pstate).pidx_p
   pidxcum_p = (*pstate).pidxcum_p
   pidxall_p = (*pstate).pidxall_p

   *pidx_p    = 0
   *pidxcum_p = 0
   *pidxall_p = 0

   oitoolwacpmgr->ac1
   oitoolwacpmgr->display, /INIT

   ; De-sensitize some widgets.
   widget_control, (*pstate).newid, SENSITIVE=0
   widget_control, (*pstate).clearid, SENSITIVE=0
   widget_control, (*pstate).dlastid, SENSITIVE=0
   widget_control, event.id, SENSITIVE=0
   widget_control, (*pstate).dlistid, SET_VALUE=''
end


; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr_event
; Default event handler. Should not be called, but is here in case default
; events might be added.
; -----------------------------------------------------------------------------
pro itoolwacpmgr_event, event
   compile_opt hidden

   help, event, /structure
end



; -----------------------------------------------------------------------------
; Procedure itoolwacpmgr__define
; Defines the object class named itoolwacpmgr.
;
; Attributes:
;   tlb       : top-level base.
;   pstate    : pointer to state structure.
;   oitool    : object reference of the host instance of itool.
;   pidx_p    : pointer to array of new pixels.
;   pidxcum_p : pointer to array of accepted pixels.
;   pidxall_p : pointer to the sum of the above two arrays.
; -----------------------------------------------------------------------------
pro itoolwacpmgr__define
   dummy = {itoolwacpmgr, tlb:0L, pstate:ptr_new(), oitool:obj_new(),$
      pidx_p:ptr_new(), pidxcum_p:ptr_new(), pidxall_p:ptr_new()}
end

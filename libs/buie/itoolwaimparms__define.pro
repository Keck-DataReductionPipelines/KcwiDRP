;+
; CLASS_NAME:
;    itoolwaimparms
;
; PURPOSE (one line):
;    To display and edit itool image parameters.
;
; CATEGORY:
;    Widgets
;
; SUPERCLASSES:
;
; SUBCLASSES:
;
; CREATION:
;    See itoolwaimparms::init
;
; METHODS:
;    itoolwaimparms::cleanup
;    itoolwaimparms::getproperty
;    itoolwaimparms::realize
;    itoolwaimparms::refresh
;    itoolwaimparms::init
;
; MODIFICATION HISTORY:
;    2004/04, Written by Doug Loucks, Consultant for Lowell Observatory.
;    (See cw_ipmgr.pro)
;    Removed all remnants of compound-widget code; replaced with
; stand-alone code that is compaitble with the new object-oriented version
; of itool.
;    Modified the usage of the state-structure variable. A pointer to
; the state structure is stored in the object instance of this tool and
; this tool's object reference is stored in its top-level base.
;
;    2006/03/15 - DWL, Minor modifications to use the methods of the new
;                   'itoolimage' object class that was added to the itool GUI.
;                   See itoolimage__define.pro and itool__define.pro for
;                   details.
;
;-

; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaimparms::cleanup
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    obj_destroy, oref
;
; INPUTS:
;    oref : An itoolwaimparms object reference.
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
;    itoolwaimparms::getproperty
;
; PURPOSE:
;   To retrieve "properties" defined for the itoolwaimparms object
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
;    itoolwaimparms::realize
;
; PURPOSE:
;    To realize a new, managed instance of the itoolwaimparms object class.
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
;    itoolwaimparms::refresh
;
; PURPOSE:
;    To refresh the widgets in this tool.
;
; CALLING SEQUENCE:
;    oref->refresh
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
;    itoolwaimparms::init
;
; PURPOSE:
;    To initialize a new instance of the itoolwaimparms object class.
;
; CALLING SEQUENCE:
;    oref = obj_new('itoolwaimparms', oitool)
;
; INPUTS:
;    oitool : An object reference of a host instance of the object class
;             "itool."
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    GROUP_LEADER = The group leader for this tool.
;
; OUTPUTS:
;    oref = The object reference of the new instance of the itoolwaimparms
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
;    oref = obj_new('itoolwaimparms', oitool, GROUP_LEADER=group_leader)
;    oref->realize
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------


; ----------------- Method Routines ------------------------------------------

; ----------------------------------------------------------------------------
; Procedure itoolwaimparms::cleanup
; ----------------------------------------------------------------------------
pro itoolwaimparms::cleanup
   compile_opt hidden

   ptr_free, self.pstate
end


; ----------------------------------------------------------------------------
; Procedure itoolwaimparms::getproperty
; ----------------------------------------------------------------------------
pro itoolwaimparms::getproperty, OITOOL=oitool, PSTATE=pstate
   compile_opt hidden

   if arg_present(oitool) then oitool = self.oitool
   if arg_present(pstate) then pstate = self.pstate
end



; ----------------------------------------------------------------------------
; Procedure itoolwaimparms::realize 
; ----------------------------------------------------------------------------
pro itoolwaimparms::realize, NO_BLOCK=no_block
   compile_opt hidden

   widget_control, self.tlb, /realize

   xmanager, 'itoolwaimparms', self.tlb, NO_BLOCK=no_block,$
      CLEANUP='itoolwaimparms_cleanup'
end


; ----------------------------------------------------------------------------
; Procedure itoolwaimparms::refresh
; Refreshes the widgets in the "Image Params" GUI, if it is active.
; ----------------------------------------------------------------------------
pro itoolwaimparms::refresh
   compile_opt hidden

   ; Get itool's image-parameters pointer.
   self.oitool->getproperty, OIMAGE=oimage
   oimage->getproperty, PIM_PARMS=pim_parms

   ; A format string.
   fmt1 = '(G0.2)'

   ; Refresh all of the widgets in the "Image Params" GUI.
   widget_control, (*self.pstate).imfileid, SET_VALUE=(*pim_parms).imfile

   widget_control, (*self.pstate).airmassid,$
      SET_VALUE=string((*pim_parms).airmass, FORMAT=fmt1)

   widget_control, (*self.pstate).exptimeid,$
      SET_VALUE=string((*pim_parms).exptime, FORMAT=fmt1)

   widget_control, (*self.pstate).expdeltaid, $
      SET_VALUE=string((*pim_parms).expdelta, FORMAT=fmt1)

   widget_control, (*self.pstate).filterid, SET_VALUE=(*pim_parms).filter
   widget_control, (*self.pstate).objectid, SET_VALUE=(*pim_parms).object
   widget_control, (*self.pstate).dateid, SET_VALUE=(*pim_parms).date
   widget_control, (*self.pstate).utid, SET_VALUE=(*pim_parms).ut
   julian = strtrim(string((*pim_parms).jd, FORMAT='(D15.5)'), 2)
   widget_control, (*self.pstate).jdid, SET_VALUE=julian
end



; ----------------------------------------------------------------------------
; Function itoolwaimparms::init
; ----------------------------------------------------------------------------
function itoolwaimparms::init, oitool, GROUP_LEADER=group_leader
   compile_opt hidden

   tlb = widget_base(TITLE='Itool Image Parameters', COLUMN=1,$
      GROUP_LEADER=group_leader, MBAR=mbar)

   ; Local state structure.
   state = {$
      airmassid:0L,$
      dateid:0L,$
      expdeltaid:0L,$
      exptimeid:0L,$
      filterid:0L,$
      imfileid:0L,$
      jdid:0L,$
      objectid:0L,$
      oitool:oitool,$
      utid:0L}

   ; Get a pointer to itool's image parameters.
   oitool->getproperty, OIMAGE=oimage
   oimage->getproperty, PIM_PARMS=pim_parms
 
   ; File Menu.
   filemenuid = widget_button(mbar, VALUE='File', /MENU)

   dummy = widget_button(filemenuid, VALUE='Verify',$
      EVENT_PRO='itoolwaimparms_verify_event')

   dummy = widget_button(filemenuid, VALUE='Dismiss',$
      EVENT_PRO='itoolwaimparms_dismiss_event')

   wb = widget_base(tlb, /ROW)

   ; The events from all of the text widgets will be handled by the
   ; default event handler (itoolwaimparms_event).

   state.imfileid = widget_text(wb, VALUE=(*pim_parms).imfile,$
      /EDITABLE, UNAME='imfile', XSIZE=15)

   w1 = widget_label(wb, VALUE='Image file name')

   f = '(G0.2)'
   wb = widget_base(tlb, /ROW)

   state.airmassid = widget_text(wb, VALUE=string((*pim_parms).airmass,$
      FORMAT=f), UNAME='airmass', XSIZE=15, /EDITABLE)

   w1 = widget_label(wb, VALUE='Airmass')

   wb = widget_base(tlb, /ROW)

   state.exptimeid = widget_text(wb, VALUE=string((*pim_parms).exptime,$
      FORMAT=f), UNAME='exptime', XSIZE=15, /EDITABLE)

   w1 = widget_label(wb, VALUE='Exposure time (seconds)')

   wb = widget_base(tlb, /ROW)

   state.expdeltaid = widget_text(wb, VALUE=string((*pim_parms).expdelta,$
      FORMAT=f), UNAME='expdelta', XSIZE=15, /EDITABLE)

   w1 = widget_label(wb, VALUE='Exposure delta (seconds)')

   wb = widget_base(tlb, /ROW)
 
   state.filterid = widget_text(wb, VALUE=(*pim_parms).filter,$
      UNAME='filter', XSIZE=15, /EDITABLE)

   w1 = widget_label(wb, VALUE='Filter code')

   wb = widget_base(tlb, /ROW)

   state.objectid = widget_text(wb, VALUE=(*pim_parms).object,$
      UNAME='object', XSIZE=15, /EDITABLE)

   w1 = widget_label(wb, VALUE='Object')
   wb = widget_base(tlb, /ROW)

   state.dateid = widget_text(wb, VALUE=(*pim_parms).date, /EDITABLE,$
     UNAME='date',  XSIZE=15)

   w1 = widget_label(wb, VALUE='Date of observation')

   wb = widget_base(tlb, /ROW)

   state.utid = widget_text(wb, VALUE=(*pim_parms).ut, /EDITABLE,$
     UNAME='ut',  XSIZE=15)

   w1 = widget_label(wb, VALUE='Start Time (UT)')

   wb = widget_base(tlb, /ROW)
   julian = strtrim(string((*pim_parms).jd, FORMAT='(D15.5)'), 2)
   state.jdid = widget_text(wb, VALUE=julian, XSIZE=15)
   w1 = widget_label(wb, VALUE='Julian Date (mid-time)')


   ; Initialize encapsulated data.
   self.tlb = tlb
   self.pstate = ptr_new(state, /NO_COPY)
   self.oitool = oitool

   widget_control, tlb, SET_UVALUE=self

   ; Return successful initialization.
   return, 1
end


; ----------------------------------------------------------------------------
; Procedure itoolwaimparms_cleanup
; ----------------------------------------------------------------------------
pro itoolwaimparms_cleanup, tlb
   compile_opt hidden

   widget_control, tlb, GET_UVALUE=oitoolwaimparms
   obj_destroy, oitoolwaimparms
end



; ----------------- Event Handlers -------------------------------------------


; ----------------------------------------------------------------------------
; Procedure itoolwaimparms_dismiss_event
; ----------------------------------------------------------------------------
pro itoolwaimparms_dismiss_event, event
   compile_opt hidden

   widget_control, event.top, /DESTROY
end


; ----------------------------------------------------------------------------
; Procedure itoolwaimparms_verify_event
; ----------------------------------------------------------------------------
pro itoolwaimparms_verify_event, event
   compile_opt hidden

   ; Get itool's pointer to the image-parameters structure.
   widget_control, event.top, GET_UVALUE=oitoolwaimparms
   oitoolwaimparms->getproperty, OITOOL=oitool
   oitool->getproperty, OIMAGE=oimage
   oimage->getproperty, PIM_PARMS=pim_parms

   print,' '
   print,'Image file name _ _ _' + (*pim_parms).imfile
   print,'Airmass _ _ _ _ _ _ _' + string((*pim_parms).airmass)
   print,'Exposure time _ _ _ _' + string((*pim_parms).exptime)
   print,'Exposure delta _ _ __' + string((*pim_parms).expdelta)
   print,'Filter code _ _ _ _ _' + (*pim_parms).filter
   print,'Object _ _ _ _ _ _ __' + (*pim_parms).object
   print,'Date of observation _' + (*pim_parms).date
   print,'Time (UT) _ _ _ _ _ _' + (*pim_parms).ut
   print,'Julian Date _ _ _ _ _'+string((*pim_parms).jd,FORMAT='(D15.5)')
end


; ----------------------------------------------------------------------------
; Procedure itoolwaimparms_event
; ----------------------------------------------------------------------------
pro itoolwaimparms_event, event
   compile_opt hidden

   ; Retrieve the itoolwaimparms object reference.
   widget_control, event.top, GET_UVALUE=oitoolwaimparms

   ; Get the itool object reference.
   oitoolwaimparms->getproperty, OITOOL=oitool

   ; Get the value of the widget that generated the event.
   widget_control, event.id, GET_VALUE=value
   val = value[0]

   ; Initialize some local variables.
   name = ''

   ; Get a pointer to itool's image parameters.
   oitool->getproperty, OIMAGE=oimage
   oimage->getproperty, PIM_PARMS=pim_parms

   ; Get the user name of the widget.
   uname = widget_info(event.id, /UNAME)

   case uname of
      'imfile' : begin
         (*pim_parms).imfile = val
         name = uname
      end

      'airmass' : begin
         (*pim_parms).airmass = val
         name = uname
      end

      'exptime' : begin
         (*pim_parms).exptime = val
         name = uname
      end

      'expdelta' : begin
         (*pim_parms).expdelta = val
         name = uname
      end

      'filter' : begin
         (*pim_parms).filter = val
         name = uname
      end

      'object' : begin
         (*pim_parms).object = val
         name = uname
      end

      'date' : begin
         (*pim_parms).date = val
         name = uname
      end

      'ut' : begin
         (*pim_parms).ut = val
         name = uname
      end

      else : begin
         print, 'Unexpected event:'
         help, event, /STRUCTURE
      end
   endcase

   ; Print a message about the new value of the parameter that changed.
   if name ne '' then print, name,' set to ',val
end


; ----------------------------------------------------------------------------
; Procedure itoolwaimparms__define
; Define the itoolwaimparms object class.
;
; Attributes:
;   tlb    : top-level base.
;   pstate : pointer to state structure.
;   oitool : object reference of the host instance of itool.
; ----------------------------------------------------------------------------
pro itoolwaimparms__define
   dummy = {itoolwaimparms, tlb:0L, pstate:ptr_new(), oitool:obj_new()}
end

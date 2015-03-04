;+
; CLASS_NAME:
;    itoolwaphparms
;
; PURPOSE (one line):
;    To display and edit itool photometry parameters.
;
; CATEGORY:
;    Widgets
;
; SUPERCLASSES:
;
; SUBCLASSES:
;
; CREATION:
;    See itoolwaphparms::init
;
; METHODS:
;    itoolwaphparms::cleanup
;    itoolwaphparms::getproperty
;    itoolwaphparms::realize
;    itoolwaphparms::init
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaphparms::cleanup
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    obj_destroy, oref
;
; INPUTS:
;    oref : An itoolwaphparms object reference.
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
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaphparms::getproperty
;
; PURPOSE:
;   To retrieve "properties" defined for the itoolwaphparms object
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
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaphparms::realize
;
; PURPOSE:
;    To realize a new, managed instance of the itoolwaphparms object class.
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
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaphparms::init
;
; PURPOSE:
;    To initialize a new instance of the itoolwaphparms object class.
;
; CALLING SEQUENCE:
;    oref = obj_new('itoolwaphparms', oitool)
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
;    oref = The object reference of the new instance of the itoolwaphparms
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
;    oref = obj_new('itoolwaphparms', oitool, GROUP_LEADER=group_leader)
;    oref->realize
;
; -----------------------------------------------------------------------------
;
; MODIFICATION HISTORY:
;    2004/04/01, Written by Doug Loucks, Consultant for Lowell Observatory.
;    (See also cw_ppmgr.pro)
;    Removed all remnants of compound-widget code; replaced with
;      stand-alone code that is compaitble with the new object-oriented version
;      of itool.
;    Modified the usage of the state-structure variable. A pointer to
;      the state structure is stored in the object instance of this tool and
;      this tool's object reference is stored in its top-level base.
;    2006/5/22 Peter L. Collins, Lowell Observatory
;               add parameter for CCD readout noise (rdnoise)
;    2006/08/07, MWB, fixed some documentation issues and relocated readnoise
;                  widget on GUI.
;
;-


; -----------------------------------------------------------------------------
; Procedure itoolwaphparms::cleanup
; -----------------------------------------------------------------------------
pro itoolwaphparms::cleanup
   compile_opt hidden

   ptr_free, self.pstate
end



; -----------------------------------------------------------------------------
; Procedure itoolwaphparms::getproperty
; -----------------------------------------------------------------------------
pro itoolwaphparms::getproperty, OITOOL=oitool, PSTATE=pstate
   compile_opt hidden

   if arg_present(oitool) then oitool = self.oitool
   if arg_present(pstate) then pstate = self.pstate
end


; -----------------------------------------------------------------------------
; Procedure itoolwaphparms::realize
; -----------------------------------------------------------------------------
pro itoolwaphparms::realize, NO_BLOCK=no_block
   compile_opt hidden

   widget_control, self.tlb, /REALIZE

   xmanager, 'itoolwaphparms', self.tlb, NO_BLOCK=no_block,$
      CLEANUP='itoolwaphparms_cleanup'
end


; -----------------------------------------------------------------------------
; Function itoolwaphparms::init
; -----------------------------------------------------------------------------
function itoolwaphparms::init, oitool, GROUP_LEADER=group_leader
   compile_opt hidden

   tlb = widget_base(TITLE='Itool Photometry Parameters', COLUMN=1,$
         GROUP_LEADER=group_leader, MBAR=mbar)

   ;Local state structure.
   state = {$
      boxmradid:0L,$
      gainid:0L,$
      logfileid:0L,$
      nomextid:0L,$
      parmsfileid:0L,$
      pscaleid:0L,$
      radiusid:0L,$
      sky1id:0L,$
      sky2id:0L,$
      zpointid:0L,$
      rdnoiseid:0L $
   }

   ; Get a pointer to itool's Photometry Parameters.
   oitool->getproperty, PPH_PARMS=pph_parms

   ; File Menu.
   filemenuid = widget_button(mbar, VALUE='File', /MENU)

   dummy = widget_button(filemenuid, VALUE='Load', UNAME='load')
   dummy = widget_button(filemenuid, VALUE='Save', UNAME='save')
   dummy = widget_button(filemenuid, VALUE='Dismiss', UNAME='dismiss')

   wb = widget_base(tlb, /ROW)
   state.parmsfileid = widget_text(wb, VALUE=(*pph_parms).parmfile,$
     XSIZE=15, /EDITABLE, UNAME='pfile')
   w1 = widget_label(wb, VALUE='Photometry parameters file name')

   wb  = widget_base(tlb, /ROW)
   state.logfileid = widget_text(wb,VALUE=(*pph_parms).logfile,$
      XSIZE=15, /EDITABLE, UNAME='logfile')
   w1  = widget_label(wb, VALUE='Photometry log file name')

   wb  = widget_base(tlb, /ROW)
   state.radiusid = widget_text(wb, VALUE=string((*pph_parms).radius,$
            FORMAT='(G0.2)'), XSIZE=15, /EDITABLE, UNAME='radius')
   w1  = widget_label(wb, VALUE='Aperture radius (pixels)')

   wb  = widget_base(tlb, /ROW)
   state.sky1id = widget_text(wb, $
          VALUE=string((*pph_parms).sky1, FORMAT='(G0.2)'), XSIZE=15,$
          /EDITABLE, UNAME='sky1')
   w1  = widget_label(wb, VALUE='Inner radius of sky annulus (pixels)')

   wb  = widget_base(tlb, /ROW)
   state.sky2id = widget_text(wb, VALUE=$
          string((*pph_parms).sky2, FORMAT='(G0.2)'), XSIZE=15,$
          /EDITABLE, UNAME='sky2')
   w1  = widget_label(wb, VALUE='Outer radius of sky annulus (pixels)')

   wb  = widget_base(tlb, /ROW)
   state.boxmradid = widget_text(wb, VALUE=string((*pph_parms).boxmrad,$
             FORMAT='(G0.2)'), XSIZE=15, /EDITABLE, UNAME='boxmrad')
   w1  = widget_label(wb, VALUE='Local maximum box radius (pixels) ')

   wb  = widget_base(tlb, /ROW)
   state.gainid = widget_text(wb, VALUE=$
          string((*pph_parms).gain, FORMAT='(G0.2)'), XSIZE=15,$
          /EDITABLE, UNAME='gain')
   w1  = widget_label(wb, VALUE='Gain of CCD (e-/DN)')

   wb  = widget_base(tlb, /ROW)
   state.pscaleid = widget_text(wb,$
      VALUE=string((*pph_parms).platescale,FORMAT='(G0.2)'), XSIZE=15,$
      /EDITABLE, UNAME='pscale')
   w1  = widget_label(wb, VALUE='Plate scale (arcseconds/pixel)')

   wb   = widget_base(tlb, /ROW)
   state.rdnoiseid = widget_text(wb , VALUE=string((*pph_parms).rdnoise,$
      FORMAT='(G0.2)'), XSIZE=15, /EDITABLE, UNAME='rdnoise')
   w1  = widget_label(wb, VALUE='CCD Readout Noise (e-/pixel)')

   wb   = widget_base(tlb, /ROW)
   w1 = cw_bgroup(wb, ['Search for local maximum',$
      'Use exact position'], /EXCLUSIVE, /NO_RELEASE, /ROW,$
      SET_VALUE=(*pph_parms).exact, UNAME='toggle')

   wb = widget_base(tlb, /COLUMN, /FRAME)

   wb1  = widget_base(wb, /ROW)
   state.nomextid = widget_text(wb1, VALUE=string((*pph_parms).nomext,$
      FORMAT='(G0.2)'), XSIZE=15, /EDITABLE, UNAME='nomext')
   w1  = widget_label(wb1, VALUE='Nominal extinction (mags/airmass)')

   wb1  = widget_base(wb, /ROW)
   state.zpointid = widget_text(wb1, VALUE=string((*pph_parms).zpoint,$
      FORMAT='(G0.2)'), XSIZE=15, /EDITABLE, UNAME='zpoint')
   w1  = widget_label(wb1, VALUE='Zero point')

   ; Initialize encapsulated data.
   self.tlb = tlb
   self.pstate = ptr_new(state, /NO_COPY)
   self.oitool = oitool

   widget_control, tlb, SET_UVALUE=self

   ; Return successful initialization.
   return, 1
end


; ----------------------------------------------------------------------------
; Procedure itoolwaphparms_cleanup
; ----------------------------------------------------------------------------
pro itoolwaphparms_cleanup, tlb
   compile_opt hidden

   widget_control, tlb, GET_UVALUE=oitoolwaphparms
   obj_destroy, oitoolwaphparms
end


; -----------------------------------------------------------------------------
; Procedure itoolwaphparms_event
; Default (main) event handler.
; -----------------------------------------------------------------------------
pro itoolwaphparms_event, event
   compile_opt hidden

   ; Retrieve the itoolwaphparms object reference.
   widget_control, event.top, GET_UVALUE=oitoolwaphparms

   ; Get the itool object reference and the pointer to the state structure.
   oitoolwaphparms->getproperty, OITOOL=oitool, PSTATE=pstate

   ; Get the value of the widget that generated the event.
   widget_control, event.id, GET_VALUE=value
   val = value[0]

   ; Get the user name of the widget that generated the event.
   uname = widget_info(event.id, /UNAME)

   ; Initialize some local variables.
   name = ''

   ; Get a pointer to itool's photometry parameters.
   oitool->getproperty, PPH_PARMS=pph_parms

   case uname of
      'dismiss' : begin
         widget_control, event.top, /DESTROY
         return
      end

      'logfile' : begin
         (*pph_parms).logfile = val
         name = 'logfile'
         (*pph_parms).edtflg = 1
      end

      'pfile' : begin
         (*pph_parms).parmfile = STRTRIM(val, 2)
         name = 'Photometry parameters file'
      end

      'radius' : begin
         (*pph_parms).radius = val
         name = 'radius'
         (*pph_parms).edtflg = 1
      end

      'sky1' : begin
         (*pph_parms).sky1 = val
         name = 'sky1'
         (*pph_parms).edtflg = 1
      end

      'sky2' : begin
         (*pph_parms).sky2 = val
         name = 'sky2'
         (*pph_parms).edtflg = 1
      end

      'boxmrad' : begin
         (*pph_parms).boxmrad = val
         name = 'boxmrad'
         (*pph_parms).edtflg = 1
      end

      'gain' : begin
         (*pph_parms).gain = val
         name = 'gain'
         (*pph_parms).edtflg = 1
      end

      'pscale' : begin
         (*pph_parms).platescale = val
         name = 'platescale'
         (*pph_parms).edtflg = 1
      end

      'toggle' : begin
         (*pph_parms).exact = event.value
      end

      'load' : begin
         fname = dialog_pickfile(TITLE='Select a Photometry Paramaters File',$
            FILE=(*pph_parms).parmfile, DIALOG_PARENT=event.top,$
            /MUST_EXIST)

         if fname[0] eq '' then return

;        if (*pph_parms).edtflg then begin
            tmsg = [ '\\\\\\ WARNING //////', $
            'Existing photometry parameters will be replaced.',$
            'Do you wish to overwrite the existing parameters?' ]

            con = qannounc(tmsg, TITLE='Warning', $
               FALSELABEL='No, cancel load request',$
               TRUELABEL='Yes, overwrite', GROUP_LEADER=event.top,$
               XSIZE=max(strlen(tmsg)))

            if not con then return
;        endif

         fmt = '(G0.2)'

         ; Save a copy of the parameters structure.
         save_ph_parms = *pph_parms

         if (*pph_parms).parmfile ne fname[0] then begin
            (*pph_parms).parmfile = fname[0]
         endif

         ; Call the procedure that loads the parameters from a file.
         itool_pplod, *pph_parms, ERROR_STR=error_str

         if error_str ne '' then begin
            result = qannounc(error_str,$
               TITLE='Error', FALSELABEL='', TRUELABEL='Dismiss',$
               xsize = strlen(error_str), GROUP_LEADER=event.top)

            *pph_parms = save_ph_parms
            return
         endif

         ; Refresh the text widgets.
         widget_control,event.top, UPDATE=0
         widget_control,(*pstate).parmsfileid, SET_VALUE=(*pph_parms).parmfile
         widget_control,(*pstate).logfileid, SET_VALUE=(*pph_parms).logfile

         widget_control,(*pstate).radiusid,$
            SET_VALUE=string((*pph_parms).radius, FORMAT=fmt)

         widget_control,(*pstate).sky1id,SET_VALUE=string((*pph_parms).sky1,$
             FORMAT=fmt)

         widget_control,(*pstate).sky2id,SET_VALUE=string((*pph_parms).sky2,$
             FORMAT=fmt)

         widget_control,(*pstate).boxmradid,$
            SET_VALUE=string((*pph_parms).boxmrad, FORMAT=fmt)

         widget_control,(*pstate).gainid,SET_VALUE=string((*pph_parms).gain,$
             FORMAT=fmt)

         widget_control,(*pstate).pscaleid,$
            SET_VALUE=string((*pph_parms).platescale,$
            FORMAT=fmt)

         widget_control,(*pstate).nomextid,$
            SET_VALUE=string((*pph_parms).nomext, FORMAT=fmt)

         widget_control,(*pstate).zpointid,$
            SET_VALUE=string((*pph_parms).zpoint, FORMAT=fmt)

         widget_control,(*pstate).rdnoiseid,$
            SET_VALUE=string((*pph_parms).rdnoiseid, FORMAT=fmt)

         widget_control, event.top, UPDATE=1
      end

      'save' : begin
         if strcompress((*pph_parms).parmfile, /REMOVE_ALL) eq '' then begin
            msg = 'Parameters file not defined.'

            result = qannounc(msg, TITLE='Error',$
               FALSELABEL='', TRUELABEL='Dismiss',$
               xsize = strlen(msg), GROUP_LEADER=event.top)

            return
         endif

         if file_test((*pph_parms).parmfile) then begin
            msg = ['Photometry Parameters file ' + (*pph_parms).parmfile +$
               ' exists.', '', 'Replace?']

            confirm = qannounc(msg, TITLE='Confirmation',$
               FALSELABEL='No, cancel request',$
               TRUELABEL='Yes, replace file',$
               xsize=max(strlen(msg)), GROUP_LEADER=event.top)

            if not confirm then return
         endif

         itool_ppsav, *pph_parms, ERROR_STR=error_str

         if error_str ne '' then begin
            result = qannounc(error_str,$
               TITLE='Error', FALSELABEL='', TRUELABEL='Dismiss',$
               xsize = strlen(!error_state.msg),$
               GROUP_LEADER=event.top)
         endif

         print,(*pph_parms).parmfile,' saved.'
      end

      'nomext' : begin
         (*pph_parms).nomext = val
         name = 'nomext'
         (*pph_parms).edtflg = 1
      end

      'zpoint' : begin
         (*pph_parms).zpoint = val
         name = 'zpoint'
         (*pph_parms).edtflg = 1
      end

      'rdnoise' : begin
         (*pph_parms).rdnoise = val
         name = 'rdnoise'
         (*pph_parms).edtflg = 1
      end

      else : begin
         help, event, /STRUCTURE
      end
   endcase

   ; Print a message indicating the parameter changed.
   if name ne '' then print,name,' set to ',val
end


; -----------------------------------------------------------------------------
; Procedure itoolwaphparms__define
; Define itoolwaphparms object class.
;
; Attributes:
;   tlb    : top-level base.
;   pstate : pointer to state structure.
;   oitool : object reference of the host instance of itool.
; -----------------------------------------------------------------------------
pro itoolwaphparms__define
   dummy = {itoolwaphparms, tlb:0L, pstate:ptr_new(), oitool:obj_new()}
end

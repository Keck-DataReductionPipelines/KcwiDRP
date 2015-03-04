;+
; CLASS_NAME:
;    itoolwatpmgr
;
; PURPOSE (one line):
;    To define, edit, and manage itool photometry templates.
;
; CATEGORY:
;    Widgets
;
; SUPERCLASSES:
;
; SUBCLASSES:
;
; CREATION:
;    See itoolwatpmgr::init
;
; METHODS:
;    itoolwatpmgr::cleanup
;    itoolwatpmgr::getproperty
;    itoolwatpmgr::setproperty
;    itoolwatpmgr::refreshobjects
;    itoolwatpmgr::realize
;    itoolwatpmgr::init
;    itoolwatpmgr::refreshtemplates
;    itoolwatpmgr::addtemplate
;    itoolwatpmgr::purgetemplates
;    itoolwatpmgr::load
;    itoolwatpmgr::save
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, consultant for Lowell Observatory, March / April
; 2004.
;    Adapted from cw_tpmgr.pro and cw_tplat.pro.
;    Removed all remnants of compound-widget code; replaced with
; stand-alone code that is compaitble with the new object-oriented version
; of itool.
;    Modified the usage of the state-structure variable. A pointer to
; the state structure is stored in the object instance of this tool and
; this tool's object reference is stored in its top-level base.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwatpmgr::cleanup
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    obj_destroy, oref
;
; INPUTS:
;    oref : An itoolwatpmgr object reference.
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
;    itoolwatpmgr::getproperty
;
; PURPOSE:
;   To retrieve "properties" defined for the itoolwatpmgr object
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
;    NUM_TPLATES  : Set this keyword to a named variable into which will be
;                   placed the number of templates currently defined.
;    OITOOL       : Set this keyword to a named variable into which will be
;                   placed a copy of the object reference of a host instance
;                   of the "itool" object class.
;    PIDX_TPLATES : Set this keyword to a named variable into which will be
;                   placed a pointer to the array of sorted indices of
;                   the templates. Templates are sorted by their names.
;    PSTATE       : Set this keyword to a named variable into which will be
;                   placed a pointer to the state structure.
;    PTPLATES     : Set this keyword to a named variable into which will be
;                   placed a pointer to the array of template-reference
;                   structures.
;    SELECTED     : Set this keyword to a named variable into which will be
;                   placed the sorted index of the selected template. If
;                   a template has not been selected, -1 will be returned.
;                   The user selects a template by clicking in the list
;                   widget that displays the list of templates.
;    TPLATE       : Set this keyword to a named variable into which will be
;                   placed a copy of the currently-selected template
;                   reference. If a template has not been selected, a blank
;                   template-reference structure will be returned (null name
;                   string and null object reference).
;                   A template reference is defined by the itool_templatelist
;                   object class. The object-reference tag in a template-
;                   reference structure is used to access the template data
;                   and to call method routines that operate on the
;                   template data.
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
;    itoolwatpmgr::setproperty
;
; PURPOSE:
;    To set "properties" defined for the itoolwatpmgr object
; class. Properties are specified as keyword arguments.
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    NUM_TPLATES : The number of templates currently defined.
;    SELECTED    : The sorted index of the currently-selected template.
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
;    itoolwatpmgr::refreshobjects
;
; PURPOSE:
;    To refresh the list widget that displays the list of objects.
;
; CALLING SEQUENCE:
;    oref->refreshobjects
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
;    itoolwatpmgr::realize
;
; PURPOSE:
;    To realize a new, managed instance of the itoolwatpmgr object class.
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
;    itoolwatpmgr::init
;
; PURPOSE:
;    To initialize a new instance of the itoolwatpmgr object class.
;
; CALLING SEQUENCE:
;    oref = obj_new('itoolwatpmgr', oitool)
;
; INPUTS:
;    oitool : An object reference of a host instance of the object class
;             "itool."
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    GROUP_LEADER : The group leader for this tool.
;    TMPLFILE     : The name of a file containing a list templates.
;
; OUTPUTS:
;    oref = The object reference of the new instance of the itoolwatpmgr
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
;    oref = obj_new('itoolwatpmgr', oitool, GROUP_LEADER=group_leader)
;    oref->realize
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwatpmgr::refreshtemplates
;
; PURPOSE:
;    To refresh the list widget that displays the list of templates.
;
; CALLING SEQUENCE:
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
;    itoolwatpmgr::addtemplate
;
; PURPOSE:
;    To add a template.
;
; CALLING SEQUENCE:
;    oref->addtemplate, name, objnam, x, y
;
; INPUTS:
;    name   : String scalar containing the name of the template.
;    objnam : String array containing the names of the objects.
;    x      : Array of x-coordinates of the objects.
;    y      : Array of y-coordinates of the objects.
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
;    itoolwatpmgr::purgetemplates
;
; PURPOSE:
;    To purge the current list of templates.
;
; CALLING SEQUENCE:
;    oref->purgetemplates
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
;    itoolwatpmgr::load
;
; PURPOSE:
;    To load a set of templates from a file.
;
; CALLING SEQUENCE:
;    oref->load
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
;    The file name is taken from the tmplfile tag of the state structure.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwatpmgr::save
;
; PURPOSE:
;    To save the current list of templates to a file.
;
; CALLING SEQUENCE:
;    oref->save, fname
;
; INPUTS:
;    fname : Scalar string containing the name of the template file to
;            to be written.
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
;-

; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::cleanup
; ------------------------------------------------------------------------------
pro itoolwatpmgr::cleanup
   compile_opt hidden

   ptr_free, self.pstate
end


; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::getproperty
; ------------------------------------------------------------------------------
pro itoolwatpmgr::getproperty, NUM_TPLATES=num_tplates,$
   OITOOL=oitool, PSTATE=pstate,$
   PIDX_TPLATES=pidx_tplates, PTPLATES=ptplates,$
   SELECTED=selected, TPLATE=tplate

   compile_opt hidden

   if arg_present(num_tplates) then num_tplates = self.num_tplates
   if arg_present(oitool) then oitool = self.oitool
   if arg_present(pstate) then pstate = self.pstate
   if arg_present(pidx_tplates) then pidx_tplates = self.pidx_tplates
   if arg_present(ptplates) then ptplates = self.ptplates
   if arg_present(selected) then selected = self.selected

   if arg_present(tplate) then begin
      if self.selected ge 0L then begin
         ; A template is selected. Return the reference structure.
         index = (*self.pidx_tplates)[self.selected]
         tplate = (*self.ptplates)[index] 
      endif else begin
         ; No template is selected. Return a blank reference structure.
         tplate = {itool_templatelist}
      endelse
   endif
end


; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::setproperty
; ------------------------------------------------------------------------------
pro itoolwatpmgr::setproperty, NUM_TPLATES=num_tplates, SELECTED=selected
   if n_elements(num_tplates) ne 0L then self.num_tplates = num_tplates
   if n_elements(selected) ne 0L then self.selected = selected
end




; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::refreshobjects
; ------------------------------------------------------------------------------
pro itoolwatpmgr::refreshobjects
   compile_opt hidden, strictarr

   if self.selected lt 0L then return
   
   ; Refresh the object-list widget. It will be cleared and de-sensitized,
   ; or it will show the remaining objects and be sensitive to events.

   pidx_tplates = self.pidx_tplates
   ptplates = self.ptplates
   index = (*pidx_tplates)[self.selected]

   (*ptplates)[index].oref->getproperty, NUMOBJ=numobj,$
      OBJ_SELECTED=obj_selected

   newvalue = (*ptplates)[index].oref->fmt()

   if obj_selected ge 0L then begin
      ; Refresh with the selected object highlighted.
      widget_control, (*self.pstate).objlistid, SET_VALUE=newvalue,$
         SET_LIST_SELECT=obj_selected,$
         SENSITIVE=(numobj gt 0L)
   endif else begin
      widget_control, (*self.pstate).objlistid, SET_VALUE=newvalue,$
         SENSITIVE=(numobj gt 0L)
   endelse
end


; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::realize
; ------------------------------------------------------------------------------
pro itoolwatpmgr::realize, MAP=map, NO_BLOCK=no_block
   compile_opt hidden

   if keyword_set(map) then begin
      widget_control, self.tlb, /MAP
      return
   endif

   widget_control, self.tlb, /REALIZE

   xmanager, 'itoolwatpmgr', self.tlb, NO_BLOCK=no_block,$
      CLEANUP='itoolwatpmgr_cleanup'
end


; ------------------------------------------------------------------------------
; Function itoolwatpmgr::init
; Photometry Template Manager.
; ------------------------------------------------------------------------------
function itoolwatpmgr::init, oitool, GROUP_LEADER=group_leader,$
   TMPLFILE=tmplfile

   tlb = widget_base(TITLE='Itool Photometry Template Manager', COLUMN=1,$
      MBAR=mbar, GROUP_LEADER=group_leader, /TLB_SIZE_EVENTS)

   ; Define the state structure.
   state = {$
      comboboxid:0L,$
      editid:0L,$
      fntxtid:0L,$
      mode:0,$
      objlistid:0L,$
      oitool:oitool,$
      tlb:tlb,$
      tlb_scr_xsize:0.0,$
      tlb_scr_ysize:0.0,$
      tmplfile:'',$
      tplistid:0L,$
      tpnameid:0L,$
      version:'phottemp_v02'$
   }

   if keyword_set(tmplfile) then state.tmplfile=tmplfile

   load_templates = 0
   if state.tmplfile ne '' then load_templates=1


   ; File Menu.
   filemenuid = widget_button(mbar, VALUE='File', /MENU)

   dummy = widget_button(filemenuid, VALUE='Load Template File',$
      EVENT_PRO='itoolwatpmgr_load_event')

   dummy = widget_button(filemenuid, VALUE='Save Templates',$
      EVENT_PRO='itoolwatpmgr_save_event')

   dummy = widget_button(filemenuid, VALUE='Save Templates As',$
      EVENT_PRO='itoolwatpmgr_saveas_event')

;  dummy = widget_button(filemenuid, VALUE='Dismiss',$
;     EVENT_PRO='itoolwatpmgr_dismiss_event')

   dummy = widget_button(filemenuid, VALUE='Exit',$
      EVENT_PRO='itoolwatpmgr_exit_event')


   ; Edit Menu.
   editmenuid = widget_button(mbar, VALUE='Edit', /MENU)

   dummy = widget_button(editmenuid, VALUE='Selected Template Name',$
      EVENT_PRO='itoolwatpmgr_tpname_event')

   dummy = widget_button(editmenuid, VALUE='Selected Object Name',$
      EVENT_PRO='itoolwatpmgr_objname_event')

   ; Template Menu.
   templatemenuid = widget_button(mbar, VALUE='Template', /MENU)

   dummy  = widget_button(templatemenuid, VALUE='Add',$
      EVENT_PRO='itoolwatpmgr_add_event')

   dummy  = widget_button(templatemenuid, VALUE='Delete',$
      EVENT_PRO='itoolwatpmgr_deltemplate_event')

   dummy = widget_button(templatemenuid, VALUE='Purge',$
      EVENT_PRO='itoolwatpmgr_purge_event')


   ; Object Menu.
   objectmenuid = widget_button(mbar, VALUE='Object', /MENU)

   dummy = widget_button(objectmenuid, VALUE='Delete',$
      EVENT_PRO='itoolwatpmgr_deleteobject_event')

   dummy = widget_button(objectmenuid, VALUE='Purge',$
      EVENT_PRO='itoolwatpmgr_purgeobjects_event')


   ; Filename widget.
   wb = widget_base(tlb, /ROW)
   dummy = widget_label(wb, VALUE='Template File:')

   dummy = widget_button(wb, VALUE='Select',$
      EVENT_PRO='itoolwatpmgr_select_event')

   state.fntxtid = widget_label(wb, VALUE=state.tmplfile, /DYNAMIC_RESIZE)

   ; Templates label.
   dummy = widget_label(tlb,$
      VALUE='Template List (Click left to select):')

   ; Template list widget.
   state.tplistid = widget_list(tlb, YSIZE=8,$
      EVENT_PRO='itoolwatpmgr_tplist_event')



; <<<<<<<<<<<<<<<<<< objects section >>>>>>>>>>>>>>>>>>>>>>>>
   dummy = widget_label(tlb,$
      VALUE='<<<<<<<<<<<<<< Objects Section >>>>>>>>>>>>>>')

   wb = widget_base(tlb, /ALIGN_CENTER, ROW=1,$
      EVENT_PRO='itoolwatpmgr_mode_event')

   dummy = widget_label(wb, VALUE='Itool Left-button Mode:')

;  dummy = cw_bgroup(wb,['Add','Active'], /EXCLUSIVE, FRAME=1,$
;     /NO_RELEASE, ROW=1, SET_VALUE=0)

   state.comboboxid = widget_combobox(wb, VALUE=['Add Objects',$
      'Active Template Photometry'])

   dummy = widget_label(tlb,$
      VALUE='Object List (Click left to select):')

   state.objlistid = widget_list(tlb, YSIZE=8,$
      EVENT_PRO='itoolwatpmgr_objlist_event')

   ; Remember the initial screen size of the top-level base.
   tlbgeom = widget_info(tlb, /geometry)
   state.tlb_scr_xsize = tlbgeom.scr_xsize
   state.tlb_scr_ysize = tlbgeom.scr_ysize

   pstate = ptr_new(state, /NO_COPY)

   self.tlb = tlb
   self.oitool = oitool
   self.pstate = pstate
   self.pidx_tplates = ptr_new(/ALLOCATE_HEAP)
   self.ptplates = ptr_new([{itool_templatelist}])
   self.selected = -1L

   ; Store the object reference to this application in the UVALUE of the
   ; top-level base
   widget_control, tlb, SET_UVALUE=self

   ; Load photometry templates from a file?
   if load_templates then self->load

   self->refreshtemplates

   ; Return successful initialization.
   return, 1
end






; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::refreshtemplates
; ------------------------------------------------------------------------------
pro itoolwatpmgr::refreshtemplates
   compile_opt hidden

   pidx_tplates = self.pidx_tplates
   ptplates = self.ptplates

   if self.num_tplates eq 0L then begin
      ; The list of templates is empty. Clear the templates-list widget
      ; to the null string and disable events from the template-list widget.
      widget_control, (*self.pstate).tplistid, SET_VALUE='', SENSITIVE=0
      return
   endif

   if self.selected ge 0L then begin
      ; A template is marked as selected. Display the template list, with the
      ; selected template highlighted.
      widget_control, (*self.pstate).tplistid,$
         SET_VALUE=(*ptplates)[*pidx_tplates].name,$
         SET_LIST_SELECT=self.selected, SENSITIVE=1

      index = (*pidx_tplates)[self.selected]
      (*ptplates)[index].oref->getproperty, MODE=mode
      widget_control, (*self.pstate).comboboxid, SET_COMBOBOX_SELECT=mode
   endif else begin
      ; No template is marked as selected. Display the template list
      ; without highlighting.
      widget_control, (*self.pstate).tplistid,$
         SET_VALUE=(*ptplates)[*pidx_tplates].name, SENSITIVE=1
   endelse
end


; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::addtemplate
; Adds one or more templates to the template list.
; ------------------------------------------------------------------------------
pro itoolwatpmgr::addtemplate, name, objnam, x, y
   compile_opt hidden

   pidx_tplates = self.pidx_tplates
   ptplates = self.ptplates
   selected = self.selected

   ; Check that the new name is unique in the existing set of templates,
   ; if there is at least one template defined.
   if self.num_tplates gt 0L then begin
      w = where((*ptplates)[*].name eq name[0], count)

      if count gt 0L then begin
         msg = ['A template named '+ name[0] + ' exists.',$
            'Template names must be unique.']

;        result =dialog_message(msg, /ERROR, DIALOG_PARENT=self.tlb)

         result = qannounc(msg, TITLE='Error',$
            TRUELABEL='Dismiss', $
            FALSELABEL='',$
            XSIZE=max(strlen(msg)),$
            GROUP_LEADER=self.tlb)

         return
      endif
   endif

   ; Check for the case where the first template is being added. Initialize
   ; pointers so that they point to undefined heap variables (as opposed to
   ; being null pointers). This makes the pointers valid, so that they may be
   ; redefined through dereferencing.

   if selected ge 0L then begin
      ; Remember the name of the selected template.
      index = (*pidx_tplates)[selected]
      selected_name = (*ptplates)[index].name
   endif else begin
      ; No template is selected.
      selected_name = ''
   endelse

   if self.num_tplates eq 0L then begin
      ; Need to handle the special case where the first template is
      ; being added. In this case, create a single-element array for
      ; the first template reference.
                                                                                
      *ptplates = [{itool_templatelist}]

      self.num_tplates = 1L
      p = 0L
   endif else begin
      ; Need to handle the case where a template is being added to an
      ; existing list. In this case, append a new template structure to the
      ; existing array of templates.
                                                                                
      *ptplates = [*ptplates, {itool_templatelist}]
      self.num_tplates = self.num_tplates + 1L
      p = self.num_tplates - 1L
   endelse

   ; Set up some default values, if the object-name argument contains a
   ; null string.

   if objnam[0] eq '' then begin
      numobj = 0
      new = [1]
      t_objnam = ['<default>']
   endif else begin
      numobj = n_elements(objnam)
      new = intarr(numobj)
      t_objnam = objnam
   endelse

   ; Initialize the new template-reference structure.
   (*ptplates)[p].name = name

   (*ptplates)[p].oref = obj_new('itool_template', name, new,$
      t_objnam, x, y)

   ; Compute an indexed-sort array for the updated template list.
   *pidx_tplates = sort(strupcase((*ptplates)[*].name))

   w = where((*ptplates)[*pidx_tplates].name eq selected_name, count)
   self.selected = w[0]        

   ; Call the refreshtemplates method, to refresh the template-list widget.
   self->refreshtemplates
end


; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::purgetemplates
; ------------------------------------------------------------------------------
pro itoolwatpmgr::purgetemplates
   compile_opt hidden

   ;Nothing to do, if the template list is empty.
   if self.num_tplates eq 0L then return

   num_tplates = self.num_tplates
   pidx_tplates = self.pidx_tplates
   ptplates = self.ptplates

   ; Get a confirmation from the user.
   msg = 'Purge all templates?'

;  result = dialog_message(msg, /QUESTION, /DEFAULT_NO,$
;     DIALOG_PARENT=self.tlb)

;  if result eq 'No' then return

   result = qannounc('Purge all templates?', TITLE='Question',$
      TRUELABEL='Yes, continue', FALSELABEL='No, Cancel',$
      XSIZE=strlen(msg),$
      GROUP_LEADER=self.tlb)

   if result eq 0 then return

   ; Need to check the ptplates pointer, because it must be
   ; dereferenced.

   if ptr_valid(ptplates) then begin
      ; Destroy all of the template-object references.
      obj_destroy, (*ptplates)[*].oref
   endif

   ; Clear and reset tags.
   self.num_tplates = 0L
   *self.pidx_tplates = [0L]
   *self.ptplates = [{itool_templatelist}]
   self.selected = -1

   ; Clear and disable both list widgets.
   widget_control, (*self.pstate).tplistid, SET_VALUE='', SENSITIVE=0
   widget_control, (*self.pstate).objlistid, SET_VALUE='', SENSITIVE=0
end


; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::load
; Loads a set of templates from a template file.
; ------------------------------------------------------------------------------
pro itoolwatpmgr::load
   compile_opt hidden

   fname = (*self.pstate).tmplfile

   if not file_test(fname) then begin
      msg = 'Template file ' + fname + ' does not exist.'

;     result = dialog_message(msg, /ERROR, DIALOG_PARENT=self.tlb)

      result = qannounc(msg, TITLE='Error',$
         TRUELABEL='Dismiss', FALSELABEL='', XSIZE=strlen(msg),$
         GROUP_LEADER=self.tlb)

      (*self.pstate).tmplfile = ''
      return
   endif

   if self.num_tplates gt 0L then begin
      msg = ['Existing templates will be purged.']

;     result = dialog_message(msg, /CANCEL, /DEFAULT_CANCEL,$
;        DIALOG_PARENT=self.tlb)

;     if result eq 'Cancel' then return

      result = qannounc(msg,$
         TITLE='Warning',$
         TRUELABEL='Purge templates and continue',$
         FALSELABEL='Cancel load request',$
         XSIZE=strlen(msg),$
         GROUP_LEADER=self.tlb)

      if result eq 0 then return

      self->purgetemplates
   endif

   get_lun,lu
   err = 0
   openr,lu,fname,error=err

   if err ne 0 then begin
      ; Problem with file.
      free_lun,lu

;     result = dialog_message(!error_state.msg, /ERROR,$
;        DIALOG_PARENT=self.tlb)

      result = qannounc(!error_state.msg, TRUELABEL='Dismiss',$
         TITLE='System Error', XSIZE=strlen(!error_state.msg),$
         FALSELABEL='', GROUP_LEADER=self.tlb)

      return
   endif

   on_ioerror,rderr

   version = ''
   readf,lu,version

   if version ne (*self.pstate).version then begin
      free_lun, lu
      msg = 'Old file version ' + version + '. Not loaded.'

;     result = dialog_message(msg, /ERROR, DIALOG_PARENT=self.tlb)

      result = qannounc(msg, TITLE='Error', XSIZE=strlen(msg),$
         TRUELABEL='Dismiss', FALSELABEL='', GROUP_LEADER=self.tlb)

      return
   endif

   count = 0

   while not eof(lu) do begin
      ; Read in the templates.
      name = ''
      readf,lu,name     ; Template name.
      numobj = 0
      readf,lu,numobj   ; # objects.
      aix = 0
      aiy = 0
      ax = 0.0
      ay = 0.0
      aobjnam = ''
      readf,lu,ax,ay,aobjnam   ; First (anchor) position.

      ; Initialize a temporary vector to hold the incoming objects
      ; (the first element will hold the anchor position).
      objnam = [strtrim(aobjnam,2)]
      x  = [ax]
      y  = [ay]

      ; Get the remaining coordinate pairs.
      tx = 0.0
      ty = 0.0
      tobjnam = ''

      for j=1,numobj-1 do begin
         readf,lu,tx,ty,tobjnam
         x  = [x,tx]
         y  = [y,ty]
         objnam = [objnam,strtrim(tobjnam,2)]
      endfor

      ; Add the new template, with all of the objects retrieved.
      self->addtemplate, name, objnam, x, y

      count = count + 1
   endwhile

   free_lun,lu


   ; Refresh the list wiget with the loaded list of templates.
   self->refreshtemplates

;  message,strn(count)+' templates loaded from file '+$
;     (*self.pstate).tmplfile,/info

   ; Update the label widget that displays the name of the template file
   ; and store the file name into the state structure.
   widget_control, (*self.pstate).fntxtid, SET_VALUE=fname
   (*self.pstate).tmplfile = fname

   return

rderr:
   free_lun,lu
   print, !error_state.msg
end


; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr::save
; Saves a set of templates to a template file.
; ------------------------------------------------------------------------------
pro itoolwatpmgr::save, fname
   compile_opt hidden

   ; The template file has the following format for each template entry:
   ; First line     : File version code (text).
   ; Second line    : Template name (text).
   ; Third line     : n     ; Number of objects (numeric).
   ; Fourth line    : x y   ; First object absolute position (numeric).
   ; Next n-1 lines : x y   ; Object positions relative to first.
   ; etc...

   if file_test(fname) then begin
      msg = ['File ' + fname + ' exists.', $
           'It will be overwritten, unless you choose',$
           'to cancel and change the name of the file.']

;     result = dialog_message(msg, /CANCEL, /DEFAULT_CANCEL,$
;        DIALOG_PARENT=self.tlb)

;     if result eq 'Cancel' then return

      result = qannounc(msg, TITLE='Warning',$
         TRUELABEL='Continue',$
         FALSELABEL='Cancel', XSIZE=max(strlen(msg)),$
         GROUP_LEADER=self.tlb)

      if result eq 0 then return
   endif

   ptplates = self.ptplates

   count = 0
   openw, lu, fname, ERROR=err, /GET_LUN

   if err eq 0 then begin
      printf, lu, (*self.pstate).version

      for j=0L, self.num_tplates-1L do begin
         (*ptplates)[j].oref->print, LUN=lu
      endfor

      close, lu
      free_lun, lu

      (*self.pstate).tmplfile = file_search(fname, /FULLY_QUALIFY_PATH)
      widget_control, (*self.pstate).fntxtid, SET_VALUE=(*self.pstate).tmplfile
   endif else begin
;     result = dialog_message(!error_state.msg, /ERROR,$
;        DIALOG_PARENT=self.tlb)

      result = qannounc(!error_state.msg, TITLE='System Error',$
         TRUELABEL='Dismiss',$
         FALSELABEL='', XSIZE=strlen(!error_state.msg),$
         GROUP_LEADER=self.tlb)
   endelse
end


; ------------------------------------------------------------------------------
; Procedure itoolwatpmgr_cleanup
; ------------------------------------------------------------------------------
pro itoolwatpmgr_cleanup, tlb
   compile_opt hidden

   widget_control, tlb, GET_UVALUE=oitoolwatpmgr

   oitoolwatpmgr->getproperty, PSTATE=pstate,$
     PIDX_TPLATES=pidx_tplates, PTPLATES=ptplates

   if ptr_valid(ptplates) then begin
      obj_destroy, (*ptplates)[*].oref
      ptr_free, ptplates, pidx_tplates
   endif

   obj_destroy, oitoolwatpmgr
end






; <<<<<<<<<<<<<<<<<<<<< Event-processing Section >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_dismiss_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_dismiss_event, event
   compile_opt hidden

   widget_control, event.top, MAP=0
end



; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_exit_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_exit_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr
   oitoolwatpmgr->getproperty, NUM_TPLATES=num_tplates

   modified = 0

   if num_tplates gt 0L then begin
      oitoolwatpmgr->getproperty, PTPLATES=ptplates

      for j=0L, num_tplates-1L do begin
         (*ptplates)[j].oref->getproperty, MODIFIED=t_modified
         modified = modified or t_modified
      endfor
   endif

   if modified then begin
      msg = ['Template changes have not been saved.',$
         'Exit The Template Manager?']

      result = qannounc(msg, TITLE='Template Manager Exit Confirmation',$
         FALSELABEL='No, cancel request', TRUELABEL='Yes, discard changes',$
         XSIZE=max(strlen(msg)),$
         GROUP_LEADER=event.top)

      if not result then return
   endif

   widget_control, event.top, /DESTROY
end



; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_load_event, event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_load_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr
   oitoolwatpmgr->getproperty, PSTATE=pstate

   fn = dialog_pickfile(TITLE='Select a template file',$
      FILE=(*pstate).tmplfile, DIALOG_PARENT=event.top)

   if fn[0] eq '' then return

   (*pstate).tmplfile = fn[0]

   oitoolwatpmgr->load
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_save_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_save_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr
   oitoolwatpmgr->getproperty, NUM_TPLATES=num_tplates, PSTATE=pstate

   if num_tplates eq 0L then begin
      msg = 'The template list is empty.'

;     result = dialog_message(msg, /ERROR, DIALOG_PARENT=event.top)

      result = qannounc(msg, TITLE='Error',$
         TRUELABEL='Dismiss',$
         FALSELABEL='', XSIZE=strlen(msg), GROUP_LEADER=event.top)

      return
   endif

   if (*pstate).tmplfile eq '' then begin
      result = qinput(PROMPT='Enter a template filename:',$
         GROUP_LEADER=event.top)

      if result[0] eq '' then return
   endif else begin
      result = (*pstate).tmplfile
   endelse

   oitoolwatpmgr->save, result[0]
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_saveas_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_saveas_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr
   oitoolwatpmgr->getproperty, NUM_TPLATES=num_tplates, PSTATE=pstate

   if num_tplates eq 0L then begin
      msg = 'The template list is empty.'

;     result = dialog_message(msg, /ERROR, DIALOG_PARENT=event.top)

      result = qannounc(msg, TITLE='Error',$
         TRUELABEL='Dismiss',$
         FALSELABEL='', XSIZE=strlen(msg), GROUP_LEADER=event.top)

      return
   endif

   result = qinput(PROMPT='Enter a template filename:',$
      GROUP_LEADER=event.top)

   if result[0] eq '' then return

   itoolwatpmgr->save, result[0]
end



; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_add_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_add_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr

   newname = qinput(PROMPT='New template name:', GROUP_LEADER=event.top)
   if newname[0] eq '' then return

   oitoolwatpmgr->addtemplate, [newname[0]], [''], [0.0], [0.0]
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_deltemplate_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_deltemplate_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr

   oitoolwatpmgr->getproperty, PSTATE=pstate,$
      PIDX_TPLATES=pidx_tplates, PTPLATES=ptplates, SELECTED=selected

   if selected ge 0L then begin
      ; This is the actual index of the template in the stored array of
      ; template references.
      index = (*pidx_tplates)[selected]

      ; Get the name of the selected template.
      name = (*ptplates)[index].name

      msg = 'Delete selected template '+name+'?'

;     result = dialog_message(msg, /QUESTION, /DEFAULT_NO,$
;        DIALOG_PARENT=event.top)

;     if result eq 'No' then return

      result = qannounc(msg, TITLE='Question',$
         TRUELABEL='Yes, Continue', FALSELABEL='No, Cancel',$
         XSIZE=strlen(msg),$
         GROUP_LEADER=event.top)

      if result eq 0 then return

      ; Ok to delete the template.

      ; Destroy the object instance.
      obj_destroy, (*ptplates)[index].oref

      ; Locate all of the templates but the one that is to be removed.
      w = where((*ptplates)[*].name ne name, count)

      if count gt 0L then begin
         ; Remove the selected template from the template-reference array.
         *ptplates = (*ptplates)[w]
                                                                                
         ; Sort the resulting array of templates and save the new
         ; sorted-index array.
         *pidx_tplates = sort(strupcase((*ptplates)[*].name))

         ; Store the new number of templates.
         num_tplates = n_elements(*ptplates)
      endif else begin
         ; The last template was deleted and the list is now empty.
         *pidx_tplates = 0L
         *ptplates = [{itool_templatelist}]
         num_tplates = 0L
      endelse

      ; Clear the selected tag.
      selected = -1L

      oitoolwatpmgr->setproperty, NUM_TPLATES=num_tplates, SELECTED=selected

      ; Refresh the template-list widget and the object-list widget.
      oitoolwatpmgr->refreshtemplates
      oitoolwatpmgr->refreshobjects
   endif
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_purge_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_purge_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr
   oitoolwatpmgr->purgetemplates
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_select_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_select_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr
   oitoolwatpmgr->getproperty, PSTATE=pstate

   f = dialog_pickfile(TITLE='Select a template file', DIALOG_PARENT=event.top)
   fn = strtrim(f,2)

   if fn[0] ne '' then begin
      widget_control, (*pstate).fntxtid, SET_VALUE=fn
      (*pstate).tmplfile = fn
;     itoolwatpmgr_load, pstate
   endif
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_fntxt_event
; Not used at this time.
; -----------------------------------------------------------------------------
pro itoolwatpmgr_fntxt_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr
   oitoolwatpmgr->getproperty, PSTATE=pstate

   widget_control, event.id, get_value=newname
   (*pstate).tmplfile = newname[0]
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_tplist_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_tplist_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr

   oitoolwatpmgr->getproperty, OITOOL=oitool, PSTATE=pstate,$
      PIDX_TPLATES=pidx_tplates, PTPLATES=ptplates

   ; Mark this template as selected.
   oitoolwatpmgr->setproperty, SELECTED=event.index

   ; Refresh the objects-list widget.
   oitoolwatpmgr->refreshobjects

   ; Refresh the itool draw window.
   oitool->draw, /WORK

   ; Get the actual index of the selected template.
   index = (*pidx_tplates)[event.index]

   ; Set the combobox mode of the selected template.
   (*ptplates)[index].oref->getproperty, MODE=mode
   widget_control, (*pstate).comboboxid, SET_COMBOBOX_SELECT=mode
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_tpname_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_tpname_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr

   ; Any templates?
   oitoolwatpmgr->getproperty, NUM_TPLATES=num_tplates, SELECTED=selected
   if num_tplates eq 0L then return

   ; Is a template selected?
   if selected lt 0L then return

   oitoolwatpmgr->getproperty, PIDX_TPLATES=pidx_tplates, PTPLATES=ptplates,$
      PSTATE=pstate

   ; Translate the sorted-template index into the actual index.
   index = (*pidx_tplates)[selected]

   ; Get the current template name.
   current_name = (*ptplates)[index].name

   ; Ask the user for the new template name.
   newname = qinput(PROMPT='New template name:', DEFAULT=current_name,$
      GROUP_LEADER=event.top)

   ; Nothing to do if the new name is the null string or if the new name
   ; is the same as the existing name.
   if (newname[0] eq '' or newname[0] eq current_name) then return

   ; Check that the new name is unique in the existing set of templates.
   w = where((*ptplates)[*].name eq newname[0], count)

   if count gt 0L then begin
      msg = ['A template named ' + newname[0] + ' exists.',$
         'Template names must be unique.']

;     result = dialog_message(msg, /ERROR, DIALOG_PARENT=event.top)

      result = qannounc(msg, TITLE='Error',$
         TRUELABEL='Dismiss',$
         FALSELABEL='', XSIZE=max(strlen(msg)), GROUP_LEADER=event.top)

      return
   endif

   ; Store the new name of the template, in the template-reference list
   ; and in the template-object instance.
   (*ptplates)[index].name = newname[0]
   (*ptplates)[index].oref->setproperty, NAME=newname[0]

   ; Generate a new indexed-sort array for the list of templates and
   ; store it.
   *pidx_tplates = sort(strupcase((*ptplates)[*].name))

   ; Locate the sorted index of the newly-named template and update the
   ; selected tag in the state structure.
   w = where((*ptplates)[(*pidx_tplates)].name eq newname[0], count)
   oitoolwatpmgr->setproperty, SELECTED=w[0]

   ; Refresh the template-list widget.
   oitoolwatpmgr->refreshtemplates
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_mode_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_mode_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr


   oitoolwatpmgr->getproperty, SELECTED=selected

   if selected ge 0L then begin
      oitoolwatpmgr->getproperty, PIDX_TPLATES=pidx_tplates,$
         PTPLATES=ptplates

      index = (*pidx_tplates)[selected]
      ; Set the mode of the selected template.
      (*ptplates)[index].oref->setproperty, MODE=event.index
   endif
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_deleteobject_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_deleteobject_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr

   ; Any templates?
   oitoolwatpmgr->getproperty, NUM_TPLATES=num_tplates, SELECTED=selected
   if num_tplates eq 0L then return

   ; Is a template selected?
   if selected lt 0L then return

   oitoolwatpmgr->getproperty, PIDX_TPLATES=pidx_tplates, PSTATE=pstate,$
      PTPLATES=ptplates

   ; Translate the sorted index into the actual index.
   index = (*pidx_tplates)[selected]
   (*ptplates)[index].oref->getproperty, NUMOBJ=numobj
   if numobj eq 0L then return
   (*ptplates)[index].oref->getproperty, OBJ_SELECTED=obj_selected
   if obj_selected lt 0L then return
   (*ptplates)[index].oref->getproperty, OBJNAM=objnam

   msg = 'Delete selected object '+ objnam[obj_selected]+'?'

   result = qannounc(msg,$
      TRUELABEL='Yes, delete the object', FALSELABEL='No, cancel the request',$
      GROUP_LEADER=event.top, XSIZE=strlen(msg))

   if not result then return

   ; Call the "deleteobject" method routine.
   (*ptplates)[index].oref->deleteobject

   oitoolwatpmgr->refreshobjects
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_purgeobjects_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_purgeobjects_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr

   oitoolwatpmgr->getproperty, NUM_TPLATES=num_tplates, SELECTED=selected

   ; Any templates?
   if num_tplates eq 0L then return

   ; Is a template selected?
   if selected lt 0L then return

   result = dialog_message('Purge the objects in this template?',$
      /QUESTION, /DEFAULT_NO, DIALOG_PARENT=event.top)

   if result eq 'No' then return

   oitoolwatpmgr->getproperty, PIDX_TPLATES=pidx_tplates, PSTATE=pstate,$
      PTPLATES=ptplates

   ; Translate the sorted index into the actual index.
   index = (*pidx_tplates)[selected]

   ; Call the "purgeobjects" method routine.
   (*ptplates)[index].oref->purgeobjects
   widget_control, (*pstate).objlistid, SET_VALUE='', SENSITIVE=0
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_objlist_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_objlist_event, event
   compile_opt hidden
                                                                                
   widget_control, event.top, GET_UVALUE=oitoolwatpmgr

   oitoolwatpmgr->getproperty, OITOOL=oitool, PIDX_TPLATES=pidx_tplates,$
      PSTATE=pstate, PTPLATES=ptplates, SELECTED=selected

   index = (*pidx_tplates)[selected]
                                                                                
   ; Mark this object as selected in the selected template.
   (*ptplates)[index].oref->setproperty, OBJ_SELECTED=event.index
                                                                                
   ; Refresh the objects-list widget and the editable text widget (name).
   oitoolwatpmgr->refreshobjects

   ; Refresh the itool draw window.
   oitool->draw
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_objname_event
; -----------------------------------------------------------------------------
pro itoolwatpmgr_objname_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitoolwatpmgr

   oitoolwatpmgr->getproperty, PIDX_TPLATES=pidx_tplates, PSTATE=pstate,$
      PTPLATES=ptplates, SELECTED=selected

   if selected lt 0L then return
   index = (*pidx_tplates)[selected]
   (*ptplates)[index].oref->getproperty, OBJ_SELECTED=obj_selected
   if obj_selected lt 0L then return

   (*ptplates)[index].oref->getproperty, OBJNAM=objnam
   current_objnam = objnam[obj_selected]

   result = qinput(PROMPT='New object name:', DEFAULT=current_objnam,$
      GROUP_LEADER=event.top)

   ; Nothing to do, if the result is a null string, or if the result is
   ; the same as the current object name.
   if result eq '' then return
   if result eq current_objnam then return

   ; Change the name of the currently-selected object and refresh the
   ; object-list widget.
   (*ptplates)[index].oref->changeobjectname, result
   oitoolwatpmgr->refreshobjects
end



; -----------------------------------------------------------------------------
; Procedure itool_templatelist__define
;
;   This procedure handles the automatic definition of a "itool_templatelist"
; structure. An array of these structures is used to remember all of the
; templates that exist. It provides the names and object references of the
; templates. The "name" tag in this structure holds the same information as
; the "name" attribute in each template instance. This minor duplication of
; information allows for the template names to be maintained in sorted order.
; Without the name tag in this array of structures, it would be necessary
; to loop through each of the object references, to retrieve the template
; names one-by-one, each time a new sorted list would be needed.
;   A parallel long-word array to the array of "itool_templatelist" structures
; is used to store the results of indexed-sort data, as returned by the IDL
; SORT function. Maintaining the indexed-sort array eliminates the need to
; reorganize the array of structures.
; -----------------------------------------------------------------------------
pro itool_templatelist__define
   dummy = {itool_templatelist, name:'', oref:obj_new()}
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr_event
; Main event handler. Handles events from the top-level base, only.
; -----------------------------------------------------------------------------
pro itoolwatpmgr_event, event
   compile_opt hidden

   ; Want to process size events, only.
   if tag_names(event, /structure_name) ne 'WIDGET_BASE' then return

   ; WARNING. Re-sizing widgets from IDL does not produce perfect results
   ; and the results may differ, depending on the widget toolkit involved.
   ; The best way to avoid re-sizing problems is to re-size sparingly;
   ; constantly increasing and decreasing the size of a top-level base will
   ; introduce cummulative errors, causing the sizes of child widgets to
   ; become increasingly out-of-sync with the size of the top-level base.

   ; Get top-level-base geometry (more accurate than the values in the
   ; event structure).
   tlbgeom = widget_info(event.top, /geometry)

   ; Get a pointer to state the structure.
   widget_control, event.top, GET_UVALUE=oref
   oref->getproperty, PSTATE=pstate

   ; Compute change in top-level base screen size.
   dx = tlbgeom.scr_xsize - (*pstate).tlb_scr_xsize
   dy = tlbgeom.scr_ysize - (*pstate).tlb_scr_ysize

   ; Get current geometry of the template-list widget and the object-list
   ; widget.
   tpgeom = widget_info((*pstate).tplistid, /geometry)
   objgeom = widget_info((*pstate).objlistid, /geometry)

   ; Adjust the screen sizes of the two list widgets. Since they hang on
   ; a column base, the change in vertical (Y) size of the top-level base
   ; must be split between the two widgets; each will be adjusted by half
   ; the change in the vertical size of the top-level base.

   widget_control, (*pstate).tplistid, scr_xsize=tpgeom.scr_xsize+dx,$
      scr_ysize=tpgeom.scr_ysize+(dy/2.0)

   widget_control, (*pstate).objlistid, scr_xsize=objgeom.scr_xsize+dx,$
      scr_ysize=objgeom.scr_ysize+(dy/2.0)

   ; Remember the new screen size of the top-level base.
   (*pstate).tlb_scr_xsize = tlbgeom.scr_xsize
   (*pstate).tlb_scr_ysize = tlbgeom.scr_ysize
end


; -----------------------------------------------------------------------------
; Procedure itoolwatpmgr__define
; Defines the itoolwatpmgr object class.
;
; Attributes:
;   tlb          : top-level base.
;   pstate       : pointer to state structure.
;   oitool       : object reference of the host instance of itool.
;   num_tplates  : number of templates.
;   pidx_tplates : pointer to an array of sorted indices to the array of
;                  template-reference structures.
;   ptplates     : pointer to an array of template-reference structures.
;   selected     : sorted index of the currently-selected template.
; -----------------------------------------------------------------------------
pro itoolwatpmgr__define
   dummy = {itoolwatpmgr, tlb:0L, pstate:ptr_new(), oitool:obj_new(),$
      num_tplates:0L, pidx_tplates:ptr_new(), ptplates:ptr_new(),$
      selected:0L}
end

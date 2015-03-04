;+
; NAME:
;    itool_tpannounce
; PURPOSE: (one line)
;    Custom text and graphical-dialog tool, specific to template corruption.
; DESCRIPTION:
;
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    result = itool\_tpannounce(oitool, text, xcen, ycen)
; INPUTS:
;    oitool : Object reference to host instance of the itool GUI.
;    text   : The text to be displayed in the text widget.
;    xcen   : The array of proposed X-locations for the active itool template.
;    ycen   : The array of proposed Y-locations for the active itool template.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    FALSELABEL=false           : Label for false button.  Default='No'
;    GROUP_LEADER=group_leader  : Group leader.
;    TRUELABEL=true             : Label for true button.  Default='Yes'
;    TITLE=title                : Title of widget.  Default='qannounc'.
;    XSIZE=ysize                : Width, in characters, of the text display.
;    YSIZE=ysize                : Max size, in rows, of text display.
; OUTPUTS:
;    result=0 if false button pressed, otherwise result=1.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;    Suspends all event generation.  A user response is required.
; RESTRICTIONS:
;
; PROCEDURE:
;    Call this function from an instance of itool, when a template is about
; to be modified with a new set of object locations. The object locations
; will be listed, numerically, in a text widget, as well as being displayed
; in a separate draw widget. The draw widget will display a copy of the
; itool work window, with plots of the template that would be centered over
; the new object locations, if the new object locations were to be accepted.
; The itool object reference passed as the first argument is used to invoke
; itool's method routines (itool::draw and itool::tpdraw). Those method
; routines accept keywords that allow them to use an alternate window number
; and an alternate template. No new routines are needed to draw into the
; stand-alone draw widget launched by this function.
;
; MODIFICATION HISTORY:
;    2004/05 Written by Doug Loucks, consultant for Lowell Observatory.
;-


pro itool_tpannounce_event, event
   compile_opt hidden

   ; Get the pointer to the result variable.
   widget_control, event.top, GET_UVALUE=presult

   ; Get the UNAME of the widget that generated the event.
   uname = widget_info(event.id, /UNAME)

   case uname of
      'true' : begin
         ; The user selected the Accept button. Set the result to true.
         *presult = 1
      end

      else : begin
         ; The user selected the Ignore button. Set the result to false.
         *presult = 0
      end
   endcase

   widget_control, event.top, /DESTROY
end


function itool_tpannounce, oitool, msg, xcen, ycen,$
   GROUP_LEADER=group_leader, FALSELABEL=falselabel, TRUELABEL=truelabel,$
   TITLE=title, XSIZE=xsize, YSIZE=ysize

   tlb = widget_base(TITLE=title, COLUMN=1,$
      /FLOATING, GROUP_LEADER=group_leader, /MODAL)

   if not keyword_set(falselabel) then falselabel = 'No'
   if not keyword_set(truelabel) then truelabel = 'Yes'
   if not keyword_set(xsize) then xsize = 80
   if not keyword_set(ysize) then ysize = 5

   ; Get a pointer to itool's state structure.
   oitool->getproperty, PSTATE=pstate_itool

   pworkstate = (*pstate_itool).pworkstate

   dummy = widget_text(tlb, VALUE=msg, XSIZE=xsize, YSIZE=ysize)

   dummy = widget_text(tlb,$
      VALUE=['ALSO: Please inspect the graphical presentation that is',$
             'displayed in the separate draw widget. It shows the new',$
             'object locations that would be applied to the active',$
             'template, if the new object locations were to be accepted.'],$
      YSIZE=4)

   r1 = widget_base(tlb, ROW=1, FRAME=1, /ALIGN_CENTER)
   dummy = widget_button(r1, VALUE=falselabel, UNAME='false')
   dummy = widget_button(r1, VALUE=truelabel, UNAME='true')

   ; Define a top-level base for the graphical presentation of the proposed
   ; new template, based upon the new object locations.

   drawtlb = widget_base(TITLE=title+' - Graphical Presentatiion', COLUMN=1,$
      /FLOATING, GROUP_LEADER=tlb, /MODAL)

   dummy = widget_label(drawtlb, VALUE='Proposed new object locations:')

   ; Create a draw widget on the stand-alone top-level base. It will have
   ; the same size as the current itool work window.

   drawid = widget_draw(drawtlb, XSIZE=(*pworkstate).xsize,$
      YSIZE=(*pworkstate).ysize)

   widget_control, drawtlb, /REALIZE

   ; Allocate a pointer to a result variable.
   presult = ptr_new(/ALLOCATE_HEAP)

   widget_control, tlb, /REALIZE

   ; Get the window number of the draw widget that will be launched on a
   ; stand-alone top-level base. This draw widget will be used for graphical
   ; presentation of the proposed new template.
   widget_control, drawid, GET_VALUE=winnum
   widget_control, tlb, SET_UVALUE=presult

   ; Call itool's draw method with the alternate work-window number. This
   ; will draw a copy of itool's current work window in the newly-launched
   ; graphical-confirmation window.
   oitool->draw, ALT_WORKWIN=winnum, /WORK

   ; Prepare to create a temporary instance of an itool template.
   num_objects = n_elements(xcen)
   name = 'temporary'
   new = intarr(num_objects)
   objnam = strarr(num_objects)
   
   ; Create a temporary itool template, containing the proposed new object
   ; locations.
   tplate = {itool_templatelist}
   tplate.name = 'temporary'
   tplate.oref = obj_new('itool_template', name, new, objnam, xcen, ycen)

   ; Call itool's tpdraw method, with the alternate template and the
   ; alternate work-window number. This draws the temporary template in
   ; the newly-launched graphical presentation window.
   oitool->tpdraw, ALT_TPLATE=tplate, ALT_WORKWIN=winnum

   xmanager, 'itool_tpannounce', tlb

   ; Back from the blocked xmanager. Extract the user-selected result,
   ; free the result pointer, and destroy the temporary template.
   result = *presult
   ptr_free, presult
   obj_destroy, tplate.oref

   return, result
end

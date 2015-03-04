;+
; NAME:
;    qannounc
; PURPOSE: (one line)
;    Scrollable text display widget with true and false response buttons.
; DESCRIPTION:
;
; CATEGORY:
;    Widget
; CALLING SEQUENCE:
;    result = qannounc(text [,keywords])
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    FALSELABEL=false           : Label for false button.  Default='No'
;                               : If set to a null string (''), the false
;                               : button is not displayed and qannounc returns 
;                               : only 1.
;    GROUP_LEADER=group_leader  : Group leader. If present, qannounc runs as a
;                                 modal application. Otherwise, it runs as
;                                 a non-modal application.
;    TRUELABEL=true             : Label for true button.  Default='Yes' unless 
;                               : FALSELABEL
;                               : is a null string, where the default for 
;                               : TRUELABEL then becomes 'Dismiss'
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
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, January, 1993.  Similar
;    to IDL User Library procedure XANNOUNCE.
;    3/93, DWL:
;      Moved False button to the upper left, making it the first button.  This
;    was done to prevent the true button from being 'pressed' as a result of
;    the space bar event generation problem.
;      Set the false button to have input focus.
;    7/22/94, DWL, Added code similar to that for compound widget event
; processing, thus eliminating the need for a common block.
;    2004/04/09, DWL. Major overhaul, to standards of recent releases
; of IDL. The overhaul is transparent and does not affect IDL applications
; using this tool.
;    A new feature was added, allowing for a single button to
; be displayed, such as "Dismiss."
;    2006/08/24, Peter L. Collins, Lowell Observatory, 2 more lines to header 
;                  describing the single button feature( see FALSELABEL) and 
;                  corresponding special 'Dismiss'  default for TRUELABEL.
;                  IN OTHER WORDS qannounc(FALSELABEL='') puts up a single 
;                  'Dismiss' button.
;-
; ------------------------------------------------------------------------------
; Procedure qannounc_event
; ------------------------------------------------------------------------------
pro qannounc_event, event
   compile_opt hidden

   ; Get the pointer to the result variable.
   widget_control, event.top, GET_UVALUE=presult

   ; Get the UVALUE of the widget that generated the event. This value will
   ; be 0 if the user pressed the false button or 1 if the user pressed the
   ; true button.
   widget_control, event.id, GET_UVALUE=result

   ; Store the retrieved UVALUE into the result variable.
   *presult = result

   ; Close down and return to caller.
   widget_control, event.top, /DESTROY
end

; ------------------------------------------------------------------------------
; Function qannounc
; ------------------------------------------------------------------------------
function qannounc, text, FALSELABEL=falselabel, GROUP_LEADER=group_leader, $
   TITLE=title, TRUELABEL=truelabel, XSIZE=xsize, YSIZE=ysize

   if n_elements(text) eq 0L then text = ''

   ; May need to set some default values for missing keywords.
   if not keyword_set(title) then title='QANNOUNCE'

   ; If the FALSELABEL keyword is truly absent, the default button label
   ; will be 'No'. This is the traditional behavior. A new feature allows
   ; the FALSELABEL button to be missing from the GUI, if the FALSELABEL
   ; keyword is present, but contains a null string. Note that passing a
   ; null string on a keyword will cause the keyword_set() function to
   ; return false, not true.
   ; The default for TRUELABEL is now 'Dismiss' if FALSELABEL is set to '',
   ; ie, suppressed.
   if not keyword_set(falselabel) then begin
      if n_elements(falselabel) eq 0L then falselabel='No' else falselabel=''
   endif
   if not keyword_set(truelabel) then $
      if falselabel eq '' then truelabel = 'Dismiss' else truelabel='Yes'

   if not keyword_set(xsize) then xsize=80
   if not keyword_set(ysize) then ysize=5

   ; Create the top-level base. It will be positioned over the calling
   ; application's group leader and will be modal.

   if keyword_set(group_leader) then begin
      tlb = widget_base(TITLE=title, COLUMN=1, /FLOATING,$
         GROUP_LEADER=group_leader, /MODAL)
   endif else begin
      tlb = widget_base(TITLE=title, COLUMN=1)
   endelse

   w1 = widget_text(tlb, VALUE=text, XSIZE=xsize, YSIZE=ysize, /SCROLL)
   wb = widget_base(tlb, /ROW, /FRAME, /ALIGN_CENTER)
   dummy = widget_button(wb, VALUE=truelabel, UVALUE=1)

   if falselabel ne '' then begin
      dummy = widget_button(wb, VALUE=falselabel, UVALUE=0)
      widget_control, dummy, /CANCEL_BUTTON, /DEFAULT_BUTTON
   endif


   ; Allocate an undefined heap variable and create a pointer to it. This
   ; variable will hold the value of the user's button action --- 0 if
   ; the user presses the false button and 1 if the user presses the
   ; true button.
   presult = ptr_new(/ALLOCATE_HEAP)

   widget_control, tlb, /REALIZE

   ; Put the pointer to the undefined heap variable into the UVALUE of
   ; the top-level base.
   widget_control, tlb, SET_UVALUE=presult

   ; This blocks, until the user presses a button.
   xmanager, 'qannounc', tlb


   ; Extract the user's selection into a local variable.
   result = *presult

   ; Free the pointer that was created.
   ptr_free, presult

   ; Return the user's result to the caller.
   return, result
end

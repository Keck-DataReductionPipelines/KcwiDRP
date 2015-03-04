;+
; NAME:
;    qinput
; PURPOSE: (one line)
;    Prompt user for input using a popup widget.
; DESCRIPTION:
;    This popup widget is meant to be used anywhere you might need input
;       from a user.  Functionally, it replaces "read,prompt='Input:',ans"
;       that would be used in a older style input methodology.  It can
;       be used in widget and non-widget programs alike but is probably
;       better used within widget programs.
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    result = qinput( [keywords] )
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;    DEFAULT  = Default value for input.  If not supplied the default has no
;                  value which is interpreted differently for each input type.
;    GROUP_LEADER = Group leader id. If present, qinput runs as a modal
;                  application. Otherwise, it runs as a non-modal application.
;    PROMPT   = Character string to be displayed as a prompt.
;    TITLE    = Title for the widget.
;
;    FLOATING = Set this keyword to accept a floating-point value.
;    INTEGER  = Set this keyword to accept an integer value.
;    LONG     = Set this keyword to accept a longword integer value.
;    STRING   = Set this keyword to accept a string value.
; OUTPUTS:
;    result = the returned value.
; KEYWORD OUTPUT PARAMETERS:
;    CANCELLED - Flag, if set means the input from user was cancelled and
;                   you should not process the returned value.
; COMMON BLOCKS:
; SIDE EFFECTS:
;    Suspends other widget event generation.  A user response is required.
; RESTRICTIONS:
;  Note: the default value you supply needs to be consistent with the
;     variable type you select if you want defaulting to work properly.
;     For example, don't give it a string if you want float values or you
;     may not get back what you intend.
; PROCEDURE:
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, July 27, 1994.
;    2002/03/06, Marc W. Buie, fixed bug in handling keyword type switches,
;                  added DEFAULT keyword.
;    2004/04/12, Doug Loucks, Cleaned up event handling. Added code that uses
;                  pointer variables. Added a 'Cancel' button.
;    2006/11/25, Nathaniel J. Cunningham, added CANCELLED keyword so that
;                  cancelling can be differentiated from null string.
;    2008/10/30, MWB, slight rewrite of internals for robust state structure.
;                  Also, now exclusively returns a scalar regardless of type.
;-

; -----------------------------------------------------------------------------
; Procedure qinput_event
; Default event handler. This event handler receives events from the
; CW_FIELD compound widget.
; -----------------------------------------------------------------------------
pro qinput_event, event

  ; Get the pointer to the result variable.
  widget_control, event.top, get_uvalue=state

  ; Store the value of the widget.
  (*(*state).presult) = event.value[0]

  widget_control, event.top, /DESTROY

end

; -----------------------------------------------------------------------------
; Procedure qinput_cancel_event
; Event handler for the 'Cancel' button.
; -----------------------------------------------------------------------------
pro qinput_cancel_event, event

  ; Get the pointer to the result variable.
  widget_control, event.top, get_uvalue=state

  ; Store a null string into the result variable and set cancelled flag.
  (*(*state).presult) = ''
  (*state).cancelled = 1

  widget_control, event.top, /DESTROY

end

; ------------------------------------------------------------------------------
; Function qinput
; ------------------------------------------------------------------------------
function qinput, PROMPT=prompt, FLOATING=floating, INTEGER=integer,$
                 GROUP_LEADER=group_leader, cancelled = cancelled, $
                 LONG=long, STRING=string, title=title, DEFAULT=default

  if not keyword_set(prompt) then prompt='Input field:'

  if not keyword_set(title) then title='Input Request:'
  if not keyword_set(default) then default=''

  ; Define the top-level-base. It will be modal and will be positioned over
  ; the widget specified as the group leader.
  if keyword_set(group_leader) then begin
     tlb = widget_base(TITLE=title, COLUMN=1, /FLOATING,$
                       GROUP_LEADER=group_leader, /MODAL)
  endif else begin
     tlb = widget_base(TITLE=title, COLUMN=1)
  endelse

  ; Add a CW_FIELD compound widget.
  dummy = cw_field(tlb,$
                   FLOATING=floating,$
                   INTEGER=integer,$
                   LONG=long,$
                   STRING=string,$
                   /RETURN_EVENTS,$
                   TITLE=prompt,$
                   VALUE=default)

  ; Add a 'Cancel' button.
  dummy = widget_button(tlb, VALUE='Cancel', EVENT_PRO='qinput_cancel_event')

  state = ptr_new({ $
             presult: ptr_new(/allocate_heap), $
             cancelled: 0 $
             })

  widget_control, tlb, /REALIZE
  widget_control, tlb, SET_UVALUE=state

  xmanager, 'qinput', tlb

  ; Get the value to be returned to the caller.
  if ptr_valid((*state).presult) then begin
     ;make sure presult is a valid pointer, and that its contents are defined
     if n_elements( (*(*state).presult) ) gt 0 then $
        rvalue = (*(*state).presult) $
     else $
        rvalue = ''
  endif else begin
     rvalue = ''
  endelse

  cancelled = (*state).cancelled

  ; Free the pointers
  ptr_free, (*state).presult
  ptr_free, state

  return, rvalue
end

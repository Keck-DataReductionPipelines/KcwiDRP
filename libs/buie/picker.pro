;+
; NAME:
;  picker
; PURPOSE:
;  Widget for selecting a text item from a list.
; DESCRIPTION:
;  This widget provides a simple mechanism for selecting a single item
;  from a list.  The items are all strings and the return from this call
;  is the string selected.
; CATEGORY:
;  Widgets
; CALLING SEQUENCE:
;  result=picker(text_array)
; INPUTS:
;  text_array - String array of items to present for selection.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  GROUP:  The widget ID of the widget that calls PICKER.  When this
;          ID is specified, a death of the caller results in the death of
;          the PICKER widget application.
;
;  SORT:   Flag, if set causes input to be sorted in selection list.  Duplicates
;             are NOT weeded out.  This function keeps track of which one you
;             select but you might not be able to tell the difference.
;  TITLE:  A scalar string to be used for the window title.  If it is
;          not specified, the default title is "Select from list"
;
;  YSIZE:  Number of lines of list to show before scrolling the list.
;          The default is 10.
;
; OUTPUTS:
;  Return value is the selected string.  If the operation was canceled,
;    then the return value is "[[[CANCEL]]]"
; KEYWORD OUTPUT PARAMETERS:
;  INDEX:  index of the item chosen
; COMMON BLOCKS:
;  MWB_PICKER: COMMON block that maintains state for the widget.
; SIDE EFFECTS:
;  This function initiates the XMANAGER if it is not already running.
; RESTRICTIONS:
;  Only one instance of the PICKER widget can be running at one time.
; PROCEDURE:
;  Create and register the widget and then exit, returning the filename
;  that was picked.
; MODIFICATION HISTORY:
;  96/06/09 - Written by Marc W. Buie
;  96/11/11 - MWB, added SORT keyword
;-

; ------------------------------------------------------------------------------
;       procedure Picker_ev
; ------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
; ------------------------------------------------------------------------------
pro picker_ev, event

common mwb_picker, list, ok, cancel, selection, e_index, idx

case event.id of

   cancel: begin
      selection = "[[[CANCEL]]]"
      e_index = -1
      widget_control, event.top, /DESTROY
      end

   list: begin
      widget_control, list, GET_UVALUE = sel_list
      selection = sel_list[event.index]
      e_index = idx[event.index]
      end

   ok: begin
      if selection ne "[[[CANCEL]]]" then $
         widget_control, event.top, /DESTROY
      end

endcase
return

end

; ------------------------------------------------------------------------------
;       procedure Picker
; ------------------------------------------------------------------------------
;  This is the actual routine that creates the widget and registers it with the
;  Xmanager.
; ------------------------------------------------------------------------------
function Picker, sel_list, GROUP = group, TITLE = title, $
   YSIZE=ysize, INDEX=index, SORT=sortlist

common mwb_picker, list, ok, cancel, selection, e_index, idx

selection = "[[[CANCEL]]]"

if xregistered("Picker") then return,selection

if badpar(ysize,[0,2,3],0,CALLER='PICKER: (ysize) ',default=10) then return,selection

if not keyword_set(title) then title = "Select from list"

if keyword_set(sortlist) then begin
   idx=sort(sel_list)
endif else begin
   idx=lindgen(n_elements(sel_list))
endelse

listbase =  widget_base(TITLE = title, /COLUMN)
list     =  widget_list(listbase, VALUE = sel_list[idx], YSIZE = ysize, $
                UVALUE = sel_list[idx])
rowbase  =  widget_base(listbase, /ROW)
ok       =  widget_button(rowbase, VALUE = "Ok")
cancel   =  widget_button(rowbase, VALUE = "Cancel")

widget_control, listbase, /REALIZE

xmanager, "Picker", listbase, EVENT_HANDLER = "picker_ev", $
        GROUP_LEADER = group, /MODAL

index = e_index
return,selection

end

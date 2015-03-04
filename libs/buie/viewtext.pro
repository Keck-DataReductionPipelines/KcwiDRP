;+
; NAME:
;    viewtext
; PURPOSE: (one line)
;    View a string, or string array, of text in a scrollable text widget.
; DESCRIPTION:
;
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    viewtext, text
; INPUTS:
;    text : String (scalar, 1-D or 2-D array) of text to be displayed.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    BLOCK    - Flag, if set will force the widget to block command line
;                 access while running.  If the MODAL flag is set, this
;                 defaults to BLOCK=1.  If MODAL is not set, then BLOCK
;                 defaults to zero.
;    EXIT     - Label for exit button.  Default is 'Dismiss'.
;    FONT     - Font to use for the text.  Default is '8x13'.
;    GROUP    - Group Leader.
;    MODAL    - Flag, if set makes the widget modal (block).
;    PRINTCMD - Command to use for printing the text in the widget, the
;               default is 'lp'.  Special support is provided for 'pspr'.
;    TITLE    - Title of widget.
;    XSIZE    - Width of text.  Default is 80 characters.
;    YSIZE    - Length of text.  Default is 40 lines.
; OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;  A temporary file 'header.lis' is written to the current directory when
;   using the hardcopy feature.  This file is deleted after it is printed.
;   For this feature to work, your current directly must be writeable.  Also,
;   if you happen to have a file with the same name in this directory it
;   will get overwritten and deleted.
;
; RESTRICTIONS:
;   This tool can only print to the default printer.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, May, 1993.
;    1994/01/07, DWL, Added Hardcopy button.
;    1994/08/25, DWL, Minor mods.
;    1996/07/02, MWB, changed YSIZE default to 40 lines.
;    2006/12/12, MWB, added PRINTCMD keyword and some minor cosmetic rewriting.
;    2006/12/15, MWB, added BLOCK keyword
;-

; ------------------------------------------------------------------------------
; Procedure viewtext_eve
; event handler for the View Header procedure.
; ------------------------------------------------------------------------------
pro viewtext_eve, event

   widget_control, event.id, /HOURGLASS

   stash = widget_info( event.handler, /CHILD )
   widget_control, stash, GET_UVALUE=state, /NO_COPY

   case event.id of
      state.dismissid : begin
         widget_control, event.handler, SET_UVALUE=state, /NO_COPY
         widget_control, event.top, /DESTROY
         return
      end

      state.hardcpyid : begin
         widget_control, state.textid, GET_VALUE=text
         openw, lu, 'header.lis', /get_lun
         maxline=0
         for j=0,n_elements(text)-1 do begin
            len = strlen(text[j])
            if len gt maxline then maxline=len
            printf,lu,text[j]
         endfor
         close,lu
         free_lun,lu
         if state.printcmd eq 'pspr' then begin
            if maxline gt 90 then begin
               cmd = state.printcmd+' -l header.lis | lp -o raw'
            endif else begin
               cmd = state.printcmd+' header.lis | lp -o raw'
            endelse
         endif else begin
            cmd=state.printcmd+' header.lis'
         endelse
         print,cmd
         spawn,cmd
         file_delete,'header.lis',/quiet
      end
   endcase

   widget_control, stash, SET_UVALUE=state, /NO_COPY

end


; ------------------------------------------------------------------------------
; Procedure viewtext
; Displays text in a scrollable text widget.
; ------------------------------------------------------------------------------
pro viewtext,text,EXIT=exitbutton,FONT=font,GROUP=group,MODAL=modal, $
             PRINTCMD=printcmd,TITLE=title,XSIZE=xsize,YSIZE=ysize, $
             BLOCK=block

   self='VIEWTEXT: '
   ; Check for required parameter.
   if n_params() ne 1 then begin
      print,self,'viewtext,text'
      return
   endif

   if badpar(text,7,[0,1,2],caller=self+'(text) ') then return

   if badpar(exitbutton,[0,7],0,caller=self+'(EXIT) ', $
                                default='Dismiss') then return
   if badpar(font,[0,7],0,caller=self+'(FONT) ', $
                                default='8x13') then return
   if badpar(modal,[0,1,2,3],0,caller=self+'(MODAL) ', $
                                default=0) then return
   if badpar(title,[0,7],0,caller=self+'(TITLE) ', $
                                default='View Text') then return
   if badpar(xsize,[0,1,2,3],0,caller=self+'(XSIZE) ', $
                                default=80) then return
   if badpar(ysize,[0,1,2,3],0,caller=self+'(YSIZE) ', $
                                default=40) then return
   if badpar(printcmd,[0,7],0,caller=self+'(PRINTCMD) ', $
                                default='lp') then return
   if modal then def_block=1 else def_block=0
   if badpar(block,[0,1,2,3],0,caller=self+'(BLOCK) ', $
                                default=def_block) then return

   ; Create the main base and set the group leader.
   mainbase = widget_base( TITLE=title, /COLUMN )
   if not keyword_set(group) then group=mainbase

   state = {printcmd: printcmd, dismissid:0L, hardcpyid:0L, textid:0L}

   ; Create the exit button.
   state.dismissid = widget_button( mainbase, VALUE=exitbutton )
   ; Create the hardcopy button.
   state.hardcpyid = widget_button( mainbase, VALUE='Hardcopy' )

   ; Create the text widget.
   state.textid = widget_text( mainbase, VALUE=text, XSIZE=xsize, YSIZE=ysize, $
                  /SCROLL, FONT=font )

   widget_control, mainbase, /REALIZE

   ;Stash the state.
   stash = widget_info( mainbase, /CHILD )
   widget_control, stash, SET_UVALUE=state, /NO_COPY

   xmanager, 'viewtext', mainbase, $
             EVENT_HANDLER='viewtext_eve', $
             GROUP_LEADER=group, $
             MODAL=modal,no_block=(not block)

end

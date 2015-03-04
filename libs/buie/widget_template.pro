;  Widget template program.  This program doesn't actually do anything but
;    is functional.  Search for zzzz and ZZZZ and replace with the true name
;    of your widget (which should match the name of the file too).  You only
;    need add real actions and additional widgets and functions to get going.
;  This template conforms to all the requirement for a compound_widget and
;    can be run non-blocking if desired.

pro zzzz_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; Free up any dynamically allocated space with the state structure
;  ptr_free,(*state).something

   ; Free up the state structure itself.
   ptr_free, state

end

pro zzzz_eve, event

   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   case event_name of

      'THE_MENU': begin
         case event.value of

;            : begin
;            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               return
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Mainbase': begin

         ; Use if you have other widgets on screen, need to take off their
         ;   size from the event x,y size.
;         info=widget_info((*state).colbaseid,/geometry)
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

         ; Use if draw window is only thing in the tool.
         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

;        refresh window here
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro zzzz

   ; optional
   if xregistered('zzzz') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. ZZZZ cannot be started.'
      return
   endif

   ;Define the main base.
   mainbase = widget_base( TITLE='ZZZZ: Widget Template', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\action 1',$
                     '0\action 2',$
                     '2\Exit',$
                     '1\Tools',$
                     '0\tool 1', $
                     '0\tool 2', $
                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase)

   win1 = widget_draw( base, XSIZE=400, YSIZE=400, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Window' )

   state = ptr_new({ $

      ; Data and information in the widget

      ; Widget ids
      drawwin: win1, $           ; ID of main draw window

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'zzzz', mainbase, $
             EVENT_HANDLER='zzzz_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='zzzz_cleanup'

end

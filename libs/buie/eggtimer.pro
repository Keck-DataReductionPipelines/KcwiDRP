;+
; NAME:
;  eggtimer
; PURPOSE:   (one line only)
;  Widget countdown timer
; DESCRIPTION:
; This program is a non-blocking widget that will count down from a
;   user modifiable duration.  You can run more than one of these at the
;   same time.  Once the time has counted down to zero, the color changes to
;   red and it starts counting up from the time that the timer expired.
; CATEGORY:
;  Widgets
; CALLING SEQUENCE:
;  eggtimer
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
;  ALERTCMD - Optional string that will be executed by the "spawn" command
;               when the timer expires.  This can be used to run a program,
;               send mail, play a sound, pretty much anything that you want.
;               The default is to not do anything special.
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2005/05/06
;-
pro eggtimer_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; Free up any dynamically allocated space with the state structure
;  ptr_free,(*state).something

   ; Free up the state structure itself.
   ptr_free, state

end

pro eggtimer_display,state

   d_window=!d.window
   widget_control, (*state).drawwin, get_value=winnum
   wset,winnum
   erase

   font=1

   ; Generate display for current values
   case (*state).state of
      0: begin
         xyouts,(*state).mid,30,strn((*state).len),align=0.5, $
            /device,charsize=10.0,font=font
         xyouts,(*state).mid,120,'Idle',align=0.5,charsize=3,/device,font=font
      end
      1: begin
         curjd=systime(/julian)
         (*state).remain=long(((*state).jdstop-curjd) * 86400.0d0 + 0.5)
         if (*state).remain gt 0 then begin
            xyouts,(*state).mid,45,strn((*state).remain), $
               align=0.5,/device,charsize=10.0,font=font
         endif else begin
            widget_control,(*state).goid,set_value=' Start  '
            widget_control,(*state).stopid,sensitive=0
            if (*state).alertcmd ne '' then spawn,(*state).alertcmd
            (*state).state=3 ; should be 3
            eggtimer_display,state
         endelse
      end
      2: begin
         xyouts,(*state).mid,30,strn((*state).remain),align=0.5, $
                /device,charsize=10.0,font=font
         xyouts,(*state).mid,120,'Paused',align=0.5,charsize=3,/device,font=font
      end
      3: begin
         (*state).remain=0
         if (*state).toggle eq 0 then begin
            since = long((systime(/julian)-(*state).jdstop)*86400.0d0+0.5)
            tag=''
            if since gt 9999 then begin
               since=9999
               tag='+'
            endif
            xyouts,(*state).mid,120,'Expired ('+strn((*state).len)+')', $
               align=0.5,charsize=3,/device,color='0000ff'xl,font=font
            xyouts,(*state).mid,30,strn(since)+tag, $
               align=0.5,/device,charsize=10.0,color='0000ff'xl,font=font
            (*state).toggle = 1
         endif else begin
            (*state).toggle = 0
         endelse
      end
      else: begin
         print,'Unknown state'
      end
   endcase

   wset,d_window

end

pro eggtimer_eve, event

   widget_control, event.top, GET_UVALUE=state
   widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   case event_name of

      'go': begin
         widget_control,(*state).goid,get_value=curname
         if curname eq ' Start  ' then begin
            widget_control,(*state).goid,set_value=' Pause  '
            widget_control,(*state).stopid,sensitive=1
            widget_control,(*state).baseid,timer=1
            curjd=systime(/julian)
            (*state).jdstop = curjd+(*state).len/86400.0d0
            (*state).remain = (*state).len
            (*state).state = 1
         endif else if curname eq ' Pause  ' then begin
            widget_control,(*state).goid,set_value='Continue'
            curjd=systime(/julian)
            (*state).remain=long(((*state).jdstop-curjd) * 86400.0d0 + 0.5)
            (*state).state = 2
         endif else begin
            widget_control,(*state).goid,set_value=' Pause  '
            widget_control,(*state).baseid,timer=1
            curjd=systime(/julian)
            (*state).jdstop = curjd+(*state).remain/86400.0d0
            (*state).state = 1
         endelse
         eggtimer_display,state
      end

      'reset': begin
         widget_control,(*state).goid,set_value=' Start  '
         widget_control,(*state).stopid,sensitive=0
         (*state).state=0
         eggtimer_display,state
      end

      'settime': begin
         if (*state).state ne 0 then return
         newlen=qinput(group_leader=(*state).mainbase,default=0,/long, $
                       prompt='New timer duration (sec)')
         if newlen gt 0 then begin
            (*state).len = newlen
            eggtimer_display,state
         endif
      end

      'stop': begin
         widget_control,(*state).goid,set_value=' Start  '
         widget_control,(*state).stopid,sensitive=0
         (*state).state=0
         eggtimer_display,state
      end

      'Timer': begin
         eggtimer_display,state
         if (*state).state eq 1 or (*state).state eq 3 then $
            widget_control,(*state).baseid,timer=1
      end

      'quit' : begin
         widget_control, event.top, /DESTROY
         return
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro eggtimer,ALERTCMD=alertcmd,TITLE=title

   self='eggtimer: '
   if badpar(alertcmd,[0,7],0,caller=self+'(ALERTCMD) ',default='') then return
   if badpar(title,[0,7],0,caller=self+'(TITLE) ',default='EggTimer') then return

   ; optional
;   if xregistered('eggtimer') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. EggTimer cannot be started.'
      return
   endif

   ;Define the main base.
   mainbase = widget_base( TITLE=title, /COLUMN, UVALUE=0)

   base = widget_base(mainbase,/col,uvalue='Timer')

   b = widget_base(base,/row)
   goid = widget_button(b,value=' Start  ',uvalue='go')
   stopid = widget_button(b,value='Stop',uvalue='stop')
   resetid = widget_button(b,value='Reset',uvalue='reset')
   t = widget_button(b,value='Set Time',uvalue='settime')
   t = widget_button(b,value='Quit',uvalue='quit')

   width=250
   win1 = widget_draw( base, XSIZE=width, YSIZE=150, RETAIN=2)

   state = ptr_new({ $

      ; Data and information in the widget
      alertcmd: alertcmd, $      ; command to spawn when timer expires
      jdstop: 0.0d0, $           ; expiration time of timer
      len:  10L, $               ; duration of timer (seconds)
      remain: 0L, $              ; time left on timer (seconds)
      state: 0, $                ; timer state (0-inactive)
                                 ;             (1-running)
                                 ;             (2-paused)
                                 ;             (3-expired)
      toggle: 0, $
      mid: width/2, $            ; middle of window

      ; Widget ids
      baseid:  base, $
      drawwin: win1, $
      goid:    goid, $
      resetid: resetid, $
      stopid:  stopid, $

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   widget_control,stopid,sensitive=0
;   widget_control,resetid,sensitive=0

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   eggtimer_display,state

   ; Give control to the XMANAGER.
   xmanager, 'eggtimer', mainbase, $
             EVENT_HANDLER='eggtimer_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='eggtimer_cleanup'

end

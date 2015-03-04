;+
; NAME:
;  noterun
; PURPOSE:
;  Widget to update notes field for runstat and other databases. 
; DESCRIPTION:
;  Read out the Notes column in a selected row of the 
;  runstat (or other) table in any of the instrument (or other) databases.
;  and place in an editable scrolled text widget, saving as appropriate.
;  Will handle multiple rundates and databases.
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  noterun
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; DATABASE - Name of MYSQL instrument database for update.
;              The default is 'pccd2obs'. The user can specify others
;              via the widget File menu.
; SELECTOR - Name of a column (which must be a CHAR(n) field, and unique) in the
;            selected database and table, used as a selector for the row 
;            to update. The default is 'Rundate.' This is a single selector- 
;            however, if the SELECTOR is 'Rundate' and the database table is 
;            pccdobs.runstat the inst field must also be 'L'. 
; TABLE    - Name of table in MYSQL database for update. 
;             The default is 'runstat'. 
; XSIZE-      Horizontal widget size, in characters, default is 80.
; YSIZE-      Vertical widget size, in lines, default is 9.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; Backslashes and single quotes in text are translated following
; the descriptions in QUOTE and DEQUOTE.
; You cannot restore a NULL to the notes field with this tool.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Peter L. Collins, 2006/10/24, Lowell Observatory
;  2006/10/27, PLC, modified to save unconditionally (except Exit Without Save)
;                   when changing data base row or exiting, to use the upgraded 
;                   QUOTE and DEQUOTE instead of runnotes, and to add the 
;                   SELECTOR keyword.
;-

pro noterun_cleanup, tlb

   print, 'noterun_cleanup'
   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; free any allocation for the notes.
   if ptr_valid((*state).notes) then ptr_free,(*state).notes
   ; Free up the state structure.
   ptr_free, state

end

; query database for list of rundates returned in qpattern.
pro noterun_rundatelist, state, qpattern
   table = (*state).table
   instdb = (*state).database

   print, 'searching ' + (*state).selector + ' on {', $
           strjoin(instdb, ','), ' }.', table
   openmysql,dblun,instdb
   query = ''
   if instdb eq 'pccdobs' and (*state).selector eq 'Rundate'  then $
       query = ' where inst = ' + quote('L') 
   query += ' ;'
   query = [ ' select ' + (*state).selector + ' from ' + table, query ]
   print, 'final query for rundates on ', instdb,' is ', query
   mysqlquery, dblun, query, qpattern,  format='a'
   free_lun, dblun
end

; save current notes in text widget to db
pro noterun_save, state
   if (*state).dirty then begin
      print, ' Saving ',(*state). rundate
      widget_control, (*state).notetextid, GET_VALUE=notes
      openmysql,dblun,(*state).database
      cmd= ' update ' + (*state).table + ' set Notes = ' + quote(notes) + $
             ' where ' + (*state).selector + ' = ' + quote((*state).rundate)
      if (*state).table eq 'pccdobs' and (*state).selector eq 'Rundate'  then $
          cmd += ' and inst = ' + quote('L') 
      cmd += ' ;'
      print,  cmd
      mysqlcmd, dblun,cmd,insertresp, nlines
      free_lun,dblun
      (*state).dirty = 0
      widget_control,(*state).rundatestatusid, SET_VALUE= 'Notes: unmodified'
   endif
end

; refresh widget with new (or the same) rundate
pro noterun_refresh, state,rundate

   (*state).rundate = rundate
   (*state).dirty = 0
   widget_control,(*state).rundateid, SET_VALUE=rundate
   widget_control,(*state).rundatestatusid, SET_VALUE='Notes: unmodified'
   cmd= ' select Notes from ' + (*state).table+ ' where ' +(*state).selector + $
           ' = ' + quote(rundate)
   if (*state).table eq 'pccdobs' and (*state).selector eq 'Rundate'  then $
       cmd += ' and inst = ' + quote('L') 
   cmd += ' ;'
   print,  cmd
   openmysql,dblun,(*state).database
   mysqlcmd,dblun,cmd,oldnotes,nlines
   free_lun, dblun
   if nlines gt 1 then begin
      ; db query succeeded.
      if oldnotes[1] ne 'NULL' then $  
         notes=dequote(oldnotes[1],/SELECTSQL) $
      else notes =''  ; NULL (Default) notes field.
      if ptr_valid((*state).notes) then ptr_free,(*state).notes
      (*state).notes = ptr_new(notes)
      widget_control, (*state).notetextid, SET_VALUE=notes,/EDITABLE ; display
   endif else begin
      widget_control,(*state).rundateid, set_value=''
      widget_control,(*state).rundatestatusid, set_value=''
      widget_control,(*state).notetextid, set_value='',editable=0
      print, 'Unexpected data base error in query', (*state).bel
   endelse
end
pro noterun_eve, event

   ; get current state
   widget_control, event.top, get_uvalue=state

   (*state).top = event.top

   if event.id eq (*state).mainbase then $
      event_name = 'mainbase' $
   else $
      widget_control, event.id,  get_uvalue=event_name, /hourglass

   ; collapse some events
   ; event_name= prevnext, subevent in [prev,next]
   pnevents = ['Prev','Next']
   pn = where( event_name eq pnevents, count)
   if count eq 1 then begin
      subevent = pnevents[pn[0]]
      event_name = 'prevnext'
   endif

   case event_name of

      'THE_MENU': begin
         ; collapse some events. 
         ; event.value is rundate ,   subevent in [Choose, Enter, First,Last]
         if stregex(event.value, '.* ' + (*state).selector,/boolean) then begin
            subevent=strsplit(event.value,' ',/extract)
            subevent=subevent[0]
            event.value = 'rundate'
         endif

         case event.value of

            'Exit' : begin
               ; unconditionally save (if dirty) before exiting.
               noterun_save, state
               widget_control, event.top, /destroy
               return
            end

            'Exit Without Save' : begin
               print, 'Exit Without Save'
               ; this is a way out if you really don't want to save.
               widget_control, event.top, /destroy
               return
            end

            'rundate' : begin
               noterun_rundatelist,state, qpattern ; get list of selector values
               nq = n_elements(qpattern)
               rundate=''
               if subevent eq 'Enter' then begin 
                  rundate= qinput(prompt='please type ' + (*state).selector, $
                                  /string)
                  rundate = rundate[0] ; whyy??
                  z = where(rundate eq qpattern)
                  if z[0] lt 0 then rundate ='' ; it wasn't on the valid list.
               endif
               if subevent eq 'Choose' then rundate = picker(qpattern) 
               if subevent eq 'First' then rundate = qpattern[0]
               if subevent eq 'Last' then rundate = qpattern[nq-1]
               if stregex(rundate,'.*CANCEL', /BOOLEAN) then rundate =''

               if rundate eq '' then print, 'no rundates available!', $ 
                                           (*state).bel $
               else begin
                  ; switching to new rundate, first save the old as needed.
                  noterun_save,state
                  noterun_refresh,state, rundate
               endelse
            end

            'Save' : begin
               noterun_save, state
            end

            'Instrument Data Base': begin
               ; it's possible there is no rundate here, but not dirty.
               noterun_save, state 
               database = qinput(/string, prompt= $
                                   'please select instrument database')
               database = database[0]
               ;print, database
               if database ne '' then begin
                  (*state).database = database
                  (*state).dirty = 0
                  (*state).rundate = ''
                  widget_control,(*state).rundateid, set_value=''
                  widget_control,(*state).rundatestatusid, set_value=''
                  widget_control,(*state).instid,set_value='     ' + database
                  widget_control,(*state).notetextid, set_value='',editable=0
               endif
            end

         endcase
      end ; the_menu

      'mainbase': begin

         ; use if you have other widgets on screen, need to take off their
         ;   size from the event x,y size.
;         info=widget_info((*state).colbaseid,/geometry)
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize


;        refresh window here
      end

      'notetext': begin
         ; any time you touch the window at all.
         if not (*state).dirty then $
            widget_control,(*state).rundatestatusid, set_value= $
                                    'notes: [modified]'
         (*state).dirty  = 1
      end

      'prevnext': begin
         noterun_save, state
         rundate = ''
         noterun_rundatelist,state,qpattern
         ;print, qpattern
         lastrundate = (*state).rundate
         if lastrundate ne '' then begin  ; obvious if already in rundate
            z = where( lastrundate eq qpattern)
            if subevent eq 'Next' and z[0] lt n_elements(qpattern) - 1 then $
               rundate = qpattern[z[0] + 1] $
            else if subevent eq 'Prev'  and z[0] gt 0 then $
               rundate = qpattern[z[0] - 1] $
            else rundate = ''  ; off the end
         endif else begin if subevent eq 'Prev' then $
            ; if no previous rundate, next goes to the first, prev to the last.
            rundate = qpattern[n_elements(qpattern)- 1] $
            else rundate = qpattern[0]
         endelse
         if rundate eq '' then begin
            print, 'no ',subevent, ' Rundate!' + (*state).bel
         endif else begin
            ; switching to new rundate, first save the old as needed.
            noterun_save,state
            noterun_refresh,state, rundate
         endelse
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro noterun, DATABASE=database,TABLE=table, SELECTOR=selector, XSIZE=xsize, $
             YSIZE=ysize
   self = 'noterun: '
   if badpar(database,  [0,7],    0,   caller=self + '(DATABASE) ', $
             default='pccd2obs') then return
   if badpar(selector,     [0,7],    0,   caller=self + '(SELECTOR) ', $
             default='Rundate') then return
   if badpar(table,     [0,7],    0,   caller=self + '(TABLE) ', $
             default='runstat') then return
   if badpar(xsize,  [0,1,2,3],    0,  caller=self + '(XSIZE) ', $
             default=80) then return
   if badpar(ysize,  [0,1,2,3],    0,  caller=self + '(YSIZE) ', $
             default=9) then return
   
   if xregistered('noterun') then begin
      print, self, 'widget already running'
      return
   endif

   if (!d.flags and 256) eq 0 then begin
      print, self, 'error- no windowing device, cannot be started.'
      return
   endif

   ;Define the main base.
   mainbase = widget_base(TITLE='NOTERUN: Notes Editing Tool', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Choose ' + selector,$
                     '0\Enter ' + selector,$
                     '0\ First ' + selector, $
                     '0\ Last ' + selector, $
                     '0\Instrument Data Base', $
                     '0\Save', $
                     '0\Exit Without Save', $
                     '2\Exit' $
                      ], UVALUE='THE_MENU', /MBAR)

   envirobase = widget_base(mainbase,/COLUMN)
   rundatebase = widget_base(envirobase,/ROW,FRAME=3)
   rundateid = widget_label(rundatebase, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   rundatestatusid = widget_label(rundatebase, VALUE='',/DYNAMIC_RESIZE, $
                                  /ALIGN_LEFT)
   previd=widget_button(rundatebase,VALUE='Prev',UVALUE='Prev',/SENSITIVE)
   nextid=widget_button(rundatebase,VALUE='Next',UVALUE='Next',/SENSITIVE)
   instid = widget_label(rundatebase, VALUE='     ' + database, FRAME=5, $
                         /DYNAMIC_RESIZE,/ALIGN_LEFT)

   notetextid=widget_text(envirobase,VALUE='', XSIZE=xsize, YSIZE=ysize, $
                          /SENSITIVE, $
                          /ALL_EVENTS,UVALUE='notetext',EDITABLE=0,/SCROLL)

   state = ptr_new({ $

      ; Data and information in the widget

      top:0, $
      mainbase: mainbase, $       ; ID of top level base.
      rundate: '', $
      rundateid: rundateid, $
      rundatestatusid: rundatestatusid, $
      instid: instid, $
      notetextid:notetextid, $
      database:database, $
      table:table, $
      selector:selector, $
      bel:string(07B), $
      dirty:0, $
      notes:ptr_new() $
      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'noterun', mainbase, $
             EVENT_HANDLER='noterun_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='noterun_cleanup'

end

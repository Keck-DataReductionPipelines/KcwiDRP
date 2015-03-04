;+
; NAME:
;  mysqldocedit
; PURPOSE:   (one line only)
;  GUI editor for database documentation system
; DESCRIPTION:
;  This program allows for editing the doc table in a database in support
;    of generating html documenation with mysqldoc.pro.
;  Most functions should be obvious.  The one tricky thing is the macro
;    name.  Any macro name you type in is not acknowledged and editable
;    until you hit <enter>.
;  All save options are transparent to the user and there is no "undo" feature.
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  mysqldocedit
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
; Written by Marc W. Buie, Lowell Observatory, 2006/12/10
; 2006/01/24, MWB, fixed a problem with removing extraneous documentation
; 2007/01/31, MWB, fixed a minor problem with mysqldoc call.
;-
pro mysqldocedit_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   if (*state).dbname ne '' then free_lun,(*state).dblun

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).errinfo
   ptr_free,(*state).fieldlist
   ptr_free,(*state).fieldtype
   ptr_free,(*state).tablelist

   ; Free up the state structure itself.
   ptr_free, state

end

pro mysqldocedit_docheck, state
   mysqldocscan,(*state).dblun,result=result,/silent
   if result[0] ne '' then begin
      widget_control,(*state).errid,sensitive=1,set_value=result
   endif else begin
      widget_control,(*state).errid,sensitive=0,set_value=''
      result=''
   endelse
   ptr_free,(*state).errinfo
   (*state).errinfo=ptr_new(result)
end

pro mysqldocedit_fieldupdate, state

   if (*state).tableselect lt 0 then return

   if (*state).tableselect eq 0 then begin
      widget_control,(*state).fieldnameid,sensitive=0,set_value='none'
      (*state).fieldselect=0

   endif else begin

      tab = string(byte(9))

      ; get list of fields for current table and put in drop down list
      cmd = 'describe '
      cmd += (*(*state).tablelist)[(*state).tableselect]
      cmd += ';'
      mysqlcmd,(*state).dblun,cmd,result,nlines

      if nlines gt 1 then begin
         fieldlist=strarr(nlines-1)
         fieldtype=strarr(nlines-1)
         for i=1,nlines-1 do begin
            words=strsplit(result[i],tab,/extract)
            fieldlist[i-1] = words[0]
            fieldtype[i-1] = words[1]
         endfor
         fieldlist=['<self>',fieldlist]
         fieldtype=['',fieldtype]
         widget_control,(*state).fieldnameid,sensitive=1, $
            set_value=fieldlist,set_droplist_select=0
         (*state).fieldselect=0
         ptr_free,(*state).fieldlist
         ptr_free,(*state).fieldtype
         (*state).fieldlist = ptr_new(fieldlist)
         (*state).fieldtype = ptr_new(fieldtype)
      endif else begin
         (*state).fieldselect=-1
         widget_control,(*state).fieldnameid,set_value='none',sensitive=0
      endelse

   endelse

end

pro mysqldocedit_refresh, state

   if (*state).dbname eq '' then begin

      widget_control,(*state).checkid,sensitive=0
      widget_control,(*state).errid,set_value='',sensitive=0
      widget_control,(*state).fieldnameid,sensitive=0
      widget_control,(*state).macroid,set_value='',sensitive=0
      widget_control,(*state).macronameid,set_value='',sensitive=0
      widget_control,(*state).rebuildid,sensitive=0
      widget_control,(*state).sourceid,set_value='',sensitive=0
      widget_control,(*state).tablenameid,sensitive=0
      widget_control,(*state).txtid,set_value='',sensitive=0
      widget_control,(*state).unitsid,set_value='',sensitive=0

   endif else begin

      widget_control,(*state).checkid,sensitive=1
      widget_control,(*state).rebuildid,sensitive=1
      widget_control,(*state).macronameid,set_value='',sensitive=1
      ptr_free,(*state).errinfo
      (*state).errinfo=ptr_new()
      widget_control,(*state).errid,set_value='',sensitive=0

      mysqldocedit_tableupdate,state
      mysqldocedit_fieldupdate,state

      mysqldocedit_show,state

   endelse

end

pro mysqldocedit_save, state
   if (*state).dbname eq '' then return
   c=','
   if (*state).dirtynote then begin
      print,'need to save note'
      if (*state).tableselect eq 0 then begin
         ; get info
         widget_control,(*state).txtid,get_value=info
         info=quote(info)
         cmd = 'select info from doc where tablename is NULL and field is NULL;'
         print,cmd
         mysqlcmd,(*state).dblun,cmd,answer,nlines
         if nlines eq 1 then begin
            cmd = 'insert into doc values (NULL,NULL,NULL,NULL,'
            if info eq '' then cmd += 'NULL' else cmd += info
            cmd += ');'
         endif else begin
            cmd = 'update doc set info='+info+ $
                  ' where tablename is NULL and field is NULL;'
         endelse

      endif else if (*state).fieldselect eq 0 then begin
         ; get descrip
         widget_control,(*state).sourceid,get_value=descrip
         descrip=strtrim(strcompress(descrip),2)
         descript=quote(descrip)

         ; get info
         widget_control,(*state).txtid,get_value=info
         info=quote(info)
         if info eq quote('') then info = 'NULL'
         cmd = 'select info from doc where tablename=' + $
               quote((*(*state).tablelist)[(*state).tableselect]) + $
               ' and field is NULL;'
         print,cmd
         mysqlcmd,(*state).dblun,cmd,answer,nlines
         if nlines eq 1 then begin
            cmd = 'insert into doc values ('+ $
                  quote((*(*state).tablelist)[(*state).tableselect]) + $
                  ',NULL,NULL,'
            if descrip eq '' then cmd += 'NULL' else cmd += quote(descrip)+c
            if info eq '' then cmd += 'NULL' else cmd += info
            cmd += ');'
         endif else begin
            cmd = 'update doc set source='+quote(descrip)+c+ $
                  'info='+info+' where tablename=' + $
                  quote((*(*state).tablelist)[(*state).tableselect]) + $
                  ' and field is NULL;'
         endelse

      endif else begin
         ; get units
         widget_control,(*state).unitsid,get_value=units
         units=strtrim(strcompress(units),2)
         if units eq '' then units = 'NULL'

         ; get source
         widget_control,(*state).sourceid,get_value=source
         source=strtrim(strcompress(source),2)
         if source eq '' then source = 'NULL'

         ; get info
         widget_control,(*state).txtid,get_value=info
         info=quote(info)
         if info eq '' then info = 'NULL'
print,info

         cmd = 'select info from doc where tablename=' + $
               quote((*(*state).tablelist)[(*state).tableselect]) + $
               ' and field=' + $
               quote((*(*state).fieldlist)[(*state).fieldselect])+';'
         print,cmd
         mysqlcmd,(*state).dblun,cmd,answer,nlines
         if nlines eq 1 then begin
            cmd = 'insert into doc values ('+ $
                  quote((*(*state).tablelist)[(*state).tableselect])+c+ $
                  quote((*(*state).fieldlist)[(*state).fieldselect])+c
            if units eq '' then cmd += 'NULL' else cmd += quote(units)
            cmd += c
            if source eq '' then cmd += 'NULL' else cmd += quote(source)
            cmd += c
            if info eq '' then cmd += 'NULL' else cmd += info
            cmd += ');'

         endif else begin
            cmd = 'update doc set units='
            if units eq '' then cmd += 'NULL' else cmd += quote(units)
            cmd += c+'source='
            if source eq '' then cmd += 'NULL' else cmd += quote(source)
            cmd += c+'info='
            if info eq '' then cmd += 'NULL' else cmd += info
            cmd += ' where tablename=' + $
                  quote((*(*state).tablelist)[(*state).tableselect]) + $
                  ' and field=' + $
                  quote((*(*state).fieldlist)[(*state).fieldselect])+';'
         endelse

      endelse
      print,cmd
      mysqlcmd,(*state).dblun,cmd,answer,nlines
      (*state).dirtynote=0
   endif
   if (*state).dirtymacro then begin
      ; check if macro name is already in database
      ; yes, update from widget.  If widget is empty, delete macro.
      ; no, add from widget, if widget is empty do nothing.
      print,'need to save macro'
      macroname = (*state).macroname

      cmd = 'select field from doc where tablename is NULL and field='+ $
            quote(macroname)+';'
      mysqlcmd,(*state).dblun,cmd,answer,nlines

      widget_control,(*state).macroid,get_value=info
      info=quote(info)
      ninfolines=n_elements(info)
      delete = 0
      if ninfolines eq 1 and info[0] eq quote('') then delete=1

      if nlines ne 2 then begin
         if not delete then begin
            cmd='insert into doc values (NULL,' + $
                quote(macroname)+c+ $
                'NULL,NULL,'+info+');'
         endif
      endif else begin
         if delete then begin
            cmd='delete from doc where tablename is NULL and field=' + $
                quote(macroname)+';'
         endif else begin
            cmd='update doc set info='+info+ $
                ' where tablename is NULL and field='+quote(macroname)+';'
         endelse
      endelse
      print,cmd
      mysqlcmd,(*state).dblun,cmd,answer,nlines

      (*state).dirtymacro=0
   endif
end

pro mysqldocedit_show, state

   if (*state).tableselect lt 0 or (*state).fieldselect lt 0 then return

   tab = string(byte(9))

   if (*state).tableselect eq 0 then begin
      print,'refresh ',(*state).dbname
      widget_control,(*state).tablenameid,sensitive=1,set_droplist_select=0
      widget_control,(*state).fieldtypeid,set_value=''
      widget_control,(*state).unitsid,set_value='',sensitive=0
      widget_control,(*state).sourceid,set_value='',sensitive=0
      cmd='select info from doc where tablename is NULL and field is NULL;'
      mysqlcmd,(*state).dblun,cmd,result,nlines
         if nlines eq 1 then begin
            widget_control,(*state).txtid,set_value='',sensitive=1
         endif else begin
            widget_control,(*state).txtid, $
               set_value=dequote(result[1],/selectsql),sensitive=1
         endelse
   endif else begin
      widget_control,(*state).tablenameid,sensitive=1, $
         set_droplist_select=(*state).tableselect
      widget_control,(*state).fieldnameid,sensitive=1, $
         set_droplist_select=(*state).fieldselect
      if (*state).fieldselect eq 0 then begin
         print,'refresh ',(*(*state).tablelist)[(*state).tableselect]
         widget_control,(*state).fieldtypeid,set_value=''
         widget_control,(*state).unitsid,set_value='',sensitive=0
         widget_control,(*state).sourceid,set_value='',sensitive=0
         cmd='select source,info from doc where tablename='+ $
             quote((*(*state).tablelist)[(*state).tableselect])+ $
             ' and field is NULL'+ $
             ';'
         mysqlcmd,(*state).dblun,cmd,result,nlines
         widget_control,(*state).unitsid,set_value='',sensitive=0
         widget_control,(*state).sourcelabelid,set_value='Descrip:'
         if nlines eq 1 then begin
            widget_control,(*state).txtid,set_value='',sensitive=1
            widget_control,(*state).sourceid,set_value='',sensitive=1
         endif else begin
            words = strsplit(result[1],tab,/extract,/preserve_null)
            words[0] = dequote(words[0])
            widget_control,(*state).txtid, $
               set_value=dequote(words[1],/selectsql),sensitive=1
            if words[0] eq 'NULL' then words[0] = ''
            widget_control,(*state).sourceid,set_value=words[0],sensitive=1
         endelse
      endif else begin
         print,'refresh ',(*(*state).tablelist)[(*state).tableselect], $
               ' ',(*(*state).fieldlist)[(*state).fieldselect]
         widget_control,(*state).fieldtypeid, $
            set_value=(*(*state).fieldtype)[(*state).fieldselect]
         cmd='select units,source,info from doc where tablename='+ $
             quote((*(*state).tablelist)[(*state).tableselect])+ $
             ' and field='+ $
             quote((*(*state).fieldlist)[(*state).fieldselect])+ $
             ';'
         mysqlcmd,(*state).dblun,cmd,result,nlines
         widget_control,(*state).sourcelabelid,set_value='Source:'
         if nlines eq 1 then begin
            widget_control,(*state).txtid,set_value='',sensitive=1
            widget_control,(*state).unitsid,set_value='',sensitive=1
            widget_control,(*state).sourceid,set_value='',sensitive=1
         endif else begin
            words = strsplit(result[1],tab,/extract,/preserve_null)
            words[0] = dequote(words[0])
            words[1] = dequote(words[1])
            widget_control,(*state).txtid, $
               set_value=dequote(words[2],/selectsql),sensitive=1
            if words[0] eq 'NULL' then words[0] = ''
            widget_control,(*state).unitsid,set_value=words[0],sensitive=1
            if words[1] eq 'NULL' then words[1] = ''
            widget_control,(*state).sourceid,set_value=words[1],sensitive=1
         endelse
      endelse
   endelse

end

pro mysqldocedit_showmacro, state

   if (*state).macroname eq '' then return

   macroname = (*state).macroname

   ; first, check to see if the name (stored in macronameid) is in the database
   cmd = 'select info from doc where tablename is NULL and field='+ $
         quote(macroname)+';'
   mysqlcmd,(*state).dblun,cmd,answer,nlines

   ;  if not it may end up being a new macro, clear the field and
   ;    sensitize
   if nlines ne 2 then begin
      widget_control,(*state).macroid,set_value='',sensitive=1

   ;  if it is, grab the macro text and put in macroid and sensitize
   endif else begin
      widget_control,(*state).macroid, $
         set_value=dequote(answer[1],/selectsql),sensitive=1
   endelse

end

pro mysqldocedit_tableupdate, state

   ; get list of tables and put in drop down list
   mysqlcmd,(*state).dblun,'show tables;',result,nlines
   if nlines gt 1 then begin
      tablelist = result[1:n_elements(result)-1]
      z=where(tablelist ne 'doc',count)
      if count gt 0 then begin
         tablelist = ['<self>',tablelist[z]]
         widget_control,(*state).tablenameid,sensitive=1, $
            set_value=tablelist,set_droplist_select=0
         (*state).tableselect=0
         ptr_free,(*state).tablelist
         (*state).tablelist = ptr_new(tablelist)
      endif else begin
         (*state).tableselect=-1
         widget_control,(*state).tablenameid,set_value='none',sensitive=0
      endelse
   endif else begin
      (*state).tableselect=-1
      widget_control,(*state).tablenameid,set_value='none',sensitive=0
   endelse

end

pro mysqldocedit_eve, event

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

            'Hide Current Table' : begin
               c = ','
               if (*state).tableselect le 0 then return
               cmd = 'select source from doc where tablename=' + $
                     quote((*(*state).tablelist)[(*state).tableselect]) + $
                     ' and field is NULL;'
               mysqlcmd,(*state).dblun,cmd,result,nlines
               ; no record, just need to add
               if nlines eq 1 then begin
                  cmd = 'insert into doc values (' + $
                     quote((*(*state).tablelist)[(*state).tableselect])+c+ $
                     'NULL'+c+ $
                     'NULL'+c+ $
                     quote('xxHIDExx')+c+ $
                     'NULL);'
                  print,cmd
                  mysqlcmd,(*state).dblun,cmd,result,nlines

               ; already a record, need to modify
               endif else begin
                  text = [ $
                          'There is already a documentation entry in the'+ $
                          ' database for table '+ $
                          (*(*state).tablelist)[(*state).tableselect], $
                          'Do you really want to overwrite the record and'+ $
                          ' hide the table?' ]
                  result=qannounc(text)
                  if result eq 1 then begin
                     cmd = 'update doc set source=' + $
                        quote('xxHIDExx')+ $
                        ' where tablename=' + $
                        quote((*(*state).tablelist)[(*state).tableselect])+ $
                        ' and field is NULL;'
                     print,cmd
                     mysqlcmd,(*state).dblun,cmd,result,nlines
                  endif
               endelse
               ; execute command
            end

            'Select Macro' : begin
               mysqldocedit_save, state
               cmd = 'select field from doc where tablename is NULL and' + $
                     ' field is not NULL order by field;'
               mysqlcmd,(*state).dblun,cmd,result,nlines
               if nlines eq 1 then begin
                  text='There are no macros defined.  Nothing to select.'
                  result=qannounc(text,group_leader=(*state).mainbase, $
                                 falselabel='')
               endif else begin
                  macrolist = result[1:nlines-1]
                  result = picker(macrolist,group=(*state).mainbase, $
                                  title='Macro List')
                  if result ne '[[[CANCEL]]]' then begin
                     widget_control,(*state).macronameid,sensitive=1, $
                                    set_value=result
                     (*state).macroname = result[0]
                     mysqldocedit_showmacro,state
                  endif
               endelse
            end

            'Exit' : begin
               mysqldocedit_save, state
               widget_control, event.top, /DESTROY
               return
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Check': begin
         mysqldocedit_save, state
         mysqldocedit_docheck, state
      end

      'DB Name': begin
         mysqldocedit_save, state
         widget_control,(*state).dbnameid,get_value=dbname
         dbname=dbname[0]

         ; close current connection if active
         if (*state).dbname ne '' then begin
            free_lun,(*state).dblun
            (*state).dblun = -100
            (*state).dbname = ''
            ptr_free,(*state).errinfo
            (*state).errinfo=ptr_new()
            (*state).macroname=''
            (*state).tableselect = -1
            (*state).fieldselect = -1
         endif

         ; flush and desensitize all controls
         widget_control,(*state).macroid,set_value='',sensitive=0
         widget_control,(*state).macronameid,set_value='',sensitive=0

         ; try to open the database
         openmysql,dblun,dbname,error
         if error eq 0 then begin
            (*state).dblun = dblun
            (*state).dbname = dbname
         endif

         mysqldocedit_refresh,state

      end

      'Error Select': begin
         mysqldocedit_save, state
         str = (*(*state).errinfo)[event.index]
         words=strsplit(str,' ',/extract)
         if words[0] eq 'Field' then begin
            field = words[1]
            table = words[4]
            z=where(table eq (*(*state).tablelist), count)
            if count ne 1 then return
            tableidx = z[0]
            (*state).tableselect = tableidx
            mysqldocedit_fieldupdate,state
            z=where(field eq (*(*state).fieldlist),count)
            if count eq 1 then begin
               fieldidx = z[0]
            endif else begin
               fieldidx = 0
            endelse
            (*state).fieldselect = fieldidx
            mysqldocedit_show, state
         endif else if words[0] eq 'Table' then begin
            table = words[1]
            z=where(table eq (*(*state).tablelist), count)
            if count ne 1 then return
            tableidx = z[0]
            (*state).tableselect = tableidx
            mysqldocedit_fieldupdate,state
            (*state).fieldselect = 0
            mysqldocedit_show, state
         endif else if words[0] eq 'Documented' then begin
            if words[1] eq 'field' then begin
               field = words[2]
               table = strmid(words[7],0,strlen(words[7])-1)
               text = [ $
                  'There is a documentation entry for the field '+ $
                  field+' that does not appear', $
                  'to exist in the table '+table+ $
                  '.  This happens when the table structure is', $
                  'changed (though this should be rather rare).', $
                  '', $
                  'Would you like to delete this apparently spurious'+ $
                  ' documentation entry or do', $
                  'you want to leave it alone?' ]
               result = qannounc(text,ysize=n_elements(text)+1, $
                        title='Delete Confirmation', $
                        truelabel='Delete Note', $
                        falselabel='Leave the entry alone.', $
                        group_leader=(*state).mainbase)
               if result eq 1 then begin
                  print,'remove documentation entry for ',table,'.',field
                  cmd = 'delete from doc where tablename='+quote(table)+ $
                        ' and field='+quote(field)+';'
                  print,cmd
                  mysqlcmd,(*state).dblun,cmd,answer,nlines
                  mysqldocedit_docheck, state
               endif
            endif else begin
               table = words[2]
               text = [ $
                  'There appears to be documentation for the table '+ $
                  table+' in the database.', $
                  'However, this table does not exist and the'+ $
                  ' documentation will remain hidden', $
                  'until such time as the table is re-created.  '+ $
                  'If the table was deleted', $
                  'permanently then the documentation should'+ $
                  ' also be deleted.', $
                  '', $
                  'Would you like to delete this apparently'+ $
                  ' spurious documentation entry or do', $
                  'you want to leave it alone?' ]
               result = qannounc(text,ysize=n_elements(text)+1, $
                        title='Delete Confirmation', $
                        truelabel='Delete '+table+' entries', $
                        falselabel='Leave everything alone', $
                        group_leader=(*state).mainbase)
               if result eq 1 then begin
                  print,'remove documentation entries for ',table
                  cmd = 'delete from doc where tablename='+quote(table)+';'
                  print,cmd
                  mysqlcmd,(*state).dblun,cmd,answer,nlines
                  mysqldocedit_docheck, state
               endif
            endelse
         endif else begin
            print,str
         endelse
      end

      'Field Name': begin
         mysqldocedit_save, state
         (*state).fieldselect = event.index
         mysqldocedit_show, state
      end

      'Macro': begin
         if event.type eq 0 then begin
            cr=byte(10)
            mysqldocedit_save, state
            widget_control,(*state).macroid,sensitive=0,set_value=''
            if event.ch eq cr then begin
               widget_control,(*state).macronameid,get_value=macroname
               (*state).macroname = macroname
               mysqldocedit_showmacro,state
            endif
         endif
      end

      'Macro Text': begin
         (*state).dirtymacro=1
      end

      'Note Text': begin
         (*state).dirtynote=1
      end

      'Rebuild html': begin
         mysqldocedit_save, state
         mysqldoc,(*state).dblun, $
            outdir=getenv('HOME')+'/bookmarks/docs/db/'+(*state).dbname, $
            dbname=(*state).dbname
      end

      'Source': begin
         (*state).dirtynote=1
      end

      'Table Name': begin
         mysqldocedit_save, state
         (*state).tableselect = event.index
         mysqldocedit_fieldupdate,state
         mysqldocedit_show, state
      end

      'Units': begin
         (*state).dirtynote=1
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro mysqldocedit

   ; optional
   if xregistered('mysqldocedit') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. MYSQLDOCEDIT cannot be started.'
      return
   endif

   self='mysqldocedit: '

   ;Define the main base.
   mainbase = widget_base( TITLE='MYSQLDOCEDIT: Widget Template', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\action 1',$
                     '0\action 2',$
                     '2\Exit',$
                     '1\Tools',$
                     '0\Hide Current Table', $
                     '0\Select Macro', $
                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase,/col)

   b1 = widget_base(base,/row)
   w1 = widget_label(b1,value='DB:')
   dbnameid = widget_text(b1,value='',/editable,xsize=10,uvalue='DB Name')
   tablenameid = widget_droplist(b1,/dynamic_resize, $
                        sensitive=0,uvalue='Table Name', value=['none'])
   fieldnameid = widget_droplist(b1,/dynamic_resize, $
                        sensitive=0,uvalue='Field Name', value=['none'])
   fieldtypeid = widget_label(b1,value='',/dynamic_resize)
   w1 = widget_label(b1,value='Units:')
   unitsid = widget_text(b1,value='',/editable,xsize=20,sensitive=0, $
                         uvalue='Units',/all_events)

   b1 = widget_base(base,/row)
   sourcelabelid = widget_label(b1,value='Source:')
   sourceid = widget_text(b1,value='',/editable,xsize=75,sensitive=0, $
                         uvalue='Source',/all_events)

   b2 = widget_base(base,/col)
   txtid = widget_text(b2,value='',xsize=80,ysize=18,uvalue='Note Text', $
                        sensitive=0,/editable,/scroll,/all_events)
   macroid = widget_text(b2,value='',xsize=80,ysize=6,uvalue='Macro Text', $
                        sensitive=0,/editable,/scroll,/all_events)
   b3 = widget_base(b2,/row)
   w1 = widget_label(b3,value='Macro:')
   macronameid = widget_text(b3,value='',/editable,xsize=8, $
                             sensitive=0,uvalue='Macro',/all_events)
   checkid = widget_button(b3,value='Check',uvalue='Check',sensitive=0)
   rebuildid = widget_button(b3,value='Push',uvalue='Rebuild html',sensitive=0)

   errid = widget_list(b2,value='',xsize=80,ysize=10, $
                        sensitive=0,uvalue='Error Select')

   state = ptr_new({ $

      ; Data and information in the widget
      dblun: -100, $             ; database pipe (negative if not connected)
      dbname: '', $              ; Name of database that is being edited
      dirtymacro: 0, $
      dirtynote: 0, $
      errinfo: ptr_new(), $      ; storage for error information
      fieldlist: ptr_new(), $
      fieldtype: ptr_new(), $
      fieldselect: -1, $
      macroname: '', $
      tablelist: ptr_new(), $
      tableselect: -1, $

      ; Widget ids

      checkid:     checkid, $
      dbnameid:    dbnameid, $
      errid:       errid, $
      fieldnameid: fieldnameid, $
      fieldtypeid: fieldtypeid, $
      macroid:     macroid, $
      macronameid: macronameid, $
      rebuildid:   rebuildid, $
      sourceid:    sourceid, $
      sourcelabelid: sourcelabelid, $
      tablenameid: tablenameid, $
      txtid:       txtid, $
      unitsid:     unitsid, $

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'mysqldocedit', mainbase, $
             EVENT_HANDLER='mysqldocedit_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='mysqldocedit_cleanup'

end

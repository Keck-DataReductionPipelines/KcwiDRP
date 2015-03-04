;+
; NAME:
;  fitsedit
; PURPOSE:
;  Interactive, widget-based editing of FITS header values.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  fitsedit
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2000/09/14.
;  2000/11/08, MWB, removed use of !err
;  2001/05/16, MWB, implemented the "Save" function which will cause the
;                      current file to be overwritten.
;  2003/10/01, MWB, converted my Filemove call to system file_move routine
;  2004/07/15, MWB, Added new tools to repair a busted keyword
;  2008/02/01, MWB, Added missing tool, "Add Integer Keyword" and fixed
;                some problems with the force tools.
;-
pro fitsedit_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

if (*state).dirty then print,'WARNING! Header changed but not saved.'

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).hdr
   ptr_free,(*state).value
   if ptr_valid( (*state).ehdr ) then ptr_free,(*(*state).ehdr)
   ptr_free,(*state).ehdr

   ; Free up the state structure itself.
   ptr_free, state

end

pro fitsedit_load, state, fn
   ; if current file is dirty, query for file save before proceeding

   if exists(fn) then begin
      widget_control,(*state).fnid,set_value=fn
      ptr_free,(*state).hdr
      (*state).hdr=ptr_new(headfits(fn))
      widget_control,(*state).textwin,set_value=(*(*state).hdr)
      (*state).curexten=0
      (*state).valid = 1
      (*state).file = fn
      value = sxpar((*(*state).hdr),'NEXTEND')
      if ptr_valid( (*state).ehdr ) then ptr_free,(*(*state).ehdr)
      ptr_free,(*state).ehdr
      if value gt 0 then begin
         (*state).ehdr = ptr_new(ptrarr(value))
         str = 'Exten '+strtrim(string(indgen(value+1)),2)
         for i=0,value-1 do begin
            (*(*state).ehdr)[i] = ptr_new(headfits(fn,exten=i+1))
         endfor
         (*state).nextend=value
         widget_control,(*state).extenid,set_value=str,sensitive=1
         widget_control,(*state).cpallid,sensitive=1
      endif else begin
         (*state).nextend=0
         widget_control,(*state).extenid,set_value='          ',sensitive=0
         widget_control,(*state).cpallid,sensitive=0
      endelse
   endif else begin
      widget_control,(*state).fnid,set_value=(*state).file
   endelse

end

pro fitsedit_getval, state, keyword, ok
   error_status=0
   catch,error_status
   if error_status ne 0 then begin
      print,'Invalid header line.'
      ok=0
   endif else begin
      message,/reset_error
      if (*state).curexten eq 0 then begin
         value = sxpar((*(*state).hdr),keyword)
      endif else begin
         value = sxpar((*(*(*state).ehdr)[(*state).curexten-1]),keyword)
      endelse
      if !error_state.code ne 0 then begin
         widget_control,(*state).keyid,set_value=''
         widget_control,(*state).valueid,set_value=''
      endif else begin
         widget_control,(*state).keyid,set_value=keyword
         ptr_free,(*state).value
         (*state).value = ptr_new(value)
         fitsedit_updateval, state
      endelse
   endelse
   catch,/cancel
end

pro fitsedit_save, state, fn

   if (*state).valid then begin
      if (*state).nextend eq 0 then begin
         a=readfits((*state).file,/noscale)
         writefits,fn,a,(*(*state).hdr)
      endif else begin
         writefits,fn,0,(*(*state).hdr)
         for exten=1,(*state).nextend do begin
            a=0
            fits_read,(*state).file,a,/data_only,exten=exten
            writefits,fn,a,(*(*(*state).ehdr)[exten-1]),/append
         endfor
      endelse
      (*state).dirty = 0
   endif

end

pro fitsedit_setval, state, keyword, newvalue
   value=(*(*state).value)
   type = size(value,/type)
   if (*state).curexten eq 0 then begin
      hdrptr = (*state).hdr
   endif else begin
      hdrptr = (*(*state).ehdr)[(*state).curexten-1]
   endelse
   case type OF
      1: begin
         sxaddpar,(*hdrptr),keyword,newvalue
         (*(*state).value) = newvalue eq 'T'
      end
      2: begin
         sxaddpar,(*hdrptr),keyword,fix(newvalue)
         (*(*state).value) = fix(newvalue)
      end
      3: begin
         sxaddpar,(*hdrptr),keyword,long(newvalue)
         (*(*state).value) = long(newvalue)
      end
      4: begin
         sxaddpar,(*hdrptr),keyword,float(newvalue)
         (*(*state).value) = float(newvalue)
      end
      5: begin
         sxaddpar,(*hdrptr),keyword,double(newvalue)
         (*(*state).value) = double(newvalue)
      end
      7: begin
         sxaddpar,(*hdrptr),keyword,newvalue
         (*(*state).value) = newvalue
      end
      else: begin
         print,'unrecognized'
      end
   endcase
   (*state).dirty=1
end

pro fitsedit_updateval, state
   value=(*(*state).value)
   type = size(value,/type)
   case type OF
      1: begin
         if value then str='T' else str='F'
      end
      2: begin
         str=strn(value)
      end
      3: begin
         str=strn(value)
      end
      4: begin
         str=strn(value,format='(g)')
      end
      5: begin
         str=strn(value,format='(d)')
      end
      7: begin
         str=strtrim(value,2)
      end
      else: begin
         print,'unrecognized'
      end
   endcase
   if str ne '' then widget_control,(*state).valueid,set_value=str
end

pro fitsedit_eve, event

   widget_control, event.top, GET_UVALUE=state

   widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   case event_name of

      'THE_MENU': begin
         case event.value of

            'Add Float Keyword': begin
               newkey = qinput(prompt='Name of keyword to add?', $
                               /string,title='Add Keyword')
               newkey = strupcase(strcompress(strtrim(newkey,2),/remove_all))
               newkey = newkey[0]
               newval = qinput(prompt='Float value?', $
                               /float,title='Add Keyword')
               newval = newval[0]
               comment = qinput(prompt='Comment?', $
                               /string,title='Add Keyword')
               comment = comment[0]
               if strmid(comment,0,1) ne ' ' then comment=' '+comment
               if (*state).curline ge 0 then begin
                  if (*state).curexten eq 0 then $
                     selkey = strtrim(strmid((*(*state).hdr)[(*state).curline],0,8),2) $
                  else $
                     selkey = strtrim( $
                        strmid((*(*(*state).ehdr)[(*state).curexten-1])[(*state).curline],0,8),2)
               endif else begin
                  selkey = ''
               endelse
               if (*state).curexten eq 0 then $
                  tmphdr=(*(*state).hdr) $
               else $
                  tmphdr=(*(*(*state).ehdr)[(*state).curexten-1])
               if selkey eq '' then begin
                  print,'Add ',newkey,'=',newval,'/',comment,' at end of header'
                  sxaddpar,tmphdr,newkey,newval,comment
               endif else begin
                  print,'Add ',newkey,'=',newval,'/',comment,' after ',selkey
                  sxaddpar,tmphdr,newkey,newval,comment,after=selkey
               endelse
               if (*state).curexten eq 0 then begin
                  ptr_free,(*state).hdr
                  (*state).hdr=ptr_new(tmphdr)
               endif else begin
                  ptr_free,(*(*state).ehdr)[(*state).curexten-1]
                  (*(*state).ehdr)[(*state).curexten-1] = ptr_new(tmphdr)
               endelse
               (*state).dirty = 1
               widget_control,(*state).textwin,set_value=tmphdr
            end

            'Add Integer Keyword': begin
               newkey = qinput(prompt='Name of keyword to add?', $
                               /string,title='Add Keyword')
               newkey = strupcase(strcompress(strtrim(newkey,2),/remove_all))
               newkey = newkey[0]
               newval = qinput(prompt='Integer value?', $
                               /integer,title='Add Keyword')
               newval = newval[0]
               comment = qinput(prompt='Comment?', $
                               /string,title='Add Keyword')
               comment = comment[0]
               if strmid(comment,0,1) ne ' ' then comment=' '+comment
               if (*state).curline ge 0 then begin
                  if (*state).curexten eq 0 then $
                     selkey = strtrim(strmid((*(*state).hdr)[(*state).curline],0,8),2) $
                  else $
                     selkey = strtrim( $
                        strmid((*(*(*state).ehdr)[(*state).curexten-1])[(*state).curline],0,8),2)
               endif else begin
                  selkey = ''
               endelse
               if (*state).curexten eq 0 then $
                  tmphdr=(*(*state).hdr) $
               else $
                  tmphdr=(*(*(*state).ehdr)[(*state).curexten-1])
               if selkey eq '' then begin
                  print,'Add ',newkey,'=',newval,'/',comment,' at end of header'
                  sxaddpar,tmphdr,newkey,newval,comment
               endif else begin
                  print,'Add ',newkey,'=',newval,'/',comment,' after ',selkey
                  sxaddpar,tmphdr,newkey,newval,comment,after=selkey
               endelse
               if (*state).curexten eq 0 then begin
                  ptr_free,(*state).hdr
                  (*state).hdr=ptr_new(tmphdr)
               endif else begin
                  ptr_free,(*(*state).ehdr)[(*state).curexten-1]
                  (*(*state).ehdr)[(*state).curexten-1] = ptr_new(tmphdr)
               endelse
               (*state).dirty = 1
               widget_control,(*state).textwin,set_value=tmphdr
            end

            'Add String Keyword': begin
               newkey = qinput(prompt='Name of keyword to add?', $
                               /string,title='Add Keyword')
               newkey = strupcase(strcompress(strtrim(newkey,2),/remove_all))
               newkey = newkey[0]
               newval = qinput(prompt='String value?', $
                               /string,title='Add Keyword')
               newval = newval[0]
               comment = qinput(prompt='Comment?', $
                               /string,title='Add Keyword')
               comment = comment[0]
               if strmid(comment,0,1) ne ' ' then comment=' '+comment
               if (*state).curline ge 0 then begin
                  if (*state).curexten eq 0 then $
                     selkey = strtrim(strmid((*(*state).hdr)[(*state).curline],0,8),2) $
                  else $
                     selkey = strtrim( $
                        strmid((*(*(*state).ehdr)[(*state).curexten-1])[(*state).curline],0,8),2)
               endif else begin
                  selkey = ''
               endelse
               if (*state).curexten eq 0 then $
                  tmphdr=(*(*state).hdr) $
               else $
                  tmphdr=(*(*(*state).ehdr)[(*state).curexten-1])
               if selkey eq '' then begin
                  print,'Add ',newkey,'=',newval,'/',comment,' at end of header'
                  sxaddpar,tmphdr,newkey,newval,comment
               endif else begin
                  print,'Add ',newkey,'=',newval,'/',comment,' after ',selkey
                  sxaddpar,tmphdr,newkey,newval,comment,after=selkey
               endelse
               if (*state).curexten eq 0 then begin
                  ptr_free,(*state).hdr
                  (*state).hdr=ptr_new(tmphdr)
               endif else begin
                  ptr_free,(*(*state).ehdr)[(*state).curexten-1]
                  (*(*state).ehdr)[(*state).curexten-1] = ptr_new(tmphdr)
               endelse
               (*state).dirty = 1
               widget_control,(*state).textwin,set_value=tmphdr
            end

            'Force to float' : begin
               lineno = (*state).curline
               if (*state).curexten eq 0 then $
                  keyword = strtrim(strmid((*(*state).hdr)[lineno],0,8),2) $
               else $
                  keyword = strtrim( $
                     strmid((*(*(*state).ehdr)[(*state).curexten-1])[lineno],0,8),2)
               keyword=strtrim(keyword[0],2)
               if keyword ne '' then begin
                  (*(*state).value) = 0.
                  fitsedit_setval,state,keyword,0.
                  widget_control,(*state).keyid,set_value=keyword
                  widget_control,(*state).valueid,set_value='0.'
                  if (*state).curexten eq 0 then begin
                     widget_control,(*state).textwin,set_value=(*(*state).hdr)
                  endif else begin
                     widget_control,(*state).textwin, $
                        set_value=(*(*(*state).ehdr)[(*state).curexten-1])
                  endelse
               endif
            end

            'Force to int' : begin
               lineno = (*state).curline
               if (*state).curexten eq 0 then $
                  keyword = strtrim(strmid((*(*state).hdr)[lineno],0,8),2) $
               else $
                  keyword = strtrim( $
                     strmid((*(*(*state).ehdr)[(*state).curexten-1])[lineno],0,8),2)
               keyword=strtrim(keyword[0],2)
               if keyword ne '' then begin
                  (*(*state).value) = 0
                  fitsedit_setval,state,keyword,0
                  widget_control,(*state).keyid,set_value=keyword
                  widget_control,(*state).valueid,set_value='0'
                  if (*state).curexten eq 0 then begin
                     widget_control,(*state).textwin,set_value=(*(*state).hdr)
                  endif else begin
                     widget_control,(*state).textwin, $
                        set_value=(*(*(*state).ehdr)[(*state).curexten-1])
                  endelse
               endif
            end

            'Force to string' : begin
               lineno = (*state).curline
               if (*state).curexten eq 0 then $
                  keyword = strtrim(strmid((*(*state).hdr)[lineno],0,8),2) $
               else $
                  keyword = strtrim( $
                     strmid((*(*(*state).ehdr)[(*state).curexten-1])[lineno],0,8),2)
               keyword=strtrim(keyword[0],2)
               if keyword ne '' then begin
                  (*(*state).value) = ''
                  fitsedit_setval,state,keyword,'        '
                  widget_control,(*state).keyid,set_value=keyword
                  widget_control,(*state).valueid,set_value=''
                  if (*state).curexten eq 0 then begin
                     widget_control,(*state).textwin,set_value=(*(*state).hdr)
                  endif else begin
                     widget_control,(*state).textwin, $
                        set_value=(*(*(*state).ehdr)[(*state).curexten-1])
                  endelse
               endif
            end

            'Open' : begin
               cd,current=current
               fn = dialog_pickfile( group=event.top, TITLE='Select file', $
                                     /must_exist, path=current, $
                                     get_path=current)
               fitsedit_load,state,fn
            end

            'Save' : begin
               fitsedit_save,state,'tmp.fits'
               file_move,'tmp.fits',(*state).file,/noexpand_path,/overwrite
            end

            'Save As' : begin
               fn = dialog_pickfile( group=event.top, TITLE='Save File As', $
                                     path=current, $
                                     get_path=current)
               fitsedit_save,state,fn
            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               RETURN
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Copy to All' : begin
         if (*state).valid then begin
            widget_control,(*state).keyid,get_value=keyword
            keyword=strtrim(keyword[0],2)
            ; Prime header
            ; look for keyword, if found, modify
            value = sxpar((*(*state).hdr),keyword,count=count)
            if count ne 0 then $
               sxaddpar,(*(*state).hdr),keyword,(*(*state).value)
            ; Loop over extensions
            ; look for keyword, if found, modify
            for i=0,(*state).nextend-1 do begin
               value = sxpar((*(*(*state).ehdr)[i]),keyword,count=count)
               if count ne 0 then $
                  sxaddpar,(*(*(*state).ehdr)[i]),keyword,(*(*state).value)
            endfor
            ; refresh current header on display
            if (*state).curexten eq 0 then begin
               widget_control,(*state).textwin,set_value=(*(*state).hdr)
            endif else begin
               widget_control,(*state).textwin, $
                  set_value=(*(*(*state).ehdr)[(*state).curexten-1])
            endelse
            (*state).dirty=1
         endif
      end

      'Extension' : begin
         (*state).curexten = event.index
         if (*state).curexten eq 0 then begin
            widget_control,(*state).textwin,set_value=(*(*state).hdr)
         endif else begin
            widget_control,(*state).textwin, $
               set_value=(*(*(*state).ehdr)[(*state).curexten-1])
         endelse
         widget_control,(*state).keyid,get_value=keyword
         keyword=strtrim(keyword[0],2)
         if keyword ne '' then $
            fitsedit_getval,state,keyword,ok
      end

      'File Name' : begin
         widget_control,(*state).fnid,get_value=fn
         fn=fn[0]
         fitsedit_load,state,fn
      end

      'Keyword' : begin
         if (*state).valid then begin
            widget_control,(*state).valueid,get_value=value
            value=value[0]
            widget_control,(*state).keyid,get_value=keyword
            keyword=strmid(strtrim(keyword[0],2),0,8)
            if keyword ne '' and value ne '' and (*state).curline ge 0 then begin
               oldline = (*(*state).hdr)[(*state).curline]
               if strtrim(oldline,2) ne 'END' then begin
                  newline = keyword
                  if strlen(newline) lt 8 then $
                     newline=newline+strmid('        ',0,8-strlen(newline))
                  newline = newline + strmid(oldline,8,999)
                  (*(*state).hdr)[(*state).curline] = newline
                  fitsedit_setval,state,keyword,value
               endif
            endif
            if (*state).curexten eq 0 then begin
               widget_control,(*state).textwin,set_value=(*(*state).hdr)
            endif else begin
               widget_control,(*state).textwin, $
                  set_value=(*(*(*state).ehdr)[(*state).curexten-1])
            endelse
        endif
      end

      'Value' : begin
         if (*state).valid then begin
            widget_control,(*state).valueid,get_value=value
            value=value[0]
            widget_control,(*state).keyid,get_value=keyword
            keyword=strtrim(keyword[0],2)
            if keyword ne '' and value ne '' then $
               fitsedit_setval,state,keyword,value
            if (*state).curexten eq 0 then begin
               widget_control,(*state).textwin,set_value=(*(*state).hdr)
            endif else begin
               widget_control,(*state).textwin, $
                  set_value=(*(*(*state).ehdr)[(*state).curexten-1])
            endelse
         endif
      end

      'Window': begin
         if (*state).valid and event.type eq 3 then begin
            error_status=0
            lineno = event.offset/(*state).linelen
            (*state).curline = lineno
            if (*state).curexten eq 0 then $
               keyword = strtrim(strmid((*(*state).hdr)[lineno],0,8),2) $
            else $
               keyword = strtrim( $
                  strmid((*(*(*state).ehdr)[(*state).curexten-1])[lineno],0,8),2)
            fitsedit_getval,state,keyword,ok
         endif
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro fitsedit

   ; optional
   if xregistered('fitsedit') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. FITSEDIT cannot be started.'
      return
   endif

   ;Define the main base.
   mainbase = widget_base( TITLE='FITSEDIT: FITS Header Editor', $
                           /COLUMN, UVALUE=0, MBAR=bar )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Open',$
                     '0\Save',$
                     '0\Save As',$
                     '2\Exit',$
                     '1\Tools',$
                     '0\Add String Keyword', $
                     '0\Add Float Keyword', $
                     '0\Add Integer Keyword', $
                     '0\Force to float', $
                     '0\Force to int', $
                     '2\Force to string'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase,col=1)

   xsize=80
   ysize=40
   case !version.os_family OF
      'Windows': begin
         font = '*FIXED*12'
         linelen = 82
      end
      else: begin
         font = ''
         linelen = 81
      end
   endcase

   b1 = widget_base(base,col=1)
   b2 = widget_base(b1,row=1)
   fnid = widget_text(b2, value='',/editable,xsize=80, $
                      font=font, uvalue='File Name')
   b2 = widget_base(b1,row=1)
   keyid = widget_text(b2, value='', /editable, xsize=8, $
                       font=font, uvalue='Keyword')
   valueid = widget_text(b2, value='', /editable, xsize=30, $
                       font=font, uvalue='Value')
   extenid = widget_droplist(b2,value=['          '],font=font, uvalue='Extension')
   widget_control,extenid,sensitive=0
   cpallid = widget_button(b2,value='  Copy to All  ',uvalue='Copy to All')
   widget_control,cpallid,sensitive=0

   win1 = widget_text(base,VALUE='',XSIZE=xsize,YSIZE=ysize,/scroll, $
                      font=font,uvalue='Window',/all_events)

   state = ptr_new({ $

      ; Data and information in the widget
      curexten: 0, $             ; Current extension being shown
      curline: -1, $             ; current header line selected
      dirty: 0, $                ; Flag, indicates if header has been touched.
      file: '', $                ; File name of current file (includes path)
      ehdr: ptr_new(), $         ; Extension headers
      hdr: ptr_new(), $          ; Header (prime header for multi-extension files).
      linelen: linelen, $        ; Length of header lines in text window
      nextend: 0, $              ; Number of extensions in file.
      valid: 0, $                ; Header information valid
      value: ptr_new(), $        ; Pointer to keyword value.

      ; Widget ids
      cpallid: cpallid, $        ; Copy to All button
      extenid: extenid, $        ; Extension dropdown list
      fnid: fnid, $              ; Label for showing current file name
      keyid: keyid, $            ; ID of keyword editable label
      valueid: valueid, $        ; ID of value editable label
      textwin: win1, $           ; ID of main text window

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'fitsedit', mainbase, $
             EVENT_HANDLER='fitsedit_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='fitsedit_cleanup'

end

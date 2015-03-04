;+
; NAME:
;  edtcoord
; PURPOSE:
;  Display a celestial coordinate and permit editing the value.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  edtcoord,ra,dec,equinox,dra,ddec,epoch
; INPUTS:
;  name- Formal or proper name for object
;  ra  - R.A. in radians
;  dec - Declination in radians
;  equinox - Equinox of coordinates (decimal year)
; OPTIONAL INPUT PARAMETERS:
;  comment - Comment string (use is dependent on application) default=''
;  dra  - proper motion in radians per year, default=0
;  ddec - proper motion in radians per year, default=0
;  epoch - Epoch of coordinates (decimal year)  default=equinox
; KEYWORD INPUT PARAMETERS:
;  NOPM - Flag, if set suppresses display and editing of the proper motion
;            data fields (dra,ddec,epoch)
; OUTPUTS:
;  All of the input parameters can be modified.
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
;  Written by Marc W. Buie, Lowell Observatory, 2000/09/03
;  2000/09/11, MWB, added name and comment fields
;-
pro edtcoord_cleanup, state

   ; Free up any dynamically allocated space with the state structure
;  ptr_free,(*state).something

   ; Free up the state structure itself.
   ptr_free, state

end

pro edtcoord_check, state

   widget_control, (*state).nameid, get_value=str
   (*state).name = str

   widget_control, (*state).raid, get_value=str
   (*state).ra = raparse(str)

   widget_control, (*state).decid, get_value=str
   (*state).dec = decparse(str)

   widget_control, (*state).equinoxid, get_value=str
   (*state).equinox = float(str)

   widget_control, (*state).commentid, get_value=str
   (*state).comment = str

   if not (*state).nopm then begin
      widget_control, (*state).draid, get_value=str
      (*state).dra = float(str)

      widget_control, (*state).ddecid, get_value=str
      (*state).ddec = float(str)

      widget_control, (*state).epochid, get_value=str
      (*state).epoch = float(str)
   endif

   edtcoord_refresh,state

end

pro edtcoord_refresh, state

   widget_control, (*state).nameid, set_value=(*state).name

   rastr,(*state).ra,1,str
   widget_control, (*state).raid, set_value=str

   decstr,(*state).dec,0,str
   widget_control, (*state).decid, set_value=str

   str=strn((*state).equinox,format='(f10.3)')
   widget_control, (*state).equinoxid, set_value=str

   widget_control, (*state).commentid, set_value=(*state).comment

   if not (*state).nopm then begin
      str=strn((*state).dra,format='(f10.4)')
      widget_control, (*state).draid, set_value=str

      str=strn((*state).ddec,format='(f10.4)')
      widget_control, (*state).ddecid, set_value=str

      str=strn((*state).epoch,format='(f10.3)')
      widget_control, (*state).epochid, set_value=str
   endif

end

pro edtcoord_eve, event

   widget_control, event.top, GET_UVALUE=state

   widget_control, event.id,  GET_UVALUE=event_name

   case event_name of

      'Check': begin
         edtcoord_check,state
      end

      'Comment': begin
         if event.type eq 0 then begin
            if event.ch eq 10b then edtcoord_check,state
         endif
      end

      'dDec': begin
         if event.type eq 0 then begin
            if event.ch eq 10b then edtcoord_check,state
         endif
      end

      'Dec': begin
         if event.type eq 0 then begin
            if event.ch eq 10b then edtcoord_check,state
         endif
      end

      'Done': begin
         edtcoord_check,state
         widget_control,event.top,/DESTROY
      end

      'dRA': begin
         if event.type eq 0 then begin
            if event.ch eq 10b then edtcoord_check,state
         endif
      end

      'Epoch': begin
         if event.type eq 0 then begin
            if event.ch eq 10b then edtcoord_check,state
         endif
      end

      'Equinox': begin
         if event.type eq 0 then begin
            if event.ch eq 10b then edtcoord_check,state
         endif
      end

      'Name': begin
         if event.type eq 0 then begin
            if event.ch eq 10b then edtcoord_check,state
         endif
      end

      'RA': begin
         if event.type eq 0 then begin
            if event.ch eq 10b then edtcoord_check,state
         endif
      end

      'Revert': begin
         (*state).name    = (*state).inname
         (*state).ra      = (*state).inra
         (*state).dec     = (*state).indec
         (*state).equinox = (*state).inequinox
         (*state).comment = (*state).incomment
         if not (*state).nopm then begin
            (*state).dra   = (*state).indra
            (*state).ddec  = (*state).inddec
            (*state).epoch = (*state).inepoch
         endif
         edtcoord_refresh,state
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro edtcoord,name,ra,dec,equinox,comment,dra,ddec,epoch,NOPM=nopm

   if xregistered('zzzz') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. ZZZZ cannot be started.'
      return
   endif

   if badpar(name,7,0,caller='EDTCOORD (name) ') then return
   if badpar(ra,[4,5],0,caller='EDTCOORD (ra) ') then return
   if badpar(dec,[4,5],0,caller='EDTCOORD (dec) ') then return
   if badpar(equinox,[0,4,5],0,caller='EDTCOORD (equinox) ') then return

   if badpar(comment,[0,7],0,caller='EDTCOORD (comment) ',default='') then return
   if badpar(dra,[0,4,5],0,caller='EDTCOORD (dra) ',default=0.0) then return
   if badpar(ddec,[0,4,5],0,caller='EDTCOORD (ddec) ',default=0.0) then return
   if badpar(epoch,[0,4,5],0,caller='EDTCOORD (epoch) ',default=equinox) then return

   if badpar(nopm,[0,1,2,3],0,caller='EDTCOORD (NOPM) ',default=0) then return

   ;Define the main base.
   mainbase = widget_base( TITLE='Coordinate Editor', /COLUMN, UVALUE=0 )

   base = widget_base(mainbase,col=1)

   b2 = widget_base(base,row=1)
   b1 = widget_button( b2, value='Done', uvalue='Done')
   b1 = widget_button( b2, value='Revert', uvalue='Revert')
   b1 = widget_button( b2, value='Check', uvalue='Check')

   widsize=12

   b2 = widget_base(base,row=1,/base_align_center)
   t1 = widget_label( b2, value=' Equinox ',/align_right )
   info=widget_info(t1,/geometry)
   widget_control,t1,set_value='Name ',xsize=info.scr_xsize
   nameid = widget_text( b2, value=name,/editable,/all_events,xsize=widsize, uvalue='Name')

   b2 = widget_base(base,row=1)
   t1 = widget_label( b2, value='RA ',xsize=info.scr_xsize,/align_right )
   rastr,ra,1,str1
   raid = widget_text( b2, value=str1,/editable,/all_events,xsize=widsize, uvalue='RA')

   b2 = widget_base(base,row=1)
   t1 = widget_label( b2, value='Dec ',xsize=info.scr_xsize,/align_right )
   decstr,dec,0,str2
   decid = widget_text( b2, value=str2,/editable,/all_events,xsize=widsize, uvalue='Dec')

   b2 = widget_base(base,row=1)
   t1 = widget_label( b2, value='Equinox ',xsize=info.scr_xsize,/align_right )
   str3=strn(equinox,format='(f10.3)')
   equinoxid = widget_text( b2, value=str3,/editable,/all_events,xsize=widsize, uvalue='Equinox')

   b2 = widget_base(base,row=1)
   t1 = widget_label( b2, value='comment ',xsize=info.scr_xsize,/align_right )
   commentid = widget_text( b2, value=comment,/editable,/all_events,xsize=widsize, uvalue='Comment')

   if not nopm then begin
      b2 = widget_base(base,row=1)
      t1 = widget_label( b2, value='dRA ',xsize=info.scr_xsize,/align_right )
      str=strn(dra,format='(f10.4)')
      draid = widget_text( b2, value=str,/editable,/all_events,xsize=widsize, uvalue='dRA')

      b2 = widget_base(base,row=1)
      t1 = widget_label( b2, value='dDec ',xsize=info.scr_xsize,/align_right )
      str=strn(ddec,format='(f10.4)')
      ddecid = widget_text( b2, value=str,/editable,/all_events,xsize=widsize, uvalue='dDec')

      b2 = widget_base(base,row=1)
      t1 = widget_label( b2, value='Epoch ',xsize=info.scr_xsize,/align_right )
      str=strn(epoch,format='(f10.3)')
      epochid = widget_text( b2, value=str,/editable,/all_events,xsize=widsize, uvalue='Epoch')
   endif else begin
      draid   = 0L
      ddecid  = 0L
      epochid = 0L
   endelse

   state = ptr_new({ $

      name:      name, $
      ra:        ra, $
      dec:       dec, $
      equinox:   equinox, $
      comment:   comment, $
      dra:       dra, $
      ddec:      ddec, $
      epoch:     epoch, $
      incomment: comment, $
      inddec:    ddec, $
      indec:     dec, $
      inepoch:   epoch, $
      inequinox: equinox, $
      inname:    name, $
      inra:      ra, $
      indra:     dra, $
      nopm:      nopm, $

      commentid: commentid, $
      decid:     decid, $
      ddecid:    ddecid, $
      draid:     draid, $
      epochid:   epochid, $
      equinoxid: equinoxid, $
      nameid:    nameid, $
      raid:      raid, $

      mainbase: mainbase $          ; ID of top level base.
      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'edtcoord', mainbase, $
             EVENT_HANDLER='edtcoord_eve',/MODAL, $
             GROUP_LEADER=mainbase

   name    = (*state).name
   ra      = (*state).ra
   dec     = (*state).dec
   equinox = (*state).equinox
   comment = (*state).comment
   dra     = (*state).dra
   ddec    = (*state).ddec
   epoch   = (*state).epoch

   edtcoord_cleanup,state

end

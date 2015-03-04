;+
; NAME:
;   exporter
; PURPOSE:
;   GUI to facilitate exporting a subset of a night's data.
; DESCRIPTION:
; CATEGORY:
;   Utility
; CALLING SEQUENCE:
;   exporter
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
;  Written by Marc W. Buie, Lowell Observatory, 2001/09/26
;  2003/10/01, MWB, converted my Filecopy call to system file_copy routine
;                   converted my Mkdir calls to IDL file_mkdir calls
;  2004/05/06, MWB, now copies standard calib directory if found
;  2004/9/21, MWB, removed obsolete call to Findfile
;-
pro exporter_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).filelist
   ptr_free,(*state).filter
   ptr_free,(*state).imagetype
   ptr_free,(*state).objects
   ptr_free,(*state).objlist

   ; Free up the state structure itself.
   ptr_free, state

end

pro exporter_export, state
   sel = widget_info((*state).objlistid,/list_select)
   if sel[0] lt 0 then return

   marked = bytarr(n_elements((*(*state).filelist)))

   ; scan everything explicitly selected, these are marked for export for sure.
   for i=0,n_elements(sel)-1 do begin
      z = where( (*(*state).objlist)[sel[i]] eq (*(*state).objects) )
      marked[z] = 1B
   endfor

   print,strn(long(total(marked))),' object files selected for export.'

   ; Of those files now marked, construct a list of filters that are included.
   z = where(marked,count)
   fils = (*(*state).filter)[z]
   fils = fils[uniq(fils,sort(fils))]

   ; check the filter selection list, if nothing selected take all.
   sel = widget_info((*state).fillistid,/list_select)
   if sel[0] ge 0 then begin
      fils = fils[sel]
      ; Must go back and unmark those with deselected filters
      filmark = bytarr(n_elements((*(*state).filelist)))
      for i=0,n_elements(fils)-1 do begin
         z = where( (*(*state).filter) eq fils[i] )
         filmark[z] = 1B
      endfor
      marked = marked*filmark
   endif

   print,'Filter list: ',fils

   ; Now look for associated calibration files.  First, grab all bias and dark frames
   z = where( (*(*state).imagetype) eq 'bias', count)
   if count ne 0 then marked[z] = 1B
   z = where( (*(*state).imagetype) eq 'dark', count)
   if count ne 0 then marked[z] = 1B

   ; Now, go through the list of filters and flag all flats for those filters.
   for i=0,n_elements(fils)-1 do begin
      z = where( fils[i] eq (*(*state).filter) and (*(*state).imagetype) eq 'flat',count)
      if count ne 0 then marked[z] = 1B
   endfor

   print,strn(long(total(marked))),' files (including calibrations) selected for export.'

   ; Check to see if output directory exists, if not, make it.
   widget_control,(*state).ddirid,get_value=ddir
   ddir=ddir[0]
   if not exists(ddir) then file_mkdir,ddir

   ; Now, copy the files
   for i=0,n_elements((*(*state).filelist))-1 do begin
      if marked[i] then begin
         src = (*(*state).filelist)[i]
         fdecomp,src,disk,dir,name,qual
         fn = name
         if qual ne '' then fn = fn+'.'+qual
         file_copy,src,ddir+fn,/overwrite
      endif
   endfor

   ; Check to see if there is a calib directory, if so, copy it to the
   ;  destination.
   widget_control,(*state).sdirid,get_value=fn
   fn = fn[0]
   if exists(fn+'calib') then begin
      file_copy,fn+'calib',ddir,/recursive
   endif else begin
      print,'Warning: no calibration directory found.'
   endelse

end

pro exporter_scandir, state, sdir
   print,'scanning new source dir: ',sdir
   list = file_search(addslash(sdir)+'*',count=count)
   file = bytarr(count)
   for i=0,count-1 do $
      file[i] = file_test(list[i],/regular)
   z=where(file eq 1,count)
   if count eq 0 then begin
      print,' no files found in this directory '
      return
   endif else begin
      object    = strarr(count)
      imagetype = strarr(count)
      filter    = strarr(count)
      file = file[z]
      list = list[z]
      for i=0,count-1 do begin
         hdr=headfits(list[i])
         simple = sxpar(hdr,'SIMPLE')
         if simple then begin
            object[i] = strtrim(sxpar(hdr,'OBJECT'),2)
            imagetype[i] = strtrim(sxpar(hdr,'IMAGETYP'),2)
            answer = strtrim(sxpar(hdr,'FILTER*'),2)
            z=where(answer ne 'Open',count)
            if count eq 0 then filter[i]='Open' $
            else if count eq 1 then filter[i]=answer[z[0]] $
            else filter[i] = strjoin(answer[z],'+')
         endif else begin
            file[i] = 0
         endelse
      endfor
      z=where(file eq 1,count)
      print,'Found ',strn(count),' files.'
      if count ne 0 then begin
         list      = list[z]
         object    = object[z]
         imagetype = imagetype[z]
         filter    = filter[z]
         objlist = object[uniq(object,sort(object))]
         displist = objlist

         for  i=0,n_elements(objlist)-1 do begin
            z=where(objlist[i] eq object,count)
            fils = filter[z]
            fils = fils[uniq(fils,sort(fils))]
            displist[i] = displist[i] + ' [' + strjoin(fils,' ',/single) + '] ' + $
                             strn(count) + ' files'
         endfor

         widget_control,(*state).objlistid,set_value=displist
         widget_control,(*state).fillistid,set_value=''
         ptr_free,(*state).objlist
         (*state).objlist = ptr_new(objlist)
         ptr_free,(*state).filelist
         (*state).filelist = ptr_new(list)
         ptr_free,(*state).objects
         (*state).objects = ptr_new(object)
         ptr_free,(*state).imagetype
         (*state).imagetype = ptr_new(imagetype)
         ptr_free,(*state).filter
         (*state).filter = ptr_new(filter)

      endif else begin
         print,'No FITS files found'
      endelse
   endelse
end

pro exporter_eve, event

   widget_control, event.top, GET_UVALUE=state

   widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

;   exit = event_name eq 'THE_MENU'
;   if exit then exit = event.value eq 'Exit'

   case event_name of

;      'THE_MENU': begin
;         case event.value of
;
;;            : begin
;;            end
;
;            'Exit' : begin
;               widget_control, event.top, /DESTROY
;               RETURN
;            end
;
;            else: begin
;               message, 'Unknown menu event:', /INFO
;               help, event, /STRUCTURE
;            end
;
;         endcase
;
;      end ; THE_MENU

      'Browse Destination Dir': begin
         widget_control,(*state).ddirid,get_value=ddir
         fn = dialog_pickfile( group=event.top, TITLE='Select destination directory', $
                                     path=ddir[0],/directory)
         if fn ne '' then begin
            fn=addslash(fn)
            widget_control,(*state).ddirid,set_value=fn
            widget_control,(*state).sdirid,get_value=sdir
            sdir=sdir[0]
            if fn eq sdir then $
               widget_control,(*state).exportid,sensitive=0 $
            else $
               widget_control,(*state).exportid,sensitive=1
         endif
      end

      'Browse Source Dir': begin
         widget_control,(*state).sdirid,get_value=sdir
         fn = dialog_pickfile( group=event.top, TITLE='Select source directory', $
                                     /must_exist, path=sdir[0],/directory)
         if fn ne '' then begin
            fn=addslash(fn)
            widget_control,(*state).sdirid,set_value=fn
            fn=fn[0]
            exporter_scandir,state,fn
            widget_control,(*state).ddirid,get_value=ddir
            ddir=ddir[0]
            if fn eq ddir and ddir ne '' then $
               widget_control,(*state).exportid,sensitive=0 $
            else $
               widget_control,(*state).exportid,sensitive=1
         endif
      end

      'Destination Dir': begin
         widget_control,(*state).ddirid,get_value=ddir
         ddir=ddir[0]
         if ddir ne '' then begin
            ddir=addslash(ddir)
            widget_control,(*state).ddirid,set_value=ddir
            widget_control,(*state).sdirid,get_value=sdir
            sdir=sdir[0]
            if ddir eq sdir and sdir ne '' then $
               widget_control,(*state).exportid,sensitive=0 $
            else $
               widget_control,(*state).exportid,sensitive=1
         endif
      end

      'Exit' : begin
         widget_control, event.top, /DESTROY
         RETURN
      end

      'Export Data' : begin
         exporter_export, state
      end

      'Filter List' : begin
         ; NOP
      end

      'Object List' : begin
         sel = widget_info((*state).objlistid,/list_select)
         if sel[0] ge 0 then begin
            widget_control,(*state).exportid,sensitive=1
            marked = bytarr(n_elements((*(*state).filelist)))

            ; scan everything explicitly selected, these are marked for export for sure.
            for i=0,n_elements(sel)-1 do begin
               z = where( (*(*state).objlist)[sel[i]] eq (*(*state).objects) )
               marked[z] = 1B
            endfor

            ; Of those files now marked, construct a list of filters that are included.
            z = where(marked,count)
            fils = (*(*state).filter)[z]
            fils = fils[uniq(fils,sort(fils))]

            widget_control,(*state).fillistid,set_value=fils
         endif else begin
            widget_control,(*state).exportid,sensitive=0
            widget_control,(*state).fillistid,set_value=''
         endelse
      end

      'Rescan Source': begin
         widget_control,(*state).sdirid,get_value=sdir
         sdir = sdir[0]
         exporter_scandir,state,sdir
      end

      'Source Dir': begin
         widget_control,(*state).sdirid,get_value=sdir
         sdir = sdir[0]
         if sdir ne '' then sdir=addslash(sdir)
         widget_control,(*state).sdirid,set_value=sdir
         exporter_scandir,state,sdir
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro exporter

   ; optional
   if xregistered('exporter') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. EXPORTER cannot be started.'
      return
   endif

   ;Define the main base.
   mainbase = widget_base( TITLE='Data Exporter', $
                           /COLUMN, UVALUE=0) ;, MBAR=bar )

;   menu = CW_PdMenu(bar, /RETURN_NAME, $
;                    ['1\File',$
;                     '0\action 1',$
;                     '0\action 2',$
;                     '2\Exit',$
;                     '1\Tools',$
;                     '0\tool 1', $
;                     '0\tool 2', $
;                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase,col=1)

   b1 = widget_base(base,row=1)
   c1 = widget_button(b1,value='Exit',uvalue='Exit')
   c1 = widget_button(b1,value='Rescan Source',uvalue='Rescan Source')
   export = widget_button(b1,value='Export Data',uvalue='Export Data',sensitive=0)
   b1 = widget_base(base,row=1)
   c1 = widget_label(b1,value='Source Dir')
   sdir = widget_text(b1,value='',uvalue='Source Dir',xsize=50,/edit)
   c2 = widget_button(b1,value='Browse',uvalue='Browse Source Dir')

   b1 = widget_base(base,row=1)
   c1 = widget_label(b1,value='Destination')
   ddir = widget_text(b1,value='',uvalue='Destination Dir',xsize=50,/edit)
   c2 = widget_button(b1,value='Browse',uvalue='Browse Destination Dir')

   b1 = widget_base(base,row=1)
   b2 = widget_base(b1,col=2)
   olist = widget_list( b2, XSIZE=60,YSIZE=10, /MULTIPLE, UVALUE='Object List' )
   flist = widget_list( b2, XSIZE=10,YSIZE=10, /MULTIPLE, UVALUE='Filter List' )

   state = ptr_new({ $

      ; Data and information in the widget
      filelist: ptr_new(), $     ; list of file names
      filter: ptr_new(), $       ; list of filters
      imagetype: ptr_new(), $    ; list of image types
      objects: ptr_new(), $      ; list of objects
      objlist: ptr_new(), $      ; list of unique object names

      ; Widget ids
      ddirid: ddir, $            ; ID of destination directory text widget
      exportid: export, $        ; ID of export button
      fillistid: flist, $        ; ID of filter list window
      sdirid: sdir, $            ; ID of source directory text widget
      objlistid: olist, $        ; ID of object list window

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'exporter', mainbase, $
             EVENT_HANDLER='exporter_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='exporter_cleanup'

end

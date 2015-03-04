;+
; NAME:
;  calibed
; PURPOSE:
;  Widget for editing CCD calibration information.
; DESCRIPTION:
;
; CATEGORY:
;  Widgets
; CALLING SEQUENCE:
;  calibed,calib,dirty,GROUP=group
; INPUTS:
;  calib - Calibration information structure (see LDCALIB.PRO)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  GROUP:  The widget ID of the widget that calls MARKDATA.  When this
;          ID is specified, a death of the caller results in the death of
;          the CALIBED widget application.
;  MODAL:  Flag, if set widget will block caller until closed.
;
; OUTPUTS:
;  dirty - Flag, if true, calibration parameters were modified.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  98/05/18 - Written by Marc W. Buie, Lowell Observatory
;  99/11/15 - MWB, fixed bug for case of empty filter list.
;-

; ------------------------------------------------------------------------------
;       procedure calibed_cleanup
; ------------------------------------------------------------------------------
pro calibed_cleanup,id
end

; ------------------------------------------------------------------------------
;       procedure calibed_chkpath
; ------------------------------------------------------------------------------
; This procedure looks at a file name.  If the leading path of the name
;   is the same as calibpath, it is stripped and replaced with '+'
; ------------------------------------------------------------------------------
pro calibed_chkpath,state,name,relname

   ; Get the leading path to look for.
   if (*state).calibpath ne '' then begin
      leadpath = (*state).calibpath
   endif else begin
      cd,current=leadpath
   endelse

   rel = ''

   ; This first is a pathological case that should never arise but
   ;   it's included for completeness
   if strlen(leadpath) eq 0 then begin
      relname = name

   ; Check a non-empty leading path to see if it starts the filename
   endif else begin

      ; Pull off part of the name equal in length to the leading path
      flead = strmid(name,0,strlen(leadpath))

      ; Windows is not case-sensitive but the names have case.  For windows
      ;  force to lower case to ignore case.
      if !version.os_family eq 'Windows' then begin
         if strlowcase(leadpath) eq strlowcase(flead) then rel='+'
      endif else begin
         if leadpath eq flead then rel='+'
      endelse

      ; if the leading path was found, strip it off and replace with '+'
      if rel eq '+' then begin
         relname = rel+strmid(name,strlen(leadpath),strlen(name))
      endif else begin
         relname = name
      endelse
   endelse
end

; ------------------------------------------------------------------------------
;       procedure calibed_eve
; ------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
; ------------------------------------------------------------------------------
pro calibed_eve, event

   widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   ; Get the state
   widget_control, event.top, GET_UVALUE=state

   case event_name of

      'Bias Name': begin
            widget_control, event.id, get_value=in_name
            in_name=strtrim(in_name[0],2)
            calibchg,*(*state).calib,'bias','',in_name,calibpath=(*state).calibpath
            widget_control, event.id, set_value=(*(*state).calib).bname
            (*state).dirty=1
         end

      'Dark Name': begin
            widget_control, event.id, get_value=in_name
            in_name=strtrim(in_name[0],2)
            calibchg,*(*state).calib,'dark','',in_name,calibpath=(*state).calibpath
            widget_control, event.id, set_value=(*(*state).calib).dname
            (*state).dirty=1
         end

      'Flat Name': begin
            widget_control, event.id, get_value=in_name
            in_name=strtrim(in_name[0],2)
            filt=(*(*state).calib).filter[(*state).fselect]
            calibchg,*(*state).calib,'flat',filt,in_name,calibpath=(*state).calibpath
            widget_control, event.id, $
               set_value=(*(*state).calib).flname[(*state).fselect]
            widget_control, (*state).flatlistid, set_value= $
               (*(*state).calib).filter + $
               replicate(' ',(*(*state).calib).nflats) + $
               (*(*state).calib).flname+ $
               replicate(' ',(*(*state).calib).nflats) + $
               (*(*state).calib).frngname, $
               set_list_select = (*state).fselect
            (*state).dirty=1
         end

      'Fringe Name': begin
            widget_control, event.id, get_value=in_name
            in_name=strtrim(in_name[0],2)
            filt=(*(*state).calib).filter[(*state).fselect]
            calibchg,*(*state).calib,'fringe',filt,in_name,calibpath=(*state).calibpath
            widget_control, event.id, $
               set_value=(*(*state).calib).frngname[(*state).fselect]
            widget_control, (*state).flatlistid, set_value= $
               (*(*state).calib).filter + $
               replicate(' ',(*(*state).calib).nflats) + $
               (*(*state).calib).flname+ $
               replicate(' ',(*(*state).calib).nflats) + $
               (*(*state).calib).frngname, $
               set_list_select = (*state).fselect
            (*state).dirty=1
         end

      'Done': begin
            widget_control, event.top, /DESTROY
            RETURN
         end

      'Flat List': begin
            (*state).fselect = event.index
            widget_control, (*state).flattextid, $
               set_value=(*(*state).calib).flname[(*state).fselect]
            widget_control, (*state).frngtextid, $
               set_value=(*(*state).calib).frngname[(*state).fselect]
            widget_control, (*state).flatpickid, sensitive=1
            widget_control, (*state).flattextid, sensitive=1
            widget_control, (*state).frngpickid, sensitive=1
            widget_control, (*state).frngtextid, sensitive=1
         end

      'Overscan': begin
            widget_control, event.id, get_value=value

            ; Construct current state
            if (*(*state).calib).xl lt 0 or (*(*state).calib).xr lt 0 then $
               overscan0 = '' $
            else $
               overscan0 = string( (*(*state).calib).xl,(*(*state).calib).xr, $
                                  format='(g0.0,":",g0.0)' )

            overscan = strtrim( value[0], 2 )
            if overscan eq '' then begin
               (*(*state).calib).xl = -1
               (*(*state).calib).xr = -1
               return
            endif
            n1 = gettok( overscan, ':' )
            n2 = gettok( overscan, ' ' )
            xl = fix( n1 )
            xr = fix( n2 )

            calval, (*state).isize[0], 0, xl, xr, errflg, $
                    banner='Error. Specified Overscan is inconsistent with ' + $
                           'the established environment.', $
                    isizelab='image frame x-size', $
                    csizelab='', $
                    mlowlab='low limit of overscan region', $
                    mhighlab='high limit of overscan region', $
                    deltamlab='size of overscan region'

            if not errflg then begin
               (*(*state).calib).xl = xl
               (*(*state).calib).xr = xr
            endif

            if (*(*state).calib).xl lt 0 or (*(*state).calib).xr lt 0 then $
               overscan = '' $
            else $
               overscan = string( (*(*state).calib).xl,(*(*state).calib).xr, $
                                  format='(g0.0,":",g0.0)' )

            widget_control, event.id, set_value=overscan
            if overscan ne overscan0 then (*state).dirty=1

         end

      'Pick Bias': begin
            if (*state).calibpath ne '' then begin
               f = dialog_pickfile( group=event.top, title='Select Bias Frame', $
                   path=(*state).calibpath, /must_exist, get_path=path )
            endif else begin
               f = dialog_pickfile( group=event.top, title='Select Bias Frame', $
                   /must_exist, get_path=path )
            endelse
            if f ne '' then begin
               calibed_chkpath,state,f,in_name
               calibchg,*(*state).calib,'bias','',in_name,calibpath=(*state).calibpath
               widget_control, (*state).biasnameid, $
                  set_value=(*(*state).calib).bname
               (*state).dirty=1
            endif
         end

      'Pick Dark': begin
            if (*state).calibpath ne '' then begin
               f = dialog_pickfile( group=event.top, title='Select Dark Frame', $
                   path=(*state).calibpath, /must_exist, get_path=path )
            endif else begin
               f = dialog_pickfile( group=event.top, title='Select Dark Frame', $
                   /must_exist, get_path=path )
            endelse
            if f ne '' then begin
               calibed_chkpath,state,f,in_name
               calibchg,*(*state).calib,'dark','',in_name,calibpath=(*state).calibpath
               widget_control, (*state).darknameid, $
                  set_value=(*(*state).calib).dname
               (*state).dirty=1
            endif
         end

      'Pick Flat': begin
            if (*state).calibpath ne '' then begin
               f = dialog_pickfile( group=event.top, title='Select Flat Frame', $
                   path=(*state).calibpath, /must_exist, get_path=path )
            endif else begin
               f = dialog_pickfile( group=event.top, title='Select Flat Frame', $
                   /must_exist, get_path=path )
            endelse
            if f ne '' then begin
               calibed_chkpath,state,f,in_name
               filt=(*(*state).calib).filter[(*state).fselect]
               calibchg,*(*state).calib,'flat',filt,in_name,calibpath=(*state).calibpath
               widget_control, (*state).flattextid, $
                  set_value=(*(*state).calib).flname[(*state).fselect]
               widget_control, (*state).flatlistid, set_value= $
                  (*(*state).calib).filter + $
                  replicate(' ',(*(*state).calib).nflats) + $
                  (*(*state).calib).flname+ $
                  replicate(' ',(*(*state).calib).nflats) + $
                  (*(*state).calib).frngname, $
                  set_list_select = (*state).fselect
               (*state).dirty=1
            endif
         end

      'Pick Fringe': begin
            if (*state).calibpath ne '' then begin
               f = dialog_pickfile( group=event.top, title='Select Fringe Frame', $
                   path=(*state).calibpath, /must_exist, get_path=path )
            endif else begin
               f = dialog_pickfile( group=event.top, title='Select Fringe Frame', $
                   /must_exist, get_path=path )
            endelse
            if f ne '' then begin
               calibed_chkpath,state,f,in_name
               filt=(*(*state).calib).filter[(*state).fselect]
               calibchg,*(*state).calib,'fringe',filt,in_name,calibpath=(*state).calibpath
               widget_control, (*state).frngtextid, $
                  set_value=(*(*state).calib).frngname[(*state).fselect]
               widget_control, (*state).flatlistid, set_value= $
                  (*(*state).calib).filter + $
                  replicate(' ',(*(*state).calib).nflats) + $
                  (*(*state).calib).flname+ $
                  replicate(' ',(*(*state).calib).nflats) + $
                  (*(*state).calib).frngname, $
                  set_list_select = (*state).fselect
               (*state).dirty=1
            endif
         end

      'Xrange': begin
            widget_control, event.id, get_value=value

            ; Construct current state
            if (*(*state).calib).x1 lt 0 or (*(*state).calib).x2 lt 0 then $
               xrange0 = '*' $
            else $
               xrange0 = string( (*(*state).calib).x1,(*(*state).calib).x2, $
                                 format='(g0.0,":",g0.0)' )

            xrange = strtrim( value[0], 2 )
            if xrange eq '' or xrange eq '*' then begin
               x1 = -1
               x2 = -1
            endif else begin
               n1 = gettok( xrange, ':' )
               n2 = gettok( xrange, ' ' )
               x1 = fix( n1 )
               x2 = fix( n2 )
            endelse

            calval, (*state).isize[0], (*(*state).calib).cxsize, x1, x2, errflg, $
                    banner='Error. Specified X-range is inconsistent with ' + $
                           'the established environment.', $
                    isizelab='image frame x-size', $
                    csizelab='calibration frame x-size', $
                    mlowlab='low limit of overscan region x-range', $
                    mhighlab='high limit of overscan region x-range', $
                    deltamlab='size of overscan region x-range'

            if not errflg then begin
               (*(*state).calib).x1 = x1
               (*(*state).calib).x2 = x2
            endif

            if (*(*state).calib).x1 lt 0 or (*(*state).calib).x2 lt 0 then $
               xrange = '*' $
            else $
               xrange = string( (*(*state).calib).x1,(*(*state).calib).x2, $
                                 format='(g0.0,":",g0.0)' )

            widget_control, event.id, set_value=xrange
            if xrange ne xrange0 then (*state).dirty=1

         end

      'Yrange': begin
            widget_control, event.id, get_value=value

            ; Construct current state
            if (*(*state).calib).y1 lt 0 or (*(*state).calib).y2 lt 0 then $
               yrange0 = '*' $
            else $
               yrange0 = string( (*(*state).calib).y1,(*(*state).calib).y2, $
                                 format='(g0.0,":",g0.0)' )

            yrange = strtrim( value[0], 2 )
            if yrange eq '' or yrange eq '*' then begin
               y1 = -1
               y2 = -1
            endif else begin
               n1 = gettok( yrange, ':' )
               n2 = gettok( yrange, ' ' )
               y1 = fix( n1 )
               y2 = fix( n2 )
            endelse

            calval, (*state).isize[1], (*(*state).calib).cysize, y1, y2, errflg, $
                    banner='Error. Specified Y-range is inconsistent with ' + $
                           'the established environment.', $
                    isizelab='image frame y-size', $
                    csizelab='calibration frame y-size', $
                    mlowlab='low limit of overscan region y-range', $
                    mhighlab='high limit of overscan region y-range', $
                    deltamlab='size of overscan region y-range'

            if not errflg then begin
               (*(*state).calib).y1 = y1
               (*(*state).calib).y2 = y2
            endif

            if (*(*state).calib).y1 lt 0 or (*(*state).calib).y2 lt 0 then $
               yrange = '*' $
            else $
               yrange = string( (*(*state).calib).y1,(*(*state).calib).y2, $
                                 format='(g0.0,":",g0.0)' )

            widget_control, event.id, set_value=yrange
            if yrange ne yrange0 then (*state).dirty=1

         end

      else: begin
            print,'Unknown widget event [',event_name,']'
            help,event,/st
         end

   endcase

end ; end of event handler

; ------------------------------------------------------------------------------
;       procedure calibed
; ------------------------------------------------------------------------------
;  This is the actual routine that creates the widget and registers it with the
;  Xmanager.
; ------------------------------------------------------------------------------
pro calibed, calib, dirty, GROUP = group, CALIBPATH=in_calibpath, $
      ISIZE=isize,MODAL=modal

   if (!d.flags and 256) ne 256 then message, $
     'ERROR - Current graphics device ' + !d.name + ' does not support windows'

   if xregistered('calibed') then return

   if badpar(calib,8,1,caller='CALIBED: (calib) ') then return
   if badpar(isize,[0,2,3],1,caller='CALIBED: (ISIZE) ', $
             npts=inpts,default=[0,0]) then return
   if badpar(modal,[0,1,2,3],0,caller='CALIBED: (MODAL) ', $
             default=0) then return

   if inpts ne 2 then begin
      print,'CALIBED: ISIZE must contain two values (x,y size)'
      return
   endif

   if badpar(in_calibpath,[0,7],0,caller='CALIBED: (CALIBPATH) ', $
                                  default='') then return
   if in_calibpath eq '' then calibpath = '' $
   else calibpath = addslash(in_calibpath)

   mainbase = widget_base( title='Calibration Setup Editor', $
                           /column, uvalue=0 )

   base = widget_base(mainbase,/column)
   b2   = widget_button( base, value='Done', uvalue='Done' )

   ;Overscan region base.
   overbase = widget_base( base, /row, /frame )
   w1 = widget_label( overbase, value='BIAS/OVERSCAN REGION:' )
   overscan = string( calib.xl, ':', calib.xr, format='(G0.0,A,G0.0)' )
   w1 = widget_text( overbase, value=overscan, /editable, $
                      XSIZE=40, uvalue='Overscan' )

   ;Cropping region base.
   cropbase = widget_base( base, /row, /frame )
   w1 = widget_label(cropbase,value='CROPPING REGION:  x-range' )
   xrange = string( calib.x1,calib.x2, format='(G0.0,":",G0.0)' )
   w1 = widget_text (cropbase,value=xrange,/editable,xsize=9,uvalue='Xrange')
   w1 = widget_label(cropbase,value='     y-range' )
   yrange = string( calib.y1,calib.y2, format='(G0.0,":",G0.0)' )
   w1 = widget_text (cropbase,value=yrange,/editable,xsize=9,uvalue='Yrange')

   ;Biasframe base.
   biasbase = widget_base( base, /row, /frame)
   w1 = widget_label( biasbase, value='BIAS FRAME:')
   w1 = widget_button( biasbase, value='Select File', uvalue='Pick Bias' )
   biasnameid = widget_text( biasbase, value=calib.bname, /edit, xsize=40, uvalue='Bias Name' )

   ;Darkframe base.
   darkbase = widget_base( base, /row, /frame)
   w1 = widget_label( darkbase, value='DARK FRAME:' )
   w1 = widget_button( darkbase, value='Select File', uvalue='Pick Dark' )
   darknameid = widget_text( darkbase, value=calib.dname, /edit, xsize=40,uvalue='Dark Name' )

   ;Flat frames base.
   flatbase = widget_base( base, /column )
   w1 = widget_label( flatbase, value='LIST OF FLATS (filtercode filename)')
   flatlistid = widget_list( flatbase, ysize=7, uvalue='Flat List' )
   if calib.nfilters ne 0 then $
      widget_control, flatlistid, set_value= $
         calib.filter+replicate(' ',calib.nfilters)+calib.flname+ $
            replicate(' ',calib.nfilters)+calib.frngname

   w1 = widget_label( flatbase, value='Change Filename of Selected Flat:' )
   b1 = widget_base( flatbase, /row )
   flatpickid = widget_button( b1, value='Select File', uvalue='Pick Flat' )
   flattextid = widget_text( b1, value='', /editable, xsize=40, uvalue='Flat Name' )

   w1 = widget_label( flatbase, value='Change Fringe Correction Filename for Selected Flat:' )
   b1 = widget_base( flatbase, /row )
   frngpickid = widget_button( b1, value='Select File', uvalue='Pick Fringe' )
   frngtextid = widget_text( b1, value='', /editable, xsize=40, uvalue='Fringe Name' )

   widget_control, flatpickid, sensitive=0
   widget_control, flattextid, sensitive=0
   widget_control, frngpickid, sensitive=0
   widget_control, frngtextid, sensitive=0

   state = ptr_new({ $
      calibpath: calibpath, $ ; Path to calibration directory
      dirty: 0B, $      ; Flag which indicates if parameters have been modified.
      fselect: -1L, $   ; Index of flat currently selected.
      isize: isize, $   ; [x,y] size of data image

      biasnameid: biasnameid, $
      darknameid: darknameid, $
      flatlistid: flatlistid, $
      flatpickid: flatpickid, $
      flattextid: flattextid, $
      frngpickid: frngpickid, $
      frngtextid: frngtextid, $

      calib: ptr_new(calib,/no_copy) $
      })

   ;Stash the state structure pointer.
   widget_control,mainbase,set_uvalue=state

   ;Realize the widget
   widget_control,mainbase,/realize

   xmanager,'calibed',mainbase,event_handler='calibed_eve', $
      group_leader=group,MODAL=modal,cleanup='calibed_cleanup'

   calib = temporary( *(*state).calib )
   dirty = (*state).dirty
   ptr_free, (*state).calib
   ptr_free, state

end

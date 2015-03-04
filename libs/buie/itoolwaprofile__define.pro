;+
; CLASS_NAME:
;    itoolwaprofile
;
; PURPOSE (one line):
;    To plot profiles of an extracted array of data.
;
; CATEGORY:
;    Widgets
;
; SUPERCLASSES:
;
; SUBCLASSES:
;
; CREATION:
;    See itoolwaprofile::init
;
; METHODS:
;    itoolwaprofile::cleanup
;    itoolwaprofile::process
;    itoolwaprofile::refresh
;    itoolwaprofile::getproperty
;    itoolwaprofile::realize
;    itoolwaprofile::init
;
; MODIFICATION HISTORY:
;    2004/04, Written by Doug Loucks, Consultant for Lowell Observatory.
;    (See cw_pfile.pro)
;    Removed all remnants of compound-widget code; replaced with
; stand-alone code that is compatible with the new object-oriented version
; of itool.
;    Modified the usage of the state-structure variable. A pointer to
; the state structure is stored in the object instance of this tool and
; this tool's object reference is stored in its top-level base.
;    Modified the incoming argument. Now, it is the object reference of the
; host instance of itool, which is stored in this tool's object instance.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaprofile::cleanup
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    obj_destroy, oref
;
; INPUTS:
;    oref : An itoolwaprofile object reference.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaprofile::process
;
; PURPOSE:
;   To perform all processing for the draw widget and the
; hardcopy (Post Script) device.
;
; CALLING SEQUENCE:
;    oref->process
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    HARDCOPY : Set this keyword to print a copy of the graphics display to the
;               default printer device.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaprofile::refresh
;
; PURPOSE:
;   To refresh this widget.
;
; CALLING SEQUENCE:
;    oref->refresh, value
;
; INPUTS:
;    value : A structure defined as {image:array, xset:xs, yset:ys}
;            where "array" is an array of data, "xs" and "ys" are the
;            coordinates of the lower-left corner of "array" in a larger
;            array from which "array" was extracted.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    TITLE : Set this keyword to a string containing the title to be displayed
;            on the title bar of the main widget.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaprofile::getproperty
;
; PURPOSE:
;   To retrieves "properties" defined for the itoolwaprofile object
; class. Properties are specified as keyword arguments.
;
; CALLING SEQUENCE:
;    oref->getproperty
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    PSTATE : Set this keyword to a named variable into which will be
;             placed a pointer to the state structure.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaprofile::realize
;
; PURPOSE:
;   To realize a new, managed instance of the itoolwaprofile object class.
;
; CALLING SEQUENCE:
;    oref->realize
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    NO_BLOCK : Set this keyword to cause the widget application to run
;               in non-blocked mode.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itoolwaprofile::init
;
; PURPOSE:
;   To initialize a new instance of the itoolwaprofile object class.
;
; CALLING SEQUENCE:
;    oref = obj_new('itoolwaprofile', oitool)
;
; INPUTS:
;    oitool : An object reference of a host instance of the object class
;             "itool."
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    GROUP_LEADER = The group leader for this tool.
;    HCTITLE      = Title of Hard Copy.
;    PLATESCALE   = Plate Scale in arcseconds per pixel (used at display time).
;                   If supplied and non-zero, display the bottom axis in
;                   arcseconds and the top axis in pixels, else display the
;                   bottom axis in pixels.
;    TITLE        = The string to be displayed on the title bar. This keyword
;                   may be specified on the call to the "refresh" method, as
;                   well, to update the title dynamically.
;
; OUTPUTS:
;    oref = The object reference of the new instance of the itoolwaprofile
;           object class.
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    After creating a new instance of this object-oriented widget application,
; it must be realized and then be given a "value" to display.
;
; EXAMPLE:
;    oref = obj_new('itoolwaprofile', oitool, GROUP_LEADER=group_leader)
;    oref->realize
;    oref->refresh, {image:array, xset:xs, yset:ys}, TITLE=title
;
; MODIFICATION HISTORY:
;
;
; -----------------------------------------------------------------------------
;-


; ------------- Method Routines -----------------------------------------------


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile::cleanup
; -----------------------------------------------------------------------------
pro itoolwaprofile::cleanup
   compile_opt hidden

   ptr_free, self.pstate, self.pimage, self.pscat, self.psmoo
end



; -----------------------------------------------------------------------------
; Procedure itoolwaprofile::process
;   This procedure performs all processing for the draw widget and the
; hardcopy (Post Script) device.
; -----------------------------------------------------------------------------
pro itoolwaprofile::process, HARDCOPY=hardcopy
   compile_opt hidden

   ; Save plotting structure system variable.
   pmult = !p.multi
   !p.multi = 0

   if (*self.pstate).shadesurf then bpp=8 else bpp=4

   if keyword_set(hardcopy) then begin
      ; Direct graphics to the Post Script device.
      dname = !d.name
      set_plot, 'ps'

      device, FILENAME='plot.ps', /PORTRAIT, XSIZE=15.0, YSIZE=15.0,$
         XOFFSET=2.5, YOFFSET=7.5, /HELVETICA, BITS_PER_PIXEL=bpp

      color1 = 0
      color2 = 0
   endif else begin
      ; Direct graphics to the draw widget.
      windo = !d.window
      widget_control, (*self.pstate).dwinid, GET_VALUE=dwin
      wset, dwin
      color1 = fix(!d.n_colors * 0.9)
      color2 = fix(!d.n_colors * 0.6)
   endelse

   position = string('Centroid: xpos= ', (*self.pstate).xpos, '   ypos= ',$
      (*self.pstate).ypos, FORMAT='(A,F8.3,A,F8.3)')

   widget_control, (*self.pstate).posid, SET_VALUE=position

   ; Scatter Profile section.
   ;  The plot arrays and variables are computed once, for each incoming image
   ;array.
   ;  By incorporating these items into a structure, it is easy to determine
   ;if the computational steps may be skipped: If the value is a structure,
   ;no action is required.
   ;  Setting this value to a scalar zero will force the computational
   ;steps to be performed and the new results to be stored.

   if ((*self.pstate).profile and (*self.pstate).scatter) then begin
      scatstat = size(*self.pscat)

      if scatstat[2] ne 8L then begin
         ; Need to perform computational steps.
         radp, *self.pimage, (*self.pstate).xcen,$
            (*self.pstate).ycen, r, i, fwhmp,$
            coeffs, /CONLY

         ir = findgen(151) / 150.0 * max(r)
         rcgauss_funct, ir, coeffs, ifit
         rmin = min(r, max=rmax)
         irmin = min(ir, max=irmax)
         xmin = 0
         xmax = max([rmax, irmax])

         (*self.pscat) = {r:r, i:i, ir:ir, ifit:ifit, fwhmp:fwhmp, xmin:xmin,$
            xmax:xmax}

         ; Save the results.
      endif

      case self.pscale of
         0.0 : begin
            plot, (*self.pscat).r, (*self.pscat).i, PSYM=4,$
               XRANGE=[(*self.pscat).xmin, (*self.pscat).xmax],$
               XTITLE='radius (pixels)', YTITLE='counts'

            oplot, (*self.pscat).ir, (*self.pscat).ifit, COLOR=color1,$
               LINESTYLE=2
         end
         else  : begin
            ra = (*self.pscat).r * self.pscale
            plot, ra, (*self.pscat).i, PSYM=4, XSTYLE=11, YSTYLE=3,$
               YMARGIN=[4,3],$
               XTITLE='radius (arcseconds)', YTITLE='counts',$
               XRANGE=[(*self.pscat).xmin * self.pscale,$
               (*self.pscat).xmax * self.pscale]

               axis, XAXIS=1, XSTYLE=3, XRANGE=[(*self.pscat).xmin,$
               (*self.pscat).xmax],$
               XTITLE='radius (pixels)'

            oplot, (*self.pscat).ir * self.pscale, (*self.pscat).ifit,$
               COLOR=color1, LINESTYLE=2
         end
      endcase

      xpos = 0.85
      ypos = 0.85
      dy   = 0.03

      xyouts, xpos, ypos, 'FWHM(pixels): ' + string((*self.pscat).fwhmp,$
         FORMAT='(F7.2)'), /NORMAL, ALIGN=1.0

      if self.pscale ne 0.0 then begin
         ypos = ypos - dy
         xyouts, xpos, ypos, 'FWHM(arcsec): ' +$

         string((*self.pscat).fwhmp * self.pscale, FORMAT='(F7.2)'),$
            /NORMAL, ALIGN=1.0
      endif

      if keyword_set(hardcopy) then begin
         if self.pscale ne 0.0 then begin
            ypos = ypos - dy
            xyouts, xpos, ypos, 'Plate Scale:  ' +$
            string(self.pscale, FORMAT='(F7.3)'), /NORMAL, ALIGN=1.0
         endif

         ypos = ypos - dy

         xyouts, xpos, ypos, 'Centroid-x:  ' + string((*self.pstate).xpos,$
            FORMAT='(F7.3)'), /NORMAL, ALIGN=1.0

         ypos = ypos - dy

         xyouts, xpos, ypos, 'Centroid-y:  ' + string((*self.pstate).ypos,$
            FORMAT='(F7.3)'), /NORMAL, ALIGN=1.0
      endif
   endif

   ; Smooth Profile section.
   ;  The plot arrays and variables are computed once, for each incoming image
   ;array, and stored in a heap variable (self.psmoo).
   ;  By incorporating these items into a structure, it is easy to determine
   ;if the computational steps may be skipped: If the value is a structure,
   ;no action is required.
   ;  Setting this value to a scalar zero will force the computational
   ;steps to be performed and the new results to be stored.

   if ((*self.pstate).profile and (*self.pstate).smooth) then begin
      smoostat = size(*self.psmoo)

      if smoostat[2] ne 8 then begin
         ; Need to perform computational steps.
         dr = 0.5
         imsize = (*self.pstate).imsize
         xcen = (*self.pstate).xcen
         ycen = (*self.pstate).ycen
         rmax = min([ xcen, ycen, imsize-xcen, imsize-ycen ])
         ringprof, *self.pimage, (*self.pstate).xcen, (*self.pstate).ycen,$
         rmax, dr, 0.0, rout, iout
         xmin = 0
         xmax = max(rout)
         *self.psmoo = {rout:rout, iout:iout, xmin:xmin, xmax:xmax}

         ; Save the results.
      endif

      case self.pscale of
         0.0 : begin
            if (*self.pstate).scatter then begin
               oplot, (*self.psmoo).rout, (*self.psmoo).iout, COLOR=color2
            endif else begin
               plot, (*self.psmoo).rout, (*self.psmoo).iout,$
                  XRANGE=[(*self.psmoo).xmin, (*self.psmoo).xmax],$
                  XTITLE='radius (pixels)', YTITLE='counts'

               oplot, (*self.psmoo).rout, (*self.psmoo).iout, PSYM=5
            endelse
         end
         else  : begin
            ra = (*self.psmoo).rout * self.pscale

            if (*self.pstate).scatter then begin
               oplot, ra, (*self.psmoo).iout, COLOR=color2
            endif else begin
               plot, ra, (*self.psmoo).iout, XSTYLE=11, YSTYLE=3,$
                  YMARGIN=[4,3],$
                  XTITLE='radius (arcseconds)', YTITLE='counts',$
                  XRANGE=[(*self.psmoo).xmin*self.pscale,$
                  (*self.psmoo).xmax*self.pscale]

                  axis, XAXIS=1, XSTYLE=3,$
                     XRANGE=[(*self.psmoo).xmin, (*self.psmoo).xmax],$
                     XTITLE='radius (pixels)'

               oplot, ra, (*self.psmoo).iout, PSYM=5
            endelse
         end
      endcase

   endif

   ; Histogram section.
   if (*self.pstate).histogram then begin
      stats, *self.pimage, /ROBO
   endif

   ; Contour plot section.
   if (*self.pstate).contour then begin
      x = (*self.pstate).xset + indgen((*self.pstate).imsize)
      y = (*self.pstate).yset + indgen((*self.pstate).imsize)

      if !order eq 1 then begin
         yr=maxmin(y)
      endif else begin
         yr=minmax(y)
      endelse

      contour, *self.pimage, x, y, YRANGE=yr, XTITLE='X-pos (pixels)',$
         YTITLE='Y-pos (pixels)'
   endif

   ; Shade Surface section.
   if (*self.pstate).shadesurf then begin
      x = (*self.pstate).xset + indgen((*self.pstate).imsize)
      y = (*self.pstate).yset + indgen((*self.pstate).imsize)

      if !order eq 1 then begin
         yr=maxmin(y)
      endif else begin
         yr=minmax(y)
      endelse

      shade_surf, *self.pimage, x, y, YRANGE=yr, XTITLE='X-pos (pixels)',$
         YTITLE='Y-pos (pixels)', ZTITLE='Counts'
   endif

   if keyword_set(hardcopy) then begin
      ; Add some text to the hard copy and close the Post Script device.
      xyouts, 0.6, 1.02, (*self.pstate).hctitle, /NORMAL, ALIGN=0.5
      xyouts, 0.6, -0.07, systime(), /NORMAL
      device, /CLOSE
      set_plot, dname

      if !version.os_family eq 'unix' then begin
         spawn, 'lpr -r plot.ps'
      endif else if !version.os_family eq 'Windows' then begin
         spawn, 'copy plot.ps lpt1:'
         spawn, 'del plot.ps'
      endif
   endif else begin
      ; Reset the draw window.
      wset, windo
   endelse

   !p.multi = pmult
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile::refresh
; The refresh method.
; -----------------------------------------------------------------------------
pro itoolwaprofile::refresh, value, TITLE=title
   compile_opt hidden

   if (*self.pstate).lock then begin
      ; The value (display) is frozen. Ignore the request.
      return
   endif

   ; Get input image information.
   stat = size(value.image)
   xsize = stat[1]
   ysize = stat[2]

   if xsize eq ysize then begin
      imsize = xsize

      ; Find image maximum and then compute the centroid.
      s = where(value.image eq max(value.image), count)
      s = s[ 0 ]
      xmax = s mod imsize
      ymax = s / imsize
      centrod, value.image, xmax, ymax, 5.0, 0.0, 0.0, 0.0, xcen, ycen, counts

      (*self.pstate).imsize = imsize
      (*self.pstate).xcen = xcen
      (*self.pstate).ycen = ycen
      (*self.pstate).xpos = value.xset + xcen
      (*self.pstate).ypos = value.yset + ycen
      (*self.pstate).xset = value.xset
      (*self.pstate).yset = value.yset

      (*self.pimage) = value.image

      ; Set the storage area for the computed smooth profile parameters to zero.
      ; This forces computation when smooth profile is selected.
      *self.pscat = 0
      *self.psmoo = 0

      ; Do the processing.
      self->process

      if keyword_set(title) then begin
         widget_control, self.tlb, TLB_SET_TITLE=title
      endif
   endif else begin
      print, 'Profile array must be square.' + string(7B)
   endelse
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile::getproperty
; -----------------------------------------------------------------------------
pro itoolwaprofile::getproperty, PSTATE=pstate
   compile_opt hidden

   if arg_present(pstate) then pstate = self.pstate
end



; -----------------------------------------------------------------------------
; Procedure itoolwaprofile::realize
; -----------------------------------------------------------------------------
pro itoolwaprofile::realize, NO_BLOCK=no_block
   compile_opt hidden

   widget_control, self.tlb, /REALIZE

   xmanager, 'itoolwaprofile', self.tlb, NO_BLOCK=no_block,$
      CLEANUP='itoolwaprofile_cleanup'
end




; -----------------------------------------------------------------------------
; Procedure itoolwaprofile::init
; -----------------------------------------------------------------------------
function itoolwaprofile::init, oitool, TITLE=title, PLATESCALE=pscale,$
   GROUP_LEADER=group_leader, HCTITLE=hctitle, HISTOGRAM=histogram

   compile_opt hidden

   ; Check pscale keyword.
   if not keyword_set(pscale) then pscale=0.0

   ; Check title keyword.
   if not keyword_set(hctitle) then hctitle=''

   ; Define the state structure.
   state = {$
      contour:0B,$
      dwinid:0L,$
      hctitle:hctitle,$
      histogram:0B,$
      imsize:0,$
      lock:0B,$
      posid:0L,$
      profile:1B,$
      profilemenuid:0L,$
      scatter:1B,$
      shadesurf:0B,$
      smooth:0B,$
      xcen:0.0, ycen:0.0,$
      xpos:0.0, ypos:0.0,$
      xset:0, yset:0}

   if keyword_set(histogram) then begin
      state.histogram = 1B
      state.profile = 0B
   endif

   ; Define the main base.
   tlb = widget_base(TITLE=title, COLUMN=1, GROUP_LEADER=group_leader,$
      MBAR=mbar)

   ; File Menu.
   filemenuid = widget_button(mbar, VALUE='File', /MENU)

   ; Hard Copy button.
   dummy = widget_button(filemenuid, VALUE='Print Hard Copy',$
      EVENT_PRO='itoolwaprofile_hdcpy_event')

   ; Save to JPEG button.
   dummy = widget_button(filemenuid, VALUE='Save to JPEG',$
      EVENT_PRO='itoolwaprofile_jpeg_event')

   dummy = widget_button(filemenuid, VALUE='Exit',$
      EVENT_PRO='itoolwaprofile_exit_event')



   ;   The Display, Plot, and Profile menus contain checked buttons that
   ; operate as radio buttons; only one may be checked at a time.
   ;   The user value of each menu-bar button is used to store
   ; the widget id of the currently-checked button under each menu.
   ; The menu-bar buttons are the parents of the buttons under each menu.
   ; The event handler for a button under a menu retrieves, from
   ; the respective parent, the stored widget id of the currently-selected
   ; button. With this widget id and the widget id of the event (event.id),
   ; it is easy to uncheck the currently-checked button and check the
   ; button that was pressed. The event handler stores the widget id of
   ; the newly-checked button into the user value of the respective
   ; parent, so it is ready for the next event.

   ; Display Menu.
   displaymenuid = widget_button(mbar, VALUE='Display', /MENU)

   dummy = widget_button(displaymenuid, VALUE='Unlocked',$
      /CHECKED_MENU, EVENT_PRO='itoolwaprofile_lock_event', uname='unlocked')

   ; The 'Unlocked' button is the default setting for the 'Display' menu.
   widget_control, dummy, /SET_BUTTON

   ; Store the widget id of the 'Unlocked' button into the user value of
   ; the 'Display' button (parent).
   widget_control, displaymenuid, SET_UVALUE=dummy

   dummy = widget_button(displaymenuid, VALUE='Locked', /CHECKED_MENU,$
      EVENT_PRO='itoolwaprofile_lock_event', uname='locked')


   ; Plot Menu.
   plotmenuid = widget_button(mbar, VALUE='Plot', /MENU)

   profileid = widget_button(plotmenuid, VALUE='Profile',$
      /CHECKED_MENU, EVENT_PRO='itoolwaprofile_scatsav_event', UNAME='profile')

   histogramid = widget_button(plotmenuid, VALUE='Histogram', /CHECKED_MENU,$
      EVENT_PRO='itoolwaprofile_scatsav_event', UNAME='histogram')

   widget_control, dummy, SET_BUTTON=(state.histogram eq 1B)

   dummy = widget_button(plotmenuid, VALUE='Contour', /CHECKED_MENU,$
      EVENT_PRO='itoolwaprofile_scatsav_event', UNAME='contour')

   dummy = widget_button(plotmenuid, VALUE='Shade Surface', /CHECKED_MENU,$
      EVENT_PRO='itoolwaprofile_scatsav_event', UNAME='shadesurf')


   ; The 'Profile' button is the default setting for the 'Plot' menu, unless
   ; the HISTOGRAM keyword is set, in which case the 'Histogram' button
   ; will be the default setting.

   if state.profile eq 1B then begin
      widget_control, profileid, /SET_BUTTON

      ; Store the widget id of the child 'Profile' button into the user value
      ; of the 'Plot' button (its parent).
      widget_control, plotmenuid, SET_UVALUE=profileid
   endif else begin
      widget_control, histogramid, /SET_BUTTON

      ; Store the widget id of the child 'Histogram' button into the user value
      ; of the 'Plot' button (its parent).
      widget_control, plotmenuid, SET_UVALUE=histogramid
   endelse

   ; Profile Menu.
   profilemenuid = widget_button(mbar, VALUE='Profile', /MENU)
   state.profilemenuid = profilemenuid

   dummy = widget_button(profilemenuid, VALUE='Scatter',$
      EVENT_PRO='itoolwaprofile_smoo_event', /CHECKED_MENU,$
      UNAME='scatter')

   ; The 'Scatter' button is the default setting for the 'Profile' menu.
   widget_control, dummy, /SET_BUTTON

   ; Store the widget id of the 'Scatter' button into the user value of
   ; the 'Profile' button (parent).
   widget_control, profilemenuid, SET_UVALUE=dummy

   dummy = widget_button(profilemenuid, VALUE='Smooth',$
      EVENT_PRO='itoolwaprofile_smoo_event', /CHECKED_MENU,$
      UNAME='smooth')

   dummy = widget_button(profilemenuid, VALUE='Scatter+Smooth',$
      EVENT_PRO='itoolwaprofile_smoo_event', /CHECKED_MENU,$
      UNAME='scatter+smooth')

   widget_control, profilemenuid, SENSITIVE=(state.profile eq 1B)


   ; Define the position label widget.
   position = string('Centroid: xpos= ', state.xpos, '   ypos= ',$
           state.ypos, FORMAT='(A,F8.3,A,F8.3)')

   state.posid = widget_label(tlb, /FRAME, VALUE=position)

   ; Define the draw window.
   state.dwinid  = widget_draw(tlb, XSIZE=400, YSIZE=400)

   ; Initialize encapsulated data.
   self.tlb = tlb
   self.pscale = pscale
   self.pstate = ptr_new(state, /NO_COPY)
   self.oitool = oitool
   self.pimage = ptr_new(/ALLOCATE_HEAP)
   self.pscat = ptr_new(/ALLOCATE_HEAP)
   self.psmoo = ptr_new(/ALLOCATE_HEAP)

   widget_control, tlb, SET_UVALUE=self

   ; Return successful initialization.
   return, 1
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile_cleanup
; -----------------------------------------------------------------------------
pro itoolwaprofile_cleanup, tlb
   compile_opt hidden

   widget_control, tlb, GET_UVALUE=oitoolwaprofile
   obj_destroy, oitoolwaprofile
end



; ------------- Event Handlers ------------------------------------------------

; -----------------------------------------------------------------------------
; Procedure itoolwaprofile_exit_event
; -----------------------------------------------------------------------------
pro itoolwaprofile_exit_event, event
   compile_opt hidden

   widget_control, event.top, /DESTROY
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile_hdcpy_event
; -----------------------------------------------------------------------------
pro itoolwaprofile_hdcpy_event, event
   compile_opt hidden

   widget_control, /HOURGLASS
   widget_control, event.top, GET_UVALUE=oitoolwaprofile
   ; Do the processing with the hardcopy flag set.
   oitoolwaprofile->process, /HARDCOPY
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile_jpeg_event
; Capture the plot and save it to a JPEG file.
; -----------------------------------------------------------------------------
pro itoolwaprofile_jpeg_event, event
   compile_opt hidden

   ; Get the name of the visual class.
   device, get_visual_name=vname

   ; Use the visual-class name to set the value that will be passed on
   ; the TRUE keyword to the WRITE_JPEG procedure.

   case vname of
      'DirectColor' : begin
         true = 1
      end

      'TrueColor' : begin
         true = 1
      end

      'PseudoColor' : begin
         true = 0
      end

      'GrayScale' : begin
         true = 0
      end

      else : begin
         msg = 'Visual class ' + vname + ' not supported.'

         con = qannounc(msg, TITLE='Error', TRUELABEL='Dismiss',$
            FALSELABEL='', XSIZE=size(msg), GROUP_LEADER=event.top)

         return
      end
   endcase

   r = dialog_pickfile(DIALOG_PARENT=event.top, FILE='idl.jpg', /WRITE)

   ; User canceled, if return string is null.
   if r[0] eq '' then return

   ; Check for existence of the file.
   if file_test(r[0]) then begin
      msg = 'File ' + r[0] + ' exists. Replace?'

      con = qannounc(msg, TITLE='Confirmation', TRUELABEL='Yes, continue',$
         FALSELABEL='No, cancel', XSIZE=strlen(msg),$
         GROUP_LEADER=event.top)

      if not con then return
   endif

   ; Get the object reference.
   widget_control, event.top, GET_UVALUE=oitoolwaprofile

   ; Get a pointer to the state structure.
   oitoolwaprofile->getproperty, PSTATE=pstate

   ; Get the window number that was assigned to the draw widget.
   widget_control, (*pstate).dwinid, GET_VALUE=winnum

   ; Save the current window-number setting.
   winnum_save = !d.window

   ; Capture the window data and save to a JPEG file.
   wset, winnum
   snap = tvrd(TRUE=true)
   write_jpeg, r[0], snap, ORDER=!order, QUALITY=100, TRUE=true
   wset, winnum_save
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile_lock_event
; -----------------------------------------------------------------------------
pro itoolwaprofile_lock_event, event
   compile_opt hidden

   ; No need to do anything, if the selection is the same as the currently-
   ; selected button.
   parent = widget_info(event.id, /PARENT)
   widget_control, parent, GET_UVALUE=displayselectid
   if event.id eq displayselectid then return

   widget_control, event.top, GET_UVALUE=oitoolwaprofile
   oitoolwaprofile->getproperty, PSTATE=pstate

   ; Get the name of the event.
   uname = widget_info(event.id, /UNAME)

   case uname of
      'unlocked' : begin
         (*pstate).lock = 0
      end

      'locked' : begin
         (*pstate).lock = 1
      end
   endcase

   ; Check the button that generated this event.
   widget_control, event.id, /SET_BUTTON

   ; Clear the previously-checked button.
   widget_control, displayselectid, SET_BUTTON=0

   ; Remember the widget id of this event.
   widget_control, parent, SET_UVALUE=event.id
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile_scatsav_event
; -----------------------------------------------------------------------------
pro itoolwaprofile_scatsav_event, event
   compile_opt hidden

   ; Don't do anything, if the request is the for the same type of plot
   ; that is currently on display.
   parent = widget_info(event.id, /PARENT)
   widget_control, parent, GET_UVALUE=plotselectid
   if event.id eq plotselectid then return

   widget_control, /HOURGLASS

   widget_control, event.top, GET_UVALUE=oitoolwaprofile
   oitoolwaprofile->getproperty, PSTATE=pstate

   ; Get the name of the event.
   uname = widget_info(event.id, /UNAME)

   case uname of
      'profile' : begin
         (*pstate).profile   = 1
         (*pstate).histogram = 0
         (*pstate).contour   = 0
         (*pstate).shadesurf = 0

         ; Sensitize the 'Profile' menu button.
         widget_control, (*pstate).profilemenuid, SENSITIVE=1
      end

      'histogram' : begin
         (*pstate).profile   = 0
         (*pstate).histogram = 1
         (*pstate).contour   = 0
         (*pstate).shadesurf = 0

         ; De-sensitize the 'Profile' menu button.
         widget_control, (*pstate).profilemenuid, SENSITIVE=0
      end

      'contour' : begin
         (*pstate).profile   = 0
         (*pstate).histogram = 0
         (*pstate).contour   = 1
         (*pstate).shadesurf = 0

         ; De-sensitize the 'Profile' menu button.
         widget_control, (*pstate).profilemenuid, SENSITIVE=0
      end

      'shadesurf' : begin
         (*pstate).profile   = 0
         (*pstate).histogram = 0
         (*pstate).contour   = 0
         (*pstate).shadesurf = 1

         ; De-sensitize the 'Profile' menu button.
         widget_control, (*pstate).profilemenuid, SENSITIVE=0
      end

      else : begin
      end
   endcase

   ; Clear the button that is currently checked.
   widget_control, plotselectid, SET_BUTTON=0

   ; Check the button that generated this event and remember its widget id
   ; in the user value of the parent.
   widget_control, event.id, /SET_BUTTON
   widget_control, parent, SET_UVALUE=event.id

   ; Process the plot request.
   oitoolwaprofile->process
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile_smoo_event
; -----------------------------------------------------------------------------
pro itoolwaprofile_smoo_event, event
   compile_opt hidden

   ; The user value of parent of the widgets on this menu holds the widget id
   ; of the button that is currently checked. That parent is the button on
   ; the menu bar named 'Profile.'
   parent = widget_info(event.id, /PARENT)

   ; Get the widget id of the currently-checked button.
   widget_control, parent, GET_UVALUE=currbuttonid

   ; If the button that generated this event is the same as the one that is
   ; currently checked, there is nothing to do.
   if event.id eq currbuttonid then return

   widget_control, /HOURGLASS

   ; Get our object reference.
   widget_control, event.top, GET_UVALUE=oitoolwaprofile

   ; Get the pointer to the state structure.
   oitoolwaprofile->getproperty, PSTATE=pstate

   ; Get the name of the event.
   uname = widget_info(event.id, /UNAME)

   ; Identify event and set control flags in the state structure.

   case uname of
      'scatter' : begin
         (*pstate).scatter = 1
         (*pstate).smooth = 0
      end

      'smooth' : begin
         (*pstate).scatter = 0
         (*pstate).smooth = 1
      end

      'scatter+smooth' : begin
         (*pstate).scatter = 1
         (*pstate).smooth = 1
      end

      else : begin
      end
   endcase

   ; Uncheck the currently-checked button.
   widget_control, currbuttonid, SET_BUTTON=0

   ; Check the button that generated this event.
   widget_control, event.id, /SET_BUTTON

   ; Save the widget id of the button that is now checked.
   widget_control, parent, SET_UVALUE=event.id

   ; Process the profile(s).
   oitoolwaprofile->process
end


; -----------------------------------------------------------------------------
; Procedure itoolwaprofile__define
; Define itoolwaprofile object class.
;
; Attributes:
;   tlb    : top-level base.
;   pstate : pointer to state structure.
;   oitool : object reference of the host instance of itool.
;   pimage : pointer to the image from itool.
;   pscale : plate scale of the image.
;   pscat  : pointer to scatter-plot data.
;   psmoo  : pointer to smooth-plot data.
; -----------------------------------------------------------------------------
pro itoolwaprofile__define
   dummy = {itoolwaprofile, tlb:0L, pstate:ptr_new(), oitool:obj_new(),$
      pimage:ptr_new(), pscale:0.0, pscat:ptr_new(), psmoo:ptr_new()}
end

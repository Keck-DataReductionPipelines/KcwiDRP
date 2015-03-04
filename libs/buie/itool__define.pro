;+
; CLASS_NAME:
;    itool
;
; PURPOSE (one line):
;    To display an image and provide tools for its visualization.
;
; CATEGORY:
;    Widgets
;
; SUPERCLASSES:
;
; SUBCLASSES:
;
; CREATION:
;    See itool::init
;
; METHODS:
;    itool::apdraw
;    itool::cleanup
;    itool::close
;    itool::cstr
;    itool::display
;    itool::draw
;    itool::getproperty
;    itool::newprofile
;    itool::nextimage
;    itool::oneph
;    itool::phact
;    itool::setproperty
;    itool::tpdraw
;    itool::trk
;    itool::realize
;    itool::init
;
; 2004, Mar and Apr, DWL,
;    This version of itool was derived from a major overhaul of cw_itool
; and related routines. It was implemented as an object class
; named 'itool.' Multiple instances may be realized and managed by host
; applications.
;    All of the widget applications that may be launched from itool have
; been re-written using object-oriented techniques and using a standard
; interface that allows simple and efficient communication of data between
; them and itool. The object-oriented approach simplifies operation on
; data, via method routines. With this approach and the standard
; interface, it should be simple to maintain itool and its related
; applications, as well as to add other widget applications in the
; future.
;    All of the itool object-oriented widget applications are named with
; "itoolwa" as the prefix. The "wa" in the name stands for "widget
; application." The portion of the file names before the two "underscore"
; characters are the object-class names. The itool widget applications are:
;
;  itoolwacpmgr__define.pro       ; The Comet Photometry Manager.
;  itoolwaimparms__define.pro     ; The Image Parameters Editor.
;  itoolwaphparms__define.pro     ; The Photometry Parameters Editor.
;  itoolwapixed__define           : The Pixel Editor.
;  itoolwaprofile__define.pro     ; The Profiles Tool.
;  itoolwatpmgr__define           : The Template Manager.
;
;  itool widget-application details:
;
;    First, each object-oriented widget application, including itool itself,
; have the following "properties" available within their respective
; object classes:
;
;    tlb          : the top-level-base of the widget application.
;    pstate       : a pointer to the widget application's state structure.
;
;    In addition, each of the widget applications listed above have the
; following property in their respective object classes:
;
;    oitool       : The object reference of the instance of itool from which
;                   the application was launched.
;
;    Other properties are defined for each object class, including
; the "itool" object class.
;
;    Finally, each object-oriented widget application, including itool,
; stores a copy of its own object reference in the UVALUE of its
; top-level base. Every event handler may retrieve a copy of the application's
; object reference by calling
;
;    widget_control, event.top, get_uvalue=oref
;
; Then, a copy of the pointer to the state structure may be obtained by calling
;
;    oref->getproperty, PSTATE=pstate
;
; Method routines have direct access to the pointer to the state structure,
; via their intrinsic "self" arguments (self.pstate).
;
;    When one of the above widget applications is launched from an instance
; of itool, itool passes a copy of its object reference to that application.
; In return, itool receives a copy of the object reference for the new
; instance of that application. The object class for itool defines
; "properties" for storing object references of each of the applications
; listed above. The Profiles Tool is allowed more than one instance
; to be active; object references for it are maintained within an object
; array.
; 
; Additional comments:
;
;    Fixed old bugs. Cleaned up event handling and data-storage techniques.
;
; Some of the external routines called are:
;
;  itool_pplod.pro    Loads photometry parameters into a
;                     structure, from a file.
;
;
;
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::apdraw
;
; PURPOSE:
;    To draw a photometric aperture in the zoom window.
;
; CALLING SEQUENCE:
;    oref->apdraw,x,y
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::cleanup
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    obj_destroy, oref
;
; INPUTS:
;    oref : An itool object reference.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::close
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    oref->close
;
; INPUTS:
;    oref : An itool object reference.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::cstr
;
; PURPOSE:
;    To compute a stretch range for the image.
;
; CALLING SEQUENCE:
;    oref->cstr, frame
;
; INPUTS:
;    frame : The image frame number (the image may be a "cube" of images).
;            Always 1 for 2-D images.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    SILENT : Set this keyword to suppress display of the stretch range.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::display
;
; PURPOSE:
;    To refresh the itool label and text widgets.
;
; CALLING SEQUENCE:
;    oref->display
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::draw
;
; PURPOSE:
;    To refresh the draw itool widgets (full, zoom, and work).
;
; CALLING SEQUENCE:
;    oref->draw
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    ALT_WORKWIN : Set this keyword to a valid window number. It will be
;                  used instead of the window number of the local work
;                  window.
;    FULL        : Set this keyword to draw the full window.
;    WORK        : Set this keyword to draw the work window.
;    ZOOM        : Set this keyword to draw the zoom window.
;    ZXCEN       : The x-coordinate of the center of the zoom array.
;    ZYCEN       : The y-coordinate of the center of the zoom array.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::getproperty
;
; PURPOSE:
;   To retrieve "properties" defined for the itool object class. Properties
; are specified as keyword arguments.
;
; CALLING SEQUENCE:
;    oref->getproperty
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    NO_BLOCK : Set this keyword to a named variable in which to return the
;               value of the NO_BLOCK property. It is passed to all of the
;               itool widget applications. If itool is running in non-blocked
;               mode, all of its widget applications will run in non-blocked
;               mode.
;    OIMAGE : Set this keyword to a named variable in which to return the
;             object reference of the current image object that is active.
;    OITOOLWACPMGR   : Set this keyword to a named variable in which to return
;                      the object reference for an active Comet Photometry
;                      Manager.
;    OITOOLWAIMPARMS : Set this keyword to a named variable in which to return
;                      the object reference for an active Image Parameters
;                      tool.
;    OITOOLWAPHPARMS : Set this keyword to a named variable in which to return
;                      the object reference for an active Photometry Parameters
;                      tool.
;    PPH_PARMS       : Set this keyword to a named variable in which to return
;                      a pointer to the ph_parms structure.
;    PSTATE          : Set this keyword to a named variable in which to return
;                      a pointer to the state structure.
;    STATUS          : Set this keyword to a named variable in which to return
;                      the itool status structure. The status structure is
;                      defined as:
;
;                      status = {flag:flag, msg:msg, lasttype:lasttype,
;                                lastpos:lastpos, object:object,
;                                lastfwhm:lastfwhm, lastmag:lastmag}
;                      where
;
;                      flag   = true if photometry parameters or if templates
;                               have been modified.
;                      msg    = A message string, indicating which have been
;                               modified.
;                      lasttype, lastpos, object, lastfwhm, and lastmag are
;                      from the im_parms structure.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::newprofile
;
; PURPOSE:
;    To launch a new instance of the Profiles widget application.
;
; CALLING SEQUENCE:
;    oref->newprofile
;
; INPUTS:
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::nextimage
;
; PURPOSE:
;    To display another image.
;
; CALLING SEQUENCE:
;    oref->nextimage, oimage
;
; INPUTS:
;    oimage : An object reference of class itoolimage.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    FIRST : Set this keyword if nextimage is called from a fresh start-up
;            of itool. Otherwise, assume that nextimage is being called
;            directly from the host application that launched this instance
;            of itool.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::oneph
;
; PURPOSE:
;    To do one single-object photometry operation.
;
; CALLING SEQUENCE:
;    oref->oneph, x, y
;
; INPUTS:
;    x, y : the work-window coordinates from a mouse-button event.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::phact
;
; PURPOSE:
;    To perform template photometry.
;
; CALLING SEQUENCE:
;    oref->phact, x, y
;
; INPUTS:
;    x, y : the work-window coordinates from a mouse-button event.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::setproperty
;
; PURPOSE:
;    To set "properties" defined for the itool object class. Properties
; are specified as keyword arguments.
;
; CALLING SEQUENCE:
;    oref->setproperty
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    OITOOLWACPMGR   : the object reference for the newly-launched Comet
;                      Photometry manager.
;    OITOOLWAIMPARMS : the object reference for the newly_launched Image
;                      Parameters tool.
;    OITOOLWAPHPARMS : the object reference for the newly_launched Photometry
;                      Parameters tool.
;    OITOOLWATPMGR   : the object reference for the newly_launched Template
;                      Manager.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::tpdraw
;
; PURPOSE:
;    To plot the current template in the work window.
;
; CALLING SEQUENCE:
;    oref->tpdraw
;
; KEYWORD PARAMETERS:
;    ALT_TPLATE  : Set this keyword to a valid {itool_templatelist} structure.
;                  It will be used as the template to draw, instead of the
;                  currently-selected template from The Template Manager.
;    ALT_WORKWIN : Set this keyword to a valid window number. It will be
;                  used instead of the window number of the local work
;                  window.
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::trk
;
; PURPOSE:
;    To refresh the cursor tracking display widgets.
;
; CALLING SEQUENCE:
;    oref->trk, x, y
;
; INPUTS:
;    x, y : the coordinates from a motion event in the work or zoom window.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::realize
;
; PURPOSE:
;    To realize a new, managed instance of the itool object class.
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
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool::init
;
; PURPOSE:
;    To initialize a new instance of the itool object class (the itool GUI).
;
; CALLING SEQUENCE:
;    oref = obj_new('itool', oimage)
;
; INPUTS:
;    oimage : An object reference of class itoolimage.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    EXPERT       : Set this keyword to turn on certain "expert" modes of
;                   operation.
;    FVISIBLE     : Set this keyword to the desired size, in pixels, of the
;                   full window (default is 128).
;    GROUP_LEADER : Set this keyword to the group leader for this instance.
;    NODISMISS    : Set this keyword, to de-sensitize the Dismiss button on
;                   the file menu and sensitize the Exit button. Otherwise,
;                   the Dismiss button will be sensitive and the Exit button
;                   will be insensitive.
;    PHOTPARMFILE : Set this keyword to a string containing the name of
;                   a photometry-parameters file.
;    SCLMIN       : Set this keyword to a minimum stretch-range value.
;    SCLMAX       : Set this keyword to a maximum stretch-range value.
;    TMPLFILE     : Set this keyword to a string containing the name of
;                   a template file.
;    WXVISIBLE    : Set this keyword to the size, in pixels, of the width
;                   of the work window (default is 500).
;    WYVISIBLE    : Set this keyword to the size, in pixels, of the height
;                   of the work window (default is 500).
;    WZOOMFACT    : Set this keyword to the ceiling of the zoom factor
;                   (default is unlimited).
;    XSIZE        : Set this keyword to the width, in pixels, of the image
;                   (required).
;    YSIZE        : Set this keyword to the height, in pixels, of the image
;                   (required).
;    ZVISIBLE     : Set this keyword to the desired size, in pixels, of the
;                   zoom window (default is 128).
;
; OUTPUTS:
;    oref = The object reference of the new instance of the itool
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
; it must be realized.
;
; EXAMPLE:
;    oref = obj_new('itool', oimage, GROUP_LEADER=group_leader)
;    oref->realize
;
; -----------------------------------------------------------------------------
; MODIFICATION HISTORY:
;    Note: Adapted from cw_itool.pro. The following history has been copied
; from cw_itool:
;    Written by Doug Loucks, Lowell Observatory, June 1994.
;    This version is a major re-write, employing the compound widget design
; introduced in Version 3 of IDL.
; 94/10/27, MWB, Lowell Observatory, fixed ZOOM bug on line 1144.
; 95/01/24, MWB, xcen,ycen size mismatch storing back to template if array is
;           3-d.  Fixed to copy *last* position from Basphote return.
; 95/06/12, MWB, Fixed bug in stretch range computation for large cubes of
;           small images.
; 95/09/07, MWB, Changed auto-stretch computation to make it more robust.
; 95/10/31, MWB, Fixed widget layout problem caused by IDL v4.0 changes.
; 96/01/06, MWB, Changed W*VISIBLE default to recognize smaller screens.
; 96/01/16, MWB, Fixed !order=1 bug in mouse event handling.
;                Fixed Stretch Menu operational bugs for handling image cubes.
; 96/06/25, MWB, Added autophot support
; 97/12/12, MWB, changed display to include more image info on main window.
; 99/08/31, MWB (with help from John Mattox), added a
;               feature that overplots the aperture on the zoom window
;               image after template or single-object photometry measurement.
; 2001/4/3, MWB, added plotting of active template in work window
; 2001/9/18, MWB, fixed color error in plots with 8-bit display.
; 2002/11/17, MWB, fixed !order=1 bug for overplotting templates.
;                  Replaced image scaling with updated skysclim routine.
; 2004/04/15, DWL, Major overhaul of the entire program.
; 2004/05/28, DWL. Added a 'Save Image to FITS' button to the File Menu.
;                     Added a 'Save Thumbnail Window to JPEG' button to the
;                     File Menu.
;                     Added a 'Save Work Window to JPEG' button to the
;                     File Menu.
;                     Added a 'Save Zoom Window to JPEG' button to the
;                     File Menu.
; 2005/11/01, DWL. Added method routine itool::close. This method allows
;                  an instance of itool to be closed, from an application
;                  that is managing the instance.
;
; 2006/03/15 - DWL, Created a new object class named 'itoolimage' (see
;                itoolimage__define.pro for details).
;                  An instance of this object class is now passed as the
;                argument to this init method, instead of the former "pim_parms"
;                argument.
;                  The changes to the itool GUI involve the places where the
;                "pim_parms" argument is required. Now, "pim_parms" is acquired
;                via the "getproperty" method of the 'itoolimage' object class,
;                using the object reference provided via the "oimage"
;                argument.
;                  Since it has always been necessary to generate and store
;                information (im_parms) about the image to be displayed in the
;                itool GUI, I felt that this was a good argument in favor of
;                combining the image data and the initialization requirements
;                for those data. Obviously, this suggested object-oriented
;                techniques. With this new object class, calling routines
;                that use the itool GUI benefit from the built-in
;                initialization and cleanup when an instance is created or
;                destroyed. It is no longer necessary for callers of the itool
;                GUI to invoke separate code segments to handle initialization
;                and cleanup of image data; they need only create and destroy
;                instances of the 'itoolimage' object class (obj_new and
;                obj_destroy).
;                  Note that the old procedure named itool_init is no longer
;                needed; the duties of that procedure are now handled by the
;                "init" method of the 'itoolimage' object class.
; 2006/04/4, MWB, fixed auto-scaling bug when looking at image cubes.
; 2006/5/22, Peter L. Collins, Lowell Observatory
;              added ccd readout noise as photometric parameter.
;-


; ------------------------------------------------------------------------------
; Procedure itool::apdraw
; Draw photometry aperture in the zoom window
; ------------------------------------------------------------------------------
pro itool::apdraw,x,y
   compile_opt hidden

   ; Simplify pointer syntax.
   pstate = self.pstate
   pzoomstate = (*pstate).pzoomstate
   widget_control, (*pstate).zoom_view, get_value=winnum
   pph_parms = self.pph_parms

   red = '0000ff'x

   curwin = !d.window
   wset, winnum

   ; Draw photometry aperture.
   npts=500

   ; The aperture is centered on the zoom-window equivalent of the incoming
   ; coordinates. These coordinates could be on fractions of a pixel.

   ; Build arrays of coordinates of the circle around the aperture. These
   ; are built in data coordinates first, then converted to zoom
   ; coordinates.

   xrad = x+(*pph_parms).radius*sin(2*!pi*findgen(npts+1)/float(npts))
   yrad = y+(*pph_parms).radius*cos(2*!pi*findgen(npts+1)/float(npts))
   data2zoom,pstate,xrad,yrad,zxrad,zyrad

   seg=15 ; breaks up the circle into segments every SEG degrees.
   ang= fix((findgen(npts+1)/npts*360.0) / seg)

   if (*pstate).eightbit then begin
      color=bytarr(npts+1)  ; alternate between color 0 and top color.
      color[where(ang eq (ang/2)*2)] = (*pstate).ncolors-1
      thick=2
   endif else begin
      color = red
      thick=1
   endelse

   ; Use the size of the zoom window to plot the aperture from normalized
   ; coordinates.

   plots,zxrad/((*pzoomstate).xvisible+1),zyrad/((*pzoomstate).yvisible+1),$
      color=color,thick=thick,/normal

   ; Restore the previous window number.
   wset, curwin
end


; ---------------------------------------------------------------------------
; Procedure itool::cleanup
; ---------------------------------------------------------------------------
pro itool::cleanup
   compile_opt hidden

   ; Determine whether or not this instance of the itool GUI has been asked
   ; to dispose of its incoming argument (a reference to the 'itoolimage'
   ; object class). This may be necessary, when the owner of the argument
   ; exits after launching the itool GUI (see itool.pro).

   if self.arg_cleanup then begin
      obj_destroy, self.oimage
   endif

   ptr_free, self.pph_parms, self.pstate
end


; ---------------------------------------------------------------------------
; Procedure itool::close
; Closes down an instance of itool.
; ---------------------------------------------------------------------------
pro itool::close
   widget_control, self.tlb, /destroy
end


; ---------------------------------------------------------------------------
; Procedure itool::cstr
; Compute the stretch range for the image.
; ----------------------------------------------------------------------------
pro itool::cstr, frame, SILENT=silent
   compile_opt hidden

   self.oimage->getproperty, PIM_PARMS=pim_parms

   if (*pim_parms).ready[frame] then return
   pimage = (*pim_parms).imageptr
   minvalue = min((*pimage)[*,*,frame], max=maxvalue)

   (*pim_parms).minvalue[frame] = minvalue
   (*pim_parms).maxvalue[frame] = maxvalue

   ; Compute stretch range parameters.
   skysclim,(*pimage)[*,*,frame],z1,z2,medvalue,sdvalue,npts=10000,lowclip=0.2,hiclip=0.8

   f = '(G0.0)'

   ; if range comes back low=hi, set to full range of image.
   if z1 eq z2 then begin
      z1 = minvalue
      z2 = maxvalue
   endif

   ; if range is still low=hi, add 1 to hi to make them different
   if z1 eq z2 then z2 = z2 + 1

   (*pim_parms).sclmin[frame] = z1
   (*pim_parms).sclmax[frame] = z2

   if (*pim_parms).sclmin[frame] lt (*pim_parms).minvalue[frame] then begin
      (*pim_parms).sclmin[frame] = (*pim_parms).minvalue[frame]
   endif

   if (*pim_parms).sclmax[frame] gt (*pim_parms).maxvalue[frame] then begin
      (*pim_parms).sclmax[frame] = (*pim_parms).maxvalue[frame]
   endif

   (*pim_parms).curmin[frame] = (*pim_parms).sclmin[frame]
   (*pim_parms).curmax[frame] = (*pim_parms).sclmax[frame]

   (*pim_parms).ready[frame] = 1B

   if not keyword_set(silent) then begin
      print, 'Median: ' + string(medvalue, format='(G0.0)') + $
      '  Sigma: ' + string(sdvalue, format='(G0.0)') + $
      '  Image min, max: ' + string(minvalue, format='(G0.0)') + ', ' + $
      string(maxvalue, format='(G0.0)')

   endif
end


; -----------------------------------------------------------------------------
; Procedure itool::display
; Update widgets.
; -----------------------------------------------------------------------------
pro itool::display
   compile_opt hidden

   pstate = self.pstate
   self.oimage->getproperty, PIM_PARMS=pim_parms
   widget_control, self.tlb, UPDATE=0

   frame = (*pim_parms).frame

   ; Compute stretch, if necessary.
   self->cstr, frame

   maxvalue = string((*pim_parms).maxvalue[frame], format=(*pstate).fmtg)
   minvalue = string((*pim_parms).minvalue[frame], format=(*pstate).fmtg)

   ; Show the image min and max.
   widget_control, (*pstate).imagemaxid, set_value=maxvalue
   widget_control, (*pstate).imageminid, set_value=minvalue

   ; Show the frame number.
   widget_control, (*pstate).frameid, set_value=string((*pim_parms).frame, $
          format=(*pstate).fmti)

   ; Show the total number of frames in this image file.
   widget_control, (*pstate).nframesid, set_value=string((*pim_parms).nframes,$
          format=(*pstate).fmti)

   if (*pim_parms).imfile ne '' then begin
      ; Show the image file name.
      title = (*pim_parms).title + ' ' + (*pim_parms).imfile
      widget_control, self.tlb, TLB_SET_TITLE=title
   endif

   ; Show the object name.
   if (*pim_parms).object ne '' or (*pim_parms).filter ne '' $
      or (*pim_parms).ut ne '' then begin

      str1=strmid('Object: '+(*pim_parms).object,0,20)
      str2=strmid('Filter: '+(*pim_parms).filter,0,20)
      str3=string('Airmas: ', (*pim_parms).airmass, format='(a,f5.3)')

      str4=strcompress(string('ExpTim: ', (*pim_parms).exptime, $
                              format='(a,f10.3," sec")'))

      jdstr, (*pim_parms).jd,0, jds
      str5='    UT: '+strmid(jds,0,10)
      str6='midtim: '+strmid(jds,11,8)
   endif else begin
      str1=''
      str2=''
      str3=''
      str4=''
      str5=''
      str6=''
   endelse

   widget_control, (*pstate).objectid, set_value=str1
   widget_control, (*pstate).filterid, set_value=str2
   widget_control, (*pstate).airmasid, set_value=str3
   widget_control, (*pstate).exptimid, set_value=str4
   widget_control, (*pstate).utdateid, set_value=str5
   widget_control, (*pstate).uttimeid, set_value=str6

   ; Set some button sensitivities.
   widget_control, (*pstate).auto2id,     SENSITIVE=((*pim_parms).nframes gt 1)
   widget_control, (*pstate).disp2id,     SENSITIVE=((*pim_parms).nframes gt 1)
   widget_control, (*pstate).extrema2id,  SENSITIVE=((*pim_parms).nframes gt 1)
   widget_control, (*pstate).animateid,   SENSITIVE=((*pim_parms).nframes gt 1)
   widget_control, (*pstate).frameid,     SENSITIVE=((*pim_parms).nframes gt 1)
   widget_control, (*pstate).nextframeid, SENSITIVE=((*pim_parms).nframes gt 1)

   curmin = string((*pim_parms).curmin[frame], format=(*pstate).fmtg)
   curmax = string((*pim_parms).curmax[frame], format=(*pstate).fmtg)

   ; Show the stretch range for the display.
   widget_control, (*pstate).dispminid, set_value=curmin
   widget_control, (*pstate).dispmaxid, set_value=curmax

   ; Refresh the "Image Params" GUI, if it is active.
   if xregistered('itoolwaimparms', /NOSHOW) then begin
      self.oitoolwaimparms->refresh
   endif

   widget_control, self.tlb, UPDATE=1
end


; ------------------------------------------------------------------------------
; Procedure itool::draw
; Update one or more of the draw widgets.
; ------------------------------------------------------------------------------
pro itool::draw, ALT_WORKWIN=alt_workwin, FULL=full, WORK=work, ZOOM=zoom,$
    ZXCEN=zxcen, ZYCEN=zycen

   compile_opt hidden

   pstate = self.pstate
   self.oimage->getproperty, PIM_PARMS=pim_parms

   ; Optional zoom center update is provided via the keywords.

   pfullstate = (*pstate).pfullstate
   pworkstate = (*pstate).pworkstate
   pzoomstate = (*pstate).pzoomstate
   pimage = (*pim_parms).imageptr

   frame  = (*pim_parms).frame
   curmin = (*pim_parms).curmin[frame]
   curmax = (*pim_parms).curmax[frame]

   curwin = !d.window

   if keyword_set(full) then begin
      ; Process the full window.

      widget_control, (*pstate).full_view, get_value=winnum
      wset, winnum

      if (*pfullstate).zfact ne 1 then begin
         tv,bytscl(rebin((*pimage)[(*pfullstate).spx:(*pfullstate).spx+$
         (*pfullstate).dx-1,$
         (*pfullstate).spy:(*pfullstate).spy+(*pfullstate).dy-1,$
         frame],$
         (*pfullstate).dx*(*pfullstate).zfact,$
         (*pfullstate).dy*(*pfullstate).zfact,$
         /SAMPLE),$
         min=curmin,max=curmax,top=(*pstate).ncolors-1),$
         (*pfullstate).xoff, (*pfullstate).yoff
      endif else begin
         tv,bytscl((*pimage)[(*pfullstate).spx:(*pfullstate).spx+$
         (*pfullstate).dx-1,$
         (*pfullstate).spy:(*pfullstate).spy+(*pfullstate).dy-1,frame],$
         min=curmin,max=curmax,top=(*pstate).ncolors-1),$
         (*pfullstate).xoff, (*pfullstate).yoff
      endelse
   endif

   if keyword_set(zoom) then begin
      ; Process the zoom window.
      widget_control, (*pstate).zoom_view, get_value=winnum
      wset, winnum

      if keyword_set(zxcen) then (*pzoomstate).xcen = zxcen
      if keyword_set(zycen) then (*pzoomstate).ycen = zycen

      ; Compute zoom-window parameters.
      (*pzoomstate).dx = (*pzoomstate).xvisible / (*pzoomstate).zfact

      if (*pzoomstate).dx gt (*pim_parms).xsize then begin
         (*pzoomstate).dx=(*pim_parms).xsize
      endif

      if (*pzoomstate).dx gt (*pim_parms).ysize then begin
         (*pzoomstate).dx=(*pim_parms).ysize
      endif

      (*pzoomstate).dy = (*pzoomstate).dx

      ; Compute the setpoint (lower left corner) of the zoom box in the
      ; data array.
      xcen = fix((*pzoomstate).xcen+0.5)
      ycen = fix((*pzoomstate).ycen+0.5)
      (*pzoomstate).spx = xcen - ((*pzoomstate).dx / 2)
      (*pzoomstate).spy = ycen - ((*pzoomstate).dy / 2)

      (*pzoomstate).xsize = (*pzoomstate).dx * (*pzoomstate).zfact
      (*pzoomstate).ysize = (*pzoomstate).dy * (*pzoomstate).zfact

      (*pzoomstate).xoff = ((*pzoomstate).xvisible - (*pzoomstate).xsize) / 2
      (*pzoomstate).yoff = ((*pzoomstate).yvisible - (*pzoomstate).ysize) / 2

      dx = (*pzoomstate).dx
      dy = (*pzoomstate).dy
      spx = (*pzoomstate).spx
      spy = (*pzoomstate).spy
      xsize = (*pim_parms).xsize
      ysize = (*pim_parms).ysize

      ; Make an array of the same type as the image and having the size of the
      ; zoom-extraction box.
      ; The idea, here, is to use a square, 2-D array, whose size is determined
      ; by the current zoom factor and the current size of the zoom-draw
      ; window. This size is defined by the dx and dy tags of the
      ; zoomstate structure. This array is re-binned by the zoom factor and
      ; displayed in the zoom-draw window.

      type = size(*pimage,/TYPE)
      pzoombuf = (*pzoomstate).pzoombuf

      ; Save the zoom buffer; it may be needed later (profile tool).
      *pzoombuf = make_array(dx,dy,TYPE=type)

      ; Build arrays of x and y indices that will be used to extract pixels
      ; from the image array and to store pixels into the zoom-extraction array.
      ; These arrays represent the exact pixel coordinates of an array that
      ; is of unit size relative to the current zoom parameters (see above).

      ix = lindgen(dx) # replicate(1L,dy)
      iy = lindgen(dy) ## replicate(1L,dx)

      ; With these arrays of indices, it is easy to index a box of
      ; pixels in the data-image array, simply by adding the setpoint (lower-
      ; left corner) coordinates (spx and spy) to each array of indices.
      ; Likewise, it is easy to index the corresponding pixels in the
      ; zoom-extraction array. In the latter case, the setpoint is [0,0].

      ; The next task is to filter the arrays of indices (ix and iy) with
      ; the WHERE function, according to the setpoint in the data-image
      ; array. When the zoom-extraction box is near the edges of the
      ; data-image array, those coordinates that lie outside the bounds of
      ; the data-image array are eliminated. This elimination process
      ; leads to an automatic offset within the zoom-extraction array.

      ; First, the bounds on the X indices.
      wx = where((ix+spx ge 0) and (ix+spx lt xsize),countx)

      if countx gt 0L then begin
         ; Found some overlap along the X dimension. Save the indices.
         iw = wx

         ; Next, the bounds of the X indices in the Y dimension.
         wy = where((iy[iw]+spy ge 0) and (iy[iw]+spy lt ysize),county)

         if county gt 0L then begin
            ; Found an overlap in the Y dimension, as well. This means that
            ; there is a rectangular overlap of the zoom box and the
            ; data-image array. Save the final overlap indices.
            iw = iw[wy]
         endif
      endif

      if n_elements(iw) gt 0L then begin
         if iw[0] ne -1L then begin
            ; Pull the selected box of pixels from the data-image array into
            ; the zoom-extraction array.
            temp = (*pimage)[*,*,frame]
            (*pzoombuf)[ix[iw],iy[iw]] = temp[ix[iw]+spx,iy[iw]+spy]
            temp = 0
         endif
      endif

      if (*pzoomstate).zfact ne 1.0 then begin
         ; Re-bin the zoom-extraction array to its zoom factor.
         zoombox = rebin(*pzoombuf,(*pzoomstate).xsize,(*pzoomstate).ysize,$
            /SAMPLE)
      endif else begin
         zoombox = *pzoombuf
      endelse

      ; Clear the zoom window to background and display the zoomed
      ; array.
      erase,0
      tv,bytscl(zoombox,min=curmin,max=curmax,top=(*pstate).ncolors-1),$
      (*pzoomstate).xoff,(*pzoomstate).yoff
   endif

   if keyword_set(work) then begin
      if keyword_set(alt_workwin) then begin
         winnum = alt_workwin
      endif else begin
         widget_control, (*pstate).work_view, get_value=winnum
      endelse

      wset, winnum

      if (*pworkstate).zfact ne 1 then begin
         tv,bytscl(rebin((*pimage)[(*pworkstate).spx:(*pworkstate).spx+$
         (*pworkstate).dx-1,$
         (*pworkstate).spy:(*pworkstate).spy+(*pworkstate).dy-1,$
         frame],$
         (*pworkstate).zfact*(*pworkstate).dx,$
         (*pworkstate).zfact*(*pworkstate).dy,$
         /SAMPLE),$
         min=curmin,max=curmax,top=(*pstate).ncolors-1),$
         (*pworkstate).xoff,(*pworkstate).yoff
      endif else begin
         tv,bytscl((*pimage)[(*pworkstate).spx:(*pworkstate).spx+$
         (*pworkstate).dx-1,$
         (*pworkstate).spy:(*pworkstate).spy+(*pworkstate).dy-1,frame],$
         min=curmin,max=curmax,top=(*pstate).ncolors-1),$
         (*pworkstate).xoff,(*pworkstate).yoff
      endelse

      ; Leave, if graphics was sent to an alternate draw window. Otherwise,
      ; continue with additional graphics in the work window.
      if keyword_set(alt_workwin) then return

      cyan = 'ffff00'x

      if (*pstate).eightbit then begin
         color = (*pstate).ncolors-1
      endif else begin
         color = cyan
      endelse

      ; Compute the setpoint and the size of the zoom-window box.
      zdx = (*pzoomstate).dx
      zdy = (*pzoomstate).dy
      wdx = zdx * (*pworkstate).zfact
      wdy = zdy * (*pworkstate).zfact
      spx = (*pzoomstate).spx * (*pworkstate).zfact
      spy = (*pzoomstate).spy * (*pworkstate).zfact

      ; Define the plot vextors.
      xbox = [spx, spx+wdx, spx+wdx, spx, spx]
      ybox = [spy, spy, spy+wdy, spy+wdy, spy]

      ; Plot a box that represents the location and size of the zoom window.
      plots, xbox, ybox, /DEVICE, COLOR=color

      self->tpdraw
   endif

   ; Restore the previous window number.
   wset, curwin

   ; Refresh any profiles widgets that may be active.
   p_oprofiles = (*self.pstate).p_oprofiles
   w = where(obj_valid(*p_oprofiles) eq 1B, count)

   if count gt 0L then begin
      ; There is at least one profiles widget active.
      ; Piece together the title for the TLB.
      newtitle = ''
      if ((*pim_parms).object ne '') then newtitle = (*pim_parms).object

      if ((*pim_parms).imfile ne '') then newtitle = newtitle +'  ' +$
         (*pim_parms).imfile

      if ((*pim_parms).ut ne '') then newtitle = newtitle + ' '+(*pim_parms).ut

      frame = (*pim_parms).frame
      spx = (*pzoomstate).spx
      spy = (*pzoomstate).spy
      dx  = (*pzoomstate).dx
      dy  = (*pzoomstate).dy

      ; Don't refresh profile, if any portion of the zoom box lies outside
      ; of the image boundaries.
;     if spx lt 0 then return
;     if spy lt 0 then return
;     if spx+dx ge (*pim_parms).xsize then return
;     if spy+dy ge (*pim_parms).ysize then return

;     array = (*pimage)[spx:spx+dx-1, spy:spy+dy-1, frame]

      for j=0L, count-1L do begin
         ; Found an active profile. Request an update.
         ; The profiles widget will ignore the update request if the
         ; display is locked).
         (*p_oprofiles)[j]->refresh, {image:(*pzoombuf), xset:spx, yset:spy},$
            TITLE='Itool Profiles ' + newtitle
      endfor
   endif
end


; ---------------------------------------------------------------------------
; Procedure itool::getproperty
; ---------------------------------------------------------------------------
pro itool::getproperty, NO_BLOCK=no_block,$
   OIMAGE=oimage,$
   OITOOLWACPMGR=oitoolwacpmgr,$
   OITOOLWAIMPARMS=oitoolwaimparms,$
   OITOOLWAPHPARMS=oitoolwaphparms,$
   OITOOLWATPMGR=oitoolwatpmgr,$
   PPH_PARMS=pph_parms,$
   PSTATE=pstate, STATUS=status

   compile_opt hidden

   if arg_present(no_block) then no_block = self.no_block
   if arg_present(oimage) then oimage = self.oimage
   if arg_present(oitoolwacpmgr) then oitoolwacpmgr = self.oitoolwacpmgr
   if arg_present(oitoolwaimparms) then oitoolwaimparms=self.oitoolwaimparms
   if arg_present(oitoolwaphparms) then oitoolwaphparms = self.oitoolwaphparms
   if arg_present(oitoolwatpmgr) then oitoolwatpmgr = self.oitoolwatpmgr
   if arg_present(pph_parms) then pph_parms = self.pph_parms
   if arg_present(pstate) then pstate = self.pstate

   if arg_present(status) then begin
      self.oimage->getproperty, PIM_PARMS=pim_parms
      lasttype = (*pim_parms).lasttype
      lastpos  = (*pim_parms).lastpos
      object   = (*pim_parms).object
      lastmag  = (*pim_parms).lastmag
      lastfwhm = (*pim_parms).lastfwhm

      phedit = (*self.pph_parms).edtflg

      ; Check for modified templates.
      modified = 0B

      if obj_valid(self.oitoolwatpmgr) then begin
         self.oitoolwatpmgr->getproperty, NUM_TPLATES=num_tplates,$
            PTPLATES=ptplates

         if num_tplates gt 0L then begin
            for j=0L, num_tplates-1L do begin
               (*ptplates)[j].oref->getproperty, MODIFIED=t_modified
               modified = modified or t_modified
            endfor
         endif
      endif

      msg = ['']

      if phedit then begin
         msg = [msg, 'Itool PHOTOMETRY parameter changes have not been saved.']
      endif

      if modified then begin
         msg = [msg, 'Itool TEMPLATE changes have not been saved.']
      endif

      flag = phedit or modified

      status = {flag:flag, msg:msg, lasttype:lasttype, lastpos:lastpos, $
            object:object, lastfwhm:lastfwhm, lastmag:lastmag}
   endif
end


; ------------------------------------------------------------------------------
; Procedure itool::newprofile
;   Launches a new Profiles widget application.
; ------------------------------------------------------------------------------
pro itool::newprofile, HISTOGRAM=histogram
   compile_opt hidden

   if not keyword_set(histogram) then histogram=0

   ; Retrieve some structures.
   self.oimage->getproperty, PIM_PARMS=pim_parms
   pimage = (*pim_parms).imageptr
   pph_parms = self.pph_parms
   pzoomstate = (*self.pstate).pzoomstate

   ; Get the set-point and size of the zoom-window piece of the image.
   xs = (*pzoomstate).spx
   ys = (*pzoomstate).spy
   dx = (*pzoomstate).dx
   dy = (*pzoomstate).dy

   ; Don't launch a profile, if any portion of the zoom box lies outside
   ; of the image bounds.
   if xs lt 0 then return
   if ys lt 0 then return
   if xs+dx ge (*pim_parms).xsize then return
   if ys+dy ge (*pim_parms).ysize then return

   ; Piece together the title for the TLB.
   title = ''
   if ((*pim_parms).object ne '') then title = (*pim_parms).object
   if ((*pim_parms).imfile ne '') then title = title +'  '+ (*pim_parms).imfile
   if((*pim_parms).ut ne '') then title = title + ' ' + (*pim_parms).ut

   ; Extract the zoomed piece.
   array = (*pimage)[xs:xs+dx-1, ys:ys+dy-1, (*pim_parms).frame]

   ; Get the Plate Scale.
   platescale = (*pph_parms).platescale

   ; Look for any open slots in the array of profile object references.
   p_oprofiles = (*self.pstate).p_oprofiles
   w = where(obj_valid(*p_oprofiles) eq 0B, count)

   if count eq 0L then begin
      ; The array of profile object references is full (all are valid).
      ; Add another five slots.
      *p_oprofiles = [*p_oprofiles, objarr(5)]

      ; Set the array index to the first of the five new slots.
      index = n_elements(*p_oprofiles) - 5L
   endif else begin
      ; There is at least one slot available for assignment. Take the first
      ; one.
      index = w[0]
   endelse

   ; Create a new instance of the profiles object class and store
   ; the new object reference into the chosen slot in the profile object-
   ; references array.
   (*p_oprofiles)[index] = obj_new('itoolwaprofile', self,$
      PLATESCALE=platescale, TITLE='Itool Profiles '+title,$
      HCTITLE=title, GROUP_LEADER=self.tlb, HISTOGRAM=histogram)

   ; Realize a new instance of the profiles widget application and
   ; refresh its display with the array of data extracted from the zoom
   ; area.
   (*p_oprofiles)[index]->realize, NO_BLOCK=self.no_block
   (*p_oprofiles)[index]->refresh, {image:array, xset:xs, yset:ys}
end


; ------------------------------------------------------------------------------
; Procedure itool::nextimage
;   Displays another image, given by its parameters structure, passed as the
; argument.
; ------------------------------------------------------------------------------
pro itool::nextimage, oimage, FIRST=first
   compile_opt hidden

   widget_control, self.tlb, /MAP

   ; If the keyword-variable "first" is set, this is a fresh start of
   ; itool, in which case an initial "oimage" argument has been
   ; provided and it is not necessary to store the "oimage"
   ; argument; it was stored at initialization time.

   if not keyword_set(first) then begin
      self.oimage = oimage
   endif

   self.oimage->getproperty, PIM_PARMS=pim_parms

   pstate = self.pstate
   pworkstate = (*pstate).pworkstate

   self->cstr, 0

   if not (*pim_parms).autophot then (*pim_parms).lasttype=0

   self->display

   self->draw, /FULL, /ZOOM, /WORK

   (*self.pph_parms).objnum = 0

   if (*pstate).scroll then begin
      xoff = max([0, ((*pim_parms).xsize-(*pworkstate).xvisible)/2 ])
      yoff = max([0, ((*pim_parms).ysize-(*pworkstate).yvisible)/2 ])
      widget_control, (*pstate).work_view, SET_DRAW_VIEW=[xoff, yoff]
   endif

   if (*pim_parms).autophot then begin
      if (*pim_parms).lasttype eq 1 then begin
         self->oneph, (*pim_parms).lastpos[0],$
         (*pim_parms).lastpos[1]
      endif else if (*pim_parms).lasttype eq 2 then begin
         if xregistered('itoolwatpmgr', /NOSHOW) then begin
            self->phact, (*pim_parms).lastpos[0],$
               (*pim_parms).lastpos[1]
         endif
      endif
   endif
end


; ------------------------------------------------------------------------------
; Procedure itool::oneph
; Do single-object photometry with display update.
; ------------------------------------------------------------------------------
pro itool::oneph, x, y
   compile_opt hidden

   pstate = self.pstate
   self.oimage->getproperty, PIM_PARMS=pim_parms
   pph_parms = self.pph_parms

   pimage = (*pim_parms).imageptr
   objnum = (*pph_parms).objnum

   basphote, (*pph_parms).gain, *pimage,$
      (*pim_parms).exptime, x, y, (*pph_parms).radius,$
      (*pph_parms).sky1, (*pph_parms).sky2, (*pph_parms).logfile, objnum,$
      AIRMASS=(*pim_parms).airmass, /ALTLOG, BOXMRAD=(*pph_parms).boxmrad,$
      EXACT=(*pph_parms).exact, FWHM=fwhm, MAG=mag, NAME=(*pim_parms).object,$
      NOMEXT=(*pph_parms).nomext, FILTER=(*pim_parms).filter, $
      FNAME=(*pim_parms).imfile, JD=(*pim_parms).jd, DT=(*pim_parms).expdelta,$
      PSCALE=(*pph_parms).platescale, RDNOISE=(*pph_parms).rdnoise, $
      XCEN=xcen, YCEN=ycen, ZPOINT=(*pph_parms).zpoint

   (*pph_parms).objnum = objnum

   (*pim_parms).lasttype = 1
   (*pim_parms).lastpos=[xcen[0],ycen[0]]
   (*pim_parms).lastmag = mag[0]
   (*pim_parms).lastfwhm = fwhm[0]


   if (*pph_parms).exact eq 0 then begin
      ; Re-draw the zoom window at the new center.
      self->draw, /WORK, /ZOOM, ZXCEN=xcen[0], ZYCEN=ycen[0]
   endif else begin
      self->draw, /WORK, /ZOOM
   endelse

   ; Update the cursor tracking widgets.
   self->trk, xcen[0], ycen[0]

   ; Draw photometry aperture.
   self->apdraw,xcen[0],ycen[0]
end


; ------------------------------------------------------------------------------
; Procedure itool::phact
; Action routine for Template Photometry.
; ------------------------------------------------------------------------------
pro itool::phact, x, y
   compile_opt hidden

   if not obj_valid(self.oitoolwatpmgr) then return
   pstate = self.pstate
   self.oimage->getproperty, PIM_PARMS=pim_parms
   pph_parms = self.pph_parms
   pdraw_state = (*pstate).pdraw_state

   ; Need a few properties from The Template Manager.
   self.oitoolwatpmgr->getproperty, SELECTED=selected, TPLATE=tplate

   if selected lt 0L then begin
      ; Template not selected.
      msg = 'A Template has not been selected.'

      result = qannounc(msg, TITLE='Error', TRUELABEL='Dismiss',$
         FALSELABEL='', XSIZE=strlen(msg), GROUP_LEADER=self.tlb)

      return
   endif

   ; Get the mode of the selected template.
   tplate.oref->getproperty, MODE=mode

   case mode of
      0 : begin ; Add mode.
         tplate.oref->addobject, x, y
         self.oitoolwatpmgr->refreshobjects
         self->trk, x, y
         (*pim_parms).lasttype = 0
      end

      1 : begin ; Left-button photometry mode.
         tplate.oref->getproperty, NUMOBJ=numobj

         if numobj eq 0L then begin
            result = dialog_message('This template has no objects.', /ERROR,$
               DIALOG_PARENT=self.tlb)

            return
         endif

         tplate.oref->getproperty, NEW=new, OBJNAM=objnam, X=tpx, Y=tpy

         ; The values of x and y are assumed to be the new anchor coordinates.

         if new[0] eq 1 then begin
            xguess = tpx
            yguess = tpy
         endif else begin
            dx = x - tpx[0]
            dy = y - tpy[0]
            xguess = tpx
            yguess = tpy
            w = where(new eq 0B, count)

            if count gt 0 then begin
               xguess[w] = xguess[w] + dx[w]
               yguess[w] = yguess[w] + dy[w]
            endif
         endelse

         ; Offset of mouse position from last known position.
         axoff = xguess[0]-tpx[0]
         ayoff = yguess[0]-tpy[0]

         ; Check if first position is within tolerance of the anchor.
         if (abs(axoff) gt 20) or (abs(ayoff) gt 20) then begin
            t = [$
            '  The location of the first object, as supplied by the left',$
            'mouse button, is more than twenty pixels away from its last',$
            'known location.',$
            '  The template could become corrupted if this starting location',$
            'is used.', $
            '  The requested starting location, last known location, and',$
            'differences are shown below:', $
            '                    ', $
            '------ Requested (x,y) ----- Last known (x,y) ---- differences',$
            '  (' + string(x, format='(F10.3)') + ',' + $
                    string(y, format='(F8.3)') + ')' + $
            '  (' + string(tpx[0],   format='(F10.3)') + ',' + $
                    string(tpy[0],   format='(F8.3)') + ')' + $
            '  (' + string(x-tpx[0],   format='(F10.3)') + ',' + $
                    string(y-tpy[0],   format='(F8.3)') + ')',$
            '',$
            '  Click OK to start photometry.']

            result = qannounc(t,$
               TITLE='First-Template Object-Location Status',$
               FALSELABEL='Cancel this request',$
               TRUELABEL='Ok, start photometry', XSIZE=max(strlen(t)),$
               GROUP_LEADER=self.tlb)

            if result eq 0 then return
         endif

         tpn = objnam

         pimage = (*pim_parms).imageptr

         ; Check for header substitution of object name.
         w = where(strupcase(tpn) eq '<DEFAULT>', count)
         if count ne 0 then tpn[w]=(*pim_parms).object

         objnum = (*pph_parms).objnum

         basphote, (*pph_parms).gain, *pimage, $
            (*pim_parms).exptime, xguess, yguess, (*pph_parms).radius,$
            (*pph_parms).sky1, (*pph_parms).sky2, (*pph_parms).logfile,$
            objnum, AIRMASS=(*pim_parms).airmass, /ALTLOG,$
            BOXMRAD=(*pph_parms).boxmrad,$
            EXACT=(*pph_parms).exact, FWHM=fwhm, MAG=mag, NAME=tpn,$
            NOMEXT=(*pph_parms).nomext, FILTER=(*pim_parms).filter,$
            FNAME=(*pim_parms).imfile, JD=(*pim_parms).jd,$
            DT=(*pim_parms).expdelta,$
            ONCHIP=onchip, PSCALE=(*pph_parms).platescale, $
            RDNOISE=(*pph_parms).rdnoise, $
            XCEN=xcen, YCEN=ycen, ZPOINT=(*pph_parms).zpoint

         (*pph_parms).objnum = objnum
         (*pim_parms).lasttype = 2
         (*pim_parms).lastpos = [xcen[0],ycen[0]]
         (*pim_parms).lastmag = mag[0]
         (*pim_parms).lastfwhm = fwhm[0]


         if (*pph_parms).exact eq 0 then begin
            ; Re-draw the zoom window at the new center.
            self->draw, /WORK, /ZOOM, ZXCEN=xcen[0], ZYCEN=ycen[0]
         endif else begin
            self->draw, /WORK, /ZOOM
         endelse

         self->trk, xcen[0], ycen[0]

         ; Draw photometry aperture.
         self->apdraw,xcen[0],ycen[0]

         ; Locate the objects found by Basphote that are not new.
         found = where((onchip eq 1B) and (new eq 0B), count)

         ; Locate the objects not found (off chip).
         lost = where(onchip eq 0B, lostcount)

         if count gt 3 then begin
            ; Compute the difference vectors to be used for the stats..
            dx = xcen[found] - xguess[found]
            dy = ycen[found] - yguess[found]
            sigmadx = stdev(dx[1:count-1],meandx)
            sigmady = stdev(dy[1:count-1],meandy)
         endif else begin
            ; Too few positions to obtain stats.
            meandx  = 0
            meandy  = 0
            sigmadx = 0
            sigmady = 0
         endelse

         print, 'Template sigmas: ', sigmadx, sigmady

         if lostcount gt 0 then begin
            ; Fix the positions of the lost object(s) by adjusting the last
            ; known position(s).
            xcen[lost] = xguess[lost] + meandx
            ycen[lost] = yguess[lost] + meandy
         endif

         ; Old position.
         oldx = tpx[1:numobj-1]
         oldy = tpy[1:numobj-1]

         ; New position.
         newx = xcen[1:numobj-1]
         newy = ycen[1:numobj-1]

         ; Differences.
         dx = newx - oldx
         dy = newy - oldy

         ; Make a sequence string vector.
         seqnum = string(indgen(numobj), format='(I2)')
         seqnum = seqnum[1:numobj-1]

         newpos = where(new eq 1B, newcount)

         ; Make a flags string vector.
         flags = replicate(' ', numobj)
         if lostcount gt 0 then flags[lost]='*'
         if newcount  gt 0 then flags[newpos]='+'
         flags = flags[ 1:numobj-1 ]

         ; Define a threshold value for the computed sigmas.
         sigmathresh = 0.5

         ; Test for sigmas greater than this threshold.
         if sigmadx gt sigmathresh or sigmady gt sigmathresh then begin
            t = [$
            '  The positions of objects in the active template have changed,',$
            'relative to each other, by an amount which may be too large.',$
            'Template corruption is possible.', $
            '  Please review the following table of positions and decide',$
            'whether to accept or ignore the new positions.', $
            '  The table lists the last known locations, the new locations,',$
            'and their differences.', $
            '        ', $
            '   ------ Last known (x,y) ----- New (x,y) --------- differences',$
            '     (' + string(tpx[0], format='(F10.3)') + ',' + $
                    string(tpy[0], format='(F8.3)') + ')' + $
            '  (' + string(xcen[0], format='(F10.3)') + ',' + $
                    string(ycen[0], format='(F8.3)') + ')' + $
            '  (' + string(xcen[0]-tpx[0], format='(F10.3)') + ',' + $
                    string(ycen[0]-tpy[0], format='(F8.3)') + $
                            ')  (anchor)', $
            '        ', $
            seqnum + ' ' + $
            '  (' + string(oldx, format='(F10.3)') + ',' + $
                    string(oldy, format='(F8.3)') + ')' + $
            '  (' + string(newx, format='(F10.3)') + ',' + $
                    string(newy, format='(F8.3)') + ')' + $
            '  (' + string(dx, format='(F10.3)') + ',' + $
                    string(dy, format='(F8.3)') + ')     ' + flags,$
            ' ', $
            '           Legend: * offchip, + new object']

;           result = qannounc(t, TITLE='Template Object Verification',$
;              FALSELABEL='Ignore, avoid template corruption', $
;              TRUELABEL='Accept new object locations', $
;              XSIZE=max(strlen(t)), ysize=16, GROUP_LEADER=self.tlb)

            result = itool_tpannounce(self, t, xcen, ycen,$
               TITLE='Template Object Verification',$
               FALSELABEL='Ignore, avoid template corruption', $
               TRUELABEL='Accept new object locations', $
               XSIZE=max(strlen(t)), ysize=16, GROUP_LEADER=self.tlb)

            if result eq 0 then begin
               (*pim_parms).lasttype = 0
               return
            endif
         endif

         ; Update the template to the new positions.
         csz = size(xcen)

         if csz[0] ne 2 then begin
            tpx = xcen
            tpy = ycen
         endif else begin
            tpx = xcen[*,csz[2]-1]
            tpy = ycen[*,csz[2]-1]
         endelse

         tplate.oref->updateobjects, new, objnam, tpx, tpy
         self.oitoolwatpmgr->refreshobjects
         self->draw, /WORK
         self->tpdraw
      end
   endcase
end


; ------------------------------------------------------------------------------
; Procedure itool::setproperty
; ------------------------------------------------------------------------------
pro itool::setproperty, OITOOLWACPMGR=oitoolwacpmgr,$
   OITOOLWAIMPARMS=oitoolwaimparms, OITOOLWAPHPARMS=oitoolwaphparms,$
   OITOOLWATPMGR=oitoolwatpmgr

   compile_opt hidden

   if keyword_set(oitoolwacpmgr) then begin
      self.oitoolwacpmgr = oitoolwacpmgr
   endif

   if keyword_set(oitoolwaimparms) then begin
      self.oitoolwaimparms = oitoolwaimparms
   endif

   if keyword_set(oitoolwaphparms) then begin
      self.oitoolwaphparms = oitoolwaphparms
   endif

   if keyword_set(oitoolwatpmgr) then begin
      self.oitoolwatpmgr = oitoolwatpmgr
   endif
end


; ------------------------------------------------------------------------------
; Procedure itool::tpdraw
; Draw template aperture in the work window
; ------------------------------------------------------------------------------
pro itool::tpdraw, ALT_TPLATE=alt_tplate, ALT_WORKWIN=alt_workwin
   compile_opt hidden

   if not xregistered('itoolwatpmgr', /NOSHOW) then return

   pstate = self.pstate
   pworkstate = (*pstate).pworkstate

   self.oitoolwatpmgr->getproperty, SELECTED=selected
   if selected lt 0L then return

   if keyword_set(alt_tplate) then begin
      tplate = alt_tplate
   endif else begin
      self.oitoolwatpmgr->getproperty, TPLATE=tplate
   endelse

   if keyword_set(alt_workwin) then begin
      winnum = alt_workwin
   endif else begin
      widget_control, (*pstate).work_view, get_value=winnum
   endelse

   ; Get the required properties of the selected or alternate template.
   tplate.oref->getproperty, MODE=mode, NUMOBJ=numobj, X=tpx, Y=tpy

   if numobj gt 0 then begin
      wset, winnum

      if (*pstate).eightbit then begin
         color = (*pstate).ncolors-1
      endif else begin
         if mode eq 0 then color='0000ff'x else color='00ff00'x
      endelse

      ; Set up plotting coordinate system.
      if !order ne 1 then begin
         plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
         xr=[0,(*pworkstate).xsize-1],yr=[0,(*pworkstate).ysize-1], $
         xstyle=5,ystyle=5,/noerase
      endif else begin
         plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
         xr=[0,(*pworkstate).xsize-1],yr=[(*pworkstate).ysize-1,0], $
         xstyle=5,ystyle=5,/noerase
      endelse

      setusym,-1
      data2work,pstate,tpx,tpy,wtpx,wtpy
      oplot,wtpx,wtpy,psym=8,color=color,symsize=2.0
      oplot,[wtpx[0]],[wtpy[0]],psym=8,color=color,symsize=3.0
      setusym,1
   endif
end

; ------------------------------------------------------------------------------
; Procedure itool::trk
; Update the cursor tracking widgets.
; ------------------------------------------------------------------------------
pro itool::trk, in_x, in_y
   compile_opt hidden

   pstate = self.pstate
   self.oimage->getproperty, PIM_PARMS=pim_parms
   x = fix(in_x+0.5)
   y = fix(in_y+0.5)
   pdraw_state = (*pstate).pdraw_state

   pimage = (*pim_parms).imageptr

   ; Clip to the bounds of the data array.
   if x lt 0 or x ge (*pim_parms).xsize then return
   if y lt 0 or y ge (*pim_parms).ysize then return

   count = (*pimage)[x,y,(*pim_parms).frame]

;  curpos = string('xloc= ', x, '   yloc= ', y, $
;                  '   count= ', count, format='(A,I4,A,I4,A,G0.0)')

   widget_control, (*pdraw_state).curxposid, UPDATE=0

   widget_control, (*pdraw_state).curxposid, SET_VALUE=string(x,$
      FORMAT='(I12)')

   widget_control, (*pdraw_state).curxposid, UPDATE=1

   widget_control, (*pdraw_state).curyposid, UPDATE=0

   widget_control, (*pdraw_state).curyposid, SET_VALUE=string(y,$
      FORMAT='(I12)')

   widget_control, (*pdraw_state).curyposid, UPDATE=1

   if abs(count) lt 1.0 or abs(count) gt 99999.0 then begin
      fmtc = '(E)'
   endif else begin
      fmtc = '(G)'
   endelse

   widget_control, (*pdraw_state).curcounid, UPDATE=0

   widget_control, (*pdraw_state).curcounid,$
      SET_VALUE=string(count, FORMAT=fmtc)

   widget_control, (*pdraw_state).curcounid, UPDATE=1
end


; ---------------------------------------------------------------------------
; Procedure itool::realize
; ---------------------------------------------------------------------------
pro itool::realize, ARG_CLEANUP=arg_cleanup, NO_BLOCK=no_block
   compile_opt hidden

   if not obj_valid(self) then return

   self.arg_cleanup = keyword_set(arg_cleanup)

   if not widget_info(self.tlb, /valid) then return
   if widget_info(self.tlb, /REALIZED) then return

   widget_control, self.tlb, /REALIZE
   self->nextimage, /FIRST

   if keyword_set(no_block) then self.no_block=no_block

   xmanager, 'itool', self.tlb, NO_BLOCK=no_block, CLEANUP='itool_cleanup'
end


; ---------------------------------------------------------------------------
; Function itool::init
; ---------------------------------------------------------------------------
function itool::init, oimage,$
   EXPERT=expert,$
   FVISIBLE=fvisible,$
   GROUP_LEADER=group_leader,$
   NODISMISS=nodismiss,$
   PHOTPARMFILE=photparmfile,$
   SCLMIN=sclmin,$
   SCLMAX=sclmax,$
   TMPLFILE=tmplfile,$
   WXVISIBLE=wxvisible,$
   WYVISIBLE=wyvisible,$
   WZOOMFACT=wzoomfact,$
   ZVISIBLE=zvisible

   if n_params() ne 1 then begin
      print, 'Missing oimage argument. itool cannot be started.'
      return, 0
   endif

   if (!d.flags and 256) eq 0 then begin
      print, 'No windowing device. itool cannot be started.'
      return, 0
   endif

   device, get_screen_size=scrsize

   if scrsize[0] gt 1300 then begin
      xvisdef = 1100
      yvisdef = 900
   endif else if scrsize[0] gt 1152 then begin
      xvisdef = 900
      yvisdef = 640
   endif else if scrsize[0] gt 1024 then begin
      xvisdef = 800
      yvisdef = 550
   endif else if scrsize[0] gt 768 then begin
      xvisdef = 500
      yvisdef = 500
   endif else begin
      xvisdef = 400
      yvisdef = 400
   endelse

   if not keyword_set(fvisible) then fvisible=128
   if fvisible eq 0 then fvisible=128

   if not keyword_set(wxvisible) then wxvisible=xvisdef
   if wxvisible eq 0 then wxvisible=xvisdef

   if not keyword_set(wyvisible) then wyvisible=yvisdef
   if wyvisible eq 0 then wyvisible=yvisdef

   if not keyword_set(zvisible) then zvisible=128
   if zvisible eq 0 then zvisible=128

   if !d.n_colors le 256 then begin
      ncolors=!d.n_colors
      eightbit = 1
      device, decomposed=0
   endif else begin
      ncolors=256
      eightbit = 0
      device, decomposed=1
   endelse

   state = {$
      airmasid:0L,$                 ; Airmass label widget id.
      animateid:0L,$                ; Animate button id.
      auto1id:0L, auto2id:0L,$      ; Stretch control menu button id.
      disp2id:0L,$                  ; Stretch control menu button id.
      dispmaxid:0L,$                ; Widget id's for stretch min and max.
      dispminid:0L,$
      eightbit:eightbit,$          ; Flag, if true, 8-bit display
      exptimid:0L,$                 ; Exposure time label widget id.
      expert:keyword_set(expert),$  ; Flag. Turns on certain "expert" modes.
      extrema1id:0L,$
      extrema2id:0L,$
      filterid:0L,$                 ; Filter label widget id.
      fmtg:'(G12.6)',$              ; Image info format.
      fmti:'(I12)',$                ; Image info format.
      font:'8x13',$                 ; Font for some label and text widgets.
      frameid:0L,$                  ; Frame number widget id.
      full_view:0L,$
      im_xsize:0,$
      im_ysize:0,$
      imageminid:0L, imagemaxid:0L,$
      imfileid:0L,$                 ; Image file name widget id.
      ipmgrid:0L,$
      ncolors:ncolors,$             ; Number of colors for display
      nextframeid:0L,$              ; ID of NextFrame button.
      nframesid:0L,$
      objectid:0L,$                 ; Object label widget id.
      p_oprofiles:ptr_new(objarr(5)),$
      pdraw_state:ptr_new(),$
      pfullstate:ptr_new(),$
      pworkstate:ptr_new(),$
      pzoomstate:ptr_new(),$
      savplt:!p,$                   ; Save area for plotting environment.
      scroll:0B,$
      tempedit:0,$                  ; Template edit flag.
      tlb:0L,$
      tmplfile:'',$                 ; Name of template manager file.
      utdateid:0L,$                 ; UT Date label widget id.
      uttimeid:0L,$                 ; UT Time label widget id.
      work_view:0L,$
      zoom_view:0L,$
      zoomvid:0L$
   }

   oimage->getproperty, PIM_PARMS=pim_parms
   xsize = (*pim_parms).xsize
   ysize = (*pim_parms).ysize

   ; Size of image array.
   state.im_xsize = xsize
   state.im_ysize = ysize

   ; Create the main base.
   tlb = widget_base(TITLE='itool', COLUMN=1, GROUP_LEADER=group_leader,$
      MBAR=mbar)

   ; Initialize the draw state structure.
   draw_state = {$
      curxposid:0L,$
      curyposid:0L,$
      curcounid:0L,$
      mainstash:0L,$
      trkflg:0B $
   }

   ; Initialize the photometry parameters structure.
   ph_parms = {$
      boxmrad:10.0, $
      edtflg:0B, $
      exact:0, $
      gain:1.0, $
      logfile:'phot.log', $
      nomext:0.0, $
      objnum:0, $
      parmfile:'', $
      parmfilever:'phot_parms_v02', $
      platescale:0.726, $
      rdnoise:10.0, $
      radius:5.0, $
      sky1:10.0, $
      sky2:50.0,$
      zpoint:0.0 $
   }

   ; **************************************************************************
   if keyword_set(photparmfile) then ph_parms.parmfile=photparmfile

   if keyword_set(tmplfile) then state.tmplfile=tmplfile


   ; File Menu.
   filemenuid = widget_button(mbar, VALUE='File', /MENU)

   dummy = widget_button(filemenuid, VALUE='Hard Copy',$
      EVENT_PRO='itool_hdcpy_event')

   dummy = widget_button(filemenuid, VALUE='Save Image to FITS',$
      EVENT_PRO='itool_savefits_event')

   dummy = widget_button(filemenuid, VALUE='Save Thumbnail Window to JPEG',$
      EVENT_PRO='itool_savejpeg_event', UNAME='full')

   dummy = widget_button(filemenuid, VALUE='Save Work Window to JPEG',$
      EVENT_PRO='itool_savejpeg_event', UNAME='work')

   dummy = widget_button(filemenuid, VALUE='Save Zoom Window to JPEG',$
      EVENT_PRO='itool_savejpeg_event', UNAME='zoom')

   dummy  = widget_button(filemenuid, VALUE='Dismiss',$
      EVENT_PRO='itool_dismiss_event',$
      SENSITIVE=(n_elements(nodismiss) eq 0L))

   dummy = widget_button(filemenuid, VALUE='Exit',$
      EVENT_PRO='itool_exit_event',$
      SENSITIVE=(n_elements(nodismiss) ne 0L))


   ; Params Menu.
   paramsmenuid = widget_button(mbar, VALUE='Parameters', /MENU)

   dummy = widget_button(paramsmenuid, VALUE='Image Parameters',$
      EVENT_PRO='itool_ipbutton_event')

   dummy = widget_button(paramsmenuid, VALUE='Photometry Parameters',$
      EVENT_PRO='itool_phpbutton_event')


   ; Tools Menu.
   toolsmenuid = widget_button(mbar, VALUE='Tools', /MENU)

   dummy = widget_button(toolsmenuid, VALUE='Comet Phot Mgr',$
      EVENT_PRO='itool_cpmbutton_event')

   dummy = widget_button(toolsmenuid, VALUE='Template Mgr',$
      EVENT_PRO='itool_tpmgr_event')

   dummy = widget_button(toolsmenuid, VALUE='Pixel Editor',$
      EVENT_PRO='itool_pixed_event')

   dummy = widget_button(toolsmenuid, VALUE='Profiles',$
      EVENT_PRO='itool_pfiles_event')

   dummy = widget_button(toolsmenuid, VALUE='Profile Trace',$
      EVENT_PRO='itool_ptrace_event')

   ; Colors Menu.
   colorsmenuid = widget_button(mbar, VALUE='Colors', /MENU)

   dummy = widget_button(colorsmenuid, VALUE='Color Table',$
      EVENT_PRO='itool_coltab_event', SENSITIVE=eightbit)

   dummy = widget_button(colorsmenuid, VALUE='Edit Palette',$
      EVENT_PRO='itool_colpal_event', SENSITIVE=eightbit)


   ; Stretch Menu.
   stretchmenuid = widget_button(mbar, VALUE='Stretch', /MENU)

   state.auto1id = widget_button(stretchmenuid, $
      VALUE='Set display stretch to computed values (current frame)',$
      EVENT_PRO='itool_auto1_event')

   state.extrema1id = widget_button(stretchmenuid, $
      VALUE='Set display stretch to image extrema (current frame)',$
      EVENT_PRO='itool_extrema1_event')

   state.extrema2id = widget_button(stretchmenuid, $
      VALUE='Set display stretch to image extrema (all frames)',$
      EVENT_PRO='itool_extrema2_event')

   state.auto2id = widget_button(stretchmenuid, $
      VALUE='Set display stretch to computed values (all frames)',$
      EVENT_PRO='itool_auto2_event')

   state.disp2id = widget_button(stretchmenuid,$
      VALUE='Copy display stretch to all frames',$
      EVENT_PRO='itool_disp2_event')


   ; Animate Menu.
   animatemenuid = widget_button(mbar, VALUE='Animate', /MENU)

   state.animateid = widget_button(animatemenuid, VALUE='Start',$
      EVENT_PRO='itool_animate_event')
 

   ; Cursor Menu.
   cursormenuid = widget_button(mbar, VALUE='Cursor', /MENU)

   freezeid = widget_button(cursormenuid, VALUE='Freeze',$
      EVENT_PRO='itool_trk_event', SENSITIVE=0, UNAME='freeze')

   trackid = widget_button(cursormenuid, VALUE='Track',$
      EVENT_PRO='itool_trk_event', SENSITIVE=1, UNAME='track')

   ; Put copies of the opposite widget ids.
   widget_control, freezeid, SET_UVALUE=trackid
   widget_control, trackid, SET_UVALUE=freezeid


   maxvalue = string(0.0, format=state.fmtg)
   minvalue = string(0.0, format=state.fmtg)
   curmax   = string(0.0, format=state.fmtg)
   curmin   = string(0.0, format=state.fmtg)
   nframes  = string(0L,  format=state.fmti)
   framen   = string(0L,  format=state.fmti)

   ; Image stats section.
   imstatbase = widget_base(tlb, ROW=1)

   wbx = widget_base(imstatbase, COLUMN=1, FRAME=1, /BASE_ALIGN_CENTER)
   wbc = widget_base(wbx, ROW=1)
   w1 = widget_label(wbc, VALUE='--- Image ---')
   wbc = widget_base(wbx, COLUMN=2)
   w1 = widget_label(wbc, VALUE='Max:   ', /ALIGN_LEFT)
   w1 = widget_label(wbc, VALUE='Min:   ', /ALIGN_LEFT)
   w1 = widget_label(wbc, VALUE='Frames:', /ALIGN_LEFT)
   state.imagemaxid = widget_label(wbc, VALUE=maxvalue, /ALIGN_RIGHT)
   state.imageminid = widget_label(wbc, VALUE=minvalue, /ALIGN_RIGHT)
   state.nframesid = widget_label(wbc, VALUE=nframes, /ALIGN_RIGHT)

   wbc = widget_base(wbx, ROW=1)

   state.nextframeid = widget_button(wbc, VALUE='Next Frame',$
      EVENT_PRO='itool_nextframe_event')


   ; Display stats section.
   wbx = widget_base(imstatbase, COLUMN=1, FRAME=1, /BASE_ALIGN_CENTER)
   wbc = widget_base(wbx, ROW=1)
   w1  = widget_label(wbc, VALUE='--- Display ---')
   wbc = widget_base(wbx, ROW=1)
   w1  = widget_label(wbc, VALUE='Max:  ')

   state.dispmaxid = widget_text(wbc,  VALUE=curmax, /EDITABLE, XSIZE=12,$
      EVENT_PRO='itool_dispmax_event')

   wbc = widget_base(wbx, ROW=1)
   w1  = widget_label(wbc, VALUE='Min:  ')

   state.dispminid = widget_text(wbc,  VALUE=curmin, /EDITABLE, XSIZE=12,$
      EVENT_PRO='itool_dispmin_event')

   wbc = widget_base(wbx, ROW=1)
   w1  = widget_label(wbc, VALUE='Frame:')

   state.frameid = widget_text(wbc,  VALUE=framen, /EDITABLE, XSIZE=12,$
      EVENT_PRO='itool_frame_event')


   widget_control, state.frameid, SENSITIVE=0
   ; ---------------------


   ; Cursor Section.
   wbx = widget_base(imstatbase, COLUMN=1, FRAME=1, /BASE_ALIGN_CENTER)
   wbc = widget_base(wbx, ROW=1)
   w1  = widget_label(wbc, VALUE='--- Cursor ---')
   wbc = widget_base(wbx, COLUMN=2)
   w1 = widget_label(wbc, VALUE='X:    ', /ALIGN_LEFT)
   w1 = widget_label(wbc, VALUE='Y:    ', /ALIGN_LEFT)
   w1 = widget_label(wbc, VALUE='Count:', /ALIGN_LEFT)

   draw_state.curxposid = widget_label(wbc, VALUE='       ', /ALIGN_RIGHT,$
      /DYNAMIC_RESIZE)

   draw_state.curyposid = widget_label(wbc, VALUE='       ', /ALIGN_RIGHT,$
      /DYNAMIC_RESIZE)

   draw_state.curcounid = widget_label(wbc, VALUE='       ', /ALIGN_RIGHT,$
      /DYNAMIC_RESIZE)

   drawbase = widget_base(tlb, /ROW)

   ; Compute the full draw window parameters.
   fullstate = {name:'full', $
      dx:0, $
      dy:0, $
      spx:0, $
      spy:0,  $
      xcen:0.0, $
      ycen:0.0, $
      xoff:0, $
      yoff:0, $
      xsize:xsize, $
      ysize:ysize, $
      xvisible:fvisible, $
      yvisible:fvisible, $
      zfact:1.0}

   sf = max([xsize/fvisible, ysize/fvisible]) + 1
   nx = xsize / sf
   ny = ysize / sf

   fullstate.dx = nx * sf
   fullstate.dy = ny * sf
   fullstate.zfact = 1.0 / sf

   fullstate.xoff = (fvisible - nx) / 2
   fullstate.yoff = (fvisible - ny) / 2

   ; Compute the work window parameters.
   workstate = {name:'work', $
      dx:xsize, $
      dy:ysize, $
      spx:0, $
      spy:0, $
      xcen:0.0, $
      ycen:0.0, $
      xoff:0, $
      yoff:0, $
      xsize:xsize, $
      ysize:ysize, $
      xvisible:wxvisible, $
      yvisible:wyvisible, $
      zfact:1.0}

   zfact = float(max([xsize/wxvisible, ysize/wyvisible]))

   if zfact lt 1.0 then begin
      ; The image size is less than the default size of the viewport.
      ; Determine if the image should be expanded (not more than a factor of
      ; 3, unless fixed by WZOOMFACT keyword).

      zfact = float(min([wxvisible/xsize, wyvisible/ysize]))
      if zfact gt 6.0 then zfact=6.0

      if keyword_set(wzoomfact) then begin
         if wzoomfact gt 0 then zfact=wzoomfact
      endif

      if zfact lt 1.0 then zfact=1.0

      workstate.xsize = zfact * xsize
      workstate.ysize = zfact * ysize

      workstate.xvisible = workstate.xsize
      workstate.yvisible = workstate.ysize
      workstate.zfact = zfact
   endif

   ; Compute the zoom draw window parameters.
   zoomstate = {name:'zoom', $
      dx:0, $
      dy:0, $
      pzoombuf:ptr_new(/ALLOCATE_HEAP),$
      spx:0, $
      spy:0,  $
      xcen:0.0, $
      ycen:0.0, $
      xoff:0, yoff:0, $
      xsize:xsize, $
      ysize:ysize, $
      xvisible:zvisible, $
      yvisible:zvisible, $
      zfact:workstate.zfact*2.0}

   zoomstate.xcen = xsize / 2
   zoomstate.ycen = ysize / 2

   retain = 2

   ; Define the full-view draw window.
   wb1 = widget_base(drawbase, /COLUMN)

   state.full_view = widget_draw(wb1, /BUTTON_EVENTS,$
      EVENT_PRO='itool_fullview_event',$
      RETAIN=retain, XSIZE=fvisible, YSIZE=fvisible)

   state.pfullstate = ptr_new(fullstate, /NO_COPY)

   ; Define the zoom draw window.
   state.zoom_view = widget_draw(wb1, /BUTTON_EVENTS,$
      /KEYBOARD_EVENTS,$
      EVENT_PRO='itool_zoomview_event',$
      /MOTION_EVENTS, RETAIN=retain,$
      XSIZE=zvisible, YSIZE=zvisible)

   dummy = widget_button(wb1, VALUE='Zoom+',$
      EVENT_PRO='zoomup_itool_event')

   dummy = widget_button(wb1, VALUE='Zoom-',$
      EVENT_PRO='zoomdn_itool_event')

   zoombase = widget_base(wb1, /ROW)
   zlabel   = widget_label(zoombase, VALUE='Zoom factor:')

   state.zoomvid = widget_label(zoombase, $
      VALUE=string(zoomstate.zfact, format='(I3)'))

   state.pzoomstate = ptr_new(zoomstate, /NO_COPY)

   dummy = widget_label(wb1, VALUE=' ')
   state.objectid = widget_label(wb1, VALUE='                    ',/ALIGN_LEFT)
   state.filterid = widget_label(wb1, VALUE='                    ',/ALIGN_LEFT)
   state.airmasid = widget_label(wb1, VALUE='                    ',/ALIGN_LEFT)
   state.exptimid = widget_label(wb1, VALUE='                    ',/ALIGN_LEFT)
   state.utdateid = widget_label(wb1, VALUE='                    ',/ALIGN_LEFT)
   state.uttimeid = widget_label(wb1, VALUE='                    ',/ALIGN_LEFT)

   ; Define the work draw window.
   state.scroll = (workstate.xsize gt workstate.xvisible) or $
                  (workstate.ysize gt workstate.yvisible)

   if workstate.xvisible gt workstate.xsize then begin
      workstate.xvisible=workstate.xsize
   endif

   if workstate.yvisible gt workstate.ysize then begin
      workstate.yvisible=workstate.ysize
   endif

   workbase  = widget_base(drawbase, /COLUMN)

   if state.scroll then begin
      state.work_view = widget_draw(workbase, /BUTTON_EVENTS, $
         /KEYBOARD_EVENTS,$
         EVENT_PRO='itool_workview_event',$
         /MOTION_EVENTS, RETAIN=retain, $
         /SCROLL, $
         XSIZE=workstate.xsize, $
         YSIZE=workstate.ysize, $
         X_SCROLL_SIZE=workstate.xvisible, $
         Y_SCROLL_SIZE=workstate.yvisible)
   endif else begin
      state.work_view = widget_draw(workbase, /BUTTON_EVENTS,$
         /KEYBOARD_EVENTS,$
         EVENT_PRO='itool_workview_event',$
         /MOTION_EVENTS, RETAIN=retain,$
         XSIZE=workstate.xsize,$
         YSIZE=workstate.ysize)
   endelse

   ; Label the work window with its zoom factor and image file name.
   wb = widget_base(workbase, /ROW)
   w1 = widget_label(wb, VALUE='Zoom factor:')
   w1 = widget_label(wb, VALUE=string(workstate.zfact, format='(I3)'))

   ; Load photometry parameters from a file?
   if ph_parms.parmfile ne '' then begin
      itool_pplod, ph_parms, ERROR_STR=error_str

      if error_str ne '' then begin
         msg = ['The following error was encountered while attempting to',$
            'load photometry parameters from file ' + ph_parms.parmfile+' :',$
            '', error_str]

         result= qannounc(error_str,$
            TITLE='Error', FALSELABEL='', TRUELABEL='Dismiss',$
            XSIZE=max(strlen(msg)), GROUP_LEADER=group_leader)
      endif
   endif

;  if state.scroll then begin
;     xoff = max([0, (xsize-workstate.xvisible)/2 ])
;     yoff = max([0, (ysize-workstate.yvisible)/2 ])
;     widget_control, state.work_view, SET_DRAW_VIEW=[xoff, yoff]
;  endif

   state.pworkstate = ptr_new(workstate, /NO_COPY)
   self.pph_parms = ptr_new(ph_parms, /NO_COPY)

   ; Create a pointer to the draw_state structure and store it into the
   ; user value of the drawbase widget.
   state.pdraw_state = ptr_new(draw_state, /NO_COPY)

   self.expert = keyword_set(expert)
   self.oimage = oimage
   state.tlb = tlb
   self.tlb = tlb

   ; Create a pointer to the state structure and put a copy into this
   ; instance of the "itool" object class.
   self.pstate = ptr_new(state, /NO_COPY)

   ; Store the object reference of this instance into the UVALUE of the
   ; top-level-base.
   widget_control, tlb, SET_UVALUE=self

   return, 1
end



; -------------- Support routines ---------------------------------------------
; Convert from work-window coordinates to data coordinates.
pro work2data,pstate,x,y,out_x,out_y
   pworkstate = (*pstate).pworkstate
   x0 = (*pworkstate).spx - 0.5
   y0 = (*pworkstate).spy - 0.5
   sf = 1.0 / (*pworkstate).zfact
   out_x = x0 + sf * (x-(*pworkstate).xoff)
   out_y = y0 + sf * (y-(*pworkstate).yoff)
end


; Convert from data coordinates to work-window coordinates.
pro data2work,pstate,x,y,out_x,out_y
   pworkstate = (*pstate).pworkstate
   x0 = (*pworkstate).spx - 0.5
   y0 = (*pworkstate).spy - 0.5
   out_x = (*pworkstate).xoff + (x-x0) * (*pworkstate).zfact
   out_y = (*pworkstate).yoff + (y-y0) * (*pworkstate).zfact
end


; Convert from zoom-window coordinates to data coordinates.
pro zoom2data,pstate,x,y,out_x,out_y
   pzoomstate = (*pstate).pzoomstate
   x0 = (*pzoomstate).spx - 0.5
   y0 = (*pzoomstate).spy - 0.5
   sf = 1.0 / (*pzoomstate).zfact
   out_x = x0 + sf * (x-(*pzoomstate).xoff)
   out_y = y0 + sf * (y-(*pzoomstate).yoff)
end


; Convert from data coordinates to zoom-window coordinates.
pro data2zoom,pstate,x,y,out_x,out_y
   pzoomstate = (*pstate).pzoomstate
   x0 = (*pzoomstate).spx - 0.5
   y0 = (*pzoomstate).spy - 0.5
   out_x = (*pzoomstate).xoff + (x-x0) * (*pzoomstate).zfact
   out_y = (*pzoomstate).yoff + (y-y0) * (*pzoomstate).zfact
end
; -------------^ Support routines ^--------------------------------------------



; -------------- Xmanager Cleanup ---------------------------------------------

; ----------------------------------------------------------------------------
; Procedure itool_cleanup
; Perform all required clean-up chores, such as freeing pointer variables.
; ----------------------------------------------------------------------------
pro itool_cleanup, tlb
   compile_opt hidden

   ; Clean up pointers.
   widget_control, tlb, GET_UVALUE=oitool
   oitool->getproperty, PSTATE=pstate
   pzoomstate = (*pstate).pzoomstate

   ptr_free, (*pzoomstate).pzoombuf

   ptr_free, (*pstate).pfullstate, (*pstate).pworkstate,$
      (*pstate).pzoomstate, (*pstate).pdraw_state

   ; Clean up profiles stuff, if needed.
   if ptr_valid((*pstate).p_oprofiles) then ptr_free, (*pstate).p_oprofiles

   obj_destroy, oitool
end


; -------------- Event Handlers -----------------------------------------------

; -----------------------------------------------------------------------------
; Procedure itool_exit_event
; -----------------------------------------------------------------------------
pro itool_exit_event, event
   compile_opt hidden

;  widget_control, event.top, GET_UVALUE=oitool
;  oitool->getproperty, STATUS=status

;  con = qannounc(status.msg, TITLE='itool exit confirmation',$
;     FALSELABEL='Cancel request',$
;     TRUELABEL='Discard changes and exit', GROUP_LEADER=event.top,$
;     XSIZE=max(strlen(status.msg)))

;  if not con then return

   widget_control, event.top, /destroy
end


; -----------------------------------------------------------------------------
; Procedure itool_fullview_event
; -----------------------------------------------------------------------------
pro itool_fullview_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   pfullstate = (*pstate).pfullstate
   pworkstate = (*pstate).pworkstate

   if (*pfullstate).zfact ne 1.0 then begin
      x = (*pfullstate).spx + (event.x - (*pfullstate).xoff) /$
      (*pfullstate).zfact

      y = (*pfullstate).spy + (event.y - (*pfullstate).yoff) /$
      (*pfullstate).zfact
   endif else begin
      x = (*pfullstate).spx + (event.x - (*pfullstate).xoff)
      y = (*pfullstate).spy + (event.y - (*pfullstate).yoff)
   endelse

   if !order ne 0 then begin
      y = (*pfullstate).ysize - 1 - y
   endif

   case event.press of
      2 : begin
         if ((*pstate).scroll) then begin
            xoff = x - (*pworkstate).xvisible / 2
            yoff = y - (*pworkstate).yvisible / 2

            if !order ne 0 then begin
               yoff=(*pim_parms).ysize-(y + (*pworkstate).yvisible / 2)
            endif

            if xoff lt 0 then xoff = 0
            if yoff lt 0 then yoff = 0
            widget_control, (*pstate).work_view, SET_DRAW_VIEW=[xoff, yoff]
         endif
      end

      else : begin
      end
   endcase
end


; -----------------------------------------------------------------------------
; Procedure itool_workview_event
; -----------------------------------------------------------------------------
pro itool_workview_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   pworkstate = (*pstate).pworkstate
   pzoomstate = (*pstate).pzoomstate
   pdraw_state = (*pstate).pdraw_state

   if !order ne 0 then begin
      eventy = (*pworkstate).ysize-event.y-1
   endif else begin
      eventy = event.y
   endelse

   work2data,pstate,event.x,eventy,datax,datay

   ; At this point, datax and datay are in data coordinates, which may
   ; lie outside of the image array bounds.

   case event.type of
      0 : begin
         ; Mouse button pressed.
         case event.press of
            1 : begin
               ; Left Button (template or comet photometry).

               if xregistered('itoolwatpmgr', /NOSHOW) then begin
                  ; Template photometry.
                  oitool->phact, datax, datay
               endif

               if xregistered('itoolwacpmgr', /NOSHOW) then begin
                  ; Refresh Comet Photometry Manager.
                  oitool->getproperty, OITOOLWACPMGR=oitoolwacpmgr
                  oitoolwacpmgr->addvalue, {x:datax, y:datay}
                  (*pim_parms).lasttype = 0
               endif
            end

            2 : begin
               ; Middle button (zoom at new center).
               oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
               oitool->trk, (*pzoomstate).xcen, (*pzoomstate).ycen
            end

            4 : begin
               ; Right button (single-object photometry).
               oitool->oneph, datax, datay
            end

            else : begin
            end
         endcase
      end

      2 : begin
         ; Motion event.
         if (*pdraw_state).trkflg then begin
            ; Refresh the cursor tracking widgets.
            oitool->trk, datax, datay
         endif

         if event.press eq 2 then begin
            ; Re-draw the zoom window at the new center.
            oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
         endif
      end

      5 : begin
         ; ASCII-keyboard event.
         ; Want release events only.
         if event.release eq 0B then return

         if event.ch eq byte('h') or event.ch eq byte('H') then begin
            ; This is a "h" event. Center the zoom window at this event's
            ; cursor position, put the new x,y positions into the cursor-
            ; tracking label widgets, and launch a new profiles widget at the
            ; new x,y location. Ask the profiles widget to plot a histogram.
            oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
            oitool->trk, (*pzoomstate).xcen, (*pzoomstate).ycen
            oitool->newprofile, /HISTOGRAM
            return
         endif

         if event.ch eq byte('p') or event.ch eq byte('P') then begin
            ; This is a "p" event. Center the zoom window at this event's
            ; cursor position, put the new x,y positions into the cursor-
            ; tracking label widgets, and launch a new profiles widget at the
            ; new x,y location.
            oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
            oitool->trk, (*pzoomstate).xcen, (*pzoomstate).ycen
            oitool->newprofile
            return
         endif

         if event.ch eq byte('z') or event.ch eq byte('Z') then begin
            ; This is a "z" event. Center the zoom window at this event's
            ; cursor position and put the new x,y positions into the cursor-
            ; tracking label widgets
            oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
            oitool->trk, (*pzoomstate).xcen, (*pzoomstate).ycen
         endif
      end

      else : begin
      end
   endcase
end


; -----------------------------------------------------------------------------
; Procedure itool_zoomview_event
; -----------------------------------------------------------------------------
pro itool_zoomview_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   pzoomstate = (*pstate).pzoomstate
   pdraw_state = (*pstate).pdraw_state

   if !order ne 0 then begin
      eventy = (*pzoomstate).ysize-event.y-1
   endif else begin
      eventy = event.y
   endelse

   ; Convert from zoom coordinates to data coordinates.
   zoom2data,pstate,event.x,eventy,datax,datay

   ; At this point, datax and datay are in data coordinates, which may
   ; lie outside of the image array bounds.

   case event.type of
      0 : begin
         ; Mouse button pressed.
         case event.press of
            1 : begin
               ; Left Button (template or comet photometry).

               if xregistered('itoolwatpmgr', /NOSHOW) then begin
                  ; Template photometry.
                  oitool->phact, datax, datay
               endif

               break

               if xregistered('itoolwacpmgr', /NOSHOW) then begin
                  ; Refresh Comet Photometry Manager.
                  oitool->getproperty, OITOOLWACPMGR=oitoolwacpmgr
                  oitoolwacpmgr->refresh, {x:datax, y:datay}
                  (*pim_parms).lasttype = 0
               endif
            end

            2 : begin
               ; Middle button (zoom at new center).
               oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
               oitool->trk, (*pzoomstate).xcen, (*pzoomstate).ycen
            end

            4 : begin
               ; Right button (single-object photometry).
               oitool->oneph,datax,datay
            end

            else : begin
            end
         endcase
      end

      2 : begin
         ; Motion event.
         if (*pdraw_state).trkflg then begin
            ; Refresh the cursor tracking widgets.
            oitool->trk, datax, datay
         endif

         if event.press eq 2 then begin
            ; Re-draw the zoom window at the new center.
            oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
         endif
      end

      5 : begin
         ; ASCII-keyboard event.
         ; Want release events only.
         if event.release eq 0B then return

         if event.ch eq byte('h') or event.ch eq byte('H') then begin
            ; This is a "h" event. Center the zoom window at this event's
            ; cursor position, put the new x,y positions into the cursor-
            ; tracking label widgets, and launch a new profiles widget at the
            ; new x,y location. Ask the profiles widget to plot a histogram.
            oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
            oitool->trk, (*pzoomstate).xcen, (*pzoomstate).ycen
            oitool->newprofile, /HISTOGRAM
            return
         endif

         if event.ch eq byte('p') or event.ch eq byte('P') then begin
            ; This is a "p" event. Center the zoom window at this event's
            ; cursor position, put the new x,y positions into the cursor-
            ; tracking label widgets, and launch a new profiles widget at the
            ; new x,y location.
            oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
            oitool->trk, (*pzoomstate).xcen, (*pzoomstate).ycen
            oitool->newprofile
            return
         endif

         if event.ch eq byte('z') or event.ch eq byte('Z') then begin
            ; This is a "z" event. Center the zoom window at this event's
            ; cursor position and put the new x,y positions into the cursor-
            ; tracking label widgets
            oitool->draw, /WORK, /ZOOM, ZXCEN=datax, ZYCEN=datay
            oitool->trk, (*pzoomstate).xcen, (*pzoomstate).ycen
         endif
      end

      else : begin
      end
   endcase
end


; ----------------------------------------------------------------------------
; Procedure itool_animate_event
; Event handler for the Animate button.
; ----------------------------------------------------------------------------
pro itool_animate_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   ; Start animation procedure.
   imfile = (*pim_parms).imfile
   nframes = (*pim_parms).nframes
   xsize = (*pim_parms).xsize
   ysize = (*pim_parms).ysize

   pworkstate = (*pstate).pworkstate
   xs = (*pworkstate).zfact * xsize
   ys = (*pworkstate).zfact * ysize

   title = 'Itool Animation'
   if imfile ne '' then title = title + ' - image file ' + imfile

   xinteranimate, GROUP=event.top, SET=[xs, ys, nframes], /SHOWLOAD, $
                  TITLE=title

   for j=0, nframes-1 do begin
      if (*pim_parms).asis[j] eq 0 then oitool->cstr, j, /SILENT
      pimage = (*pim_parms).imageptr

      xinteranimate, FRAME=j, GROUP=event.top, IMAGE=$
         bytscl(rebin((*pimage)[*,*,j], xs, ys, /SAMPLE), $
            min=(*pim_parms).curmin[j], max=(*pim_parms).curmax[j], $
            TOP=(*pstate).ncolors-1)
   endfor

   xinteranimate, 50, GROUP=event.top
end


; ----------------------------------------------------------------------------
; Procedure itool_auto1_event
; Event handler for the current-frame stretch button.
; ----------------------------------------------------------------------------
pro itool_auto1_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   ; Set display stretch to computed values (current frame).
   frame = (*pim_parms).frame
   oitool->cstr, frame, /SILENT
   (*pim_parms).asis[frame] = 0B
   (*pim_parms).curmin[frame] = (*pim_parms).sclmin[frame]
   (*pim_parms).curmax[frame] = (*pim_parms).sclmax[frame]

   ; Refresh all of the widgets.
   oitool->display
   oitool->draw, /FULL, /ZOOM, /WORK
end


; ----------------------------------------------------------------------------
; Procedure itool_auto2_event, event
; Event handler for the all-frames stretch button.
; ----------------------------------------------------------------------------
pro itool_auto2_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   ; Set display stretch to computed values (all frames).

   for frame=0, (*pim_parms).nframes-1 do begin
      oitool->cstr,  frame, /SILENT
      (*pim_parms).asis[frame]   = 0B
      (*pim_parms).curmin[frame] = (*pim_parms).sclmin[frame]
      (*pim_parms).curmax[frame] = (*pim_parms).sclmax[frame]
   endfor

   ; Refresh all widgets.
   oitool->display
   oitool->draw, /FULL, /ZOOM, /WORK
end


; ----------------------------------------------------------------------------
; Procedure itool_colpal_event
; Event handler for the "Edit Palette" button.
; ----------------------------------------------------------------------------
pro itool_colpal_event, event
   compile_opt hidden
   xpalette, GROUP=event.top
end


; ----------------------------------------------------------------------------
; Procedure itool_coltab_event
; Event handler for the "Color Table" button.
; ----------------------------------------------------------------------------
pro itool_coltab_event, event
   compile_opt hidden
   xloadct, GROUP=event.top
end


; ----------------------------------------------------------------------------
; Procedure itool_cpmbutton_event
; Event handler for the "Comet Phot Mgr" button.
; ----------------------------------------------------------------------------
pro itool_cpmbutton_event, event
   compile_opt hidden

   if xregistered('itoolwacpmgr') then return

   if xregistered('itoolwatpmgr', /NOSHOW) then begin
      result = dialog_message(['The Comet Photometry Manager may not be',$
         'started while Template Photometry is active.'], /ERROR,$
         DIALOG_PARENT=event.top)

      return
   endif

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, NO_BLOCK=no_block, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   ; All clear to start.
   oitoolwacpmgr = obj_new('itoolwacpmgr', oitool, GROUP_LEADER=event.top)

   if obj_valid(oitoolwacpmgr) then begin
      oitoolwacpmgr->realize, NO_BLOCK=no_block
      oitool->setproperty, OITOOLWACPMGR=oitoolwacpmgr
   endif else begin
      result = dialog_message('Error starting The Comet Photometry Manager',$
         /ERROR, DIALOG_PARENT=event.top)
   endelse
end


; ----------------------------------------------------------------------------
; Procedure itool_disp2_event
; Event handler for the "Copy display stretch to all frames" button.
; ----------------------------------------------------------------------------
pro itool_disp2_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   ; Copy display stretch to all frames.
   frame = (*pim_parms).frame

   for j=0, (*pim_parms).nframes-1 do begin
      (*pim_parms).curmin[j] = (*pim_parms).curmin[frame]
      (*pim_parms).curmax[j] = (*pim_parms).curmax[frame]
      (*pim_parms).asis[j] = 1B
   endfor

   ; Refresh all widgets.
   oitool->display
   oitool->draw, /FULL, /ZOOM, /WORK
end


; ----------------------------------------------------------------------------
; Procedure itool_dispmax_event
; Event handler for the editable text widget that sets display maximum.
; ----------------------------------------------------------------------------
pro itool_dispmax_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   widget_control, event.id, get_value=value

   (*pim_parms).curmax[(*pim_parms).frame] = value[0]
   (*pim_parms).asis[(*pim_parms).frame] = 1B

   ; Refresh all widgets.
   oitool->display
   oitool->draw, /FULL, /ZOOM, /WORK
end


; ----------------------------------------------------------------------------
; Procedure itool_dispmin_event
; Event handler for the editable text widget that sets display minimum.
; ----------------------------------------------------------------------------
pro itool_dispmin_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   widget_control, event.id, get_value=value

   (*pim_parms).curmin[(*pim_parms).frame] = value[0]
   (*pim_parms).asis[(*pim_parms).frame] = 1B

   ; Refresh all widgets.
   oitool->display
   oitool->draw, /FULL, /ZOOM, /WORK
end


; ----------------------------------------------------------------------------
; Procedure itool_dismiss_event
; Event handler for the "Dismiss" button.
; ----------------------------------------------------------------------------
pro itool_dismiss_event, event
   compile_opt hidden
   widget_control, event.top, MAP=0
end


; ----------------------------------------------------------------------------
; Procedure itool_extrema1_event
; Event handler for "Set display stretch to image extrema (current frame)"
; button.
; ----------------------------------------------------------------------------
pro itool_extrema1_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   ; Set display stretch to image extrema (current frame).

   j = (*pim_parms).frame
   (*pim_parms).curmin[j] = (*pim_parms).minvalue[j]
   (*pim_parms).curmax[j] = (*pim_parms).maxvalue[j]
   (*pim_parms).asis[j] = 1B

   ; Refresh all widgets.
   oitool->display
   oitool->draw, /FULL, /ZOOM, /WORK
end


; ----------------------------------------------------------------------------
; Procedure itool_extrema2_event
; Event handler for the "Set display stretch to image extrema (all frames)"
; button.
; ----------------------------------------------------------------------------
pro itool_extrema2_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, PIM_PARMS=pim_parms, PSTATE=pstate

   ; Set display stretch to image extrema (all frames).

   for j=0, (*pim_parms).nframes-1 do begin
      oitool->cstr, j, /SILENT
      (*pim_parms).curmin[j] = (*pim_parms).minvalue[j]
      (*pim_parms).curmax[j] = (*pim_parms).maxvalue[j]
      (*pim_parms).asis[j] = 1B
   endfor

   ; Refresh all widgets.
   oitool->display
   oitool->draw, /FULL, /ZOOM, /WORK
end

; ----------------------------------------------------------------------------
; Procedure itool_frame_event
; ----------------------------------------------------------------------------
pro itool_frame_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   widget_control, event.id, get_value=value

   ; Requested frame.
   frame = long(value[0])

   ; Get total number of frames.
   nframes = (*pim_parms).nframes

   if (frame ge 0) and (frame le nframes-1) then begin
      ; The requested frame is legal.
      (*pim_parms).frame = frame
      oitool->display
      oitool->draw, /FULL, /ZOOM, /WORK
   endif else begin
      ; The requested frame is bunk. Put the current frame number back
      ; into the text widget.
      widget_control, (*pstate).frameid,$
      set_value=string((*pim_parms).frame, format=(*pstate).fmti)
   endelse
end


; ----------------------------------------------------------------------------
; Procedure itool_hdcpy_event
; Event handler for the "Hard Copy" button.
; ----------------------------------------------------------------------------
pro itool_hdcpy_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   ; Make a hard copy of the current image.
   pimage = (*pim_parms).imageptr
   frame = (*pim_parms).frame

   title = (*pim_parms).object + '  ' + (*pim_parms).imfile + '  ' +$
   (*pim_parms).ut

   hardim, (*pimage)[*,*,frame], (*pim_parms).curmin[frame], $
      (*pim_parms).curmax[frame], TITLE=title, WIDTH=18, AUTOSIZE=1, /negative
end


; ----------------------------------------------------------------------------
; Procedure itool_savefits_event
; ----------------------------------------------------------------------------
pro itool_savefits_event, event
   compile_opt hidden

   name = dialog_pickfile(DIALOG_PARENT=event.top, FILE='idl.fits', /WRITE)

   if name[0] eq '' then return

   if file_test(name[0]) then begin
      msg = 'File ' + name[0] + ' exists. Replace?'

      con = qannounc(msg, TRUELABEL='Yes, continue', FALSELABEL='No, cancel',$
         GROUP_LEADER=event.top, XSIZE=strlen(msg))

      if not con then return
   endif

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms
   pimage = (*pim_parms).imageptr
   writefits, name, *pimage
end


; ----------------------------------------------------------------------------
; Procedure itool_savejpeg_event
; ----------------------------------------------------------------------------
pro itool_savejpeg_event, event
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
   widget_control, event.top, GET_UVALUE=oitool

   ; Get a pointer to the state structure.
   oitool->getproperty, PSTATE=pstate

   ; Get the name of the widget that generated the event.
   uname = widget_info(event.id, /UNAME)

   ; Set te widget id accordingly.
   case uname of
      'full' : wid = (*pstate).full_view
      'work' : wid = (*pstate).work_view
      'zoom' : wid = (*pstate).zoom_view
   endcase

   ; Get the window number that was assigned to the requested draw widget.
   widget_control, wid, GET_VALUE=winnum

   ; Save the current window-number setting.
   winnum_save = !d.window

   ; Capture the window data and save to a JPEG file.
   wset, winnum
   snap = tvrd(TRUE=true)
   write_jpeg, r[0], snap, ORDER=!order, QUALITY=100, TRUE=true
   wset, winnum_save
end




; ----------------------------------------------------------------------------
; Procedure itool_ipbutton_event
; Event handler for the "Image Params" button.
; ----------------------------------------------------------------------------
pro itool_ipbutton_event, event
   compile_opt hidden

   if xregistered('itoolwaimparms', /NOSHOW) then return

   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, NO_BLOCK=no_block, OITOOLWAIMPARMS=oitoolwaimparms

   if obj_valid(oitoolwaimparms) then obj_destroy, oitoolwaimparms

   ; Launch the Image Parameters GUI.
   oitoolwaimparms = obj_new('itoolwaimparms', oitool, GROUP_LEADER=event.top)

   if obj_valid(oitoolwaimparms) then begin
      oitool->setproperty, OITOOLWAIMPARMS=oitoolwaimparms
      oitoolwaimparms->realize, NO_BLOCK=no_block
   endif else begin
      print, 'Unable to start the Image Parameters Tool.'
   endelse
end


; ----------------------------------------------------------------------------
; Procedure itool_nextframe_event
; Event handler for the "Next Frame" button.
; ----------------------------------------------------------------------------
pro itool_nextframe_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   (*pim_parms).frame = ((*pim_parms).frame+1) MOD (*pim_parms).nframes

   ; Refresh all widgets.
   oitool->display
   oitool->draw, /FULL, /ZOOM, /WORK
end


; ----------------------------------------------------------------------------
; Procedure itool_pfiles_event
; Event handler for the "Profiles" button.
; ----------------------------------------------------------------------------
pro itool_pfiles_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=oitool
   oitool->newprofile
end


; ----------------------------------------------------------------------------
; Procedure itool_phpbutton_event
; Event handler for the "Photometry Params" button.
; ----------------------------------------------------------------------------
pro itool_phpbutton_event, event
   compile_opt hidden

   if xregistered('itoolwaphparms', /NOSHOW) then return

   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, NO_BLOCK=no_block, OITOOLWAPHPARMS=oitoolwaphparms

   if obj_valid(oitoolwaphparms) then obj_destroy, oitoolwaphparms

   ; Launch the Image Parameters GUI.
   oitoolwaphparms = obj_new('itoolwaphparms', oitool, GROUP_LEADER=event.top)

   if obj_valid(oitoolwaphparms) then begin
      oitool->setproperty, OITOOLWAPHPARMS=oitoolwaphparms
      oitoolwaphparms->realize, NO_BLOCK=no_block
   endif else begin
      print, 'Unable to start the Photometry Parameters Tool.'
   endelse
end


; ----------------------------------------------------------------------------
; Procedure itool_pixed_event
; Event handler for the "Pixel Editor" button.
; ----------------------------------------------------------------------------
pro itool_pixed_event, event
   compile_opt hidden

   if xregistered('itoolwapixed', /NOSHOW) then return

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, NO_BLOCK=no_block, OIMAGE=oimage, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   pzoomstate = (*pstate).pzoomstate
   pimage = (*pim_parms).imageptr

   oitoolwapixed = obj_new('itoolwapixed', oitool, pimage,$
      (*pzoomstate).xcen, (*pzoomstate).ycen, GROUP_LEADER=event.top)

   if obj_valid(oitoolwapixed) then begin
      oitoolwapixed->realize, NO_BLOCK=no_block
   endif
end


; ----------------------------------------------------------------------------
; Procedure itool_ptrace_event
; Event handler for the "Profile Trace" button.
; ----------------------------------------------------------------------------
pro itool_ptrace_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, OIMAGE=oimage, PPH_PARMS=pph_parms, PSTATE=pstate
   oimage->getproperty, PIM_PARMS=pim_parms

   pzoomstate = (*pstate).pzoomstate
   pimage = (*pim_parms).imageptr

   x = (*pzoomstate).xcen
   y = (*pzoomstate).ycen

   d_name = !d.name
   portrait
   !p.multi=[0,2,3]
   portrait

   ; mag, snr vs. objrad
   r1 = 2
   r2 = (*pph_parms).sky1
   dr = 1

   npts = fix((r2-r1)/dr)
   r = findgen(npts)/dr+r1

   sky1 = (*pph_parms).sky1
   sky2 = (*pph_parms).sky2

   basphote,(*pph_parms).gain,*pimage,(*pim_parms).exptime,x,y,r,sky1,sky2,$
      boxmrad=(*pph_parms).boxmrad,/nolog,/silent, $
      rdnoise=(*pph_parms).rdnoise, $
      err=magerr,fwhm=fwhm,mag=mag,skymean=skymean,skyerr=skyerr

   setusym, -1
   plot,r,magerr,psym=8,$
      xtitle='Object aperture radius',ytitle='Mag Uncertainty',$
      title=(*pim_parms).imfile+'  '+(*pim_parms).object+'  '+$
      (*pim_parms).filter

   ploterror,r,mag,magerr,psym=8,yr=maxmin([mag+magerr,mag-magerr]),$
      xtitle='Object aperture radius',ytitle='Instrumental Magnitude',$
      title=(*pim_parms).imfile+'  sky1,sky2='+strn(sky1)+','+strn(sky2)

   ; sky,mag vs. sky1
   r1 = (*pph_parms).radius
   r2 = (*pph_parms).sky2-5
   dr = 1

   npts = fix((r2-r1)/dr)
   r = findgen(npts)/dr+r1

   objrad = (*pph_parms).radius
   sky2   = (*pph_parms).sky2

   basphote,(*pph_parms).gain,*pimage,(*pim_parms).exptime,x,y,objrad,r,sky2,$
      boxmrad=(*pph_parms).boxmrad,/nolog,/silent, $
      rdnoise=(*pph_parms).rdnoise, $
      err=magerr,fwhm=fwhm,mag=mag,skymean=skymean,skyerr=skyerr

   ploterror,r,skymean,skyerr,psym=8,$
   xtitle='Inner sky radius',ytitle='Sky signal (counts/pixel)',$
   title=(*pim_parms).imfile+'  '+strn((*pim_parms).exptime,format='(f10.1)')+$
   'sec   (x,y)='+strn(x)+','+strn(y)

   ploterror,r,mag,magerr,psym=8,yr=maxmin([mag+magerr,mag-magerr]),$
   xtitle='Inner sky radius',ytitle='Instrumental Magnitude',$
   title=(*pim_parms).imfile+'  objrad='+strn(objrad)+'  sky2='+strn(sky2)

   ; List of stuff in im_parms is in itoolimage__define.pro

   ; sky,mag vs. sky2
   r1 = (*pph_parms).sky1+5
   r2 = (*pph_parms).sky2
   dr = 1

   npts = fix((r2-r1)/dr)
   r = findgen(npts)/dr+r1

   objrad = (*pph_parms).radius
   sky1   = (*pph_parms).sky1

   basphote,(*pph_parms).gain,*pimage,(*pim_parms).exptime,x,y,objrad,sky1,r,$
      boxmrad=(*pph_parms).boxmrad,/nolog,/silent, $
      rdnoise=(*pph_parms).rdnoise, $
      err=magerr,fwhm=fwhm,mag=mag,skymean=skymean,skyerr=skyerr

   jdstr,(*pim_parms).jd,0,str

   ploterror,r,skymean,skyerr,psym=8,$
      xtitle='Outer sky radius',ytitle='Sky signal (counts/pixel)',$
      title=(*pim_parms).imfile+'  '+str

   ploterror,r,mag,magerr,psym=8,yr=maxmin([mag+magerr,mag-magerr]),$
      xtitle='Outer sky radius',ytitle='Instrumental Magnitude',$
      title=(*pim_parms).imfile+'  objrad='+strn(objrad)+'  sky1='+strn(sky1)

   hardcopy
   !p.multi=0
   set_plot,d_name
end


; ----------------------------------------------------------------------------
; Procedure itool_tpmgr_event
; Event handler for the "Template Mgr" button.
; ----------------------------------------------------------------------------
pro itool_tpmgr_event, event
   compile_opt hidden

   if xregistered('itoolwatpmgr') then begin
      widget_control, event.top, GET_UVALUE=oitool
      oitool->getproperty, OITOOLWATPMGR=oitoolwatpmgr
      oitoolwatpmgr->realize, /MAP
      return
   endif

   if xregistered('itoolwacpmgr', /NOSHOW) then begin
      result = dialog_message(['The Template Manager may not be',$
         'started while Comet Photometry is active.'], /ERROR,$
         DIALOG_PARENT=event.top)

      return
   endif

   ; All clear to start.

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, NO_BLOCK=no_block, PSTATE=pstate

   ; Launch the Template Manager.
   oitoolwatpmgr = obj_new('itoolwatpmgr', oitool, GROUP_LEADER=event.top,$
      TMPLFILE=(*pstate).tmplfile)

   if obj_valid(oitoolwatpmgr) then begin
      oitoolwatpmgr->realize, NO_BLOCK=no_block
      oitool->setproperty, OITOOLWATPMGR=oitoolwatpmgr
   endif else begin
      result = dialog_message('Error starting The Template Manager',$
         /ERROR, DIALOG_PARENT=event.top)
   endelse
end


; ----------------------------------------------------------------------------
; Function itool_trk_event
; Event handler for the "'Freeze','Track'" button group.
; ----------------------------------------------------------------------------
pro itool_trk_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, PSTATE=pstate
   pdraw_state = (*pstate).pdraw_state
   uname = widget_info(event.id, /UNAME)

   case uname of
      'freeze' : begin
         (*pdraw_state).trkflg = 0
      end

      'track' : begin
         (*pdraw_state).trkflg = 1
      end

      else : begin
      end
   endcase

   widget_control, event.id, GET_UVALUE=wid, SENSITIVE=0
   widget_control, wid, SENSITIVE=1
end


; ----------------------------------------------------------------------------
; Procedure zoomdn_itool_event
; Event handler for the "Zoom-" button.
; ----------------------------------------------------------------------------
pro zoomdn_itool_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, PSTATE=pstate

   pzoomstate = (*pstate).pzoomstate

   if (*pzoomstate).zfact gt 1.0 then begin
      (*pzoomstate).zfact = (*pzoomstate).zfact - 1.0
      newzoom = string((*pzoomstate).zfact, format='(I3)')
      widget_control, (*pstate).zoomvid, set_value=newzoom
      oitool->draw, /WORK, /ZOOM
   endif
end


; ----------------------------------------------------------------------------
; Procedure zoomup_itool_event
; Event handler for the "Zoom+" button.
; ----------------------------------------------------------------------------
pro zoomup_itool_event, event
   compile_opt hidden

   ; Get the pointer to the main state structure.
   widget_control, event.top, GET_UVALUE=oitool
   oitool->getproperty, PSTATE=pstate

   pzoomstate = (*pstate).pzoomstate
   (*pzoomstate).zfact = (*pzoomstate).zfact + 1.0
   newzoom = string((*pzoomstate).zfact, format='(I3)')
   widget_control, (*pstate).zoomvid, set_value=newzoom
   oitool->draw, /WORK, /ZOOM
end


; ----------------------------------------------------------------------------
; Procedure itool_event
; Default event handler. Not used at this time (widgets have individual
; event handlers). This event handler could be used for events from the
; top-level base, such as size events. It is here as a place-holder.
; ----------------------------------------------------------------------------
pro itool_event, event
   compile_opt hidden

   print, 'ITOOL_EVENT: Default event (currently, no action).'
   help,event, /structure
end


; ----------------------------------------------------------------------------
; Procedure itool__define
; Defines the itool object class.
;
; Attributes:
;   arg_cleanup     : Flag indicating whether or not the ARG_CLEANUP keyword
;                     to the "realize" method was set. Default is 0 (false).
;                       If set to 1 (true), the itool GUI will dispose of
;                     its argument (instance of the 'itoolimage' object class)
;                     upon closing down.
;   expert          : Flag. Turns on certain "expert" modes of operation.
;   no_block        : flag passed to the XMANAGER NO_BLOCK keyword.
;   oitoolwacpmgr   : object reference to an instance of the itoolwacpmgr
;                     object class.
;   oitoolwaimparms : object reference to an instance of the itoolwaimparms
;                     object class.
;   oitoolwaphparms : object reference to an instance of the itoolwaphparms
;                     object class.
;   oitoolwatpmgr   : object reference to an instance of the itoolwatpmgr
;                     object class.
;   pph_parms       : pointer to a ph_parms structure.
;   pstate          : pointer to state structure.
;   tlb             : top-level base.
; ----------------------------------------------------------------------------
pro itool__define
   dummy = {itool,$
      arg_cleanup:0,$
      expert:0,$
      no_block:0,$
      oimage:obj_new(),$
      oitoolwacpmgr:obj_new(),$
      oitoolwaimparms:obj_new(),$
      oitoolwaphparms:obj_new(),$
      oitoolwatpmgr:obj_new(),$
      pph_parms:ptr_new(),$
      pstate:ptr_new(),$
      tlb:0L}
end

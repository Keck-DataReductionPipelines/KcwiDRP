;+
; NAME:
;    ccdphot
; PURPOSE: (one line)
;    General purpose display and processing of CCD image (FITS) files.
; DESCRIPTION:
;    A general purpose widget application which displays and processes CCD
; image files. This includes bias, dark, flat field, and fringe correction
; plus serves the photometry reduction (itool).
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    ccdphot[, keywords]
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    BLOCK         : If set, ccdphot will run in blocked mode.
;    EXPERT        : Flag. Turns on certain "expert" modes of operation.
;                    (1) Suppresses dialog during "Auto" mode, regarding
;                        stop condition.
;    CALIBFILE=    : Calibration startup file.  Default is CALIBPATH/files.cal
;
;    CALIBPATH=    : Path for calibration files.  Default is PATH/calib
;
;    KEYLIST=      : Name of a file containing a correspondence list. This list
;                    associates a set of standard names with the actual keyword
;                    names found in a FITS file header. If this keyword is
;                    omitted, a default list is used, as if a file with the
;                    following contents had been supplied:
;                     AIRMASS   K  AIRMASS
;                     DATE      K  DATE-OBS
;                     DATETMPL  T  DD-MM-YYYY
;                     EXPDELTA  V  0.0
;                     EXPTIME   K  EXPTIME
;                     FILTER    K  FILTERS
;                     FILENAME  K  CCDFNAME
;                     OBJECT    K  OBJECT
;                     UT        K  UT 
;
;                    The middle column is a flag. It may be K, for Keyword,
;                    T, for Template, or V, for Value. If it is V, the contents
;                    of the third field on that line should make sense for the
;                    name in the first field.
;
;    PATH=         : Optional path for image and calibration directory.
;                    If not specified, the current directory is used.
;
;    PATTERN       : Optional file-search pattern. Default is '*.*' .
;
;    PHOTPARMFILE  : Optional photometry parameter file.  Passed to Itool.
;
;    TMPLFILE      : Optional template file.  Passed to Itool.
;
;    WZOOMFACT     : Forces the itool main draw window to have a specified
;                    zoom factor (passed to itool).
;
;  WXVISIBLE       : Creates work window with explicit x-size (passed to
;                    itool).
;
;  WYVISIBLE       : Creates work window with explicit y-size (passed to
;                    itool).
;
; OUTPUTS:
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
;    Written by Doug Loucks, Lowell Observatory, December, 1993.
;    Created by combining lccd.pro, loral.pro, and ccdphot_inst_init.pro.
;    Added features include: Correspondence list for FITS header keywords
;       and recognition of old format calibration environment files.
;    1/26/94, DWL, Added header date-parsing code to ldimage procedure.
;    3/2/94,  DWL, Added keylist item EXPDELTA.
;    3/3/94,  DWL, Improved the template and header date-parsing process.
;    3/10/94, DWL, Bug fixes.
;    4/94,    DWL, Thorough validation of all environment parameters (overscan,
;       cropping region, and bias/flat size compatibility).
;    3/1/95,  MWB, Added support for a dark count calibrator (optional).
;    6/8/95,  MWB, Changed defaults on calibration paths.  Also, if file
;                  name begins with "+", CALIBPATH will replace the "+".
;    10/31/95, MWB, Added CATCH error handler to trap bad image file reads
;                  using READFITS.  This prevents a crash to the IDL prompt
;                  when an invalid file is read.
;    3/18/96, MWB, Minor change to code that reads UT from header.  Format
;                  requires two colons in string (HH:MM:SS) but also now
;                  allows SS to be a floating point number (ie., SS.sss).
;    6/25/96, MWB, implemented AUTO(-photometry) button.
;    1/22/97, MWB, fixed AUTO infinite loop if bad template positions found.
;    2/5/97, MWB, added FWHM and Mag safety shutoff in AUTO
;    6/13/97, MWB, extracted keylist to external general routines.
;    99/06/08, MWB, massive rewrite.  Now using external (and general)
;                   calibration read/write/edit routines.  This caused
;                   big changes in the internal structure and data organization
;                   of this tool.  There are probably lingering bugs at
;                   this stage due to the extensive nature of the edits.
;    99/07/22, MWB, added WXVISIBLE and WYVISIBLE keyword pass through.
;    99/11/15, MWB, fixed bug when starting up a calibration environment
;                   when no filters or previous environment existed.
;
;    2004/04/15, DWL, Major overhaul. Replaced old event-handling code with
;                  individual event handlers. Eliminated intrinsic pointers
;                  (NO_COPY on WIDGET_CONTROL) and replaced with explicit
;                  pointers. Fixed stash location of the state structure.
;                  Previously, it was stored in the first child, which is
;                  necessary in a compound widget, but not in a stand-alone
;                  widget application. Modified to use the new object-
;                  oriented version of itool.
;
;    2006/03/15, DWL, Fixed problem when switching between raw and processed
;                  mode. Applied minor modifications to reflect changes to the
;                  itool GUI. Those changes involved the single argument to
;                  the itool GUI. Previously, it was a pointer to a structure.
;                  Now, it is an object reference of the 'itoolimage' class.
;                  This new class eliminates the need for the procedure
;                  itool_init. Instead, initialization and cleanup are
;                  handled by the init and cleanup methods defined for
;                  the 'itoolimage' object class.
;
;    2006/08/07, MWB, fixed bug in handling the CALIBPATH keyword.
;    2007/01/17, MWB, fixed bug in auto increment hitting end of file list.
;    2007/06/27, MWB, change header on output FITS to set BSCALE=1,BZERO=0
;-

; -----------------------------------------------------------------------------
; Internal support procedures.
; A list of the procedures in this file (in order of occurrence) are:
;
;   ccdphot_di       Display image with itool.
;   ccdphot_pr       Performs bias and flat field correction.
;   ccdphot_af       Adds a new filter code to the calibration structure.
;   ccdphot_li       Loads an image into memory.
;   ccdphot_lc       Loads calibration environment files.
;   ccdphot_sc       Saves the current calibration environment.
;   ccdphot_event    Default event handler (currently, not used).
;   ccdphot

; -----------------------------------------------------------------------------
; Procedure ccdphot_af
;   Adds a filter to the calibration structure if it isn't already there.
; -----------------------------------------------------------------------------
pro ccdphot_af,state,newfilter
   compile_opt hidden

   if ptr_valid(state.pcalib) then begin
      z=where(newfilter eq (*state.pcalib).filter,count)
   endif else begin
      ldcalib,'files.cal', calib, valid, CALIBPATH=state.calibpath,/empty
      state.pcalib = ptr_new(calib, /NO_COPY)
      count=0
   endelse

   if count eq 0 then begin
      calibchg,*state.pcalib,'flat',newfilter,'',CALIBPATH=state.calibpath
      state.calibdirty = 1
   endif
end

; -----------------------------------------------------------------------------
; Procedure ccdphot_cleanup
; -----------------------------------------------------------------------------
pro ccdphot_cleanup, tlb
   compile_opt hidden

   widget_control, tlb, GET_UVALUE=pstate
   obj_destroy, (*pstate).oimage_raw
   obj_destroy, (*pstate).oimage_cal

   ptr_free, (*pstate).pcalib,$
      (*pstate).p_hdrinfo, (*pstate).p_hdrlist, (*pstate).p_header

   ptr_free, pstate
end

; -----------------------------------------------------------------------------
; Procedure ccdphot_di
; Calls itool to display the image.
; -----------------------------------------------------------------------------
pro ccdphot_di, state, AUTOPHOT=autophot, PHOTTYPE=phottype, POSITION=position
   compile_opt hidden

   if state.imfile eq '' then return

   ; Set the pointer to the raw or the calibrated image parameters.
   if state.processflag then begin
      state.oimage = state.oimage_cal
   endif else begin
      state.oimage = state.oimage_raw
   endelse

   sz = size(autophot)

   state.oimage->getproperty, PIM_PARMS=pim_parms

   if sz[1] eq 7 then begin
      if autophot eq (*pim_parms).object then begin
         (*pim_parms).autophot = 1
         (*pim_parms).lasttype = phottype
         (*pim_parms).lastpos  = position
      endif
   endif

   ; Set this flag to true, if the next image differs in size from
   ; the current image in the itool display.
   sizeflag = ((*pim_parms).xsize ne state.current_xsize) or $
              ((*pim_parms).ysize ne state.current_ysize)

   if sizeflag then begin
      ; Image size has changed. Close the current instance of itool.
      ; This will force a new instance below.

      if obj_valid(state.oitool) then begin
         state.oitool->close
      endif
   endif

   ; Display the new image in an existing instance of itool, or launch a
   ; new instance first.

   if obj_valid(state.oitool) then begin
      state.oitool->nextimage, state.oimage
   endif else begin
      state.oitool = obj_new('itool', state.oimage,$
         EXPERT=state.expert,$
         GROUP_LEADER=state.tlb,$
         PHOTPARMFILE=state.photparmfile,$
         TMPLFILE=state.tmplfile,$
         WZOOMFACT=state.wzoomfact,$
         WXVISIBLE=state.wxvisible,$
         WYVISIBLE=state.wyvisible)

      state.oitool->realize, NO_BLOCK=state.no_block
   endelse

   ; Save the size of the current image being displayed by itool.
   state.current_xsize = (*pim_parms).xsize
   state.current_ysize = (*pim_parms).ysize
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_pr
; Performs bias and flat field corrections.
; -----------------------------------------------------------------------------
pro ccdphot_pr, state
   compile_opt hidden

   bel = string(7B)
   if state.imfile eq '' then return
   if obj_valid(state.oimage_cal) then return

   msg = 'Image: ' + state.imfile

   if state.processflag eq 0 then begin
      msg = msg + '  (raw).'
      print, ''
      print, msg
      return
   endif

   image=0
   state.oimage_raw->getproperty, PIM_PARMS=pim_parms_raw
   p_rawimage = (*pim_parms_raw).imageptr
   nframes = (*pim_parms_raw).nframes
   sz = [state.xsize,state.ysize]

   for i=0, nframes-1 do begin
      if i eq 0 then begin
         if nframes eq 1 then begin
            ccdproc, *p_rawimage, *state.p_hdrinfo, *state.pcalib,$
               calimage

            sz=size(calimage, /dimen)
         endif else begin
            ccdproc, (*p_rawimage)[*,*,i], *state.p_hdrinfo,$
               *state.pcalib,tmpimage

            sz=size(tmpimage, /dimen)
            calimage = fltarr(sz[0],  sz[1],  nframes, /NOZERO)
            calimage[*,*,i] = tmpimage
            tmpimage = 0
         endelse
      endif else begin
         ccdproc, (*p_rawimage)[*,*,i], *state.p_hdrinfo,$
            *state.pcalib,tmpimage

         calimage[*,*,i] = tmpimage
         tmpimage = 0
      endelse
   endfor

   ; The "calimage" argument becomes undefined on the next line. The argument
   ; data become part of a heap (pointer) variable, via a NO_COPY keyword.
   ; This is efficient, because data are not copied.
   state.oimage_cal = obj_new('itoolimage', calimage)

   state.oimage_cal->getproperty, PIM_PARMS=pim_parms_cal

   (*pim_parms_cal).airmass   = (*pim_parms_raw).airmass
   (*pim_parms_cal).autophot  = (*pim_parms_raw).autophot
   (*pim_parms_cal).date      = (*pim_parms_raw).date
   (*pim_parms_cal).exptime   = (*pim_parms_raw).exptime
   (*pim_parms_cal).expdelta  = (*pim_parms_raw).expdelta
   (*pim_parms_cal).filter    = (*pim_parms_raw).filter
   (*pim_parms_cal).imfile    = (*pim_parms_raw).imfile
   (*pim_parms_cal).jd        = (*pim_parms_raw).jd
   (*pim_parms_cal).object    = (*pim_parms_raw).object
   (*pim_parms_cal).ut        = (*pim_parms_raw).ut

   print, msg
end

; -----------------------------------------------------------------------------
; Procedure ccdphot_li
; Loads an image file into the image frame array.
; -----------------------------------------------------------------------------
pro ccdphot_li, state, fname, AUTOPHOT=autophot, ERROR=error,$
   PHOTTYPE=phottype, POSITION=position

   compile_opt hidden

   error = 0
   bel = string(7B)

   if not file_test(fname) then begin
      print, 'File ' + fname + ' not found.' + bel
;     widget_control, state.imtextid, SET_VALUE=state.imfile
      error = 1
      return
   endif

   catch, error_status

   if error_status ne 0 then begin
      print, 'Error loading ' + fname + bel
;     widget_control, state.imtextid, SET_VALUE=state.imfile
      error = 1
      return
   endif

   tframe = float(readfits(fname, hdr, /SILENT))

   catch,/CANCEL

   if tframe[0] eq -1 then begin
      print, 'Error reading file '+fname+' as a FITS file.'+bel
;     widget_control, state.imtextid, SET_VALUE=state.imfile
      error = 1
      return
   endif

   *(state.p_header) = hdr

   ; Clean up for next image.
   if obj_valid(state.oimage_raw) then obj_destroy, state.oimage_raw
   if obj_valid(state.oimage_cal) then obj_destroy, state.oimage_cal

   ; Initialize the new image parameters structure for the raw image.
   state.oimage_raw = obj_new('itoolimage', tframe)
   state.oimage_raw->getproperty, PIM_PARMS=pim_parms

   ; Check for overscan error.
   calval, (*pim_parms).xsize, 0, state.xl, state.xr, errflg,$
      BANNER='LOAD IMAGE: Error. Image x-size is inconsistent with the ' +$
             'established overscan size or range.',$
      ISIZELAB='image frame x-size',$
      CSIZELAB='',$
      MLOWLAB='low limit of overscan region',$
      MHIGHLAB='high limit of overscan region',$
      DELTAMLAB='size of overscan region'

   err = errflg

   ; Check for x-range error.
   calval, (*pim_parms).xsize, state.cxsize, state.x1, state.x2, errflg,$
      BANNER='LOAD IMAGE: Error. Image x-size is inconsistent with the ' +$
             'established environment.',$
      ISIZELAB='image frame x-size',$
      CSIZELAB='calibration frame x-size',$
      MLOWLAB='low limit of cropping region x-range',$
      MHIGHLAB='high limit of cropping region x-range',$
      DELTAMLAB='size of cropping region x-range'

   err = err or errflg

   ; Check for y-range error.
   calval, (*pim_parms).ysize, state.cysize, state.y1, state.y2, errflg,$
      BANNER='LOAD IMAGE: Error. Image Y-size is inconsistent with the ' +$
             'established environment.',$
      ISIZELAB='image frame y-size',$
      CSIZELAB='calibration frame y-size',$
      MLOWLAB='low limit of cropping region y-range',$
      MHIGHLAB='high limit of cropping region y-range',$
      DELTAMLAB='size of cropping region y-range'

   err = err or errflg

   if err then begin
      print, '...Image ' + fname + ' cannot be loaded.'
;     widget_control, state.imtextid, SET_VALUE=state.imfile
      error = 1
      return
   endif

   state.imfile = fname
   widget_control, state.imtextid, SET_VALUE=state.imfile

   state.xsize   = (*pim_parms).xsize
   state.ysize   = (*pim_parms).ysize

   ; Extract some info from the header, using the list of header correspondence
   ; names.
   parsekey, *state.p_header, *state.p_hdrlist, *state.p_hdrinfo

   ; Copy the header imformation to the image-parameters structure.
   (*pim_parms).airmass  = (*state.p_hdrinfo).airmass
   (*pim_parms).date     = (*state.p_hdrinfo).date
   (*pim_parms).expdelta = (*state.p_hdrinfo).expdelta
   (*pim_parms).exptime  = (*state.p_hdrinfo).exptime
   (*pim_parms).imfile   = (*state.p_hdrinfo).imfile
   (*pim_parms).filter   = (*state.p_hdrinfo).filter
   (*pim_parms).object   = (*state.p_hdrinfo).object
   (*pim_parms).ut       = (*state.p_hdrinfo).ut
   (*pim_parms).jd       = (*state.p_hdrinfo).jd

   ; Bust up the original file name, if different from header, use file name.
   fdecomp,fname,disk,dir,name,qual
   if qual ne '' then name = name + '.' + qual
   if name ne (*pim_parms).imfile then (*pim_parms).imfile = name

   (*pim_parms).autophot = 0

   if (*pim_parms).imfile eq '' then (*pim_parms).imfile='(No name)'

   ccdphot_af, state, (*pim_parms).filter

   ccdphot_pr, state
   ccdphot_di, state, AUTOPHOT=autophot, PHOTTYPE=phottype, POSITION=position
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_lc
; Loads calibration files, based upon the contents of a calibration
; environment file.
; -----------------------------------------------------------------------------
pro ccdphot_lc, state
   compile_opt hidden

   bel = string(7B)

   if state.calibfile eq '' then return

   if not file_test(state.calibpath+state.calibfile) then begin
      print, 'Calibration Environment File ' +$
         state.calibpath + state.calibfile + ' not found.'+bel
      widget_control, state.caltextid, SET_VALUE=''
      return
   endif

   if state.calibdirty then begin
      t = ['  Calibration environment changes may not have been saved.',$
      '  If you do not wish to lose any changes, cancel this operation and',$
      'save the current environment.']

      con = qannounc(t, TITLE='Calibration Environment Save Confirmation',$
      FALSE='Cancel', TRUE='Ok, continue', XSIZE=max(strlen(t)))

      if not con then return
      state.calibdirty = 0
   endif

   ldcalib,state.calibfile,calib,valid,CALIBPATH=state.calibpath

   if not valid then begin
      print, 'Calibration file is not valid.  Unable to load.' + bel
      return
   endif else begin
      ptr_free, state.pcalib
      state.pcalib = ptr_new(calib, /no_copy)
   endelse

   ;Set calibration environment save flag.
   state.calibdirty = 0

   ;Process and display the image (if loaded).
   ccdphot_pr, state
   ccdphot_di, state
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_sc
; Saves the current calibration environment to a file.
; -----------------------------------------------------------------------------
pro ccdphot_sc, state
   compile_opt hidden

   bel = string(7B)

   if state.calibfile eq '' then return

   if file_test(state.calibpath+state.calibfile) then begin
      t =$
      ['  Calibration environment file ' + state.calibpath+state.calibfile +$
       ' exists.',$
        '  It will be overwritten, unless you choose to cancel and change',$
        'the name of the file.' ]

      con = qannounc(t, TITLE='File Overwrite Confirmation',$
      FALSE='Cancel', TRUE='Overwrite File', XSIZE=max(strlen(t)))

      if not con then return
   endif

   wrcalib,(*state.pcalib),state.calibfile,CALIBPATH=state.calibpath

   state.calibdirty = 0

   print, 'Calibration environment saved to file ' +$
             state.calibpath+state.calibfile
end


;<<<<<<<<<<<<<<<<<<<<<<<<< EVENT PROCESSING <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

; -----------------------------------------------------------------------------
; Procedure ccdphot_exit_event
; -----------------------------------------------------------------------------
pro ccdphot_exit_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate

   if obj_valid((*pstate).oitool) then begin
      (*pstate).oitool->getproperty, STATUS=status
   endif else begin
      status = {msg:''}
   endelse

   tmsg = ['']
   ask = not (*pstate).expert

   if (*pstate).calibdirty then begin
      tmsg = [ tmsg,$
      'Calibration environment changes have not been saved.' ]

      ask = 1
   endif

   if not ask then begin
      widget_control, event.top, /DESTROY
      return
   endif

   tmsg = [tmsg, '****************************************',$
   'Do you really wish to quit CCDPHOT?' ]

   msg = [status.msg, tmsg]

   con = qannounc(msg, TITLE=' CCDPHOT Exit Confirmation',$
      GROUP_LEADER=event.top,$
      XSIZE=max(strlen(msg)), YSIZE=5)

   if con then begin
      widget_control, event.top, /DESTROY
   endif
end


; -----------------------------------------------------------------------------
; Function ccdphot_rawpro_event
; -----------------------------------------------------------------------------
function ccdphot_rawpro_event, event
   compile_opt hidden

   ; Raw/Processed toggle event.

   widget_control, /HOURGLASS
   widget_control, event.top, GET_UVALUE=pstate

   (*pstate).processflag = event.value
   ccdphot_pr, *pstate
   ccdphot_di, *pstate
   return, 0
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_prev_event
; -----------------------------------------------------------------------------
pro ccdphot_prev_event, event
   compile_opt hidden

   widget_control, /HOURGLASS
   bel = string(7B)
   widget_control, event.top, GET_UVALUE=pstate

   basename = file_basename((*pstate).imfile)

   nextfn = nextfile(basename,PATH=(*pstate).impath,PATTERN=(*pstate).pattern,$
      /PREVIOUS)

   if nextfn[0] eq '' then begin
      print, 'Error. File before '+(*pstate).impath+basename+$
         ' not found using search pattern '+(*pstate).pattern+ bel

      return
   endif
                                                                                
;  btemp = byte(basename)
;  w = where(btemp ge 48 and btemp le 57, count)
                                                                                
;  if count gt 0L then begin
;     fmt = strcompress('(i' + string(count) + '.' + string(count) + ')',$
;           /remove_all)
                                                                                
;     z = long(string(btemp[w]))
;     z = (z-1L) > 0L
;     btemp[w] = byte(string(z mod (10^count),format=fmt))
;     basename = string(btemp)
;  endif else begin
;     print, 'Error. Unable to decrement image filename (no numerics)' + bel
;     return
;  endelse

   ccdphot_li, *pstate, (*pstate).impath+nextfn[0], ERROR=error
                                                                                
   if error then begin
      print, 'Error. Unable to load file '+(*pstate).impath+nextfn[0]+bel
   endif else begin
      (*pstate).imfile = (*pstate).impath+nextfn[0]

      widget_control, (*pstate).imtextid,$
      SET_VALUE=(*pstate).impath+nextfn[0]
   endelse
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_next_event
; -----------------------------------------------------------------------------
pro ccdphot_next_event, event
   compile_opt hidden

   widget_control, /HOURGLASS
   bel = string(7B)
   widget_control, event.top, GET_UVALUE=pstate

   if (*pstate).imfile eq '' then begin
      ; No file has been loaded, yet. Use the image path and get a list
      ; of all files matching the pattern.
      list = file_search((*pstate).impath+(*pstate).pattern)

      if list[0] ne '' then begin
         ; At least one file matches the pattern. Pick the first one in the
         ; list and load it.
         nextfn = file_basename(list[0])
         ccdphot_li, *pstate, (*pstate).impath+nextfn[0],ERROR=error

;        widget_control, (*pstate).imtextid, SET_VALUE=(*pstate).impath +$
;           nextfn

         if error then begin
            print, 'Error. Unable to load file '+(*pstate).impath+nextfn[0]
            return
         endif

         ; Sensitize the Prev button.
         widget_control, (*pstate).prev_id, SENSITIVE=1
         return
      endif else begin
         print, 'Directory ' + (*pstate).impath + ' has no files matching' +$
            ' pattern '+(*pstate).pattern

         return
      endelse
   endif


   ; Come here if a file has already been loaded. In this case, the position
   ; of the current filename in the list will be determined and the next
   ; file name in the list will be loaded.
   basename = file_basename((*pstate).imfile)
   nextfn = nextfile(basename,PATH=(*pstate).impath,PATTERN=(*pstate).pattern)

   if nextfn[0] eq '' then begin
      print, 'Error. File after '+(*pstate).impath+basename+$
         ' not found using search pattern '+(*pstate).pattern+ bel

      return
   endif

;  btemp = byte(basename)
;  w = where(btemp ge 48 and btemp le 57, count)

;  if count gt 0L then begin
;     fmt = strcompress('(i' + string(count) + '.' + string(count) + ')',$
;           /remove_all)

;     z = long(string(btemp[w]))
;     btemp[w] = byte(string((z+1) mod (10^count),format=fmt))
;     basename = string(btemp)
;  endif else begin
;     print, 'Error. Unable to increment image filename (no numerics)' + bel
;     return
;  endelse

   ccdphot_li, *pstate, (*pstate).impath+nextfn[0],ERROR=error
   
   if error then begin
      print, 'Error. Unable to load file '+(*pstate).impath+nextfn[0]
      return
   endif else begin
      (*pstate).imfile = (*pstate).impath+nextfn[0]

      widget_control, (*pstate).imtextid,$
      SET_VALUE=(*pstate).impath+nextfn[0]
   endelse
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_auto_event
; -----------------------------------------------------------------------------
pro ccdphot_auto_event, event
   compile_opt hidden

   widget_control, /HOURGLASS
   widget_control, event.top, GET_UVALUE=pstate

   bel = string(7B)

   msg = [$
   'This command will automatically proceed through the data with the same',$
   'photometry extraction as done on the last frame. By default, this process',$
   'will continue until the object name changes, or, there are no more data.',$
   'If this is what you wish to do, then select "Stop on Last".  However, you',$
   'might want to stop on a certain frame number. If you do want to stop on',$
   'a certain frame number, then select "Set Stop Point". If you choose the',$
   'latter option, you will be asked for the desired stopping point. However,',$
   'a changing object name will take precedence over a stop point. So, if you',$
   'set a stop point after the object changes, it will stop earlier than you',$
   'think.']

   if (*pstate).imfile ne '' then begin
      sameobject = 0
      fwhmok     = 0
      magok      = 0
      fail = 1
      fileok = 0
      firstpass=1
      thisframe = -1
      lastframe = -2

      repeat begin
         (*pstate).oitool->getproperty, STATUS=it_info
         lasttype = (it_info).lasttype
         lastpos  = (it_info).lastpos
         object   = (it_info).object
         lastfwhm = (it_info).lastfwhm
         lastmag  = (it_info).lastmag

         if it_info.lasttype ne 0 then begin
            if firstpass then begin
               if not (*pstate).expert then begin
                  ok = qannounc(msg, TITLE='Automatic Photometry Procedure',$
                     XSIZE=max(strlen(msg)), YSIZE=n_elements(msg),$
                     GROUP_LEADER=event.top,$
                     TRUELABEL='Stop on Last', FALSELABEL='Set Stop Point')

                  if ok then begin
                     lastframe = 999
                  endif else begin
                     lastframe = qinput(PROMPT='Frame number to stop on?',$
                     GROUP_LEADER=event.top, /INTEGER)
                  endelse
               endif else begin
                  lastframe = 999
               endelse

               firstpass=0
            endif

            basename = file_basename((*pstate).imfile)

            nextfn = nextfile(basename,PATH=(*pstate).impath,$
               PATTERN=(*pstate).pattern)

            if nextfn[0] ne '' then begin
               ccdphot_li, *pstate, (*pstate).impath+nextfn[0],$
                  AUTOPHOT=object,$
                  PHOTTYPE=lasttype, POSITION=lastpos, ERROR=error

               if error then begin
                  sameobject = 0
                  fileok = 0
                  magok = 0
                  fwhmok = 0
               endif else begin
                  widget_control, (*pstate).imtextid, SET_VALUE=f
                  (*pstate).oitool->getproperty, STATUS=it_info
                  sameobject = (it_info.object eq object)
                  magok      = (abs(it_info.lastmag-lastmag) lt 10.0)
                  fwhmok     = (abs(it_info.lastfwhm-lastfwhm)/lastfwhm lt 1.0)
                  fail = 0
                  fileok = 1
               endelse
            endif else begin
               thisframe=-1

               print,'  Filename cannot be incremented. Unable to run' +$
                 ' Auto-photometry.'+bel
               break
            endelse
         endif else begin
            print,'Cannot do auto-photometry.'+bel
            print,'   No position has yet been defined for this object.'
            lastframe=thisframe
         endelse
      endrep until not sameobject or lastframe eq thisframe or not magok or $
         not fwhmok

      if not fail then begin
         if not fileok then $
            print,'  Auto-photometry halted. '+$
            'Next file incomplete or missing.'+ bel $

         else if not sameobject then $
            print,'  Auto-photometry halted.  Object changed.' + bel $

         else if thisframe ge lastframe then $
            print,'  Auto-photometry halted.  Stopping point reached.' + bel $

         else if not magok then $
            print,'  Auto-photometry halted. '+$
            'Magnitude of object changed too much.' + bel $

         else if not fwhmok then $
            print,'  Auto-photometry halted. '+$
            'FWHM of object changed too much.' + bel $

         else $
            print,'  Unknown ending condition for Auto, '+$
            'this should not happen.' + bel
      endif
   endif else begin
      print, 'Error. No image.' + bel
   endelse
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_viewheader_event
; -----------------------------------------------------------------------------
pro ccdphot_viewheader_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate

   if (*pstate).imfile ne '' then begin
      t = 'Image Header' + ' - file ' + (*pstate).imfile
      viewtext, *(*pstate).p_header, GROUP=event.top, TITLE=t,$
         XSIZE=81, YSIZE=40
   endif else begin
      print, 'Error. No image.'
   endelse
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_writefits_event
; -----------------------------------------------------------------------------
pro ccdphot_writefits_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate
   (*pstate).oimage_cal->getproperty, PIM_PARMS=pim_parms_cal
   p_calimage = (*pim_parms_cal).imageptr

   ; Write the image to a FITS file (allows a calibrated image to be saved).
   if (*pstate).imfile ne '' then begin
      ; There is an image loaded. Do it.
      filename = 'c_' + (*pim_parms_cal).imfile
      hdr = *(*pstate).p_header
      sxaddpar,hdr,'BSCALE',1.0
      sxaddpar,hdr,'BZERO',0.0
      writefits, filename, *p_calimage, hdr
      print, 'Image saved to file ' + filename
   endif
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_selectfile_event
; -----------------------------------------------------------------------------
pro ccdphot_selectfile_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate

   f = dialog_pickfile(GROUP=event.top, TITLE='Select Image Frame',$
       PATH=(*pstate).impath,FILTER=(*pstate).pattern)

   if f[0] eq '' then return

   basename = file_basename(f)

   ccdphot_li,*pstate,(*pstate).impath+basename[0],ERROR=error

   if error then begin
      return
   endif else begin
      widget_control, (*pstate).imtextid, SET_VALUE=fn
      widget_control, (*pstate).prev_id, SENSITIVE=1
   endelse
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_imtext_event
; -----------------------------------------------------------------------------
pro ccdphot_imtext_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate
   widget_control, event.id, GET_VALUE=value

   dirname = file_dirname(value[0],/MARK_DIRECTORY)
   basename = file_basename(value[0])

   if basename ne '' then begin
      if file_test(dirname+basename) then begin
         ccdphot_li, *pstate, dirname+basename, ERROR=error

         if error then begin
            return
         endif else begin
            (*pstate).impath = dirname
            (*pstate).imfile = basename
            widget_control,(*pstate).imtextid,SET_VALUE=dirname+basename
            widget_control,(*pstate).prev_id,SENSITIVE=1
         return
         endelse
      endif else begin
         print, 'File ' + dirname+basename + ' not found.'
         widget_control,(*pstate).imtextid,SET_VALUE=''
      endelse
   endif else begin
      if (*pstate).imfile ne '' then begin
         widget_control, (*pstate).imtextid, SET_VALUE=(*pstate).imfile
      endif
   endelse
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_caload_event
; -----------------------------------------------------------------------------
pro ccdphot_caload_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate
   ccdphot_lc, *pstate
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_casave_event
; -----------------------------------------------------------------------------
pro ccdphot_casave_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate
   ccdphot_sc, *pstate
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_casave_event
; -----------------------------------------------------------------------------
pro ccdphot_casave_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate
   pcalib = (*pstate).pcalib
   if not ptr_valid(pcalib) then return

   calibed, *pcalib, dirty, group=event.top,$
      CALIBPATH=(*pstate).calibpath,$
      ISIZE=[(*pstate).xsize,(*pstate).ysize],/modal

   (*pstate).calibdirty = (*pstate).calibdirty or dirty
   ccdphot_pr, *pstate
   ccdphot_di, *pstate
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_capick_event
; -----------------------------------------------------------------------------
pro ccdphot_capick_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate

   f = dialog_pickfile(GROUP=event.top, TITLE='Select Calibration File',$
      PATH=(*pstate).calibpath)

   fn = strtrim(f[0], 2)

   if fn ne '' then begin
      (*pstate).calibfile = fn
      widget_control, (*pstate).caltextid, SET_VALUE=fn
   endif
end


; -----------------------------------------------------------------------------
; Procedure ccdphot_catext_event
; -----------------------------------------------------------------------------
pro ccdphot_catext_event, event
   compile_opt hidden

   widget_control, event.top, GET_UVALUE=pstate

   widget_control, event.id, GET_VALUE=value
   f = strtrim(value[0], 2)
   (*pstate).calibfile = f
   print, 'Calibration file set to ' + f
end



; -----------------------------------------------------------------------------
; Procedure ccdphot_event
; Default event handler for ccdphot. All widgets have their own event handlers,
; so this handler should never be called. It is here for future use, if
; events generated by the top-level base need to be handled.
; -----------------------------------------------------------------------------
pro ccdphot_event, event
   compile_opt hidden

   help, event, /STRUCTURE
end



; -----------------------------------------------------------------------------
; Procedure ccdphot
; -----------------------------------------------------------------------------
pro ccdphot, CALIBFILE=calibfile, CALIBPATH=calibpath, BLOCK=block,$
             EXPERT=expert,$
             KEYLIST=in_keylist,$
             PATTERN=pattern,$
             TITLE=title,$
             PATH=path, PHOTPARMFILE=photparmfile,$
             TMPLFILE=tmplfile, WZOOMFACT=wzoomfact,$
             WXVISIBLE=wxvisible, WYVISIBLE=wyvisible

   if xregistered('ccdphot') then return

   bel = string(7B)

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. CCDPHOT cannot be started.'
      return
   endif

   if badpar(wzoomfact,[0,1,2,3],0,$
         CALLER='CCDPHOT: (wzoomfact) ',DEFAULT=0) then return

   if badpar(wxvisible,[0,1,2,3],0,$
         CALLER='CCDPHOT: (wxvisible) ',DEFAULT=0) then return

   if badpar(wyvisible,[0,1,2,3],0,$
         CALLER='CCDPHOT: (wyvisible) ',DEFAULT=0) then return

   if badpar(path,[0,7],0,$
         CALLER='CCDPHOT: (path) ',DEFAULT='./') then return

   if badpar(pattern,[0,7],0,$
         CALLER='CCDPHOT: (pattern) ',DEFAULT='*.*') then return

   if badpar(calibpath,[0,7],0,CALLER='CCDPHOT: (calibpath) ', $
                DEFAULT=addslash(path)+'calib/') then return

   if badpar(calibfile,[0,7],0,$
         CALLER='CCDPHOT: (calibfile) ',DEFAULT='files.cal') then return
   if badpar(photparmfile,[0,7],0,$
         CALLER='CCDPHOT: (photparmfile) ',DEFAULT='') then return
   if badpar(tmplfile,[0,7],0,$
         CALLER='CCDPHOT: (tmplfile) ',DEFAULT='') then return

   if keyword_set(block) then no_block = 0 else no_block = 1

   ; Initialize the main state structure.
   state = {$
      pcalib:ptr_new(),$
      calibfile:calibfile,$
      calibdirty:0,$
      calibpath:addslash(calibpath),$
      caltextid:0L,$
      current_xsize:0L,$
      current_ysize:0L,$
      cxsize:0, cysize:0,$
      expert:keyword_set(expert),$
      imfile:'',$
      impath:addslash(path),$
      imtextid:0L,$
      new:1B,$
      next_id:0L,$
      no_block:no_block,$
      oimage:obj_new(),$
      oimage_raw:obj_new(),$
      oimage_cal:obj_new(),$
      oitool:obj_new(),$
      p_hdrinfo:ptr_new(/ALLOCATE_HEAP),$
      p_hdrlist:ptr_new(/ALLOCATE_HEAP),$
      p_header:ptr_new(/ALLOCATE_HEAP),$
      pattern:pattern,$
      photparmfile:photparmfile,$
      prev_id:0L,$
      processflag:1,$
      rawid:0L,$
      tlb:0L,$
      tmplfile:tmplfile,$
      wxvisible: wxvisible,$
      wyvisible: wyvisible,$
      wzoomfact: wzoomfact,$
      xl:-1, xr:-1,$
      x1:-1, x2:-1, y1:-1, y2:-1,$
      xrange:'*', xrangeid:0L,$
      yrange:'*', yrangeid:0L,$
      xsize:0,$
      ysize:0}

   ; Get header correspondence list.
   if keyword_set(in_keylist) then begin
      loadkeys,in_keylist, *state.p_hdrlist
   endif else begin
      loadkeys,'[[DEFAULT]]', *state.p_hdrlist
   endelse

   if not keyword_set(title) then title='CCDPHOT'

   ; Define the main base.
   state.tlb = widget_base(TITLE=title, COLUMN=1, MBAR=mbar)

   ; File Menu.
   filemenuid = widget_button(mbar, VALUE='File', /MENU)

   dummy = widget_button(filemenuid, VALUE='Save Calibrated Image to FITS',$
      EVENT_PRO='ccdphot_writefits_event')

   dummy = widget_button(filemenuid, VALUE='Exit',$
      EVENT_PRO='ccdphot_exit_event')


   ; View Menu.
   viewmenuid = widget_button(mbar, VALUE='View', /MENU)

   dummy = widget_button(viewmenuid, VALUE='ViewHeader',$
      EVENT_PRO='ccdphot_viewheader_event')


   ; Main button base.
   b1 = widget_base(state.tlb, /ALIGN_CENTER, ROW=1, FRAME=1)

   state.rawid = cw_bgroup(b1, ['Raw', 'Processed'], /EXCLUSIVE,$
      EVENT_FUNC='ccdphot_rawpro_event',$
      /NO_RELEASE, /ROW, SET_VALUE=state.processflag)

   state.prev_id = widget_button(b1, VALUE='Prev',$
      EVENT_PRO='ccdphot_prev_event',SENSITIVE=0)

   state.next_id = widget_button(b1, VALUE='Next',$
      EVENT_PRO='ccdphot_next_event',SENSITIVE=1)

   dummy = widget_button(b1, VALUE='Auto',$
      EVENT_PRO='ccdphot_auto_event')

   ; Image select base.
   imagebase = widget_base(state.tlb, ROW=1, FRAME=1, UVALUE=0)
   w1 = widget_label(imagebase, VALUE='IMAGE FRAME:')

   dummy = widget_button(imagebase, VALUE='Select File',$
      EVENT_PRO='ccdphot_selectfile_event')

   state.imtextid = widget_text(imagebase, VALUE=state.imfile, /EDITABLE,$
      EVENT_PRO='ccdphot_imtext_event', XSIZE=50)

   ; Calibration environment file base.
   calbase = widget_base(state.tlb, COLUMN=1, FRAME=1, UVALUE=0)
   w1 = widget_label(calbase, VALUE='CALIBRATION ENVIRONMENT')
   b2 = widget_base(calbase, /ROW)

   dummy = widget_button(b2, VALUE='Load',$
      EVENT_PRO='ccdphot_caload_event')

   dummy = widget_button(b2, VALUE='Save',$
      EVENT_PRO='ccdphot_casave_event')

   dummy = widget_button(b2, VALUE='Edit',$
      EVENT_PRO='ccdphot_casave_event')

   dummy = widget_button(b2, VALUE='Select File',$
      EVENT_PRO='ccdphot_capick_event')

   state.caltextid = widget_text(b2, VALUE=state.calibfile,$
      EVENT_PRO='ccdphot_catext_event', /EDITABLE, XSIZE=40)

   if state.calibfile ne '' then begin
      ; Call the calibration file load routine.
      ccdphot_lc, state
   endif


   ; Realize the main base.
   widget_control, state.tlb, /REALIZE

   ; Stash a pointer to the state structure into the UVALUE of the
   ; top-level-base.
   pstate = ptr_new(state, /NO_COPY)
   widget_control, (*pstate).tlb, SET_UVALUE=pstate

   ; Give control to the xmanager.
   xmanager, 'ccdphot', (*pstate).tlb, CLEANUP='ccdphot_cleanup',$
      NO_BLOCK=no_block
end

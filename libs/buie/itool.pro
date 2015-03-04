;+
; NAME:
;    itool
; PURPOSE: (one line)
;    General purpose image display (front-end for itool\_\_define).
; DESCRIPTION:
;    This procedure simplifies the use of the object-oriented itool GUI,
; (the 'itool' object class) by acting as a "front end" procedure which may
; be called from the IDL command prompt.
;    By default, this procedure runs the itool GUI in non-blocked mode.
; There is a side-effect from running in non-blocked mode: only a copy of
; the input argument is passed to the itool GUI. Hence, any changes to the
; image within the itool GUI will be lost. Currently, the pixel editor in
; the itool GUI is the only mechanism for modifying the image. If the user
; wishes to retrieve a modified image argument, the itool GUI must be
; launched in blocked mode. Setting the BLOCK keyword to this procedure
; will run the itool GUI in blocked mode. Upon closing the GUI, this
; procedure will exit to the IDL command prompt and the input image
; argument will be available. Any changes made to the image while running the
; itool GUI in blocked mode will be reflected in the image argument.
;
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;
; INPUTS:
;    image : The image array.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;  BLOCK          : If set, forces the itool GUI to run in blocked mode.
;                   Otherwise, the itool GUI is launched in non-blocked mode.
;                   Running the itool GUI in blocked mode allows the itool GUI
;                   to modify this procedure's incoming image argument.
;                   Otherwise, the itool GUI works with a separate copy of
;                   the image, in which case any changes made within the
;                   itool GUI will be lost.
;  FVISIBLE       : Size of the full-view window (128 pixels).
;  PHOTPARMFILE   : Optional photometry parameters file.
;  TMPLFILE       : Optional Photometry template file.
;  SCLMIN         : Stretch range minimum.
;  SCLMAX         : Stretch range maximum.
;  WXVISIBLE      : Creates work window with explicit x-size (500).
;  WYVISIBLE      : Creates work window with explicit y-size (500).
;  WZOOMFACT      : Ceiling for work-view zoom factor (unlimited).
;  ZVISIBLE       : Size of the zoom window (128 pixels).
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
;    Written by Doug Loucks, Lowell Observatory, July 29, 1994. This is a small
; 'front-end' for the new compound widget version of itool (cw_itool).
;
;  94/09/27 - Marc W. Buie, Lowell Observatory.  Modified exit on itool_event
;               to eliminate crash on exit if photometry or template parms
;               were changed.
;
;  2004/04/15 - Doug Loucks, Consultant for Lowell Observatory. Overhauled to
;               work with the new object-oriented version of itool
;               (itool__define).
;  2004/05/15 - DWL, Added the BLOCK keyword. See description for details.
;  2006/03/15 - DWL, Minor modifications to reflect changes to the itool GUI.
;                 Those changes involved the single argument to
;                 the itool GUI. Previously, it was a pointer to a structure.
;                 Now, it is an object reference of the 'itoolimage' class.
;                 This new class eliminates the need for the procedure
;                 itool_init. Instead, initialization and cleanup are
;                 handled by the init and cleanup methods defined for
;                 the 'itoolimage' object class.
;                 Also, there is a new keyword to the 'realize' method of
;                 the 'itool' object class. That keyword is ARG_CLEANUP.
;                 If set, the itool GUI will handle the destruction of the
;                 object passed as its single argument. Otherwise, it is the
;                 caller's responsibility to destroy the object of
;                 the 'itoolimage' class. This procedure sets the ARG_CLEANUP
;                 keyword, whenever the itool GUI is launched in non-blocked
;                 mode.
;-

; ----------------------------------------------------------------------------
; Procedure Itool
;   Front-end program, to launch an instance of the itool GUI, using the
; 'itool' object class.
; ----------------------------------------------------------------------------
pro itool, image,$
   BLOCK=block,$
   FVISIBLE=fvisible,$
   PHOTPARMFILE=photparmfile, $
   SCLMIN=sclmin,$
   SCLMAX=sclmax, $
   TMPLFILE=tmplfile, $
   WXVISIBLE=wxvisible,$
   WYVISIBLE=wyvisible, $
   WZOOMFACT=wzoomfact, $
   ZVISIBLE=zvisible

   if n_params() ne 1L then begin
      print, 'Usage: itool, image'
      return
   endif

   if keyword_set(block) then no_block=0 else no_block=1
   if not keyword_set(photparmfile) then photparmfile=''
   if not keyword_set(tmplfile) then tmplfile=''

   oimage = obj_new('itoolimage', image)
   oimage->getproperty, PIM_PARMS=pim_parms

   if keyword_set(sclmin) then begin
      (*pim_parms).ready = 1B
      (*pim_parms).sclmin = sclmin
   endif

   if keyword_set(sclmax) then begin
      (*pim_parms).ready = 1B
      (*pim_parms).sclmax = sclmax
   endif


   ; Create a new instance of the object-oriented itool GUI.
   oitool = obj_new('itool', oimage,$
      FVISIBLE=fvisible,$
      /NODISMISS,$
      PHOTPARMFILE=photparmfile,$
      TMPLFILE=tmplfile,$
      WXVISIBLE=wxvisible,$
      WYVISIBLE=wyvisible,$
      WZOOMFACT=wzoomfact,$
      ZVISIBLE=zvisible)

   pimage = (*pim_parms).imageptr

   if no_block then begin
      ; Realize the itool GUI in non-blocked mode and return to the caller.
      ; Note: in this case, the itool GUI will destroy the instance of the
      ; 'itoolimage' object that is passed via its argument.
      oitool->realize, /ARG_CLEANUP, /NO_BLOCK

      ; The following line preserves the incoming image argument for the
      ; caller, by recovering the image data from the pointer that is created
      ; by the init method of the 'itoolimage' object class.
      image = *pimage
      return
   endif else begin
      ; Realize the itool GUI in blocked mode. In this case, the ARG_CLEANUP
      ; keyword to the 'realize' method of the itool GUI is not set; the
      ; 'itoolimage' object created here is destroyed in this procedure, after
      ; it regains control---after the itool GUI is closed by the user.
      oitool->realize
   endelse

   ; Come here after closing the blocked instance of the itool GUI.

   ; Recover the image array into the caller's image argument.
   image = *pimage

   obj_destroy, oimage
end

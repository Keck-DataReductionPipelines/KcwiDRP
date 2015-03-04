;+
; CLASS_NAME:
;    itool_template
;
; PURPOSE (one line):
;    Plots profiles of an extracted array of data.
;
; DESCRIPTION:
;    This object class implements itool templates, which are used by itool
; itself and by The Template Manager.
; The Template Manager is a stand-alone GUI, which may be launched from a
; button in itool. Communication of data between itool and The
; Template Manager is important and is achieved through the use of IDL
; pointer variables. Specifically, itool and The Template Manager have
; access to each other's object references.
;    The purpose of the "itool_template" object class is to store itool
; template data and to provide method routines for operating on those data.
;    The primary data that are maintained in an instance of an itool
; template are the image (device) coordinates of key locations in that
; image. These locations are identified by the user of itool and are
; referred to as template "objects," meaning stars or planetary objects
; that appear on an image taken with a CCD camera mounted on a telescope.
;    An itool photometry template keeps track of the locations of objects
; in an image, as their positiions shift from one exposure to another. As
; exposures are collected, preliminary photometry for the objects identified
; in a template may be computed and saved during the course of an observation
; run.
;
; CATEGORY:
;    Widgets
;
; SUPERCLASSES:
;
; SUBCLASSES:
;
; CREATION:
;    See itool_template::init
;
; METHODS:
;    itool_template::cleanup
;    itool_template::print
;    itool_template::fmt
;    itool_template::addobject
;    itool_template::updateobjects
;    itool_template::deleteobject
;    itool_template::purgeobjects
;    itool_template::getproperty
;    itool_template::changeobjectname
;    itool_template::setproperty
;    itool_template::init
;
; MODIFICATION HISTORY:
;    2004, Mar. Written by Doug Loucks, consultant for Lowell Observatory.
;
;
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::cleanup
;
; PURPOSE:
;    Self-explanatory
;
; CALLING SEQUENCE:
;    obj_destroy, oref
;
; INPUTS:
;    oref : An itool_template object reference.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::print
;
; PURPOSE:
;    To print a template to standard output, or to a file.
;
; CALLING SEQUENCE:
;    oref->print
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    LUN : Set this keyword to the logical unit number of the file to
;          be written. If absent, output will go to IDL's standard output.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::fmt
;
; PURPOSE:
;    To format a list of objects for printing.
;
; CALLING SEQUENCE:
;    result = oref->fmt()
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    result : A string array containing the formatted list of objects. If
;             the object list is empty, a null string is returned.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::addobject
;
; PURPOSE:
;    To add a new object to the list of objects.
;
; CALLING SEQUENCE:
;    oref->addobject, x, y
;
; INPUTS:
;    x, y : the image (device) coordinates of the new object.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::updateobjects
;
; PURPOSE:
;    To add a new set of objects, replacing the existing set of objects.
;
; CALLING SEQUENCE:
;    oref->updateobjects, new, objnam, x, y
;
; INPUTS:
;    new    : a flag array, indicating "new" status for each object.
;    objnam : a string array of object names.
;    x, y   : arrays of x and y coordinates of the objects.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::deleteobject
;
; PURPOSE:
;    To delete a selected object from the object list.
;
; CALLING SEQUENCE:
;    oref->deleteobject
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::purgeobjects
;
; PURPOSE:
;    To purge the objects in the object list.
;
; CALLING SEQUENCE:
;    oref->purgeobjects
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::getproperty
;
; PURPOSE:
;   To retrieve "properties" defined for the itool_template object
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
;    MODE : Set this keyword to a named variable in which the mode of
;           the template will be returned. The mode of a selected template
;           controls the behavior of a "left click" in itool's main draw
;           widget. Two settings are possible: "Add Objects" or
;           "Active Template Photometry," which are stored as 0 and 1,
;           respectively and are selected via a combobox widget.
;           If the mode is "Add Objects," new "objects" are added to a
;           selected template, as the user "left-clicks" in the main draw
;           widget. If the mode is "Active Template Photometry," left-clicking
;           in the main draw widget launches photometric calculations for the
;           "objects" in a template. The default mode for a new instance of a
;           template is "Add Objects."
;
;    MODIFIED     : Set this keyword to a named variable in which the modified
;                   flag for the template will be returned.
;
;    NUMOBJ       : Set this keyword to a named variable in which the number
;                   of objects in the template will be returned.
;
;    OBJ_SELECTED : Set this keyword to a named variable in which the index
;                   of the currently-selected object will be returned.
;
;    NEW          : Set this keyword to a named variable in which the array
;                   of "new" flags will be returned.
;
;    OBJNAM       : Set this keyword to a named variable in which the array
;                   of object names will be returned.
;
;    X, Y         : Set these keywords to named variables in which the
;                   x and y coordinates of the objects will be returned.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::changeobjectname
;
; PURPOSE:
;    To change the name of an object that is highlighted (selected).
;
; CALLING SEQUENCE:
;    oref->changeobjectname, objnam
;
; INPUTS:
;    objnam : the new name to be given to the selected object.
;
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::setproperty
;
; PURPOSE:
;    To set "properties" defined for the itool_template object class.
; Properties are specified as keyword arguments.
;
; CALLING SEQUENCE:
;    oref->setproperty
;
; INPUTS:
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;    MODE         : the mode of the template (see getproperty for details).
;    NAME         : the name of the template.
;    OBJ_SELECTED : the index of the selected object. A value of -1 indicates
;                   that no object is selected.
;    OBJNAM       : the name of the selected object.
;
; -----------------------------------------------------------------------------
; METHOD NAME:
;    itool_template::init
;
; PURPOSE:
;    To initialize a new instance of the itool_template object class.
;
; CALLING SEQUENCE:
;    oref = obj_new('itool_template', templatename, new, objnam, x, y)
;
; INPUTS:
;    templatename ; The name of the new template.
;    new          ; An array of flags for the objects.
;    objnam       ; An array of names for the objects.
;    x            ; An array of x-coordinates for the objects.
;    y            ; An array of y-coordinates for the objects.
;
; OPTIONAL INPUTS:
;    
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    oref = The object reference of the new instance of the itool_template
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
;    oref = obj_new('itool_template', oitool, GROUP_LEADER=group_leader)
;    oref->realize
;
; -----------------------------------------------------------------------------
;-


; -----------------------------------------------------------------------------
; Method cleanup
;   Cleans up storage that may have been allocated for each instance of
; the "itool_template" object class.
;   This method is called indirectly, as a result of calling the IDL
; obj_destroy procedure.
; -----------------------------------------------------------------------------
pro itool_template::cleanup
   compile_opt hidden

   ptr_free, self.pnew, self.pobjnam, self.px, self.py
end


; -----------------------------------------------------------------------------
; Method print
;   Prints a template to standard output, or to a file.
;   The LUN keyword specifies the IDL Logical Unit Number of a template file
; that is open for output. If the LUN keyword is absent, output will go
; to where IDL sends its standard output.
; -----------------------------------------------------------------------------
pro itool_template::print, LUN=lun
   compile_opt hidden

   ; Printing will go the standard out, or to a file.
   if n_elements(lun) eq 0L then lun=-1

   if self.numobj gt 0L then begin
      ; This template is non-null (it has at least one object).
      printf, lun, self.name
      printf, lun, self.numobj

      ; Print the objects (their coordinates and their names).
      for  k=0L, self.numobj-1L do begin
         printf, lun, (*self.px)[k], (*self.py)[k], (*self.pobjnam)[k],$
         FORMAT='(2F10.3,4X,A)'
      endfor

      ; Clear the modified flag.
      self.modified = 0
   endif else begin
      print, 'Warning. Null template ' + self.name + ' - not written.'
   endelse
end


; -----------------------------------------------------------------------------
; Method fmt
;   Formats a list of objects for printing and returns the result as a
; string array.
; -----------------------------------------------------------------------------
function itool_template::fmt
   compile_opt hidden

   if self.numobj gt 0 then begin
      ; Return the formatted object list.

      return, string(indgen(self.numobj), format='(I3)' ) + ' ' +$
         string(*self.pobjnam, format='(A20)') +$
         string(*self.px, format='(F10.3)') +$
         string(*self.py, format='(F10.3)')
   endif else begin
      ; Return a null string.
      return, ''
   endelse
end


; -----------------------------------------------------------------------------
; Method addobject
; -----------------------------------------------------------------------------
pro itool_template::addobject, x, y
   if self.numobj eq 0L then begin
      ; This is the anchor position.
      *self.pobjnam = '<default>'
      *self.pnew = [1]
      *self.px = [x]
      *self.py = [y]
      self.numobj = 1L
   endif else begin
      *self.pobjnam = [*self.pobjnam, '<default>']
      *self.pnew = [*self.pnew, 1]
      *self.px = [*self.px, x]
      *self.py = [*self.py, y]
      self.numobj = self.numobj + 1L
   endelse

   self.modified = 1
end


; -----------------------------------------------------------------------------
; Method updateobjects
; Stores a new set of object data. Use this method when the entire list of
; objects must change.
; -----------------------------------------------------------------------------
pro itool_template::updateobjects, new, objnam, x, y
   compile_opt hidden

   if n_elements(new) gt 0L then *self.pnew = new

   if n_elements(objnam) gt 0L then begin
      *self.pobjnam = objnam
   endif

   if n_elements(x) gt 0L then *self.px = x
   if n_elements(y) gt 0L then *self.py = y

   self.numobj = n_elements(*self.px)
   self.modified = 1
end


; -----------------------------------------------------------------------------
; Method deleteobject
;   Deletes an object from the object list.
; -----------------------------------------------------------------------------
pro itool_template::deleteobject
   compile_opt hidden

   ; Are there any objects in this template?
   if self.numobj eq 0L then return

   ; Is one of the objects in this template selected?
   if self.obj_selected lt 0 then return

   ; Simplify pointer syntax.
   pnew = self.pnew
   pobjnam = self.pobjnam
   px = self.px
   py = self.py

   ; Get the object-selected index for the currently-selected template.
   selected = self.obj_selected

   ; Force the name of the selected object to the null string (Object names
   ; are set to non-null values ('<default>') when they are added to
   ; the list.
   (*pobjnam)[selected] = ''

   ; Now, the objects that are to be kept may be found.
   keep = where(*pobjnam ne '', count)

   if count gt 0L then begin
      ; Squash the deleted object from the list, by re-defining the arrays
      ; that define the objects.
      *pnew = (*pnew)[keep]
      *pobjnam = (*pobjnam)[keep]
      *px = (*px)[keep]
      *py = (*py)[keep]

      ; Update the number-of-objects tag in the template structure.
      self.numobj = n_elements(*pobjnam)
   endif else begin
      ; The last object was deleted and the list is now empty.
      ; Update the number-of-objects tag in the template structure.
      self.numobj = 0L

      ; Reduce storage that is no longer needed.
      *pnew = 0
      *pobjnam = ''
      *px = 0
      *py = 0

      ; Set the mode to zero, which is the "Add objects" mode.
      self.mode = 0
   endelse

   ; Either way, there will be no selected objects.
   self.obj_selected = -1L
   self.modified = 1
end


; -----------------------------------------------------------------------------
; Method purgeobjects
;   Purges the objects in the object list.
; -----------------------------------------------------------------------------
pro itool_template::purgeobjects
   compile_opt hidden

   if self.numobj eq 0L then return

   ; Free any storage that may have been allocated for objects.
   ptr_free, self.pnew, self.pobjnam, self.px, self.py

   self.pnew = ptr_new(/ALLOCATE_HEAP)
   self.pobjnam = ptr_new(/ALLOCATE_HEAP)
   self.px = ptr_new(/ALLOCATE_HEAP)
   self.py = ptr_new(/ALLOCATE_HEAP)

   ; Clear certain properties.
   self.numobj = 0L
   self.obj_selected = -1
   self.modified = 1
   self.mode = 0
end


; -----------------------------------------------------------------------------
; Method getproperty
; -----------------------------------------------------------------------------
pro itool_template::getproperty, MODE=mode, MODIFIED=modified, NUMOBJ=numobj,$
   OBJ_SELECTED=obj_selected, NEW=new, OBJNAM=objnam,  X=x, Y=y

   if arg_present(mode) then mode = self.mode
   if arg_present(modified) then modified = self.modified
   if arg_present(numobj) then numobj = self.numobj
   if arg_present(obj_selected) then obj_selected = self.obj_selected

   if self.numobj eq 0L then return

   if arg_present(new) then begin
      if ptr_valid(self.pnew) then new = *self.pnew
   endif

   if arg_present(objnam) then begin
      if ptr_valid(self.pobjnam) then objnam=*self.pobjnam
   endif

   if arg_present(x) then begin
      if ptr_valid(self.px) then x = *self.px
   endif

   if arg_present(y) then begin
      if ptr_valid(self.py) then y = *self.py
   endif
end


; -----------------------------------------------------------------------------
; Method itool_template::changeobjectname
; Changes the name of an object that is highlighted (selected) in the
; object-list widget.
; -----------------------------------------------------------------------------
pro itool_template::changeobjectname, objnam
   ; Nothing to do if no object is selected.
   if self.obj_selected lt 0L then return

   (*self.pobjnam)[self.obj_selected] = objnam
   self.modified = 1
end


; -----------------------------------------------------------------------------
; Method setproperty
;   Sets certain properties of a template instance.
; -----------------------------------------------------------------------------
pro itool_template::setproperty, MODE=mode, NAME=name,$
   OBJ_SELECTED=obj_selected, OBJNAM=objnam

   compile_opt hidden

   modified = 0

   if n_elements(mode) gt 0L then begin
      self.mode = mode
   endif

   if n_elements(name) gt 0L then begin
      self.name = name
      modified = 1
   endif

   if n_elements(obj_selected) gt 0L then begin
      self.obj_selected = obj_selected
   endif

   if n_elements(objnam) gt 0L then begin
      if self.obj_selected ge 0L then begin
         (*self.pobjnam)[self.obj_selected] = objnam
         modified = 1
      endif
   endif

   ; Update the modified flag, in case it changed.
   self.modified = self.modified or modified
end


; -----------------------------------------------------------------------------
; Method init
;   Initializes the properties of each new instance of this object class.
; -----------------------------------------------------------------------------
function itool_template::init, templatename, new, objnam, x, y
   compile_opt hidden

   if new[0] eq 1 then begin
      numobj=0L
   endif else begin
      numobj=n_elements(new)
      self.mode=1
   endelse

   self.name = templatename
   self.obj_selected = -1
   self.numobj = numobj
   self.pnew = ptr_new(new)
   self.pobjnam = ptr_new(objnam)
   self.px = ptr_new(x)
   self.py = ptr_new(y)

   ; Return successful initialization.
   return, 1
end


; -----------------------------------------------------------------------------
; Procedure itool_template__define
;   The object-definition procedure for the "itool_template" object class.
; This procedure defines the "itool_template" object class automatically,
; when the obj_new() function is invoked.
;
; Attributes:
;   name         : The name of the template.
;   mode         : The mode flag:
;                  0="Add Objects"
;                  1="Active Template Photometry"
;   modified     : The modified flag.
;   obj_selected : The index of the selected object.
;   numobj       : The number of objects in the template.
;   pnew         : A pointer to the "new" flag array
;   pobjnam      : A pointer to the array of object names.
;   px           : A pointer to the array of x-coordinates
;   py           : A pointer to the array of y-coordinates
; -----------------------------------------------------------------------------
pro itool_template__define
   dummy = {itool_template,$
      name:'',$
      mode:0,$
      modified:0,$
      obj_selected:0L,$
      numobj:0L,$
      pnew:ptr_new(),$
      pobjnam:ptr_new(),$
      px:ptr_new(),$
      py:ptr_new() $
   }
end

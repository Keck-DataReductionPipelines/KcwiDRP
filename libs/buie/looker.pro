;+
; NAME:
;  looker
; PURPOSE:
;  Visual identification and measurement of moving objects in digital images.
; DESCRIPTION:
;
;  This program handles visually inspecting large digital images and permiting
;     measuring positions of the objects found.  The object lists created
;     by this program are compatible with similar files used by ASTROM,
;     GARTH, and FINDSRC.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  looker
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  GAIN     - Gain of CCD (e-/DN), default=3.0
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
;                    The middle column is a flag. It may be K, for Keyword,
;                    T, for Template, or V, for Value. If it is V, the contents
;                    of the third field on that line should make sense for the
;                    name in the first field.
;
;  OBJRAD    - Radius of the object aperture to use when centroiding objects.
;                Default is 6 pixels.
;
;  PATH      - Optional path for original image directory.
;                If not specified, the current directory is used.
;
;  PSCALE    - Nominal plate scale of images in arcsec/pixel (default=0.26)
;
;  XSIZE     - x size, in pixels, of the main window (default=1070)
;
;  YSIZE     - x size, in pixels, of the main window (default=820)
;
;  ZXSIZE    - x size, in pixels, of the score window (default=148)
;
;  ZXSIZE    - x size, in pixels, of the score window (default=296)
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; PROCEDURE:
;
;  This program is intended to work with a collect of images taken on a single
;    night of observing.  Generally it will be best to work in a directory
;    separate from the raw (or processed) image data.   The optional keyword,
;    PATH, is used to point to the directory containing the images.  You
;    will also need to provide a keyword correspondence file for decoding the
;    FITS headers.
;
;  The images can either be in a simple format of one image per file or can
;    be collections under the multi-group FITS format as used by the KPNO
;    MOSAIC camera.
;
;  Images that cover the same region of sky are considered to be a group
;    identified by the "field name" which is taken from the OBJECT keyword
;    in the FITS header.  If the images are multi-group files then the group
;    being processed is identified by an extension tag of the form "xN" where
;    N is the extension number being processed.
;
;  You start using this program by creating a "new" field (under the File
;    menu option).  To start a new field you will need to select an image
;    that serves as the first epoch view of the field.  If you have images
;    of varying quality it would be wise to make the first image selected be
;    one of the best.  However, under most circumstances you will pick the
;    earliest image.  After selecting the first frame it will be displayed as
;    a red image.  This image is always loaded into the red display channel.
;
;  To begin looking for moving objects, you must have at least two images
;     of the field.  Add the second (or third, ...) to the list for this field
;     with the appropriate File menu option.  If you already have multiple
;     images identified then you can simply select the appropriate "secondary"
;     image.  The secondary image is another image of the same field at a
;     different time from the first image loaded.  The secondary image is
;     always loaded into the blue and green planes of the display.  Any object
;     that appears in both epochs at the same place will look white.  An
;     object that moves will show as a red/cyan pair of images.
;
;  Before you can effectively identify object, you must register the secondary
;     frame relative to the first image.  In the Object status area of the
;     tool it will indicate "Updating Offset" if image registration is in
;     progress.  This mode is automatically entered when adding a new image to
;     the list.  You can also enter this mode on your own with the "Adjust
;     Frame Registration" option on the File menu.  In this mode, click left
;     on a red (first epoch) star image.  Then, click middle on the same
;     star in the blue (second epoch) image.  The relative offset will be
;     computed and the second frame will be shifted to align with the first
;     frame.  When you are done with the registration click the right button
;     to exit the registration mode.
;
;  The three mouse buttons are used for different functions in all three "image"
;     windows.  These windows are called "Main" -- the largest window which
;     shows the aligned image pair; "Zoom" -- small, square window in the lower
;     right that shows a logrithmic stretch on the image just measured; and
;     "Score" -- window in the upper right that is used for navigating and
;     keeping track of what you've looked at so far.  The score window shows
;     a minified schematic of the full sized image.  Areas that have been
;     looked at are marked in green with the intensity increasing for each
;     time the area is viewed.  The current display area is highlighted with
;     a purple border.
;
;  Here are the principle mouse function in each window:
;     Score -
;        Left: Center Main window at location of cursor.
;        Middle: Move by half a image window in the direction of greatest
;                 offset (vertical or horizontal) between the cursor and
;                 the location of the current display area.
;        Right:  Center Main window on the "slot" whose center is closest
;                 to the cursor.  The "slots" are integral mappings of the
;                 main window into the full image and are highly quantized.
;                 Clicking right on a black region will always give you new
;                 areas to view.
;
;     Main -
;        Left:   Measure object nearest cursor in first epoch (red) image.
;                 The location clicked is used as a starting point for the
;                 measurement and eventual position comes from a centroid
;                 centered on the object even if you don't click precisely
;                 on the object.  This action will cause the Zoom window to be
;                 updated with an extraction centered on the centroided
;                 location and a red circle will overdrawn on the object.
;        Middle: Same as the left button except the second epoch (cyan) image
;                 is measured.
;        Right:  Select the nearest object (by its first epoch position)
;                 to be the current object.  You can see this position on
;                 the main image by the red circles and associated object
;                 number label.  You must click within 50 pixels of the
;                 object to select it.  When an object is selected, the
;                 last known centroid location on the first epoch image
;                 is used to update the zoom window as if you had just clicked
;                 left on the object.
;
;        Note, there is are a few protections built into this window.  If
;        the main window location changes and the current object is no longer
;        on the window, the object changes to be a NEW object.  If you click
;        too far from a previous measurement on the same object you will get
;        a warning box in case you meant to start a new object.
;
;     Zoom -
;        Left:   Re-locate the object position to the cursor position without
;                 changing the zoom window view.  The overdrawn circle will
;                 move, but not the image.  Use this feature to override an
;                 automatic centroid position that is corrupted from field
;                 stars, cosmic rays, or chip defects.
;        Middle: Same as left except the zoom window is re-centered on the
;                 location where you clicked.  The display stretch is also
;                 recomputed using the pixel at the center as the peak
;                 intensity for the object.
;        Right:  Not used.
;
;  There is a pattern to the measurement process.  On the first pair of images
;     you will generally click left then middle on each new object.  You
;     will either click "New Object" or this will be automatic if object is
;     on a new piece of the image.  Once all the objects are found and measured
;     in the first pair of images you will then look at the second pair (or
;     third, ...)  Click left in the score window to center the main window,
;     then click right on the red circle then click middle on the cyan image
;     for that object.
;
;  If you happen to click on a pair of objects that really are just chip
;     defects that look like a moving object, you will get a warning.  Unless
;     you are very, very sure it is real you should not keep such objects.
;
;  The "Snap Object" tool on the pull-down menu will create a set of 10 files
;     which are extractions on the current object.  The files all have the
;     object name as the root of the name with a suffix of .tiff.  The code
;     between _ and . determines the type of extraction:
;       1   - linear stretch, centered on position 1 in frame 1
;       1c  - linear stretch, centered on mid-point in frame 1
;       1l  - power law stretch, centered on position 1 in frame 1
;       1lc - power law stretch, centered on mid-point in frame 1
;       2   - linear stretch, centered on position 2 in frame 2   
;       2c  - linear stretch, centered on mid-point in frame 2    
;       2l  - power law stretch, centered on position 2 in frame 2
;       2lc - power law stretch, centered on mid-point in frame 2 
;       c   - color composite, linear stretch
;       l   - color composite, power law stretch.
;
; RESTRICTIONS:
;
;  Even though you appear to be able to browse to a different directory
;  for the raw data, don't.  All the raw frames should be in the same directory
;  and pointed to by the PATH keyword.  Also, all the field names (aka, object
;  lists) should be kept in the same directory as well.
;
;  It is assumed that images from the same night are the same size.
;
;  For multi-group images, it is assumed that all the relevant exposure
;  information can be culled from the extension header.  In other words,
;  the bulk of the primary header must be found in each of the extension
;  headers.
;
;  This entire scheme is predicated on there being just a "few" images on
;  each field.  In theory there is no upper limit to the number of images
;  on a field but in practice too large a number gets to be rather cumbersome
;  in looking at the .obj files.  Basically things get really cluttered if
;  there are too many related frames.  This isn't too bad since you don't
;  need that many frames to get good astrometry anyway.  Three frames tells
;  you just as much as 100 frames as long as the time span from first to
;  last is the same.
;
; MODIFICATION HISTORY:
;  98/10/27, Written by Marc W. Buie, Lowell Observatory
;  98/11/20, MWB, added OBJRAD keyword
;  99/03/19, MWB, fixed bug that corrupted obj files on adding 3rd frame.
;  99/03/22, MWB, fixed bug for missing dt on non-group images.
;  99/03/30, MWB, added rudimentary hardcopy function (File menu).
;  99/04/01, MWB, fixed minor bug in cursor handling in zoom window.
;  99/04/22, MWB, added Autoload function and menu toggle
;  99/04/26, MWB, added rudimentary b/w blinking
;  99/12/07, MWB, fixed problem with overwriting an existing .obj file with
;                 non-group data, also fixed problem that would allow data
;                 processing to proceed without having been queried for a
;                 name to go with initials.
;  2001/01/17, MWB, added "Snap Object" tool.
;  2001/04/27, MWB, added FITS sub-image saves to "Snap Object"
;  2001/09/04, MWB, added "Toggle Color Mode" option, displays difference image
;  2002/02/19, MWB, fixed bug that caused crash when going to a different
;                 field when new field had fewer images than current field.
;  2002/09/25, MWB, added GAIN keyword
;  2002/11/20, MWB, changed skysclim call to take advantage of LOWCLIP/HICLIP
;  2003/07/02, MWB, added new Edit menu button and added a function to set all
;                     ? objects to n in one operation.  Please use with care.
;  2003/07/30, MWB, eliminated all 'readfits' calls, changed to fits_read
;  2004/07/09, MWB, added support/protection regarding salted(fake) objects.
;  2004/07/18, MWB, changed all Findfile calls to file_search
;  2004/11/12, MWB, changed where object id labels plot in main window
;  2005/03/30, MWB, now plots a predicted location for an object when 3 or
;                     more frames are involved.  Also, object is no longer
;                     automatically deselected when it goes off the frame
;                     when there are 3 or more frames (with two frames the
;                     deselect works as before).
;  2005/05/06, MWB, desensitize Previous and Next buttons when in frame
;                     offset mode, also suppress right button exit of frame
;                     registration when two frames have not yet been
;                     measured.
;  2006/11/14, MWB, fixed a minor display problem with difference imaging.
;  2007/01/03, MWB, added fine adjustment to registration controls.
;-

;------------------------------------------------------------------------------
pro looker_cleanup, tlb

   widget_control, tlb, get_uvalue=state
   looker_svobjlist,state
   ptr_free,(*state).r
   ptr_free,(*state).g
   ptr_free,(*state).flags
   ptr_free,(*state).fntrip
   ptr_free,(*state).fullname
   ptr_free,(*state).hdr1
   ptr_free,(*state).hdr2
   ptr_free,(*state).im1
   ptr_free,(*state).im2
   ptr_free,(*state).info1
   ptr_free,(*state).info2
   ptr_free,(*state).initials
   ptr_free,(*state).idstr
   ptr_free,(*state).offvals
   ptr_free,(*state).dt
   ptr_free,(*state).xyvals
   ptr_free,(*state).rate
   ptr_free,(*state).dir
   ptr_free, state
   setusym,-1

end

;----------------------------------------------------------------------------
pro looker_computerate, state

   if (*state).nobj eq 0 or (*state).info2 eq ptr_new() or $
      (*state).idxfn2 lt 0 then return

   if (*state).idxfn2 ge (*state).nfiles then begin
      (*state).idxfn2 = -1
      return
   endif

   ; Compute rate of motion and direction for all objects
   xa1 = (*(*state).xyvals)[0,*]
   ya1 = (*(*state).xyvals)[1,*]
   idx = (*state).idxfn2*2
   xb1 = (*(*state).xyvals)[idx+2,*] - (*(*state).offvals)[idx]
   yb1 = (*(*state).xyvals)[idx+3,*] - (*(*state).offvals)[idx+1]
   dt  = float((*(*state).info2).jd-(*(*state).info1).jd)*24.0
   rate=sqrt((xb1-xa1)^2+(yb1-ya1)^2)/dt*(*state).pscale
   dir =atan(yb1-ya1,xb1-xa1) * !radeg
   z=where(xa1 lt 0.0 or ya1 lt 0.0 or $
           (*(*state).xyvals)[idx+2,*] lt 0.0 or $
           (*(*state).xyvals)[idx+3,*] lt 0.0,count)
   if count ne 0 then rate[z]=0.0

   *(*state).rate = rate[*]
   *(*state).dir  = dir[*]

end

;----------------------------------------------------------------------------
; This takes care of refreshing the visual display in all relevant windows
; based on the current state.  Note that state is a pointer to an anonymous
; structure.
pro looker_display, state, NOLOAD=noload, NOSCORE=noscore

   if not keyword_set(noload)  then noload=0
   if not keyword_set(noscore) then noscore=0

   ; Do nothing if the object list name is blank.
   if (*state).fnobj eq '' then return

   setusym,-1

   ; Did the object list change?  If so, refresh internal information and display.
   if (*state).fnobj ne (*state).lastobj then begin

      if not exists( (*state).objpath + (*state).fnobj ) then begin
         print,'Error!  ',(*state).objpath + (*state).fnobj,' does not exists.'
         return
      endif

      ; Read the object list
      rdoblist,(*state).objpath+(*state).fnobj,nobj,fntrip,dt,offvals, $
               xyvals,flags,idstr,nfiles

      ptr_free,(*state).fntrip
      ptr_free,(*state).dt
      ptr_free,(*state).offvals
      ptr_free,(*state).xyvals
      ptr_free,(*state).flags
      ptr_free,(*state).idstr
      ptr_free,(*state).rate
      ptr_free,(*state).dir
      (*state).nobj    = nobj
      (*state).nfiles  = nfiles
      (*state).fntrip  = ptr_new(fntrip)
      (*state).offvals = ptr_new(offvals)
      (*state).dt      = ptr_new(dt)
      if (*state).idxfn2+1 ge (*state).nfiles then begin
         if (*state).nfiles eq 2 then $
            (*state).idxfn2 = 0 $
         else $
            (*state).idxfn2 = -1
      endif

      if nobj ne 0 then begin
         (*state).xyvals  = ptr_new(xyvals)
         (*state).flags   = ptr_new(flags)
         (*state).idstr   = ptr_new(idstr)
         (*state).rate    = ptr_new(findgen(nobj))
         (*state).dir     = ptr_new(findgen(nobj))
         looker_computerate,state
      endif

      (*state).x0 = 0
      (*state).y0 = 0

      fn1 = (*state).path+fntrip[0]
      if exists(fn1+'.fits') then ft='.fits' else ft=''

      if not exists(fn1+ft) then begin
         print,'Error!  Image file ',fn1+ft,' could not be found.'
         (*state).fnobj = ''
         return
      endif

      hdr=headfits(fn1+ft)
      numext=sxpar(hdr,'NEXTEND') > 1
      (*state).numext = numext

      if numext gt 1 then begin
         ; Bust up the object name and get the extension.
         words=strmid((*state).fnobj,0,strlen((*state).fnobj)-4)
         xpos = strpos(words,'x',/reverse_search)
         if xpos ge 1 then begin
            (*state).field  = strmid(words,0,xpos)
            (*state).exttag = strmid(words,xpos+1,999)
         endif else begin
            (*state).field  = words
            (*state).exttag = ''
         endelse
      endif else begin
         words=strsplit((*state).fnobj,'.',/extract)
         (*state).field = words[0]
         (*state).exttag = ''
      endelse

      if ptr_valid( (*state).im1  ) then ptr_free,(*state).im1
      if ptr_valid( (*state).hdr1 ) then ptr_free,(*state).hdr1

      if (*state).exttag ne '' then begin
         print,'Load first image ',fn1,'x',(*state).exttag
         fits_read,fn1+ft,tmp,hdr1,exten=fix((*state).exttag),/noscale
         (*state).im1  = ptr_new(tmp,/no_copy)
         (*state).hdr1 = ptr_new(hdr1)
      endif else begin
         print,'Load first image ',fn1+ft
         fits_read,fn1+ft,tmp,hdr1,/noscale
         (*state).im1 = ptr_new(tmp,/no_copy)
         (*state).hdr1 = ptr_new(hdr1)
      endelse

      sz=size(*(*state).im1)
      (*state).nx = sz[1]
      (*state).ny = sz[2]

      ; Compute scale factor in each axis to go from full frame to score frame.
      xf = float((*state).nx) / (*state).szsx
      yf = float((*state).ny) / (*state).szsy

      ; The biggest factor is the one we'll use.
      (*state).sf = max([xf,yf])

      parsekey,hdr1,(*state).hdrlist,info1
      if ptr_valid( (*state).info1 ) then ptr_free,(*state).info1
      (*state).info1 = ptr_new(info1)

      widget_control, (*state).mainbase, UPDATE=0
      widget_control, (*state).fieldid,  SET_VALUE=(*state).fnobj
      widget_control, (*state).fn1id,    SET_VALUE=fntrip[0]
      widget_control, (*state).fn2id,    SET_VALUE=''
      widget_control, (*state).mainbase, UPDATE=1

      (*state).lastobj = (*state).fnobj
      (*state).lastfn2 = ''
      if not (*state).autoload or (*state).idxfn2 ge nfiles-1 then $
         (*state).idxfn2  = -1
      (*state).curobj  = -1
      (*state).curframe = 0
      looker_setzoom,state
      noload=0
      looker_updatescore,state,/reset
      looker_objinfo,state

      if (*state).nfiles eq 2 then begin
         widget_control, (*state).mainbase, UPDATE=0
         widget_control, (*state).fn2id,    SET_VALUE=(*(*state).fntrip)[1]
         widget_control, (*state).mainbase, UPDATE=1
      endif
   endif


   ; Try to set up the second file automatically
   if (*state).autoload then begin
      if (*state).nfiles ne 2 and (*state).idxfn2+1 gt 0 then begin
         widget_control, (*state).mainbase, UPDATE=0
         widget_control, (*state).fn2id, $
                         SET_VALUE=(*(*state).fntrip)[(*state).idxfn2+1]
         widget_control, (*state).mainbase, UPDATE=1
      endif
   endif

   widget_control, (*state).fn2id,  get_value=fn2
   if fn2 ne '' and (fn2 ne (*state).lastfn2 or (*state).idxfn2 eq -1) then begin
      fn = (*state).path+fn2
      if exists(fn+'.fits') then ft='.fits' else ft=''
      if not exists(fn+ft) then begin
         print,'Error!  Image file ',fn+ft,' could not be found.'
         return
      endif

      if ptr_valid( (*state).im2  ) then ptr_free,(*state).im2
      if ptr_valid( (*state).hdr2 ) then ptr_free,(*state).hdr2

      if (*state).exttag ne '' then begin
         print,'Load second image ',fn2,'x',(*state).exttag
         fits_read,fn+ft,tmp,hdr2,exten=fix((*state).exttag),/noscale
         (*state).im2  = ptr_new(tmp)
         (*state).hdr2 = ptr_new(hdr2)
      endif else begin
         print,'Load second image ',fn2+ft
         fits_read,fn+ft,tmp,hdr2,/noscale
         (*state).im2 = ptr_new(tmp,/no_copy)
         (*state).hdr2 = ptr_new(hdr2)
      endelse

      parsekey,hdr2,(*state).hdrlist,info2
      if ptr_valid( (*state).info2 ) then ptr_free,(*state).info2
      (*state).info2 = ptr_new(info2)

      ; need index of current file into offset array
      z=where(*(*state).fntrip eq fn2,count)
      z=z[0]-1
      (*state).idxfn2 = z
      looker_computerate, state

      ; set obj index to a new object id if frame 2 newly loaded.
      if (*state).lastfn2 eq '' then $
         (*state).curobj = (*state).nobj
      looker_objinfo,state

      (*state).lastfn2 = fn2
      (*state).curframe = 0
      looker_setzoom,state
      looker_updatescore,state,/reset
      noload=0

   endif

   if not noload then begin

      ; Making a working copy of the desired corner of the display.
      x0 = (*state).x0
      y0 = (*state).y0

      ; Compute upper extraction boundary from main array
      x1 = x0 + (*state).szx - 1
      y1 = y0 + (*state).szy - 1

      ; Limit upper boundary to edge of array by shifting to edge (if needed).
      xshift = x1 - ( (*state).nx-1 ) > 0
      yshift = y1 - ( (*state).ny-1 ) > 0
      x0 = x0 - xshift
      x1 = x1 - xshift
      y0 = y0 - yshift
      y1 = y1 - yshift

      ; Now check to make sure we aren't pushing past the lower or left edges.
      xshift = x0 < 0
      yshift = y0 < 0
      x0 = x0 - xshift
      x1 = x1 - xshift
      y0 = y0 - yshift
      y1 = y1 - yshift

      ; Final check in case the display image is larger than full image.
      if x1 gt (*state).nx-1 then x1 = (*state).nx-1
      if y1 gt (*state).ny-1 then y1 = (*state).ny-1

      ; If display image is larger than full image, must first blank out rgb.
      if x1-x0+1 lt (*state).szx or y1-y0+1 lt (*state).szy then $
         (*(*state).r)[*] = 0B

      ; Extract sub-image from frame 1
      sub = (*(*state).im1)[x0:x1,y0:y1]

      ; Get the sky scaling levels for the subimages.
      widget_control, (*state).minsigid, GET_VALUE=dm
      widget_control, (*state).maxsigid, GET_VALUE=dp
      dm = float(dm[0])
      dp = float(dp[0])

      ; Scale sub-image for red image (frame 1)
      skysclim,sub,lowval,hival,amean,asig,lowclip=0.1,hiclip=0.9
      alowval=amean + dm*asig
      ahival =amean + dp*asig
      (*(*state).r)[0,0] = bytscl(sub,min=alowval,max=ahival,top=255)
      (*state).x0 = x0
      (*state).y0 = y0
      (*state).highval1 = ahival
      (*state).lowval1  = alowval

      if fn2 ne '' then begin

         ; offset between frame 2 and reference frame
         xoff = fix( (*(*state).offvals)[(*state).idxfn2*2]   + 0.5 )
         yoff = fix( (*(*state).offvals)[(*state).idxfn2*2+1] + 0.5 )

         ; extraction section from full frame 2
         x0a = x0 + xoff
         x1a = (x1 + xoff) < ( (*state).nx-1 ) ; don't go beyond end of frame
         y0a = y0 + yoff
         y1a = (y1 + yoff) < ( (*state).ny-1 )

         ; location that this will paste into
         i0 = 0 - (x0a < 0)  ; don't go past edge
         i1 = x1a-x0a
         j0 = 0 - (y0a < 0)
         j1 = y1a-y0a

         ; Adjust start if region is off edge
         x0a = x0a + i0
         y0a = y0a + j0

         if (*state).colormode eq 1 then begin

            ; Grab overlap region from frame 2 into temporary array
            sub = 0.0 ; kill memory from last use first
            sub = (*(*state).im2)[x0a:x1a,y0a:y1a]

            ; Deterine the sky scaling for this subimage.
            skysclim,sub,lowval,hival,amean,asig,lowclip=0.1,hiclip=0.9
            alowval=amean + dm*asig
            ahival =amean + dp*asig

            ; If display image is larger than full image, must first blank out rgb.
            if x1a-x0a+1 lt (*state).szx or y1a-y0a+1 lt (*state).szy then $
               (*(*state).g)[*] = 0B

            (*(*state).g)[i0,j0]=bytscl(sub,min=alowval,max=ahival,top=255)
            (*state).highval2 = ahival
            (*state).lowval2  = alowval

         endif else begin
            ; Grab overlap region from frame 2 into temporary array
            sub1 = sub
            sub1[i0,j0] = (*(*state).im2)[x0a:x1a,y0a:y1a]

            ; difference the images
            sub = temporary(float(sub))-float(sub1)

            ; Deterine the sky scaling for this subimage.
            skysclim,sub,lowval,hival,amean,asig,lowclip=0.1,hiclip=0.9
            alowval=amean + dm*asig
            ahival =amean + dp*asig

            (*(*state).r)=bytscl(sub,min=alowval,max=ahival,top=255)
            (*(*state).g)=(*(*state).r)
            (*state).highval1 = ahival
            (*state).lowval1  = alowval
            (*state).highval2 = ahival
            (*state).lowval2  = alowval
         endelse

         if not noscore then looker_updatescore,state,x0,x1,y0,y1

      endif else begin
         (*(*state).g)[*] = 0B
      endelse

   endif

   widget_control, (*state).drawwin, get_value=winnum
   WSET, winnum

   tv,[[[*(*state).r]],[[*(*state).g]],[[*(*state).g]]],true=3
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=(*state).x0+[0,(*state).szx-1], $
      yr=(*state).y0+[0,(*state).szy-1], $
      xstyle=5,ystyle=5,/noerase

   if (*state).nobj ne 0 then $
      z=where( (*(*state).flags) ne 'n', count) $
   else $
      count = 0

   if count ne 0 and not (*state).nomarks and (*state).nfiles gt 1 then begin
      for i=0,(*state).nobj-1 do begin

         if (*(*state).flags)[i] eq 'n' then continue

         pospred = bytarr((*state).nfiles)
         xraw = (*(*state).xyvals)[indgen((*state).nfiles)*2  ,i]
         yraw = (*(*state).xyvals)[indgen((*state).nfiles)*2+1,i]
         xoff = fltarr((*state).nfiles)
         yoff = fltarr((*state).nfiles)
         xoff[1:(*state).nfiles-1] = $
            (*(*state).offvals)[indgen((*state).nfiles-1)*2]
         yoff[1:(*state).nfiles-1] = $
            (*(*state).offvals)[indgen((*state).nfiles-1)*2+1]
         xpos = xraw - xoff
         ypos = yraw - yoff

         zb = where(xraw lt 0.0 and yraw lt 0.0, countbad)
         zg = where(xraw ge 0.0 and yraw ge 0.0, countg)
         if countbad ne 0 and countg le 1 then begin
            pospred[zb] = 2B
         endif else if countbad ne 0 then begin
            dt = [0.,(*(*state).dt)]
            if countg eq 2 then begin
               interp,dt[zg],xpos[zg],dt[zb],xpred
               xpos[zb] = xpred
               interp,dt[zg],ypos[zg],dt[zb],ypred
               ypos[zb] = ypred
               pospred[zb] = 1B
            endif else begin
               xc = poly_fit(dt[zg],xpos[zg],1)
               xpos[zb] = poly(dt[zb],xc)
               yc = poly_fit(dt[zg],ypos[zg],1)
               ypos[zb] = poly(dt[zb],yc)
               pospred[zb] = 1B
            endelse
         endif

         pfs = 0
         for j=0,(*state).nfiles-1 do begin
            if pospred[j] eq 2 then continue

            ; select color for symbol
            if j eq 0 then color='0000ff'xl $
            else if j eq (*state).idxfn2+1 then color='ffff00'xl $
            else           color='40ff30'xl

            ; select symbol size
            if pospred[j] eq 0 then begin
               ss = 2.5
               cs = 1.0
               dtx = 9.0
               dty = 0.0
               if pfs eq 0 then begin
                  pfs = 1
                  dotag = 1
               endif
            endif else begin
               ss = 5.0
               cs = 1.5
               color='ff00ff'xl
               dtx = 13.0
               dty = 0.0
               dotag = 1
            endelse

            oplot,[xpos[j]],[ypos[j]],psym=8,color=color,symsize=ss

            if dotag then begin
               if (*(*state).flags)[i] eq 'y' then $
                  color='00ffff'xl $
               else if (*(*state).flags)[i] eq 's' then $
                  color='19e212'xl $
               else $
                  color='6020ff'xl
               tag = strb36(i,pad=2)
               xyouts,xpos[j]+dtx,ypos[j]+dty,tag, $
                  align=0.0,color=color,charsize=cs
               dotag=0
            endif

         endfor
      endfor

;      for i=0,n_elements((*(*state).fntrip))-1 do begin
;
;         ; extract just the objects to be plotted
;         xyvals = (*(*state).xyvals)[i*2:i*2+1,z]
;
;         ; only plot those that are valid measurements
;         zz = where(xyvals[0,*] gt 0.0 and xyvals[1,*] gt 0.0, count)
;
;         if count ne 0 then begin
;
;            ; select color for symbol
;            if i eq 0 then color='0000ff'xl $
;            else if i eq (*state).idxfn2+1 then color='ffff00'xl $
;            else           color='40ff30'xl
;
;            ; compute offset for this object
;            if i eq 0 then begin
;               xoff = 0
;               yoff = 0
;            endif else begin
;               xoff = (*(*state).offvals)[(i-1)*2]
;               yoff = (*(*state).offvals)[(i-1)*2+1]
;            endelse
;
;            oplot,xyvals[0,zz]-xoff, $
;                  xyvals[1,zz]-yoff, $
;                  psym=8,color=color,symsize=2.5
;
;            if i eq 0 then begin
;               for j=0,count-1 do begin
;                  if (*(*state).flags)[z[zz[j]]] eq 'y' then $
;                     color='00ffff'xl $
;                  else if (*(*state).flags)[z[zz[j]]] eq 's' then $
;                     color='19e212'xl $
;                  else $
;                     color='6020ff'xl
;                  tag = strb36(z[zz[j]],pad=2)
;                  xyouts,xyvals[0,zz[j]]+9.0,xyvals[1,zz[j]],tag, $
;                     align=0.0,color=color
;               endfor
;            endif
;
;         endif
;
;         ; plot predicted location for spots that are invalid, but only
;         ;   if there are at least two valid measurements.
;
;      endfor

      if (*state).lines and not (*state).nomarks then begin
         for i=0,(*state).nobj-1 do begin
            if (*(*state).flags)[i] ne 'n' then begin
               xyvals = (*(*state).xyvals)[*,i]
               xvals = xyvals[indgen(n_elements(xyvals)/2)*2]
               yvals = xyvals[indgen(n_elements(xyvals)/2)*2+1]
               xoff  = [0.,(*(*state).offvals)[indgen(n_elements(xyvals-1)/2)*2]]
               yoff  = [0.,(*(*state).offvals)[indgen(n_elements(xyvals-1)/2)*2+1]]
               z=where(xvals ge 0. and yvals ge 0.,count)
               if count ge 2 then begin
                  z=[z[0],z[count-1]]
                  x = xvals[z]-xoff[z]
                  y = yvals[z]-yoff[z]
                  ang = atan(y[1]-y[0],x[1]-x[0])+!pi/2
                  dx = (*state).radius*cos(ang)
                  dy = (*state).radius*sin(ang)
                  oplot,x+dx,y+dy,color='702080'xl
               endif
            endif
         endfor
      endif

   endif

end

;------------------------------------------------------------------------------
pro looker_getinitials,state,newinitials

   widget_control, (*state).initialsid, GET_VALUE=curinitials
   curinitials=curinitials[0]

   if curinitials eq '' then begin
      text = [ $
         'You have not entered your initials next to the "Next/Previous" buttons.  This', $
         'information is needed to track who was responsible for a given measurement.', $
         'You must provide this information before you can proceed with ANY operation', $
         'in this program.']
      result=dialog_message(text,/ERROR)
      newinitials=''
      return
   endif

   newinitials=strcompress(strupcase(curinitials),/remove_all)
   if newinitials ne '' then begin
      z=where(newinitials eq (*(*state).initials), count)
      if count eq 0 then begin
         print,'Add [',newinitials,'] to id list.'
         repeat begin
            newname = qinput(PROMPT='Please enter the name to go with ' + $
                                    'initials ['+newinitials+']' )
            newname=strtrim(strcompress(newname[0]),2)
         endrep until newname ne ''
         if (*state).n_initials eq 0 then begin
            (*(*state).initials)[0] = newinitials
            (*(*state).fullname)[0] = newname
            (*state).n_initials     = 1
         endif else begin
            *(*state).initials   = [*(*state).initials,newinitials]
            *(*state).fullname   = [*(*state).fullname,newname]
               (*state).n_initials = (*state).n_initials + 1
         endelse
         wrmatch,(*(*state).initials),(*(*state).fullname),'looker.ids'
      endif else if count ne 1 then begin
         print,'Error!, ',newinitials,' is multiply defined in looker.ids'
         print,'This should not happen but if it does, please exit looker'
         print,'and examine and fix the looker.ids file to remove the duplicate'
         print,'or otherwise fix the problem.'
      endif
   endif

end

;------------------------------------------------------------------------------
pro looker_getsubimage,state,xpos,ypos,frame,wid,sky,subim

   ; Extract image sub-section around object, watch out for image edges.
   x10 = fix(xpos) - fix(wid)/2
   x20 = fix(x10)  + fix(wid) - 1
   y10 = fix(ypos) - fix(wid)/2
   y20 = fix(y10)  + fix(wid) - 1

   x1=max([x10,0])
   x2=min([x20,(*state).nx-1])
   y1=max([y10,0])
   y2=min([y20,(*state).ny-1])

   subim=replicate(sky,wid,wid)

   if frame eq 1 then $
      subim[x1-x10,y1-y10] = (*(*state).im1)[x1:x2,y1:y2] $
   else $
      subim[x1-x10,y1-y10] = (*(*state).im2)[x1:x2,y1:y2]

end

;------------------------------------------------------------------------------
pro looker_measure,state,x,y,exact,error,xpos,ypos,maxsig,sky,skysig, $
                   NOUPDATE=noupdate

   error = 0

   ; if measuring an "old" object, give some protection against accidentally
   ;   clicking much too far from last known position.
   if not keyword_set(noupdate) and (*state).nobj ge 0 and $
      (*state).curobj ne (*state).nobj then begin

      if (*state).curframe eq 1 then begin
         oldx = (*(*state).xyvals)[ 0, (*state).curobj ]
         oldy = (*(*state).xyvals)[ 1, (*state).curobj ]
      endif else begin
         oldx = (*(*state).xyvals)[ ((*state).idxfn2+1)*2,   (*state).curobj ]
         oldy = (*(*state).xyvals)[ ((*state).idxfn2+1)*2+1, (*state).curobj ]
      endelse

      dist = sqrt( (x-oldx)^2 + (y-oldy)^2 )

      if dist gt 50.0 and oldx ge 0.0 and oldy ge 0.0 then begin
         text = [ $
            'The position you just clicked on is quite far from the last known', $
            'position of this object.  Perhaps you forgot to change objects or', $
            'start a new one?  In any case, if you made a mistake just click', $
            'Abort and this operation will be aborted.  If you want to proceed,', $
            'just click on the Continue button.']
         if qannounc(text,FALSE='Continue',TITLE='Position Warning', $
                     TRUE='Abort Measurement',xsize=72,ysize=5) then begin
            error=1
            return
         endif
      endif

   endif else if not keyword_set(noupdate) and $
                 (*state).curobj eq (*state).nobj and $
                 (*state).curframe eq 2 then begin

      text = [ $
         'The current object has not been measured on the first frame yet.', $
         'This usually happens when you have forgotten to select the object for', $
         'which you are trying to measure a second position.  If you meant to', $
         'measure an object for which there is no position in the first frame,', $
         'then click Continue to override this warning.  If you did not mean to', $
         'make a measurement on a new object, then click Abort Measurement, right', $
         'click the object to select it and then try again.']
      if qannounc(text,FALSE='Continue',TITLE='Right-click Measurement Warning', $
                  TRUE='Abort Measurement',xsize=72,ysize=7) then begin
         error=1
         return
      endif

   endif

   ; Make the measurement
   if (*state).curframe eq 1 then $
      basphote,(*state).gain,*(*state).im1,(*(*state).info1).exptime, $
         x,y,(*state).radius,(*state).radius+10.0,(*state).radius+30.0, $
         boxmrad = (*state).radius > 5,fwhm=fwhm,max=maxsig,exact=exact, $
         mag=mag,err=err,/nolog,pscale=(*state).pscale, $
         fname=(*(*state).fntrip)[0],name=(*state).curobjname, $
         xcen=xpos,ycen=ypos,skymean=sky,skyerr=skysig $
   else $
      basphote,(*state).gain,*(*state).im2,(*(*state).info1).exptime, $
         x,y,(*state).radius,(*state).radius+10.0,(*state).radius+30.0, $
         boxmrad = (*state).radius > 5,fwhm=fwhm,max=maxsig,exact=exact, $
         mag=mag,err=err,/nolog,pscale=(*state).pscale, $
         fname=(*(*state).fntrip)[(*state).idxfn2+1],name=(*state).curobjname, $
         xcen=xpos,ycen=ypos,skymean=sky,skyerr=skysig

   if not keyword_set(noupdate) then begin

      looker_getinitials,state,curinitials

      ; If new object, add new empty entry to list.
      if (*state).nobj eq 0 then begin
         (*state).xyvals = ptr_new(replicate(-1.0,2*n_elements((*(*state).fntrip))))
         (*state).flags  = ptr_new(['y'])
         (*state).idstr  = ptr_new([curinitials])
         (*state).nobj   = 1
         (*state).rate   = ptr_new(fltarr(1))
         (*state).dir    = ptr_new(fltarr(1))
      endif else if (*state).curobj eq (*state).nobj then begin
         *(*state).xyvals = [[*(*state).xyvals], $
                          [replicate(-1.0,2*n_elements((*(*state).fntrip)))]]
         *(*state).flags  = [*(*state).flags,'y']
         *(*state).idstr  = [*(*state).idstr,curinitials]
          (*state).nobj   = (*state).nobj + 1
      endif

      if (*state).curframe eq 1 then begin
         (*(*state).xyvals)[ 0, (*state).curobj ] = xpos
         (*(*state).xyvals)[ 1, (*state).curobj ] = ypos
      endif else begin
         (*(*state).xyvals)[ ((*state).idxfn2+1)*2,   (*state).curobj ] = xpos
         (*(*state).xyvals)[ ((*state).idxfn2+1)*2+1, (*state).curobj ] = ypos
      endelse

      ; check motion of this pair against the "chip motion", if the same put
      ;   up a warning/decision dialog box.
      dx = (*(*state).xyvals)[ ((*state).idxfn2+1)*2,   (*state).curobj ] - $
           (*(*state).xyvals)[ 0, (*state).curobj ]
      dy = (*(*state).xyvals)[ ((*state).idxfn2+1)*2+1, (*state).curobj ] - $
           (*(*state).xyvals)[ 1, (*state).curobj ]
      xoff = (*(*state).offvals)[(*state).idxfn2*2]
      yoff = (*(*state).offvals)[(*state).idxfn2*2+1]
      if abs(dx) lt 0.5 and abs(dy) lt 0.5 and $
         (abs(xoff) gt 0.5 or abs(yoff) gt 0.5) then begin
         text = [ $
            'The object you have just measured is in the same exact absolute pixel location', $
            'on both frames.  This coincidence probably means the object is a fixed image', $
            'defect that appears to move because of the slight mis-registration between', $
            'frames.  This object should probably be marked as "n", just click "Remove"', $
            'to do so.  Otherwise, if you feel this is a real object, click "Good Object"', $
            'to keep it as a valid object.' ]
         if qannounc(text,FALSE='Good Object',TITLE='Coincidence Warning', $
                     TRUE='Remove',xsize=80,ysize=6) then begin
            error=1
            (*(*state).flags)[(*state).curobj] = 'n'
            (*state).curobj=(*state).nobj
            looker_setzoom,state,/reset
         endif
      endif

      looker_computerate,state

      ; Update entry.

      if (*state).curobj lt (*state).nobj then begin
         idstr = (*(*state).idstr)[(*state).curobj]
         idstr = idstr[0]
         looker_updateidstr,idstr,curinitials
         (*(*state).idstr)[(*state).curobj] = idstr
      endif
      (*state).dirtylist = 1
      looker_display,state,/noload
      looker_objinfo,state
   endif

end

;------------------------------------------------------------------------------
pro looker_objinfo,state

   if (*state).newfile eq 0B then begin

      fn0 = (*(*state).fntrip)[0]
      words = strsplit(fn0,'.',/extract)

      if (*state).curobj ge 0 then begin

         if (*state).curobj ne (*state).nobj then begin
            if (*(*state).rate)[(*state).curobj] ne 0.0 then begin
               ratestr = string((*(*state).rate)[(*state).curobj],'"', $
                               (*(*state).dir )[(*state).curobj], $
                               format='(f10.2,a,"/hr, ",f10.1,"dg")')
               ratestr = strcompress(strtrim(ratestr,2))
            endif else begin
               ratestr = 'rate not valid'
            endelse
         endif else begin
            ratestr = 'rate not valid'
         endelse

         objstr = strmid(words[0],strlen(words[0])-2) + words[1] + $
                  (*state).exttag + strb36((*state).curobj,pad=2)
      endif else begin
         objstr = ' '
         ratestr = ' '
      endelse

      (*state).curobjname = objstr

      if (*state).curobj eq (*state).nobj then objstr = objstr+'  NEW'

      if (*state).curobj ge 0 and (*state).curobj ne (*state).nobj then begin
         if (*(*state).flags)[(*state).curobj] eq 's' then begin
            widget_control, (*state).flagid, SENSITIVE=0
         endif else begin
            widget_control, (*state).flagid, SENSITIVE=1
            if (*(*state).flags)[(*state).curobj] eq '?' then nval=0
            if (*(*state).flags)[(*state).curobj] eq 'y' then nval=1
            if (*(*state).flags)[(*state).curobj] eq 'n' then nval=2
            widget_control, (*state).flagid, SET_VALUE=nval
         endelse
      endif else begin
         widget_control, (*state).flagid, SENSITIVE=0
      endelse
   endif else begin
      objstr = 'Updating Offset'
      ratestr = ''
   endelse

   widget_control, (*state).mainbase, UPDATE=0
   widget_control, (*state).curobjid, SET_VALUE=objstr
   widget_control, (*state).rateid,   SET_VALUE=ratestr
   widget_control, (*state).mainbase, UPDATE=1

end

;------------------------------------------------------------------------------
pro looker_refreshzoom,state,xpos,ypos,RESET=reset

   ; size of extraction region
   wid = (*state).szsx/(*state).zf
   zsz = wid * (*state).zf

   widget_control, (*state).zoomwin, get_value=winnum
   WSET, winnum
   tv,rebin((*state).zim,zsz,zsz,/sample)

   if (*state).curframe ne 0 and not keyword_set(reset) then begin

      x = (xpos - (*state).zx0) * (*state).zf
      y = (ypos - (*state).zy0) * (*state).zf

      theta=findgen(361.0)/!radeg
      xcirc=(*state).radius*cos(theta)
      ycirc=(*state).radius*sin(theta)

      plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
         xr=(*state).zx0+[-0.5,wid-0.5], $
         yr=(*state).zy0+[-0.5,wid-0.5], $
         xstyle=5,ystyle=5,/noerase

      if (*state).curframe eq 1 then color='0040ff'xl $
      else color='ff8000'xl

      oplot,xcirc+xpos,ycirc+ypos,color=color

   endif

end

;------------------------------------------------------------------------------
pro looker_setalloffset,state

   ; protect against non-group files
   if (*state).numext eq 1 then return

   print,'Copying offset to all other chips in this field.'

   ; Save current object list file.
   looker_svobjlist,state

   ; Loop over all the extensions on this field.
   for iext=1,(*state).numext do begin

      fnobj = (*state).objpath+(*state).field+'x'+strb36(iext)+'.obj'

      if exists(fnobj) then begin

         ; Load the object list file.
         rdoblist,fnobj,nobj,fnlist,dt,offvals,xyvals,flags,idstr,nfiles

         ; copy the offsets from the current pair
         offvals[(*state).idxfn2*2] = (*(*state).offvals)[(*state).idxfn2*2+[0,1]]

         ; Save the object list file.
         wroblist,fnobj,nobj,fnlist,dt,offvals,xyvals,flags,idstr,nfiles

      endif
   endfor

end

;------------------------------------------------------------------------------
pro looker_setzoom,state,xpos,ypos,peaksig,sky,skysig,RESET=reset

   if (*state).curframe eq 0 or keyword_set(reset) then begin
      (*state).zim[*]=0B
      looker_refreshzoom,state,reset=reset
      return
   endif

   ; size of extraction region
   wid = (*state).szsx/(*state).zf
   zsz = wid * (*state).zf

   ; Extract image sub-section around object, watch out for image edges.
   x10 = xpos - wid/2
   x20 = x10  + wid - 1
   y10 = ypos - wid/2
   y20 = y10  + wid - 1

   x1=max([x10,0])
   x2=min([x20,(*state).nx-1])
   y1=max([y10,0])
   y2=min([y20,(*state).ny-1])

   subim=replicate(sky,wid,wid)

   if (*state).curframe eq 1 then $
      subim[x1-x10,y1-y10] = (*(*state).im1)[x1:x2,y1:y2] $
   else $
      subim[x1-x10,y1-y10] = (*(*state).im2)[x1:x2,y1:y2]

   (*state).zx0 = x10
   (*state).zy0 = y10

   subim = ( (subim-(sky-2.0*skysig) > 0) + 10 )^0.1
   maxv  = ( (peaksig > (sky+2.0*skysig))-(sky-2.0*skysig) + 10 )^0.1
   minv  = 1.0 > min(subim)

   (*state).zim=bytscl(subim,min=minv,max=maxv,top=255)

   looker_refreshzoom,state,xpos,ypos

end

;------------------------------------------------------------------------------
pro looker_svobjlist,state

   if (*state).dirtylist then begin

      print,'Updating object list file ',(*state).fnobj

      if (*state).nobj eq 0 then begin
         xyvals = 0
         flags = ''
         idstr = ''
      endif else begin
         xyvals  = (*(*state).xyvals)
         flags   = (*(*state).flags)
         idstr   = (*(*state).idstr)
      endelse

      wroblist,(*state).objpath+(*state).fnobj,(*state).nobj,(*(*state).fntrip), $
         (*(*state).dt), $
         (*(*state).offvals),xyvals,flags,idstr,n_elements((*(*state).fntrip))

      (*state).dirtylist = 0

   endif
end

;------------------------------------------------------------------------------
pro looker_updateidstr,idstr,initials

   words=strsplit(idstr,',',/extract)
   if words[0] eq '' then begin
      idstr=initials
   endif else begin
      if words[n_elements(words)-1] ne initials then $
         idstr = idstr+','+initials
   endelse

end

;------------------------------------------------------------------------------
pro looker_updatescore,state,x0,x1,y0,y1,RESET=reset,REFRESH=refresh

   ; Make a local copy to save some typing
   sf = (*state).sf

   if not keyword_set(refresh) then begin
      if keyword_set(reset) then begin

         (*state).srgb[*,*,0] = 20B
         (*state).srgb[*,*,1] = 20B
         (*state).srgb[*,*,2] = 0B

         i0 = fix(( 0-(*state).nx/2.0 ) / sf + (*state).szsx/2.0) > 0
         j0 = fix(( 0-(*state).ny/2.0 ) / sf + (*state).szsy/2.0) > 0
         i1 = fix(i0 + (*state).nx/sf) < ((*state).szsx-1)
         j1 = fix(j0 + (*state).ny/sf) < ((*state).szsy-1)

         (*state).srgb[i0:i1,j0:j1,*] = 0B

      endif else begin

         i0 = fix(( x0-(*state).nx/2.0 ) / sf + (*state).szsx/2.0) > 0
         j0 = fix(( y0-(*state).ny/2.0 ) / sf + (*state).szsy/2.0) > 0
         i1 = fix(( x1-(*state).nx/2.0 ) / sf + (*state).szsx/2.0) < ((*state).szsx-1)
         j1 = fix(( y1-(*state).ny/2.0 ) / sf + (*state).szsy/2.0) < ((*state).szsy-1)

         inc = 50B
         bsub = (*state).srgb[i0:i1,j0:j1,1]
         z=where(bsub lt 185B-inc,count)
         if count ne 0 then begin
            bsub[z] = bsub[z] + inc
            (*state).srgb[i0:i1,j0:j1,1] = bsub
         endif
      endelse
   endif

   widget_control, (*state).scorewin, get_value=winnum
   WSET, winnum

   tv,(*state).srgb,true=3

   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr = (*state).nx/2 + (*state).szsx * [-0.5,0.5] * (*state).sf, $
      yr = (*state).ny/2 + (*state).szsy * [-0.5,0.5] * (*state).sf, $
      xstyle=5,ystyle=5,/noerase

   ; outline current area display in main window
   xv = (*state).x0 + [ 0.0, 1.0, 1.0, 0.0, 0.0 ] * (*state).szx
   yv = (*state).y0 + [ 0.0, 0.0, 1.0, 1.0, 0.0 ] * (*state).szy
   oplot,xv,yv,color='800070'xl

   if (*state).nobj ne 0 then $
      z=where( (*(*state).flags) ne 'n', count) $
   else $
      count = 0

   if count ne 0 then begin
      for i=0,n_elements((*(*state).fntrip))-1 do begin

         ; extract just the objects to be plotted
         xyvals = (*(*state).xyvals)[i*2:i*2+1,z]

         ; only plot those that are valid measurements
         zz = where(xyvals[0,*] gt 0.0 and xyvals[1,*] gt 0.0, count)

         if count ne 0 then begin

            ; select color for symbol
            if i eq 0 then color='0000ff'xl $
            else if i eq (*state).idxfn2+1 then color='ffff00'xl $
            else           color='40ff30'xl

            ; compute offset for this object
            if i eq 0 then begin
               xoff = 0
               yoff = 0
            endif else begin
               xoff = (*(*state).offvals)[(i-1)*2]
               yoff = (*(*state).offvals)[(i-1)*2+1]
            endelse

            oplot,xyvals[0,zz]-xoff, $
                  xyvals[1,zz]-yoff, $
                  psym=1,color=color,symsize=0.8

            if i eq 0 then begin
               for j=0,count-1 do begin
                  if (*(*state).flags)[z[zz[j]]] eq 'y' then $
                     color='00ffff'xl $
                  else if (*(*state).flags)[z[zz[j]]] eq 's' then $
                     color='19e212'xl $
                  else $
                     color='6020ff'xl
                  tag = strb36(z[zz[j]],pad=2)
                  xyouts,xyvals[0,zz[j]],xyvals[1,zz[j]]+5.0*(*state).sf,tag, $
                     align=0.5,color=color
               endfor
            endif

         endif

      endfor
   endif

end
;------------------------------------------------------------------------------

PRO looker_eve, event

   widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   widget_control, event.top, GET_UVALUE=state

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   if event_name ne 'Initials' and not exit then begin
      looker_getinitials,state,curinitials
      if curinitials eq '' then return
   endif

   CASE event_name OF

      'THE_MENU': BEGIN
         case event.value OF

            'Add to image list': begin
               fn = dialog_pickfile( group=event.top, path=(*state).path, $
                                     TITLE='Select image', get_path=path, $
                                     FILTER='*', /must_exist)

               if fn ne '' and (*state).path eq path then begin
                  looker_svobjlist,state
                  hdr=headfits(fn)
                  parsekey,hdr,(*state).hdrlist,info
                  root = strlowcase(strcompress(info.object,/remove_all))
                  if root ne (*state).field then begin
                     text = [ $
                        'The field/object name ('+root+') from file '+fn, $
                        'does not match the name of the first frame ('+(*state).field+').', $
                        'If the object names are wrong and this is really the same field as', $
                        'the first, then click on Continue.  If you selected the wrong file by', $
                        'accident, then click on Abort.' ]
                     if qannounc(text,FALSE='Continue',TITLE='Position Warning', $
                                 TRUE='Abort',xsize=65,ysize=5) then begin
                        fn=''
                     endif
                  endif

                  if fn ne '' then begin

                     ; take off the path from the raw file name
                     fn0  = strmid(fn,strlen(path),999)

                     ; look to see if the name has a trailing '.fits', if
                     ;   so, strip it off.
                     if strmid(fn0,strlen(fn0)-5,5) eq '.fits' then $
                        fn0 = strmid(fn0,0,strlen(fn0)-5)

                     ; Make sure selected file isn't already in list.
                     z=where(fn0 eq *(*state).fntrip,count)
                     if count ne 0 then return

                     ; add to all the .obj files for image if is a multi-group
                     ;   extension file.
                     numext=sxpar(hdr,'NEXTEND') > 1
                     if numext ne (*state).numext then begin
                        text = [ $
                           'The file '+fn+' does not have the same extension'+ $
                           ' structure as the first', $
                           'frame ('+(*state).field+').  This file add reque'+ $
                           'st is begin ignored.' ]
                        dd=dialog_message(text,/error)
                     endif
                     if numext gt 1 then begin
                        looker_svobjlist,state
                        for iext=1,numext do begin
                           fnobj = (*state).objpath+(*state).field+'x'+strb36(iext)+'.obj'
                           if exists(fnobj) then begin
                              rdoblist,fnobj,nobj,fnlist,dt,offvals,xyvals,flags,idstr,nfiles
                              z=where(fn0 eq fnlist,count)
                              if count eq 0 then begin
                                 fnlist = [fnlist,fn0]
                                 nfiles = nfiles + 1
                                 if nfiles eq 2 then begin
                                    offvals = [0.,0.]
                                    dt  = [(info.jd-(*(*state).info1).jd)*24.0]
                                 endif else begin
                                    offvals = [ offvals, [0.,0.] ]
                                    dt  = [dt,(info.jd-(*(*state).info1).jd)*24.0]
                                 endelse
                                 if nobj ne 0 then $
                                    xyvals  = [ [xyvals], replicate(-1.0,2,nobj) ]
                                 wroblist,fnobj,nobj,fnlist,dt,offvals,xyvals,flags,idstr,nfiles
                              endif
                           endif
                        endfor
                     endif else begin
                        if (*state).nfiles eq 1 then begin
                           dt  = [(info.jd-(*(*state).info1).jd)*24.0]
                        endif else begin
                           dt  = [*(*state).dt,(info.jd-(*(*state).info1).jd)*24.0]
                        endelse
                     endelse

                     *(*state).fntrip  = [ [*(*state).fntrip],fn0 ]
                     (*state).idxfn2 = (*state).nfiles-1
                     if (*state).nfiles eq 1 then begin
                        (*state).offvals = ptr_new([0.,0.])
                        (*state).dt      = ptr_new(dt)
                     endif else begin
                        *(*state).offvals = [ *(*state).offvals, [0.,0.] ]
                        *(*state).dt      = dt
                     endelse
                     (*state).nfiles   = (*state).nfiles+1
                     if (*state).nobj ne 0 then $
                        *(*state).xyvals = [ [*(*state).xyvals], $
                                             replicate(-1.0,2,(*state).nobj) ]
                     (*state).newfile  = 9B
                     (*state).dirtylist = 1

                     widget_control, (*state).mainbase, UPDATE=0
                     widget_control, (*state).fn2id,    SET_VALUE=fn0
                     widget_control, (*state).mainbase, UPDATE=1
                     looker_display,state
                  endif

               endif

               if fn ne '' and (*state).path ne path then begin
                  print,'Error!  path to image must be ',(*state).path
                  print,'   you have selected a file from ',path
               endif

            end

            'Adjust frame registration': begin
               if (*state).idxfn2 lt 0 then return
               (*state).newfile = ((*state).newfile and 248B) or 1B
               (*(*state).offvals)[(*state).idxfn2*2] = [0.0,0.0]
               widget_control, (*state).previd, SENSITIVE=0
               widget_control, (*state).nextid, SENSITIVE=0
               widget_control, (*state).leftid, SENSITIVE=0
               widget_control, (*state).rightid, SENSITIVE=0
               widget_control, (*state).upid, SENSITIVE=0
               widget_control, (*state).downid, SENSITIVE=0
; desensitize a bunch of buttons to prevent trouble
; previous
               looker_objinfo,state
               looker_display,state
            end

            'Set all ? to N': begin
               z = where( (*(*state).flags) eq '?', count )
               if count ne 0 then begin
                  (*state).dirtylist = 1
                  (*(*state).flags)[z] = 'n'
                  for i=0,count-1 do begin
                     idstr = (*(*state).idstr)[z[i]]
                     looker_updateidstr,idstr,curinitials
                     (*(*state).idstr)[z[i]] = idstr
                  endfor
                  if (*state).curobj lt (*state).nobj then begin
                     if (*(*state).flags)[(*state).curobj] eq 'n' then begin
                        (*state).curobj=(*state).nobj
                        (*state).curframe = 0
                        looker_setzoom,state,/reset
                     endif
                  endif
                  looker_display,state,/noload
                  looker_updatescore,state,/refresh
                  looker_objinfo,state
               endif
            end

            'Autoload Toggle': begin
               (*state).autoload = not (*state).autoload
               if (*state).fnobj eq '' then return
            end

            'Change secondary field': begin

               if (*state).newfile gt 0B then (*state).newfile=0B

               widget_control, (*state).fn1id, get_value=fn1

               if fn1 eq '' then return

               if (*state).nfiles eq 1 then return

               z=where(*(*state).fntrip ne fn1,count)
               if count eq 0 then return

               newstr=picker((*(*state).fntrip)[z],group=event.top, $
                             title='Pick Second Frame')
               if newstr ne '[[[CANCEL]]]' then begin
                  widget_control, (*state).mainbase, UPDATE=0
                  widget_control, (*state).fn2id,    SET_VALUE=newstr
                  widget_control, (*state).mainbase, UPDATE=1
                  (*state).idxfn2 = -1
               endif
               looker_display,state

            end

            'Copy registration': begin
               if (*state).fnobj eq '' then return
               looker_setalloffset,state
            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               RETURN
            end

            'Load field by object id' : begin
               looker_svobjlist,state
               objid = qinput(PROMPT='Object id to view? ')
               objid = strtrim(strcompress(objid[0]),2)
               if strlen(objid) eq 8 then begin
                  daycode=strmid(objid,0,2)
                  f1tag  =strmid(objid,2,3)
                  xtag   =strmid(objid,5,1)
                  objcode=strmid(objid,6,2)
                  goodid=1
               endif else if strlen(objid) eq 7 then begin
                  daycode=strmid(objid,0,2)
                  f1tag  =strmid(objid,2,3)
                  xtag   =''
                  objcode=strmid(objid,5,2)
                  goodid=1
               endif else begin
                  goodid=0
               endelse

               if goodid then begin
                  list=file_search((*state).path+'*'+daycode+'.'+f1tag+'*')
                  fn=list[0]
                  path=(*state).path
                  hdr=headfits(fn)
                  numext=sxpar(hdr,'NEXTEND') > 1
                  parsekey,hdr,(*state).hdrlist,info
                  root = strlowcase(strcompress(info.object,/remove_all))
                  fn0  = strmid(fn,strlen(path),999)
                  if strmid(fn0,strlen(fn0)-5,5) eq '.fits' then $
                     fn0 = strmid(fn0,0,strlen(fn0)-5)
                  if numext eq 1 then begin
                     fnobj = root+'.obj'
                  endif else begin
                     fnobj = root+'x'+xtag+'.obj'
                  endelse
                  if exists((*state).objpath+fnobj) then begin
                     (*state).fnobj = fnobj
                     looker_display,state
                     objnum = strb36(objcode)
                     if objnum ge 0 and objnum lt (*state).nobj then begin
                        if (*(*state).flags)[objnum] ne 'n' then begin
                           (*state).curobj = objnum
                           (*state).curframe = 1
                           looker_objinfo,state
                           x = (*(*state).xyvals)[0,(*state).curobj]
                           y = (*(*state).xyvals)[1,(*state).curobj]
                           looker_measure,state,x,y,0,error,xpos,ypos,maxsig,sky,skysig,/noupdate
                           xm = (fix( x-(*state).szx/2.0 ) < ((*state).nx-(*state).szx) ) > 0
                           ym = (fix( y-(*state).szy/2.0 ) < ((*state).ny-(*state).szy) ) > 0
                           (*state).x0 = xm
                           (*state).y0 = ym
                           looker_display,state
                           looker_setzoom,state,x,y,maxsig,sky, $
                              skysig*sqrt( ((*state).radius+30.0)^2 - $
                                           ((*state).radius+10.0)^2 *!pi )
                        endif
                     endif
                  endif else begin
                     print,'Object file ',fnobj,' not found.'
                  endelse
               endif else begin
                  print,'Illegal object id [',objid,'], ignoring.'
               endelse
            end

            'Postscript Hardcopy': begin
               if (*state).lastfn2 eq '' then return
               hardim,[[[*(*state).r]],[[*(*state).g]],[[*(*state).g]]], $
                  true=3,queue='chani',file='looker.ps',/noprint,autosize=2, $
                  /landscape ;,title='KPNO/MOSAIC  1998 Nov 18'
               hardim,*(*state).r, $
                  queue='chani',file='looker1.ps',/noprint,autosize=2, $
                  /landscape ;,title='KPNO/MOSAIC  1998 Nov 18'
               hardim,*(*state).g, $
                  queue='chani',file='looker2.ps',/noprint,autosize=2, $
                  /landscape ;,title='KPNO/MOSAIC  1998 Nov 18'
            end

            'Refresh display': begin
               if (*state).fnobj eq '' then return
               looker_display,state,/noload,/noscore
               looker_updatescore,state,/refresh
            end

            'Select existing field': begin
               fn = dialog_pickfile( group=event.top, path=(*state).objpath, $
                                     get_path=path, TITLE='Select field', $
                                     FILTER='*.obj', /must_exist)
               if fn ne '' then begin
                  looker_svobjlist,state
                  (*state).fnobj   = strmid(fn,strlen(path),999)
                  (*state).objpath = path
                  looker_display,state
               endif
            end

            'Snap object': begin

               if (*state).idxfn2 lt 0 then return
               if (*state).curobj eq (*state).nobj or (*state).curobj lt 0 then return

               ; size of extraction region
               wid = (*state).szsx/(*state).zf
               zsz = wid * (*state).zf

               ; offset between frame 2 and reference frame
               xoff = fix( (*(*state).offvals)[(*state).idxfn2*2]   + 0.5 )
               yoff = fix( (*(*state).offvals)[(*state).idxfn2*2+1] + 0.5 )

               ; Positions of the objects in their own frames
               xa = (*(*state).xyvals)[0,(*state).curobj]
               ya = (*(*state).xyvals)[1,(*state).curobj]
               xb = (*(*state).xyvals)[ ((*state).idxfn2+1)*2,   (*state).curobj ]
               yb = (*(*state).xyvals)[ ((*state).idxfn2+1)*2+1, (*state).curobj ]

               ; position of second image in frame 1
               xb1 = xb - xoff
               yb1 = yb - yoff

               ; distance moved between images
               dist = sqrt((xa-xb1)^2 + (ya-yb1)^2)

               ; mid-point between two object positions in frame 1
               xmid = (xa+xb1)/2.0
               ymid = (ya+yb1)/2.0

               fmt='(a,2(1x,f6.1),1x,f7.3,"+/-",f6.3,1x,g10.3,"+/-",g7.3)'

               print,'extract around object',(*state).curobj

               basphote,(*state).gain,*(*state).im1,(*(*state).info1).exptime, $
                  xa,ya,(*state).radius,(*state).radius+10.0,(*state).radius+30.0, $
                  boxmrad = (*state).radius > 5,fwhm=fwhm,max=maxsig1,/exact, $
                  mag=mag,err=err,/nolog,pscale=(*state).pscale, $
                  fname=(*(*state).fntrip)[0],name=(*state).curobjname, $
                  xcen=xpos,ycen=ypos,skymean=sky1,skyerr=skysig1,/silent
               print,'frame 1 x,y ',xa,ya,mag,err,sky1,skysig1,format=fmt

               basphote,(*state).gain,*(*state).im2,(*(*state).info1).exptime, $
                  xb,yb,(*state).radius,(*state).radius+10.0,(*state).radius+30.0, $
                  boxmrad = (*state).radius > 5,fwhm=fwhm,max=maxsig2,/exact, $
                  mag=mag,err=err,/nolog,pscale=(*state).pscale, $
                  fname=(*(*state).fntrip)[(*state).idxfn2+1],name=(*state).curobjname, $
                  xcen=xpos,ycen=ypos,skymean=sky2,skyerr=skysig2,/silent
               print,'frame 2 x,y ',xb,yb,mag,err,sky2,skysig2,format=fmt

               ; Get the sky scaling levels for the subimages.
               widget_control, (*state).minsigid, GET_VALUE=dm
               widget_control, (*state).maxsigid, GET_VALUE=dp
               dm = float(dm[0])
               dp = float(dp[0])

               ; Probably would use LZW but it seems to need licensing.
               compression=0

               widget_control, (*state).curobjid, GET_VALUE=objname

               ; FITS versions, add dw extra pixels to extraction
               dw=300
               looker_getsubimage,state,xa,ya,1,wid+dw,sky1,subim1
               hdr1 = (*(*state).hdr1)
               sxaddpar,hdr1,'NAXIS1',wid+dw
               sxaddpar,hdr1,'NAXIS2',wid+dw
               sxaddpar,hdr1,'XLLHC',fix(xa)-fix(wid+dw)/2
               sxaddpar,hdr1,'YLLHC',fix(ya)-fix(wid+dw)/2
               writefits,objname+'_1.fits',subim1,hdr1

               looker_getsubimage,state,xb,yb,2,wid+dw,sky2,subim2
               hdr2 = (*(*state).hdr2)
               sxaddpar,hdr2,'NAXIS1',wid+dw
               sxaddpar,hdr2,'NAXIS2',wid+dw
               sxaddpar,hdr2,'XLLHC',fix(xb)-fix(wid+dw)/2
               sxaddpar,hdr2,'YLLHC',fix(yb)-fix(wid+dw)/2
               writefits,objname+'_2.fits',subim2,hdr2

               ; Linear stretch versions
               ; image 1 B/W, centered on object
               looker_getsubimage,state,xa,ya,1,wid,sky1,subim1
               subim1 = reverse(float(subim1)-sky1,2)
               bsubim1 = bytscl(subim1,min=(*state).lowval1-sky1, $
                                       max=(*state).highval1-sky1,top=255)
               write_tiff,objname+'_1.tiff',bsubim1,compression=compression

               ; image 2 B/W, centered on object
               looker_getsubimage,state,xb,yb,2,wid,sky2,subim2
               subim2 = reverse(float(subim2)-sky2,2)
               bsubim2 = bytscl(subim2,min=(*state).lowval2-sky2, $
                                       max=(*state).highval2-sky2,top=255)
               write_tiff,objname+'_2.tiff',bsubim2,compression=compression

               ; Power-law stretch versions
               lsubim1 = ( (subim1+2.0*skysig1 > 0) + 10 )^0.1
               maxv1   = ( ((maxsig1-sky1) > (2.0*skysig1))+2.0*skysig1 + 10 )^0.1
               minv1   = 1.0 > min(lsubim1)
               lsubim1 = bytscl(lsubim1,min=minv1,max=maxv1,top=255)
               write_tiff,objname+'_1l.tiff',lsubim1,compression=compression

               lsubim2 = ( (subim2+2.0*skysig2 > 0) + 10 )^0.1
               maxv2   = ( ((maxsig2-sky2) > (2.0*skysig2))+2.0*skysig2 + 10 )^0.1
               minv2   = 1.0 > min(lsubim2)
               lsubim2 = bytscl(lsubim2,min=minv2,max=maxv2,top=255)
               write_tiff,objname+'_2l.tiff',lsubim2,compression=compression

               ; image 1 B/W, centered on object mid-point
               looker_getsubimage,state,xmid,ymid,1,wid+dist,sky1,subim1
               subim1 = reverse(float(subim1)-sky1,2)
               bsubim1 = bytscl(subim1,min=(*state).lowval1-sky1, $
                                       max=(*state).highval1-sky1,top=255)
               write_tiff,objname+'_1c.tiff',bsubim1,compression=compression

               ; image 2 B/W, centered on object mid-point
               looker_getsubimage,state,xmid+xoff,ymid+yoff,2,wid+dist,sky2,subim2
               subim2 = reverse(float(subim2)-sky2,2)
               bsubim2 = bytscl(subim2,min=(*state).lowval2-sky2, $
                                       max=(*state).highval2-sky2,top=255)
               write_tiff,objname+'_2c.tiff',bsubim2,compression=compression

               ; color composite of image 1 and 2
               write_tiff,objname+'_c.tiff',[[[bsubim1]],[[bsubim2]],[[bsubim2]]], $
                  planarconfig=2

               ; Power-law stretch versions
               lsubim1 = ( (subim1+2.0*skysig1 > 0) + 10 )^0.1
               maxv1   = ( ((maxsig1-sky1) > (2.0*skysig1))+2.0*skysig1 + 10 )^0.1
               minv1   = 1.0 > min(lsubim1)
               lsubim1 = bytscl(lsubim1,min=minv1,max=maxv1,top=255)
               write_tiff,objname+'_1lc.tiff',lsubim1,compression=compression

               lsubim2 = ( (subim2+2.0*skysig2 > 0) + 10 )^0.1
               maxv2   = ( ((maxsig2-sky2) > (2.0*skysig2))+2.0*skysig2 + 10 )^0.1
               minv2   = 1.0 > min(lsubim2)
               lsubim2 = bytscl(lsubim2,min=minv2,max=maxv2,top=255)
               write_tiff,objname+'_2lc.tiff',lsubim2,compression=compression

               write_tiff,objname+'_l.tiff',[[[lsubim1]],[[lsubim2]],[[lsubim2]]], $
                  planarconfig=2

            end

            'Start new field': begin
               path=''
               fn = dialog_pickfile( group=event.top, path=(*state).path, $
                                     TITLE='Select image', get_path=path, $
                                     FILTER='*', /must_exist)

               if fn ne '' and (*state).path eq path then begin
                  looker_svobjlist,state
                  hdr=headfits(fn)
                  numext=sxpar(hdr,'NEXTEND') > 1
                  parsekey,hdr,(*state).hdrlist,info
                  root = strlowcase(strcompress(info.object,/remove_all))
                  fn0  = strmid(fn,strlen(path),999)
                  if strmid(fn0,strlen(fn0)-5,5) eq '.fits' then $
                     fn0 = strmid(fn0,0,strlen(fn0)-5)
                  if numext eq 1 then begin
                     fnobj = root+'.obj'
                     if not exists((*state).objpath+fnobj) then begin
                        openw,lun,(*state).objpath+fnobj,/get_lun
                        printf,lun,fn0
                        free_lun,lun
                     endif
                  endif else begin
                     fnlist=strarr(numext)
                     for i=0,numext-1 do begin
                        exttag = 'x'+strb36(i+1)
                        fnobj = root+exttag+'.obj'
                        if not exists((*state).objpath+fnobj) then begin
                           openw,lun,(*state).objpath+fnobj,/get_lun
                           printf,lun,fn0
                           free_lun,lun
                        endif
                        fnlist[i]=fnobj
                     endfor
                     fnobj=picker(fnlist,group=event.top, $
                                   title='Pick Extension')
                     if fnobj eq '[[[CANCEL]]]' then fnobj=''
                  endelse

                  if fnobj ne '' then begin
                     (*state).fnobj = fnobj
                     looker_display,state
                  endif
               endif

               if fn ne '' and (*state).path ne path then begin
                  print,'Error!  path to image must be ',(*state).path
                  print,'   you have selected a file from ',path
               endif

            end

            'Suppress overlay': begin
               (*state).nomarks = not (*state).nomarks
               if (*state).fnobj eq '' then return
               looker_display,state,/noload,/noscore
               looker_updatescore,state,/refresh
            end

            'Toggle color mode': begin
               (*state).colormode = not (*state).colormode
               if (*state).fnobj eq '' then return
               looker_display,state,/noscore
               looker_updatescore,state,/refresh
            end

            'Toggle lines': begin
               (*state).lines = not (*state).lines
               if (*state).fnobj eq '' then return
               looker_display,state,/noload,/noscore
               looker_updatescore,state,/refresh
            end

            else: begin
               MESSAGE, 'Unknown menu event:', /INFO
               HELP, event, /STRUCTURE
            end

         endcase

      END

      'blink': begin
         if (*state).idxfn2 lt 0 then return
         if (*state).blink eq 0 then begin
            (*state).blink = 1024
            widget_control,(*state).blinkid,timer=float((*state).blink)/1000.0
         endif else if (*state).blink eq -1 then begin
            (*state).blink = 0
         endif else if (*state).blink eq -2 then begin
            ; single step to next frame
            (*state).blinkframe = not (*state).blinkframe
         endif else if (*state).blink gt 0 then begin
            widget_control,(*state).blinkid,timer=float((*state).blink)/1000.0
            (*state).blinkframe = not (*state).blinkframe
         endif else begin
            print,'LOOKER: Illegal blink value! ',(*state).blink
            (*state).blink = 0
         endelse
         if (*state).blink eq 0 then begin
            ; restore normal display
            looker_display,state,/noload
         endif else begin
            ; show blinkframe
            widget_control, (*state).drawwin, get_value=winnum
            WSET, winnum
            if (*state).blinkframe then $
               tv,*(*state).g $
            else $
               tv,*(*state).r
         endelse
      end

      'blink slower': begin
         if (*state).idxfn2 lt 0 or (*state).blink eq 0 then begin
            return
         endif else if (*state).blink gt 0 then begin
            (*state).blink = (*state).blink*2
            if (*state).blink gt 8192 then (*state).blink=8192
         endif
      end

      'blink faster': begin
         if (*state).idxfn2 lt 0 or (*state).blink eq 0 then begin
            return
         endif else if (*state).blink gt 0 then begin
            (*state).blink = (*state).blink/2
            if (*state).blink lt 512 then (*state).blink=512
         endif else if (*state).blink eq -2 then begin
            (*state).blink = 1024
            widget_control,(*state).blinkid,timer=float((*state).blink)/1000.0
         endif
         ; do nothing if blink -1 or < -2
      end

      'blink pause': begin
         if (*state).idxfn2 lt 0 or (*state).blink eq 0 then $
            return $
         else $
            (*state).blink = -2
      end

      'blink stop': begin
         if (*state).idxfn2 lt 0 or (*state).blink eq 0 then return
         (*state).blink = -1
      end

      'Down1': begin
         if (*state).idxfn2 lt 0 then return
         idx = (*state).idxfn2*2
         (*(*state).offvals)[idx+1] = (*(*state).offvals)[idx+1]+1
         (*state).dirtylist = 1
         idx = (*state).idxfn2*2
         looker_display,state,/noscore
         looker_objinfo,state
      end

      'Initials': begin
         looker_getinitials,state,curinitials
         widget_control, event.id, SET_VALUE=curinitials
      end

      'Left1': begin
         if (*state).idxfn2 lt 0 then return
         idx = (*state).idxfn2*2
         (*(*state).offvals)[idx] = (*(*state).offvals)[idx]+1
         (*state).dirtylist = 1
         idx = (*state).idxfn2*2
         looker_display,state,/noscore
         looker_objinfo,state
      end

      'Next Field': begin

         if (*state).fnobj eq '' then return

         fnlist = file_search((*state).objpath+'*.obj',count=count)
         if count eq 0 then begin
            print,'No files matching pattern: ',(*state).objpath+'*.obj'
            return
         endif

         oldfn = (*state).objpath+(*state).fnobj
         z=where(oldfn eq fnlist,count)
         if count eq 0 then begin
            print,'Error!  cannot find current file, this should not happen.'
            return
         endif

         if z[0] ne n_elements(fnlist)-1 then begin
            looker_svobjlist,state
            (*state).fnobj   = strmid(fnlist[z[0]+1],strlen((*state).objpath),999)
            looker_display,state
         endif else begin
            print,'Cannot go forward, already at last file.'
         endelse

      end

      'New Object': begin
         if (*state).newfile gt 0B then return
         if (*state).curobj eq (*state).nobj or (*state).curobj lt 0 then return
         (*state).curobj = (*state).nobj
         looker_objinfo,state
         looker_setzoom,state,/reset
      end

      'Previous Field': begin

         if (*state).fnobj eq '' then return

         fnlist = file_search((*state).objpath+'*.obj',count=count)
         if count eq 0 then begin
            print,'No files matching pattern: ',(*state).objpath+'*.obj'
            return
         endif

         oldfn = (*state).objpath+(*state).fnobj
         z=where(oldfn eq fnlist,count)
         if count eq 0 then begin
            print,'Error!  cannot find current file, this should not happen.'
            return
         endif

         if z[0] ne 0 then begin
            looker_svobjlist,state
            (*state).fnobj   = strmid(fnlist[z[0]-1],strlen((*state).objpath),999)
            looker_display,state
         endif else begin
            print,'Cannot go back, already at first file.'
         endelse

      end

      'Right1': begin
         if (*state).idxfn2 lt 0 then return
         idx = (*state).idxfn2*2
         (*(*state).offvals)[idx] = (*(*state).offvals)[idx]-1
         (*state).dirtylist = 1
         idx = (*state).idxfn2*2
         looker_display,state,/noscore
         looker_objinfo,state
      end

      'Score': BEGIN
         if (*state).lastfn2 eq '' then return
         case event.press OF
            0: begin
            end

            ; Move to cursor location
            1: begin
               xm = fix(( event.x-(*state).szsx/2.0 ) * (*state).sf + (*state).nx/2.0)
               ym = fix(( event.y-(*state).szsy/2.0 ) * (*state).sf + (*state).ny/2.0)

               (*state).x0 = xm - (*state).szx/2
               (*state).y0 = ym - (*state).szy/2
               looker_display,state

            end

            ; Move half field either up/down or right/left depending on largest
            ;   distance between current point and cursor point.
            2: begin
               xm = fix(( event.x-(*state).szsx/2.0 ) * (*state).sf + (*state).nx/2.0)
               ym = fix(( event.y-(*state).szsy/2.0 ) * (*state).sf + (*state).ny/2.0)

               newx0 = xm - (*state).szx/2
               newy0 = ym - (*state).szy/2
               xdiff = (*state).x0 - newx0
               ydiff = (*state).y0 - newy0
               if abs(xdiff) gt abs(ydiff) then begin
                  newx0 = (*state).x0 + (xdiff gt 0.0 ? -1.0 : 1.0) * (*state).szx/2
                  newy0 = (*state).y0
               endif else begin
                  newx0 = (*state).x0
                  newy0 = (*state).y0 + (ydiff gt 0.0 ? -1.0 : 1.0) * (*state).szy/2
               endelse
               (*state).x0 = newx0
               (*state).y0 = newy0
               looker_display,state
            end

            ; Move full field either up/down or right/left depending on largest
            ;   distance between current point and cursor point.
            4: begin
               xm = fix(( event.x-(*state).szsx/2.0 ) * (*state).sf + (*state).nx/2.0)
               ym = fix(( event.y-(*state).szsy/2.0 ) * (*state).sf + (*state).ny/2.0)

               xm = xm / (*state).szx * (*state).szx
               ym = ym / (*state).szy * (*state).szy

               (*state).x0 = xm
               (*state).y0 = ym
               looker_display,state
            end
            else: begin
               MESSAGE, 'Unknown window event:', /INFO
               HELP, event, /STRUCTURE
            end

         endcase

         ; if current object is no longer on frame, then set current
         ;    object to NEW
         if (*state).curobj ne (*state).nobj and (*state).nfiles le 2 then begin
            objx = (*(*state).xyvals)[0,(*state).curobj]
            objy = (*(*state).xyvals)[1,(*state).curobj]
            if objx lt (*state).x0 or objy lt (*state).y0 or $
               objx gt (*state).x0+(*state).szx-1 or $
               objy gt (*state).y0+(*state).szy-1 then begin

               (*state).curobj = (*state).nobj
               looker_setzoom,state,/reset
               looker_objinfo,state
            endif
         endif

      END

      'Set Flag': begin

         if (*state).newfile gt 0B then return
         if (*state).curobj eq (*state).nobj or (*state).curobj lt 0 then return

         (*(*state).flags)[(*state).curobj] = event.value
         idstr = (*(*state).idstr)[(*state).curobj]
         looker_updateidstr,idstr,curinitials
         (*(*state).idstr)[(*state).curobj] = idstr

         (*state).dirtylist = 1
         if event.value ne 'y' then begin
            (*state).curobj=(*state).nobj
            looker_setzoom,state,/reset
         endif

         looker_display,state,/noload,/noscore
         looker_updatescore,state,/refresh
         looker_objinfo,state
      end

      'Set Max Sig': begin
         widget_control, event.id, GET_VALUE=hivalue
         hivalue = float(fix(hivalue[0]*10.0))/10.0
         widget_control, (*state).minsigid, GET_VALUE=lovalue
         lovalue=float(lovalue[0])
         if lovalue eq hivalue then hivalue=hivalue+0.1
         hivalue = strcompress(string(hivalue,format='(f10.1)'))
         widget_control, (*state).maxsigid, SET_VALUE=hivalue
         looker_display, state, /noscore
      end

      'Set Min Sig': BEGIN
         widget_control, event.id, GET_VALUE=lovalue
         lovalue = float(fix(lovalue[0]*10.0))/10.0
         widget_control, (*state).maxsigid, GET_VALUE=hivalue
         hivalue=float(hivalue[0])
         if lovalue eq hivalue then lovalue=lovalue-0.1
         lovalue = strcompress(string(lovalue,format='(f10.1)'))
         widget_control, (*state).minsigid, SET_VALUE=lovalue
         looker_display, state, /noscore
      END

      'Up1': begin
         if (*state).idxfn2 lt 0 then return
         idx = (*state).idxfn2*2
         (*(*state).offvals)[idx+1] = (*(*state).offvals)[idx+1]-1
         (*state).dirtylist = 1
         idx = (*state).idxfn2*2
         looker_display,state,/noscore
         looker_objinfo,state
      end

      'Window': BEGIN
         if (*state).lastfn2 eq '' then return
         case event.press OF
            0: begin
            end
            1: begin

               ; set the current frame being measured
               (*state).curframe = 1

               noupdate = (*state).newfile and 1B

               looker_measure,state,event.x+(*state).x0, $
                  event.y+(*state).y0,0,error,xpos,ypos,maxsig,sky,skysig, $
                  NOUPDATE=noupdate

               if error then return

               if not noupdate then begin
                  looker_setzoom,state,xpos,ypos,maxsig,sky, $
                     skysig*sqrt( ((*state).radius+30.0)^2 - $
                                  ((*state).radius+10.0)^2 *!pi )
               endif else begin
                  if ((*state).newfile and 7B) eq 7B then begin
                     (*state).newfile = (*state).newfile and 249B
                     (*(*state).offvals)[(*state).idxfn2*2] = [0.0,0.0]
                     looker_display, state, /noscore
                     (*state).ref2=[0.0,0.0]
                  endif
                  (*state).ref1 = [xpos,ypos]
                  (*state).newfile = (*state).newfile or 2B
                  if ((*state).newfile and 7B) eq 7B then begin
                     (*(*state).offvals)[(*state).idxfn2*2] = $
                        (*state).ref2-(*state).ref1
                     looker_display, state, /noscore
                  endif
                  looker_objinfo,state
               endelse

            end
            2: begin

               ; set the current frame being measured
               (*state).curframe = 2

               noupdate = (*state).newfile and 1B

               ; offset between frame 2 and reference frame
               xoff = fix( (*(*state).offvals)[(*state).idxfn2*2]   + 0.5 )
               yoff = fix( (*(*state).offvals)[(*state).idxfn2*2+1] + 0.5 )

               ; position for measurement
               xc = event.x+(*state).x0 + xoff
               yc = event.y+(*state).y0 + yoff

               looker_measure,state,xc,yc,0,error,xpos,ypos,maxsig,sky,skysig, $
                  NOUPDATE=noupdate

               if error then return

               if not noupdate then begin
                  looker_setzoom,state,xpos,ypos,maxsig,sky, $
                     skysig*sqrt( ((*state).radius+30.0)^2 - $
                                  ((*state).radius+10.0)^2 *!pi )
               endif else begin
                  if ((*state).newfile and 7B) eq 7B then begin
                     (*state).newfile = (*state).newfile and 249B
                     (*(*state).offvals)[(*state).idxfn2*2] = [0.0,0.0]
                     (*state).ref1=[0.0,0.0]
                     looker_display, state, /noscore
                  endif
                  (*state).ref2 = [xpos,ypos]
                  (*state).newfile = (*state).newfile or 4B
                  if ((*state).newfile and 7B) eq 7B then begin
                     (*(*state).offvals)[(*state).idxfn2*2] = $
                        (*state).ref2-(*state).ref1
                     looker_display, state, /noscore
                  endif
                  looker_objinfo,state
               endelse

            end
            4: begin

               if (*state).newfile and 1B then begin
                  if ((*state).newfile and 7B) eq 7B then begin
                     (*(*state).offvals)[(*state).idxfn2*2] = (*state).ref2-(*state).ref1
                     if ((*state).newfile and 8B) eq 8B then looker_setalloffset,state
                     (*state).dirtylist=1
                     widget_control, (*state).previd, SENSITIVE=1
                     widget_control, (*state).nextid, SENSITIVE=1
                     widget_control, (*state).leftid, SENSITIVE=1
                     widget_control, (*state).rightid, SENSITIVE=1
                     widget_control, (*state).upid, SENSITIVE=1
                     widget_control, (*state).downid, SENSITIVE=1
                     looker_updatescore,state,/reset
                     looker_display, state
                     (*state).newfile = 0B
                     looker_objinfo,state
                  endif else begin
                     print,'Warning, registration positions not valid, no change made.'
                  endelse
               endif else begin
                  if (*state).nobj eq 0 then return

                  z=where( (*(*state).flags) ne 'n', count)
                  if count eq 0 then return

                  x = event.x+(*state).x0
                  y = event.y+(*state).y0

                  dist = sqrt((x-(*(*state).xyvals)[0,z])^2 + $
                              (y-(*(*state).xyvals)[1,z])^2)

                  if min(dist) gt 50 then begin
                     print,'Error!  No object within 50 pixels, selection not changed.'
                     return
                  endif

                  zz=where(dist eq min(dist))
                  (*state).curobj = z[zz[0]]

                  (*state).curframe = 1

                  looker_objinfo,state

                  looker_measure,state,x,y,0,error,xpos,ypos,maxsig,sky,skysig,/noupdate

                  x = (*(*state).xyvals)[0,(*state).curobj]
                  y = (*(*state).xyvals)[1,(*state).curobj]

                  looker_setzoom,state,x,y,maxsig,sky, $
                     skysig*sqrt( ((*state).radius+30.0)^2 - $
                                  ((*state).radius+10.0)^2 *!pi )
               endelse

            end
            else: begin
               MESSAGE, 'Unknown window event:', /INFO
               HELP, event, /STRUCTURE
            end
         endcase
      END

      'Zoom': BEGIN
         if (*state).newfile gt 0B then return
         if (*state).lastfn2 eq '' or (*state).curframe eq 0 then return
         case event.press OF
            0: begin
            end
            1: begin
               ; position for measurement
               xc = (float(event.x)+0.5)/(*state).zf+(*state).zx0-0.5
               yc = (float(event.y)+0.5)/(*state).zf+(*state).zy0-0.5

               looker_measure,state,xc,yc,1,error
               if error then return

               looker_refreshzoom,state,xc,yc

            end
            2: begin
               ; position for measurement
               xc = (float(event.x)+0.5)/(*state).zf+(*state).zx0-0.5
               yc = (float(event.y)+0.5)/(*state).zf+(*state).zy0-0.5

               looker_measure,state,xc,yc,1,error,xpos,ypos,maxsig,sky,skysig
               if error then return

               looker_setzoom,state,xpos,ypos,maxsig,sky, $
                  skysig*sqrt( ((*state).radius+30.0)^2 - $
                               ((*state).radius+10.0)^2 *!pi )
            end
            4: begin
               print,'right click in zoom window'
               help,event,/st
            end
            else: begin
               MESSAGE, 'Unknown window event:', /INFO
               HELP, event, /STRUCTURE
            end
         endcase
      END

      ELSE : BEGIN
         print,'EVENT NAME: ',event_name
         MESSAGE, 'Unknown event:', /INFO
         HELP, event, /STRUCTURE
      END

   ENDCASE

END ; end of event handler



;------------------------------------------------------------------------------

PRO looker, KEYLIST=in_keylist, PATH=path, PSCALE=pscale, OBJRAD=objrad, $
   XSIZE=szx, YSIZE=szy, ZXSIZE=szsx, ZYSIZE=szsy, GAIN=gain

   if xregistered('looker') then return

   IF (!d.flags and 256) eq 0 THEN BEGIN
      print, 'Error. No windowing device. LOOKER cannot be started.'
      return
   ENDIF

   IF !d.n_colors ne 16777216 THEN BEGIN
      print,'Error.  24-bit display device is required.'
      return
   ENDIF

   IF badpar(path,[0,7],0, $
         CALLER='LOOKER: (PATH) ',DEFAULT='') THEN RETURN
   if path ne '' then path=addslash(path)

   IF badpar(objrad,[0,2,3,4,5],0, $
         CALLER='LOOKER: (OBJRAD) ',DEFAULT=6.0) THEN RETURN
   IF badpar(pscale,[0,4,5],0, $
         CALLER='LOOKER: (PATH) ',DEFAULT=0.26) THEN RETURN
   IF badpar(szx,[0,2,3],0, $
         caller='LOOKER: (XSIZE) ',default=1070) THEN RETURN
   IF badpar(szy,[0,2,3],0, $
         caller='LOOKER: (YSIZE) ',default=820) THEN RETURN
   IF badpar(szsx,[0,2,3],0, $
         caller='LOOKER: (ZXSIZE) ',default=148) THEN RETURN
   IF badpar(szsy,[0,2,3],0, $
         caller='LOOKER: (ZYSIZE) ',default=296) THEN RETURN
   IF badpar(gain,[0,2,3,4,5],0, $
         caller='LOOKER: (GAIN) ',default=3.0) THEN RETURN

   IF KEYWORD_SET( in_keylist ) THEN $
      loadkeys,in_keylist,hdrlist $
   ELSE $
      loadkeys,'[[DEFAULT]]',hdrlist

   if exists('looker.ids') then begin
      rdmatch,'looker.ids',initials,fullname
      n_initials = n_elements(initials)
   endif else begin
      n_initials = 0L
      initials = strarr(1)
      fullname = strarr(1)
   endelse

   setusym,-1

   ;Define the main base.
   mainbase = widget_base( TITLE='LOOKER: Visual Inspection and Measuring Tool', $
                           /COLUMN, UVALUE=0, MBAR=bar )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Select existing field',$
                     '0\Load field by object id',$
                     '0\Change secondary field', $
                     '0\Start new field',$
                     '0\Add to image list', $
                     '0\Autoload Toggle', $
                     '0\Postscript Hardcopy', $
                     '2\Exit',$
                     '1\Edit', $
                     '2\Set all ? to N', $
                     '1\Tools',$
                     '0\Adjust frame registration', $
                     '0\Copy registration', $
                     '0\Refresh display', $
                     '0\Suppress overlay', $
                     '0\Toggle lines', $
                     '0\Toggle color mode', $
                     '2\Snap object'], UVALUE='THE_MENU', /MBAR)

   base = widget_base( mainbase, /ROW )

   win1 = widget_draw( base, XSIZE=szx, YSIZE=szy, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Window' )

   cbase = widget_base( base, /COLUMN )

   win2 = widget_draw( cbase, XSIZE=szsx, YSIZE=szsy, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Score' )

   b1 = widget_base( cbase, /row, /frame)
   previd = widget_button( b1, VALUE='Previous', uvalue='Previous Field' )
   nextid = widget_button( b1, VALUE='Next', uvalue='Next Field' )
   initialsid = widget_text(b1, VALUE='', /EDITABLE, XSIZE=4, uvalue='Initials' )

   b1 = widget_base( cbase, /column, /frame)
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value=' Field:' )
   field = widget_label(b2, value='', /dynamic_resize )
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value='   Red:' )
   first = widget_label( b2, value='', /dynamic_resize )
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value='  Cyan:' )
   second = widget_label( b2, value='', /dynamic_resize )

   b1 = widget_base( cbase, /row, /frame)
   leftid = widget_button(b1, VALUE=' <- ', uvalue='Left1')
   rightid = widget_button(b1, VALUE=' -> ', uvalue='Right1')
   t1 = widget_label(b1, value='  ')
   upid = widget_button(b1, VALUE='  ^  ', uvalue='Up1')
   downid = widget_button(b1, VALUE='  v  ', uvalue='Down1')

   b1 = widget_base( cbase, /column, /frame)
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value='Object:' )
   curobj = widget_label( b2, value=' ', /align_left, /dynamic_resize )
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value=' V/Dir:' )
   rate = widget_label( b2, value=' ', /align_left, /dynamic_resize )
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value=' Flags:' )
   flag = cw_bgroup( b2, ['?','y','n'], /no_release, $
                          /return_name, /exclusive, /row, uvalue='Set Flag' )

   newobj = widget_button( cbase, VALUE='New Object', uvalue='New Object' )

   win3 = widget_draw( cbase, XSIZE=szsx, YSIZE=szsx, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Zoom' )

   b2 = widget_base( cbase, /row, /frame)
   t1 = widget_button(b2, VALUE='X', uvalue='blink stop')
   t1 = widget_button(b2, VALUE='-', uvalue='blink slower')
   t1 = widget_button(b2, VALUE='[ ]', uvalue='blink pause')
   t1 = widget_button(b2, VALUE='+', uvalue='blink faster')
   blinkid = widget_button(b2, VALUE='>', uvalue='blink')

   b2 = widget_base( cbase, /column, /frame)
   t1 = widget_label( b2, VALUE='Max Display (sigma)' )
   maxsig = widget_text( b2, VALUE='5', /EDITABLE, XSIZE=5, uvalue='Set Max Sig' )
   b2 = widget_base( cbase, /column, /frame)
   t1 = widget_label( b2, VALUE='Min Display (sigma)' )
   minsig = widget_text( b2, VALUE='-3', /EDITABLE, XSIZE=5, uvalue='Set Min Sig' )

   widget_control, flag, SENSITIVE=0

   state = ptr_new({ $

      ; Data and information in the widget
      autoload: 1, $             ; Automatically load frame pair if set.
      blink: 0, $                ; Blink rate in milliseconds.  Disabled if 0
      blinkframe: 0B, $          ; Current blinking frame showing.
                                 ; Zero means the primary and 1 means 2ndary
      blinkid: blinkid, $        ; widget id for timer events and starting.
      colormode: 1, $            ; True, color composite, false=b/w difference
      curframe: 0, $             ; Current frame being measured.
      curobj: -1, $              ; Current object number being manipulated.
      curobjname: '', $          ; Current object name.
      dir: ptr_new(), $          ; Direction of motion (degrees)
      dirtylist: 0, $            ; Flag, if true object/file list needs saving.
      exttag: '', $              ; Image extension tag.
      field: '', $               ; Name of current field.
      flags: ptr_new(), $        ; Object validity flags.
      fnobj: '', $               ; File name of current field object list.
      fntrip: ptr_new(), $       ; Pointer to string array of file names for field.
      dt: ptr_new(), $           ; Pointer to array of delta t from first frame.
      fullname: ptr_new(fullname), $ ; String array of full names to go with initials.
      g: ptr_new(bytarr(szx,szy)), $ ; green (and blue) plane of color cube for display
      gain: gain, $              ; Gain of CCD (e-/ADU).
      hdr1: ptr_new(), $         ; Header of frame 1
      hdr2: ptr_new(), $         ; Header of frame 2
      hdrlist: hdrlist, $        ; FITS header keyword correspondence list.
      highval1:  0.0, $          ; top of stretch for image 1.
      highval2:  0.0, $          ; top of stretch for image 2.
      idstr: ptr_new(), $        ; Object measurement description strings.
      idxfn2: -1, $              ; Index for file 2 into oblist data.
      im1: ptr_new(), $          ; Pointer to primary epoch image.
      im2: ptr_new(), $          ; Pointer to secondary epoch image.
      info1: ptr_new(), $        ; Pointer to information structure on image 1.
      info2: ptr_new(), $        ; Pointer to information structure on image 2.
      initials: ptr_new(initials), $ ; String array of initials of scanner.
      lastobj: '', $             ; Name of last object list file loaded.
      lastfn2: '', $             ; Name of last secondary image loaded.
      lines:   1B, $             ; Flag, if true connecting lines are drawn.
      lowval1:  0.0, $           ; bottom of stretch for image 1.
      lowval2:  0.0, $           ; bottom of stretch for image 2.
      newfile: 0B, $             ; Control byte for allowing registration of new
                                 ;   frame pairs.  Meaning if set:
                                 ;   bit 0 (1) = Frame registration in progress on
                                 ;                 current image pair.
                                 ;   bit 1 (2) = Reference for im1 is known
                                 ;   bit 2 (4) = Reference for im2 is known
                                 ;   bit 3 (8) = First pair being examined, when
                                 ;                 offset is valid, copy to other
                                 ;                 extensions.
      nfiles: 0, $               ; Number of files in current list.
      n_initials: n_initials, $  ; Number of intials/names on record.
      nomarks: 0, $              ; Flag, if true suppress overplot in main window
      numext: 1, $               ; Number of frame extensions.
      nobj: 0, $                 ; Number of objects in current list.
      nx: 0L, $                  ; X size of image(s).
      ny: 0L, $                  ; Y size of image(s).
      objpath: '', $             ; Path to object list file.
      offvals: ptr_new(), $      ; Offsets between frames.
      path: path, $              ; Location for raw image data.
      pscale: pscale, $          ; Plate scale of image.
      r: ptr_new(bytarr(szx,szy)), $ ; red plane of color cube for display
      radius: objrad, $          ; Object aperture radius.
      rate: ptr_new(), $         ; Rate of motion ("/hr)
      ref1: [0.0,0.0], $         ; Reference position of image 1.
      ref2: [0.0,0.0], $         ; Reference position of image 2.
      sf: 1.0, $                 ; Scale factor between full image and score.
      srgb: bytarr(szsx,szsy,3), $ ; color cube for score
      szsx: szsx, $              ; X-width of score display in pixels.
      szsy: szsy, $              ; Y-width of score display in pixels.
      szx: szx, $                ; X-width of main display in pixels.
      szy: szy, $                ; Y-width of main display in pixels.
      x0: 0, $                   ; LLHC of display window from full image.
      xyvals: ptr_new(), $       ; Object locations.
      y0: 0, $                   ; LLHC of display window from full image.
      zim: bytarr(szsx/4,szsx/4), $ ; Zoom image for display.
      zf: 4, $                   ; Zoom factor for zoom window display.
      zx0: 0, $                  ; LLHC of zoom display window from full image.
      zy0: 0, $                  ; LLHC of zoom display window from full image.

      ; Widget ids
      curobjid: curobj, $        ; ID of label that shows current object id.
      downid:   downid, $
      drawwin:  win1, $          ; ID of main draw window
      fieldid:  field, $         ; ID of label that shows current field name.
      flagid:   flag, $          ; ID of object flag button list.
      fn1id:    first, $         ; ID of label that shows current image #1
      fn2id:    second, $        ; ID of label that shows current image #2
      initialsid: initialsid, $  ; ID of label that shows initials of scanner
      leftid:   leftid, $
      mainbase: mainbase, $      ; ID of top level base.
      maxsigid: maxsig, $        ; ID of text widget with display max
      minsigid: minsig, $        ; ID of text widget with display max
      nextid:   nextid, $        ; ID of Next button
      previd:   previd, $        ; ID of Previous button
      rateid:   rate, $          ; ID of label that shows rate/dir of current
      rightid:  rightid, $
      scorewin: win2, $          ; ID of score and pan control window
      upid:     upid, $
      zoomwin:  win3 $           ; ID of zoom window

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   XMANAGER, 'looker', mainbase, $
             EVENT_HANDLER='looker_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='looker_cleanup'

end

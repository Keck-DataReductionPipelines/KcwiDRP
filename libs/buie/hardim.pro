;+
; NAME:
;  hardim
; PURPOSE: (one line)
;  Create a postscript image and/or print from an image.
; DESCRIPTION:
;
;  This program will take an image and create a hard copy image via
;  Postscript.  Color tables will work only with a color printer.
;
;  If the plot device is not PS upon entry, this program will temporarily
;  switch devices if the file is printed.  If the file is not printed,
;  the device is left active and open so that additional information
;  can be added to the plot.
;
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  hardim,image,min,max
; INPUTS:
;  image - Input array to convert to a postscript image.
;  min   - Input array value to scale to output value of 0.
;  max   - Input array value to scale to !d.n_colors-1 (peak color).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;  AUTOSIZE     - Automatic output image size setting:
;                    0 - no automatic action (default).
;                    1 - resize if too big.
;                    2 - make image as large as possible.
;  BITS         - Number of output bits for image, default=8.
;  CTABLE       - Color table number to use for print.  Default is 0
;                   for the B/W lookup table.  If 0, behavior will default
;                   to non-color printing.
;  DELPOS       - Position offset (x,y) in centimeters for image.
;  ENCAPSULATED - Flag, if true ==> generate encapsulate postscript.
;                   This will automatically set NOPRINT.
;  FILE         - Name of postscript file, default is idl.ps
;  LANDSCAPE    - Flag, if true ==> landscape mode output orientation.
;                        Default is portrait.
;  NEGATIVE     - Invert the color table if set.
;  NOCLOSE      - If true, prevents closing the output device in
;                    encapsulated mode.
;  NOBOX        - When set will suppress the pixel axes around image.
;  NOPRINT      - Flag, if true ==> do not send the postscript file to the
;                   the printer.  Default to FALSE.
;  POSITION     - 2 element vector specifying the location of the lower
;                   left hand corner of the image relative to the page
;                   setup origin.  The default is to center the image
;                   on the page (less 3-hole punch margin).  Values are
;                   in centimeters.
;  QUEUE        - Name of the print queue to send postscript file to.
;                   Defaults to the default printer.
;  TITLE        - Title to put over image.
;  TRUE_COLOR   - Indicates 24-bit color input image if set, values are
;                    1 - pixel interleave (3, width, height)
;                    2 - row interleave   (width, 3, height)
;                    3 - image interleave (width, height, 3)
;                    sets bits=8 automatically.
;  XBORDER      - Two element vector with x border to put around image.
;                    Values are in cm, default = [0,0], used only for
;                    encapsulated output.
;  YBORDER      - Two element vector with y border to put around image.
;                    Values are in cm, default = [0,0], used only for
;                    encapsulated output.
;  WIDTH        - Width of image in cm.  Default = 15cm.
; OUTPUTS:
;  A postscript file is created.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This program knows about two kinds of printers, HP Laser Jet IIIsi
;  and a Tektronix Phaser IIsd color printer.  The color printer is
;  known by the queue name of 'chani'.  Error checking is in place to
;  make sure the image is not scaled off the page for each device.
;  If the queue is not chani, it is assumed to point at an HP printer.
;
;  The print queueing is specific to Unix and Win32.
;
; PROCEDURE:
; MODIFICATION HISTORY:
;  3/3/93, written by Marc W. Buie, Lowell Observatory
;  3/15/93, MWB, encapsulated output was forced to be full page, modified
;     so that output matches the image size.
;  94/10/05, MWB, added DELPOS keyword
;  96/01/06, MWB, added support for auto-spool for DOS/Windows
;  96/08/24, MWB, added AUTOSIZE
;  97/05/29, MWB, changed COLORTABLE to CTABLE and added COLOR keyword.
;  2000/10/13, MWB, removed SPAWN in this routine, now calling hardcopy.
;-
pro hardim,image,min,max, $
      AUTOSIZE=autosize, BITS=bits, $
      COLOR=color, CTABLE=colortable, DELPOS=delpos, $
      ENCAPSULATED=encapsulated, $
      FILE=file, LANDSCAPE=landscape, NEGATIVE=neg, NOBOX=nobox, $
      NOCLOSE=noclose, NOPRINT=noprint, POSITION=inpos, QUEUE=queue, $
      TITLE=title, TRUE_COLOR=true_color, XBORDER=xborder, YBORDER=yborder, $
      WIDTH=width,SILENT=silent

; Provide default values for numeric keywords.
if not keyword_set(autosize)   then autosize   = 0
if not keyword_set(bits)       then bits       = 8
if not keyword_set(colortable) then colortable = 0
if not keyword_set(color)      then color      = 0
if not keyword_set(file)       then file       = 'idl.ps'
if not keyword_set(width)      then width      = 15
if not keyword_set(silent)     then silent     = 0
if not keyword_set(title)      then title      = ''
if not keyword_set(xborder)    then xborder    = [0.,0.]
if not keyword_set(yborder)    then yborder    = [0.,0.]
if not keyword_set(delpos)     then delpos     = [0.,0.]

if not keyword_set(encapsulated) then begin
   xborder=xborder*0.
   yborder=yborder*0.
endif

; Verify valid image for printing.
im_size = size(image)
if im_size[0] ne 2 and not keyword_set(true_color) then begin
   print,'ERROR: image must be 2-d'
   return
endif

if im_size[0] ne 3 and keyword_set(true_color) then begin
   print,'ERROR: image must be 3-d for true color'
   return
endif

if keyword_set(true_color) then begin
   if true_color eq 1 then begin
      nx = im_size[2]
      ny = im_size[3]
   endif else if true_color eq 2 then begin
      nx = im_size[1]
      ny = im_size[3]
   endif else if true_color eq 3 then begin
      nx = im_size[1]
      ny = im_size[2]
   endif else begin
      print,'ERROR: ',true_color,' is an illegal value for TRUE_COLOR.'
      return
   endelse
endif else begin
   nx = im_size[1]
   ny = im_size[2]
endelse

; Save the current device name.
old_dev = !d.name
p_font = !p.font
if old_dev ne 'PS' then set_plot,'PS'

; Check for encapsulated output, disallow printing.
if keyword_set(encapsulated) then begin
   device,/encapsulated
   noprint=1
endif

;Set the output device file name.
device,file=file,/Helvetica
!p.font=0

;The height is computed from the width so that pixels are square.
height=width*ny/nx

if keyword_set(landscape) then begin
   maxheight = 7.8*2.54
   maxwidth  = 8.6*2.54
endif else begin
   maxwidth  = 7.8*2.54
   maxheight = 8.6*2.54
endelse

; Set position on paper.
if keyword_set(inpos) then begin
   xoffset = inpos[0]
   yoffset = inpos[1]
endif else begin
   if keyword_set(encapsulated) then begin
      if keyword_set(nobox) then begin
         xoffset = 0
         yoffset = 0
      endif else begin
         xoffset = 0.5
         yoffset = 0.5
      endelse
   endif else begin
      xoffset=((maxwidth-width)/2.0) > 0.0
      yoffset=((maxheight-height)/2.0) > 0.0
   endelse
endelse

; Adjust maxwidth and maxheight given new xoffset and yoffset
maxwidth  = maxwidth  - xoffset
maxheight = maxheight - yoffset

;Check for output image too large for media.

; Resize if too big.
if autosize eq 1 then begin
   if width gt maxwidth then begin
      sf = maxwidth/width
      width  = sf * width
      height = sf * height
   endif
   if height gt maxheight then begin
      sf = maxheight/height
      width  = sf * width
      height = sf * height
   endif

; Resize to max size.
endif else if autosize eq 2 then begin
   sf = maxwidth/width
   width  = sf * width
   height = sf * height
   if height gt maxheight then begin
      sf = maxheight/height
      width  = sf * width
      height = sf * height
   endif

;Abort if bad.
endif else begin
   if width gt maxwidth then begin
      print,'ERROR: width of output would be clipped on printer.  Aborting.'
      device,/close
      if old_dev ne !d.name then set_plot,old_dev
      return
   endif
   if height gt maxheight then begin
      print,'ERROR: height of output would be clipped on printer.  Aborting.'
      device,/close
      if old_dev ne !d.name then set_plot,old_dev
      return
   endif
endelse

print,'HARDIM: width  ',maxwidth,width,xoffset
print,'HARDIM: height ',maxheight,height,yoffset

;Look up and set the width and height for output device.
if keyword_set(landscape) then begin
   device,/landscape
   if keyword_set(encapsulated) then $
      device,xsize=width+2*xoffset+total(xborder), $
             ysize=height+2*yoffset+total(yborder) $
   else $
      device,xsize=7.8*2.54,ysize=8.6*2.54, $
         xoffset=0.2*2.54+delpos[0],yoffset=9.8*2.54+delpos[1]
endif else begin
   device,/portrait
   if keyword_set(encapsulated) then $
      device,xsize=width+2*xoffset+total(xborder), $
             ysize=height+2*yoffset+total(yborder) $
   else $
      device,xsize=7.8*2.54,ysize=8.6*2.54, $
         xoffset=0.5*2.54+delpos[0],yoffset=1.2*2.54+delpos[1]
endelse

;Set the color table, if appropriate.  Note that a normal b/w laser printer
;  will not like a color table.
if colortable eq 0 then begin
   if keyword_set(true_color) then begin
      device,/color,bits=8
   endif else device,color=color,bits=bits
   loadct,0,/silent
   if keyword_set(neg) then $
      img=!d.n_colors-1-bytscl(image,min=min,max=max,top=!d.n_colors-1) $
   else $
      img=bytscl(image,min=min,max=max,top=!d.n_colors-1)
endif else begin
   if not keyword_set(encapsulated) and queue ne 'chani' and queue ne 'orion' then begin
      print,'ERROR: colors not valid unless queue is set to chani.  Aborting.'
      device,/close
      if old_dev ne !d.name then set_plot,old_dev
      return
   endif
   device,/color,bits=bits
   img=bytscl(image,min=min,max=max,top=!d.n_colors-1)
   loadct,colortable,/silent
   if keyword_set(neg) then negative
endelse

if keyword_set(true_color) then $
   tv,img,(xoffset+xborder[0])*1000,(yoffset+yborder[0])*1000, $
          xsize=width*1000,ysize=height*1000,/device,true=true_color $
else $
   tv,img,(xoffset+xborder[0])*1000,(yoffset+yborder[0])*1000, $
          xsize=width*1000,ysize=height*1000,/device

;Set up plotting region on image and draw axes.
if not keyword_set(nobox) then plotstyles=1 else plotstyles=5

pos=[xoffset+xborder[0],yoffset+yborder[0], $
     xoffset+xborder[0]+width,yoffset+yborder[0]+height]*1000
plot,[0],position=pos,xr=[0,nx],xstyle=plotstyles,yr=[0,ny],ystyle=plotstyles, $
   /nodata,/noerase,/device,xticklen=-0.01,yticklen=-.01*ny/nx,title=title

;If printing, close file, send to printer and reset the display device.
if not keyword_set(noprint) then begin
   hardcopy,queue=queue
   if old_dev ne !d.name then set_plot,old_dev
endif

;If encapsulated, close things out.
if keyword_set(encapsulated) then begin
   if keyword_set(noclose) then begin
      if not silent then print,'Note: Output device still open for output.'
   endif else begin
      device,/close
      if old_dev ne !d.name then set_plot,old_dev
      !p.font=p_font
   endelse
endif else if keyword_set(noprint) and keyword_set(noclose) and not silent then begin
   print,'Note: Output device still open for output.'
endif else if keyword_set(noprint) and not keyword_set(noclose) then begin
   device,/close
   if old_dev ne !d.name then set_plot,old_dev
   !p.font=p_font
endif

end

;+
; NAME: 
;  setwin
;
; PURPOSE: 
;  Set current draw window, create if needed.
;
; DESCRIPTION:
;  This is intended to be a more general call for controlling the use of
;    multiple plot windows especially in scripts or programs.  In its simplest
;    form, just type   setwin,NUMBER   to make window NUMBER the current window.
;    If the window does not exist it will be created.  The other optional inputs
;    work as described in the IDL documentation for WDEL, WSHOW, WINDOW, WSET.
;    If the size, location, and title of the window does not change, then the window
;    is not recreated and its status on the screen is unchanged.
;
;  If the current display device is PS or PRINTER, most of the information
;    held by this routine is ignored.  Instead, the x and y sizes are used
;    to set the aspect ratio of the plot, using as much of the full page as
;    possible.  This routine will make a decision between portrait and landscape
;    orientations.
;
; CATEGORY:
;  Utility
;
; CALLING SEQUENCE:
;  setwin,id
;
; INPUTS:
;  id - window number to make the current draw window  (0 <= id <= 31)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  ERASE - Flag, if set causes the window to be erased after setting.
;  SHOW  - Flag, if set causes the window to be brought to the front.
;  XPOS  - X position of the lower left corner of the window
;  YPOS  - Y position of the lower left corner of the window
;  XSIZE - the x size of the window.
;  YSIZE - the y size of the window.
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;   MWB_SETWIN - hold internal data between calls
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  There is a limitation of the MAC platform.  You cannot easily discover if
;    a previously opened plot window still exists on the desktop.  Until this
;    is (ever) fixed, you should NEVER close (not minimize, that's ok) a window
;    once it's been opened.  I could make this function in this case (rather
;    than crashing), but it would destroy the intended functionality of this
;    program.
;
;  Also, this program only manages the first 32 "direct" plot windows.  It
;    cannot deal with the extra windows automatically managed beyond 31.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  94/04/26 - Written by Marc W. Buie, Lowell Observatory
;  98/01/29 - MWB, Added TITLE keyword and improved use of XPOS,YPOS
;  2000/04/03, MWB, Complete rewrite to improve cross-platform functionality.
;  2010/09/14, MWB, changed defaults on xsize/ysize
;-
pro setwin,id, ERASE=erase,SHOW=show,TITLE=title, $
               XPOS=xpos,YPOS=ypos,XSIZE=xsize,YSIZE=ysize

   common mwb_setwin,info

   if badpar(id,[1,2,3],0,CALLER='SETWIN: (id) ') then return
   if badpar(erase,[0,1,2,3],0,CALLER='SETWIN: (ERASE) ',default=0) then return
   if badpar(show,[0,1,2,3],0,CALLER='SETWIN: (SHOW) ',default=0) then return
   if badpar(xpos,[0,1,2,3],0,CALLER='SETWIN: (XPOS) ',default=-1) then return
   if badpar(ypos,[0,1,2,3],0,CALLER='SETWIN: (YPOS) ',default=-1) then return
   if badpar(xsize,[0,1,2,3],0,CALLER='SETWIN: (XSIZE) ',default=-1) then return
   if badpar(ysize,[0,1,2,3],0,CALLER='SETWIN: (YSIZE) ',default=-1) then return
   if badpar(title,[0,7],0,CALLER='SETWIN: (TITLE) ',default='[[default]]') then return

   if id lt 0 or id ge 32 then begin
      print,'SETWIN: window id must be >= 0 and < 32'
      return
   endif

   ; If no structure yet, initialize
   sz_info=size(info)
   if sz_info[n_elements(sz_info)-2] ne 10 then info = ptrarr(32)

   ; Start with the assumption that the window doesn't need creation, this
   ;   may be modified later.
   create = 0

   if (!d.flags and 256) ne 0 and !d.name ne 'MAC' then begin
      device,window_state=win_state
      if win_state[id] eq 0 then create=1
   endif

   ; Has this window been used before?
   if not ptr_valid(info[id]) then begin
      create = 1
      if xsize lt 0 then xsize=512
      if ysize lt 0 then ysize=512
      info[id] = ptr_new( { id: id, $
                       xpos: xpos, $
                       ypos: ypos, $
                       xsize: xsize, $
                       ysize: ysize, $
                       xoffset: 0, $
                       yoffset: 0, $
                       title: title } )
   endif else if (!d.flags and 256) ne 0 and not create then begin
      wset,id
      (*info[id]).xsize = !d.x_size
      (*info[id]).ysize = !d.y_size
      device,get_window_position=wp
      (*info[id]).xpos  = wp[0] - (*info[id]).xoffset
      if !d.name eq 'WIN' or !d.name eq 'MAC' then begin
         (*info[id]).ypos=wp[1] - (*info[id]).ysize - (*info[id]).yoffset
      endif else begin
         (*info[id]).ypos=wp[1]
      endelse
   endif

   if xsize ge 0 and (*info[id]).xsize ne xsize then begin
      (*info[id]).xsize=xsize
      create=1
   endif
   if ysize ge 0 and (*info[id]).ysize ne ysize then begin
      (*info[id]).ysize=ysize
      create=1
   endif
   if xpos ge 0 and (*info[id]).xpos ne xpos then begin
      if !d.name eq 'WIN' or !d.name eq 'MAC' then begin
         (*info[id]).xpos=xpos
      endif else begin
         (*info[id]).xpos=xpos
      endelse
      create=1
   endif
   if ypos ge 0 and (*info[id]).ypos ne ypos then begin
      (*info[id]).ypos=ypos
      create=1
   endif

   if title ne '[[default]]' and title ne (*info[id]).title then begin
      (*info[id]).title = title
      create=1
   endif

   ; If postscript device, then just adjust xsize and ysize according to
   ;   the input.  The goal is to keep the aspect ratio the same.  This will
   ;   choose between portrait and landscape and will make the plot the full
   ;   size of the page subject to the constraint of the apsect ratio of the
   ;   known size of the chosen window.
   if !d.name eq 'PS' or !d.name eq 'PRINTER' then begin
      aspect = float((*info[id]).ysize)/float((*info[id]).xsize)
      if aspect lt 1.0 then begin ; more wide than tall, landscape
         xsize = 25.0
         ysize = 25.0*(*info[id]).ysize/(*info[id]).xsize
         fac = 19.0/ysize
         if fac lt 1.0 then begin
            xsize=xsize*fac
            ysize=ysize*fac
         endif
         setpage,/landscape,xsize=xsize,ysize=ysize
      endif else begin ; more tall than wide, portrait
         ysize = 25.0
         xsize = 25.0*(*info[id]).xsize/(*info[id]).ysize
         fac = 19.0/xsize
         if fac lt 1.0 then begin
            xsize=xsize*fac
            ysize=ysize*fac
         endif
         setpage,/portrait,xsize=xsize,ysize=ysize
      endelse
      return
   endif

   ; If not a windowing device, exit before doing anything.
   if (!d.flags and 256) eq 0 then return

   ; if the window needs to be created, check and see if positions, sizes,
   ;  or title are defaulted.
   ;  if so, make the defaults match the way the system does it.
   if create eq 1 then begin
      device,get_screen_size=sc
      case id mod 4 OF
         0: begin
            default=[sc[0]/2,sc[1]/2,sc[0]/2-10,sc[1]/2-25]
         end
         1: begin
            default=[sc[0]/2,0,sc[0]/2-10,sc[1]/2-25]
         end
         2: begin
            default=[0,sc[1]/2,sc[0]/2-10,sc[1]/2-25]
         end
         3: begin
            default=[0,0,sc[0]/2-10,sc[1]/2-25]
         end
      endcase
      if (*info[id]).xpos  lt 0 then (*info[id]).xpos =default[0]
      if (*info[id]).ypos  lt 0 then (*info[id]).ypos =default[1]
      if (*info[id]).xsize lt 0 then (*info[id]).xsize=default[2]
      if (*info[id]).ysize lt 0 then (*info[id]).ysize=default[3]
      if (*info[id]).title eq '[[default]]' then (*info[id]).title='IDL '+strn(id)
      window,id,xpos=(*info[id]).xpos-(*info[id]).xoffset, $
                ypos=(*info[id]).ypos-(*info[id]).yoffset, $
                xsize=(*info[id]).xsize,ysize=(*info[id]).ysize, $
                title=(*info[id]).title
      device,get_window_position=wp
      if !d.name eq 'WIN' or !d.name eq 'MAC' then begin
         (*info[id]).yoffset = wp[1] - (*info[id]).ysize - (*info[id]).ypos
      endif else begin
         (*info[id]).yoffset = wp[1] - (*info[id]).ypos
      endelse
      (*info[id]).xoffset = wp[0] - (*info[id]).xpos
   endif else begin
      wset,id
   endelse

   ; Erase if set
   if erase then erase

   ; Bring to front if SHOW set
   if show then wshow

end

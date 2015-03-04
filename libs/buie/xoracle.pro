;+
; NAME:
;	xoracle
; PURPOSE:
;	Display ``oracle'' animation image sequences.
; CATEGORY:
;	Widgets
; CALLING SEQUENCE:
;	XORACLE,frame_1,frame_2
; INPUTS:
;       frame_1 - Animation sequence frame number to start with.
;       frame_2 - Animation sequence frame number to end with.
; OPTIONAL INPUT PARAMETERS:
;       IMDELTA - interval between loaded images (default = 1)
; KEYWORD PARAMETERS:
;	GROUP = The widget ID of the widget that calls Xoracle.  When this
;		ID is specified, a death of the caller results in a death of
;		Xoracle
; OUTPUTS:
; OPTIONAL OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;	Initiates the XManager if it is not already running.
; RESTRICTIONS:
; PROCEDURE:
;  Read a sequence of oracle animation files in byte format and animate.
; MODIFICATION HISTORY:
;	Created 12/18/91 by: Marc Buie, Lowell Observatory
;-

; Common block items:
; frames         - image cube holding the data for the current animation run.
; first_image    - frame number of the first image in the cube.
; last_image     - frame number of the last image in the cube.
; nrows          - number of rows in each frame/image.
; ncols          - number of columns in each frame/image.
; nframes        - number of frames in the image cube.
; currentframe   - Index of the current frame within the image cube.
; imagedelta     - Inteval between loaded frames.
; animatedelta   - Animation interval between displayed frames.
; framedelta     - Interval between displayed frames.
; delay          - Amount of delay between image updates.
; animatepause   - Button id.
; animatespeed   - Slider id.
; t0             - Time of next image update.
; animateframe   - Slider id for controlling displayed frame when paused.
; animatebase    - Global widget base.
; anwin          - Window id for animation window.

; ------------------------------------------------------------------------------
;	procedure Xoracle_bck
; ------------------------------------------------------------------------------
; This routine performs the background tasks while the XManager awaits new
; events.  There is a speed control that controls the length of time between
; updates of the animation.  If the current time is earlier than the expiration
; time, this routine simply returns.  When the current time is later than the
; expiration time, this routine increments the frame pointer and then
; displays the appropriate frame.  If the animation is paused, the routine
; also returns without doing anything except updating the time counter.
; ------------------------------------------------------------------------------

pro Xoracle_bck, baseid


common Xoracle_com, frames, first_image, last_image, nrows, ncols, nframes, $
                    currentframe, imagedelta, animatestep, $
                    framedelta, delay, animatepause, animatespeed, t0, $
                    animateframe, animatebase, anwin

t = systime(1)

if t ge t0 then begin
   t0 = t + delay
   if framedelta eq 0 then return
   currentframe = currentframe + framedelta
   if currentframe ge nframes then currentframe = currentframe - nframes
   wset,anwin
   tv,frames[*,*,currentframe],/order
   empty
endif

end ;============== end of Xoracle background task routine ================

; ------------------------------------------------------------------------------
;	procedure Xoracle_clean
; ------------------------------------------------------------------------------

pro Xoracle_clean, baseid

common Xoracle_com, frames, first_image, last_image, nrows, ncols, nframes, $
                    currentframe, imagedelta, animatestep, $
                    framedelta, delay, animatepause, animatespeed, t0, $
                    animateframe, animatebase, anwin

frames = 0
nframes = 0

end ;============== end of Xoracle clean-up routine =======================

; ------------------------------------------------------------------------------
;	procedure Xoracle_ev
; ------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
; ------------------------------------------------------------------------------
PRO Xoracle_ev, event

common Xoracle_com, frames, first_image, last_image, nrows, ncols, nframes, $
                    currentframe, imagedelta, animatestep, $
                    framedelta, delay, animatepause, animatespeed, t0, $
                    animateframe, animatebase, anwin

WIDGET_CONTROL, event.id, GET_UVALUE = eventval		;find the user value
							;of the widget where
							;the event occured
CASE eventval OF

   "ANIMATESPEED": $ ; Set new animation rate
      begin
         widget_control, animatespeed, get_value = temp
         if temp eq 100 then delay=0. else delay = 2./(1.+temp)
         t0 = systime(1) + delay
      end

   "ANIMATEPAUSE": $
      begin
         widget_control, animatespeed, sensitive=0
         XBackRegister, "xoracle_bck", animatebase, /UNREGISTER
         framedelta = 0
         widget_control, animateframe, set_value=currentframe
         widget_control, animateframe, /sensitive
      end

   "ANIMATEFRAME": $
      begin
         widget_control, animateframe, get_value = currentframe
         wset,anwin
         tv,frames[*,*,currentframe],/order
         empty
      end

   "ANIMATE": $
      begin
         widget_control, animateframe,sensitive=0
         if framedelta eq 0 then XBackRegister, "xoracle_bck", animatebase
         framedelta = animatestep
         widget_control, animatespeed, /sensitive
      end

   "ANIMPALADJ": Xloadct, GROUP = event.top

   "ANIMEXIT": widget_control, event.top, /destroy

   ELSE: MESSAGE, "Event User Value Not Found",/continue

ENDCASE

END ;============= end of Xoracle event handling routine task =============

; ------------------------------------------------------------------------------
;	procedure Xoracle_load
; ------------------------------------------------------------------------------
; This routine loads the data.
; ------------------------------------------------------------------------------

pro Xoracle_load, f1, f2, fstep

common Xoracle_com, frames, first_image, last_image, nrows, ncols, nframes, $
                    currentframe, imagedelta, animatestep, $
                    framedelta, delay, animatepause, animatespeed, t0, $
                    animateframe, animatebase, anwin

;Load the first image
frames = rdbyt('plch'+string(f1,format='(i4.4)')+'.byt')
s=size(frames)
ncols=s[1]
nrows=s[2]
nframes=fix((f2-f1+1)/fstep)
print,'Allocating ',float(ncols)*long(nrows)*long(nframes)/1024./1024.,' Mbytes in ',nframes,' frames'
frames = bytarr(ncols,nrows,nframes)
imagedelta = fstep
first_image = f1
last_image = f2

; Load the rest of the images
j=0
for i=f1,f2,fstep do begin
   fname = 'plch'+string(i,format='(i4.4)')+'.byt'
   frames[*,*,j] = rdbyt(fname)
   j = j + 1
endfor

end


; ------------------------------------------------------------------------------
;	procedure Xoracle
; ------------------------------------------------------------------------------
; This routine creates the widget and registers it with the XManager.
; ------------------------------------------------------------------------------
PRO Xoracle, frame1, frame2, IMDELTA = IMDELTA, GROUP = GROUP

common Xoracle_com, frames, first_image, last_image, nrows, ncols, nframes, $
                    currentframe, imagedelta, animatestep, $
                    framedelta, delay, animatepause, animatespeed, t0, $
                    animateframe, animatebase, anwin

if XRegistered("Xoracle") then return	;only one instance of Xoracle allowed

if not(keyword_set(IMDELTA)) then imdelta=1

Xoracle_load, frame1, frame2, imdelta ; Load the frames

animatebase = widget_base(/row, title = "Xoracle")	;create the main base
animatecntl = widget_base(animatebase, /column, /frame, xpad=10, ypad=30)

animatespeed = widget_slider(animatecntl, $
		xsize=240, /drag, $
		uvalue="ANIMATESPEED", $
		value=100, maximum = 100, minimum = 0, $
		title="Animation Speed")

animateframe = widget_slider(animatecntl, $
		xsize=240, $
		uvalue="ANIMATEFRAME", $
		value=0, minimum=0, maximum=nframes-1, $
		title="Animation Frame")

animatebuts  = widget_base(animatecntl, /row)

animatepause = widget_button(animatebuts, value="Still", uvalue="ANIMATEPAUSE")

animate      = widget_button(animatebuts, value="Animate", uvalue="ANIMATE")

XPdMenu, [      '"Adjust Color Palette"         ANIMPALADJ',    $
		'"Load New Set"			ANIMLOAD',      $
                '"Done"				ANIMEXIT'], animatecntl

i = widget_base(animatebase, /column)
animatedisplay=widget_draw(i,xsize=ncols,ysize=nrows, $
                           xoffset = 280, yoffset = 20, retain = 0)

;*** Typically, any widgets you need for your application are created here.
;*** Create them and use Xoraclebase as their base.  They will be realized
;*** (brought into existence) when the following line is executed.

WIDGET_CONTROL, animatebase, /REALIZE

widget_control, animatedisplay, get_value=anwin

currentframe = 0
framedelta = 1
animatestep = framedelta
t0 = systime(1)
delay = 0.

XManager, "Xoracle", animatebase, $
		BACKGROUND = "Xoracle_bck", $
		EVENT_HANDLER = "Xoracle_ev", $
		GROUP_LEADER = GROUP, $
		CLEANUP = "xoracle_clean"

END ;==================== end of Xoracle main routine =======================

;+
; NAME:
;	xrunplot
; PURPOSE:
;	Display a complete graphical summary of ``plutomem'' output logs.
; CATEGORY:
;	Widgets
; CALLING SEQUENCE:
;	XRUNPLOT
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;	GROUP = The widget ID of the widget that calls Xrunplot.  When this
;		ID is specified, a death of the caller results in a death of
;		Xrunplot
; OUTPUTS:
; OPTIONAL OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;	Initiates the XManager if it is not already running.
; RESTRICTIONS:
; PROCEDURE:
;	Create and register the widget and then exit.
; MODIFICATION HISTORY:
;	Written by Marc Buie, 1991 Dec 12
;  2000/11/08, MWB, removed use of obsolete ()
;-

; ------------------------------------------------------------------------------
;	procedure Xrunplot_ev
; ------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
;*** This is the event handling routine for the Xrunplot widget.  It is 
;*** responsible for dealing with the widget events such as mouse clicks on
;*** buttons in the Xrunplot widget.  The tool menu choice routines are 
;*** already installed.  This routine is required for the Xrunplot widget to
;*** work properly with the XManager.
; ------------------------------------------------------------------------------
PRO Xrunplot_ev, event

common runsum,iname,up,caim,chi2,test,s,i1,i2,p1,p2,p3,p4

WIDGET_CONTROL, event.id, GET_UVALUE = eventval		;find the user value
							;of the widget where
							;the event occured

type=tag_names(event,/structure)

if (type eq 'WIDGET_BUTTON') then begin

   widget_control,event.id,get_value=eventval

   if (eventval eq 'Done') then widget_control,event.top,/destroy

   if (eventval eq 'Hardcopy') then Xrun_hard

endif else if (type eq 'WIDGET_SLIDER') then begin

   widget_control,event.id,get_value=eventval
   widget_control,event.id,get_uvalue=slider

   if (slider eq 'i1') then Xrun_plot,eventval,i2

   if (slider eq 'i2') then Xrun_plot,i1,eventval

endif else begin

print,'Event type of ',type,' not yet supported'

end

END ;============= end of Xrunplot event handling routine task =============


pro Xrun_plot,new_i1,new_i2

common runsum,iname,up,caim,chi2,test,s,i1,i2,p1,p2,p3,p4

i1=new_i1
i2=new_i2

!p.multi=0

yr=[min([caim[i1:i2],chi2[i1:i2]]),max([caim[i1:i2],chi2[i1:i2]])]
 
xt='Iteration number ('+string(iname[0],format='(a4)')+')'

widget_control,get_value=window,p1

wset,window
plot,chi2,xr=[i1,i2],charsize=1.0,xtitle=xt,ytitle='Chi^2',yrange=yr,ymargin=[3,1]
oplot,caim,line=1

widget_control,get_value=window,p2

wset,window
plot,chi2-caim,xr=[i1,i2],charsize=1.0,xtitle=xt,ytitle='delta Chi^2',ymargin=[3,1]

widget_control,get_value=window,p3

wset,window
plot,s,xr=[i1,i2],charsize=1.0,ytitle='Entropy',ymargin=[3,1],xtitle=xt

widget_control,get_value=window,p4

wset,window
plot,test,xr=[i1,i2],charsize=1.0,xtitle=xt,ytitle='Test',ymargin=[3,1]
 
u = where( up eq 'u', count )
if count ne 0 then begin
   oplot,u,u*0,psym=4
end

end

pro Xrun_hard

common runsum,iname,up,caim,chi2,test,s,i1,i2,p1,p2,p3,p4

!p.multi=[0,1,4]
olddevice=!d.name
portrait

yr=[min([caim[i1:i2],chi2[i1:i2]]),max([caim[i1:i2],chi2[i1:i2]])]
 
xt='Iteration number ('+string(iname[0],format='(a4)')+')'

plot,chi2,xr=[i1,i2],charsize=2.0,xtitle=xt,ytitle='Chi^2',yrange=yr
oplot,caim,line=1

plot,chi2-caim,xr=[i1,i2],charsize=2.0,xtitle=xt,ytitle='delta Chi^2'

plot,s,xr=[i1,i2],charsize=2.0,ytitle='Entropy',xtitle=xt

plot,test,xr=[i1,i2],charsize=2.0,xtitle=xt,ytitle='Test'
 
u = where( up eq 'u', count )
if count ne 0 then begin
   oplot,u,u*0,psym=4
end

hardcopy
set_plot,olddevice

end


; ------------------------------------------------------------------------------
;	procedure Xrunplot
; ------------------------------------------------------------------------------
; This routine creates the widget and registers it with the XManager.
;*** This is the main routine for the Xrunplot widget.  It creates the
;*** widget and then registers it with the XManager which keeps track of the 
;*** currently active widgets.  
; ------------------------------------------------------------------------------
PRO Xrunplot, GROUP = GROUP

common runsum,iname,up,caim,chi2,test,s,i1,i2,p1,p2,p3,p4

;only one instance of the Xrunplot widget is allowed.
;If it is already managed, do nothing and return

IF(XRegistered("Xrunplot") NE 0) THEN RETURN

;*** Next the main base is created.  You will probably want to specify either
;*** a ROW or COLUMN base with keywords to arrange the widget visually.

   first=1

   spawn,'collect',unit=lun

   iname0=''
   up0=''
   on_ioerror,exit
   while(not eof(lun)) do begin
      readf,lun,iname0,up0,caim0,chi20,test0,s0,format='(a8,24x,a1,16x,f6.1,7x,f6.1,7x,f6.4,2x,e12.5)'
      if ( first eq 1 ) then begin
         iname = iname0
         up    = up0
         caim  = caim0
         chi2  = chi20
         test  = test0
         s     = s0
         first = 0
      endif else begin
         iname = [iname,iname0]
         up    = [up,up0]
         caim  = [caim,caim0]
         chi2  = [chi2,chi20]
         test  = [test,test0]
         s     = [s,s0]
      end
   endwhile
exit:
   free_lun,lun

   i1 = 0
   i2 = n_elements(caim)-1

Xrunplotbase = WIDGET_BASE(TITLE="Xrunplot",/column)	;create the main base

p1=widget_draw(Xrunplotbase,xsize=500,ysize=150)

p2=widget_draw(Xrunplotbase,xsize=500,ysize=150)

p3=widget_draw(Xrunplotbase,xsize=500,ysize=150)

p4=widget_draw(Xrunplotbase,xsize=500,ysize=150)

gang=widget_base(Xrunplotbase,/frame,/row,xpad=15,space=50)

s1=widget_slider(gang,TITLE='Starting iteration number', $
   minimum=i1,maximum=i2,value=i1,uvalue='i1')

s2=widget_slider(gang,TITLE='Ending iteration number', $
   minimum=i1,maximum=i2,value=i2,uvalue='i2')

b2 = widget_button(Xrunplotbase,value="Hardcopy")

b1 = widget_button(Xrunplotbase,value="Done")

WIDGET_CONTROL, Xrunplotbase, /REALIZE			;create the widgets
							;that are defined

Xrun_plot,i1,i2

XManager, "Xrunplot", Xrunplotbase, $			;register the widgets
		EVENT_HANDLER = "Xrunplot_ev", $	;with the XManager
		GROUP_LEADER = GROUP			;and pass through the
							;group leader if this
							;routine is to be 
							;called from some group
							;leader.

END ;==================== end of Xrunplot main routine =======================

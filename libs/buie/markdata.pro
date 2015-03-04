;+
; NAME:
;  markdata
; PURPOSE:
;  Widget for marking/unmarking bad data.
; DESCRIPTION:
;  Mouse click and drag will define a selection of points.  Newly selected
;  points will be marked in green.  To mark these as bad, click the 'Bad'
;  button.  To mark these green points as good, click the 'Good' button.
;  Bad points are plotted in red.  The rest of the control buttons should
;  be self-explanatory.
; CATEGORY:
;  Widgets
; CALLING SEQUENCE:
;  markdata,y,bad
;  markdata,x,y,bad
;  markdata,x,y,yerr,bad
;  markdata,x,xerr,y,yerr,bad
; INPUTS:
;  y   - Dependent variable.
;  bad - Flag array, 0 --> good data, 1 --> bad data.
; OPTIONAL INPUT PARAMETERS:
;  x   - (3, 4 or 5 arg input) Independent variable (if not supplied, ordinal
;           point number is used instead.
;  xerr - (5 arg input) Uncertainty on x.
;  yerr - (4 or 5 arg input) Uncertainty on y.
; KEYWORD INPUT PARAMETERS:
;  GROUP:  The widget ID of the widget that calls MARKDATA.  When this
;          ID is specified, a death of the caller results in the death of
;          the MARKDATA widget application.
;
;  TITLE:  A scalar string to be used for the window title.  If it is
;          not specified, the default title is "Data Editor"
;
;  XTITLE: X title for plot, default='x'
;
;  YTITLE: Y title for plot, default='y'
;
;  PTITLE: Title string for plot.
;
;  XSIZE:  ysize of plot window, default=600
;
;  YSIZE:  ysize of plot window, default=400
;
;  SCALING: 0 -> start with Scale Good (default)
;           1 -> start with Scale Good (default)
;
;  XFLIP:  Flag, true means to flip the x-axis
;  YFLIP:  Flag, true means to flip the y-axis
;  CONNECT: flag, true means to connect data points with lines
;
; OUTPUTS:
;  bad - modified flag array.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
;  MWB_MARKDATA: COMMON block that holds the new vector for the bad values
;                  during the time it takes to exit the routine.
; SIDE EFFECTS:
;  This function initiates the XMANAGER if it is not already running.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/10/29 - Written by Marc W. Buie, Lowell Observatory
;  97/2/5, MWB, added Clear, and All Bad buttons.
;  2003/05/31, MWB, added Worst button.
;  2003/06/17, MWB, rewrote internal structure, some GUI changes
;  2003/06/18, MWB, added x,xerr,y,yerr option
;  2004/06/13, MWB, added support for UINT, ULONG, LONG64, ULONG64
;  2007/01/15, Peter L. Collins, Lowell Observatory
;                allowed case of all points bad to work.
;-

; ------------------------------------------------------------------------------
;       procedure markdata_cleanup
; ------------------------------------------------------------------------------
;  This is used to pass the new bad values array from the structure in the
;    widget to the main routine so that the modified values can be returned
;    to the calling program.
; ------------------------------------------------------------------------------

pro markdata_cleanup,tlb

   ; kludge used just to get the modified badvals back to the calling routine
   common mwb_markdata,badvals

   widget_control,tlb,GET_UVALUE=state

   badvals = (*state).bad

   ptr_free, state

end

; ------------------------------------------------------------------------------
;       procedure markdata_plot
; ------------------------------------------------------------------------------
; Plot the data, marking the three data states
;
; There are lots of different kinds of plots, but, not all are possible for
;    all the possible input types
;
;               +-------+-------+-------+------+-------+
;               |   2   |   3   |   4   |   5  |  type |
;               +-------+-------+-------+------+-------+
; i      y      |   X   |   -   |   -   |   -  |    0  |
; x      y      |   -   |   X   |   X   |   X  |    1  |
; x      y/ysig |   -   |   -   |   X   |   X  |    2  |
; x/xsig y/ysig |   -   |   -   |   -   |   X  |    3  |
; x      ysig   |   -   |   -   |   X   |   X  |    4  |
; y      ysig   |   -   |   -   |   X   |   X  |    5  |
; x      xsig   |   -   |   -   |   -   |   X  |    6  |
; y      xsig   |   -   |   -   |   -   |   X  |    7  |
; xsig   ysig   |   -   |   -   |   -   |   X  |    8  |
; y      x/xsig |   -   |   -   |   -   |   X  |    9  |
; x      y/rsig |   -   |   -   |   -   |   X  |   10  |
; x      rsig   |   -   |   -   |   -   |   X  |   11  |
; y      rsig   |   -   |   -   |   -   |   X  |   12  |
;               +-------+-------+-------+------+-------+
; ------------------------------------------------------------------------------
pro markdata_plot, state, full

   ; Find data not marked bad
   zg=where((*state).bad eq 0,countg)
   zb=where((*state).bad eq 1,countb)
   zn=where((*state).new eq 1,countn)

   if full then begin
      if countg eq 0 then $
         message,'Warning no good data left!',/info

      ; Determine the plot scaling.
      if (*state).scaling eq 0 or countg eq 0 then begin
         xr=minmax((*state).px,/nan)
         yr=minmax((*state).py,/nan)
      endif else begin
         xr=minmax((*state).px[zg],/nan)
         yr=minmax((*state).py[zg],/nan)
      endelse

      if (*state).xflip then xr=reverse(xr)
      if (*state).yflip then yr=reverse(yr)

      ; Plot axes
      plot,[0],[1],xr=xr,yr=yr,/nodata, $
         xtitle=(*state).pxtitle,ytitle=(*state).pytitle,title=(*state).ptitle

      if countg gt 0 then begin
         if (*state).plottype eq  0 or (*state).plottype eq  1 or $
            (*state).plottype eq  4 or (*state).plottype eq  5 or $
            (*state).plottype eq  6 or (*state).plottype eq  7 or $
            (*state).plottype eq  8 or (*state).plottype eq 11 or $
            (*state).plottype eq 12                         then begin
            oplot,[(*state).px[zg]],[(*state).py[zg]],psym=(*state).connect*8, $
               color=(*state).color[9], symsize=0.75+(*state).connect*0.25
         endif else if (*state).plottype eq 2 then begin
            oploterror,[(*state).px[zg]],[(*state).py[zg]],[(*state).yerr[zg]], $
               psym=(*state).connect*8,color=(*state).color[9], $
               symsize=0.75+(*state).connect*0.25
         endif else if (*state).plottype eq 3 then begin
            oploterror,[(*state).px[zg]],[(*state).py[zg]], $
               [(*state).xerr[zg]],[(*state).yerr[zg]], $
               psym=(*state).connect*8,color=(*state).color[9], $
               symsize=0.75+(*state).connect*0.25
         endif else if (*state).plottype eq 9 then begin
            oploterror,[(*state).px[zg]],[(*state).py[zg]],[(*state).xerr[zg]], $
               psym=(*state).connect*8,color=(*state).color[9], $
               symsize=0.75+(*state).connect*0.25
         endif else if (*state).plottype eq 10 then begin
            oploterror,[(*state).px[zg]],[(*state).py[zg]],[(*state).rerr[zg]], $
               psym=(*state).connect*8,color=(*state).color[9], $
               symsize=0.75+(*state).connect*0.25
         endif
      endif
   endif

   ; Plot bad data
   if countb ne 0 then $
      oplot,[(*state).px[zb]],[(*state).py[zb]],psym=8, $
         color=(*state).color[1],symsize=0.75+(*state).connect*0.25

   ; Highlight newly marked data
   if countn ne 0 then $
      oplot,[(*state).px[zn]],[(*state).py[zn]],psym=8, $
         color=(*state).color[2],symsize=0.75+(*state).connect*0.25

end

; ------------------------------------------------------------------------------
pro markdata_setdata, state

   case (*state).plottype of

      0: begin
         (*state).px = (*state).x
         (*state).py = (*state).y
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = (*state).ytitle
      end

      1: begin
         (*state).px = (*state).x
         (*state).py = (*state).y
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = (*state).ytitle
      end

      2: begin
         (*state).px = (*state).x
         (*state).py = (*state).y
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = (*state).ytitle
      end

      3: begin
         (*state).px = (*state).x
         (*state).py = (*state).y
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = (*state).ytitle
      end

      4: begin
         (*state).px = (*state).x
         (*state).py = (*state).yerr
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = (*state).ytitle+' error'
      end

      5: begin
         (*state).px = (*state).y
         (*state).py = (*state).yerr
         (*state).pxtitle = (*state).ytitle
         (*state).pytitle = (*state).ytitle+' error'
      end

      6: begin
         (*state).px = (*state).x
         (*state).py = (*state).xerr
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = (*state).xtitle+' error'
      end

      7: begin
         (*state).px = (*state).y
         (*state).py = (*state).xerr
         (*state).pxtitle = (*state).ytitle
         (*state).pytitle = (*state).xtitle+' error'
      end

      8: begin
         (*state).px = (*state).xerr
         (*state).py = (*state).yerr
         (*state).pxtitle = (*state).xtitle+' error'
         (*state).pytitle = (*state).ytitle+' error'
      end

      9: begin
         (*state).px = (*state).y
         (*state).py = (*state).x
         (*state).pxtitle = (*state).ytitle
         (*state).pytitle = (*state).xtitle
      end

     10: begin
         (*state).px = (*state).x
         (*state).py = (*state).y
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = (*state).ytitle
      end

     11: begin
         (*state).px = (*state).x
         (*state).py = (*state).rerr
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = 'combined error'
      end

     12: begin
         (*state).px = (*state).y
         (*state).py = (*state).rerr
         (*state).pxtitle = (*state).xtitle
         (*state).pytitle = 'combined error'
      end

   endcase

end

; ------------------------------------------------------------------------------
pro markdata_setworst, state, numtomark

   case (*state).plottype of

      0: begin
         ; NOP
      end

      1: begin
         ; NOP
      end

      2: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).yerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

      3: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).yerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

      4: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).yerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

      5: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).yerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

      6: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).xerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

      7: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).xerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

      8: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).yerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

      9: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).xerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

     10: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).rerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

     11: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).rerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

     12: begin
         z=where((*state).new eq 0 and (*state).bad eq 0,count)
         if count ne 0 then begin
            zz=sort((*state).rerr[z])
            if count gt numtomark then zz=zz[n_elements(zz)-numtomark:*]
            (*state).new[z[zz]]=1
         endif
      end

   endcase

end

; ------------------------------------------------------------------------------
;       procedure markdata_eve
; ------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
; ------------------------------------------------------------------------------
pro markdata_eve, event

   ; Get the state
   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   plotit=1
   fullplot=1

   case event_name of

      'THE_MENU': begin
         case event.value of

            'All Bad': begin
                  (*state).bad[*]=1
               end

            'All Good': begin
                  (*state).bad[*]=0
                  ; Protection against NaN's
                  z=where(finite((*state).y) eq 0,count)
                  if count ne 0 then begin
                     print,'MARKDATA: warning, ',strcompress(count), $
                        ' NaN values automatically marked bad.'
                     (*state).bad[z]=1
                  endif
               end

            'Flip X' : begin
               (*state).xflip = not (*state).xflip
            end

            'Flip Y' : begin
               (*state).yflip = not (*state).yflip
            end

            'Scale All': begin
                  (*state).scaling = 0
               end

            'Scale Good': begin
                  (*state).scaling = 1
               end

            'Toggle Lines': begin
                  if (*state).connect eq 1 then $
                     (*state).connect = -1 $
                  else $
                     (*state).connect = 1
               end

            'x-y': begin
               (*state).plottype = 1
               markdata_setdata,state
            end

            'x-y/ysig': begin
               (*state).plottype = 2
               markdata_setdata,state
            end

            'x/xsig-y/ysig': begin
               (*state).plottype = 3
               markdata_setdata,state
            end

            'x-ysig': begin
               (*state).plottype = 4
               markdata_setdata,state
            end

            'y-ysig': begin
               (*state).plottype = 5
               markdata_setdata,state
            end

            'x-xsig': begin
               (*state).plottype = 6
               markdata_setdata,state
            end

            'y-xsig': begin
               (*state).plottype = 7
               markdata_setdata,state
            end

            'xsig-ysig': begin
               (*state).plottype = 8
               markdata_setdata,state
            end

            'y-x/xsig': begin
               (*state).plottype = 9
               markdata_setdata,state
            end

            'x-y/rsig': begin
               (*state).plottype = 10
               markdata_setdata,state
            end

            'x-rsig': begin
               (*state).plottype = 11
               markdata_setdata,state
            end

            'y-rsig': begin
               (*state).plottype = 12
               markdata_setdata,state
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Bad': begin
            z=where((*state).new eq 1,count)
            if count ne 0 then begin
               (*state).bad[z]=1
               (*state).new[z]=0
            endif
            fullplot=0
         end

      'Clear': begin
            (*state).new[*]=0
         end

      'Done': begin
            widget_control,event.top,/DESTROY
            return
         end

      'Replot': begin
         ; NOP
         end

      'Window': begin
         case event.type of
            0: begin   ; button press
                  (*state).x1 = event.x
                  (*state).y1 = event.y
                  (*state).lastx = event.x
                  (*state).lasty = event.y
                  (*state).drag = 1
                  device,set_graphics_function=6
               end
            1: begin   ; button release
                  (*state).x2 = event.x
                  (*state).y2 = event.y
                  (*state).drag = 0
                  device,set_graphics_function=3
                  xlim=minmax([(*state).x1,(*state).x2])
                  ylim=minmax([(*state).y1,(*state).y2])
                  lim=convert_coord(xlim,ylim,/device,/to_data)
                  z=where((*state).px ge min(lim[0,*]) and $
                          (*state).px le max(lim[0,*]) and $
                          (*state).py ge min(lim[1,*]) and $
                          (*state).py le max(lim[1,*]), count)
                  j=check_math()  ; suppresses math errors here.
                  if count ne 0 then begin
                     (*state).new[z]=1
                  endif
               end
            2: begin    ; motion
                  plotit=0
                  if (*state).drag then begin
                     if (*state).x1 ne (*state).lastx or $
                        (*state).y1 ne (*state).lasty then $
                        plots,/device,/noclip, $
                           [(*state).x1,(*state).x1, $
                            (*state).lastx,(*state).lastx,(*state).x1], $
                           [(*state).y1,(*state).lasty, $
                            (*state).lasty,(*state).y1,   (*state).y1], $
                           color=(*state).color[0]

                     plots,/device,/noclip, $
                        [(*state).x1,(*state).x1, $
                         event.x,event.x, (*state).x1], $
                        [(*state).y1,event.y, $
                         event.y,(*state).y1,(*state).y1], $
                        color=(*state).color[9]

                     (*state).lastx = event.x
                     (*state).lasty = event.y
                  endif
               end
            else: begin
                  message,'Unknown event',/info
               end
            endcase
         end

      'Good': begin
            z=where((*state).new eq 1,count)
            if count ne 0 then begin
               (*state).bad[z]=0
               (*state).new[z]=0
            endif
            fullplot=0
         end

      'Worst': begin
            markdata_setworst, state, 1
            fullplot=0
         end

      '10 Worst': begin
            markdata_setworst, state, 10
            fullplot=0
         end

      else: begin
            message,'Unknown widget event',/info
         end

   endcase

   ; Replot the data if not in the middle of a drag event
   if not (*state).drag and plotit then begin
      markdata_plot,state,fullplot
   endif

end

; ------------------------------------------------------------------------------
;       procedure markdata
; ------------------------------------------------------------------------------
;  This is the actual routine that creates the widget and registers it with the
;  Xmanager.
; ------------------------------------------------------------------------------
pro markdata, arg1, arg2, arg3, arg4, arg5, $
     GROUP = group, TITLE = title, XSIZE=xsize, YSIZE=ysize, $
     XTITLE=xtitle, YTITLE=ytitle, SCALING=scaling, $
     XFLIP=xflip, YFLIP=yflip, CONNECT=connect, PTITLE=ptitle, $
     PLOTTYPE=plottype

   common mwb_markdata,badvals

   if (!D.FLAGS AND 256) NE 256 then message, $
     'ERROR - Current graphics device ' + !D.NAME + ' does not support windows'

   if xregistered('markdata') then return

   if n_params() lt 2 or n_params() gt 5 then $
      message,'Usage: markdata,[x],[xerr],y,[yerr],bad'

   if n_params() eq 2 then begin
      if badpar(arg1,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (y) ',npts=n1) then return
      if badpar(arg2,[1,2,3],  1,CALLER='MARKDATA: (bad) ',npts=n2) then return
      if badpar(plottype,[0,1,2,3],0,CALLER='MARKDATA: (PLOTTYPE) ',default=0) then return
      if plottype ne 0 then begin
         print,'MARKDATA: warning, only plottype=0 is allowed'
         plottype=0
      endif
      npts=max([n1,n2])
   endif

   if n_params() eq 3 then begin
      if badpar(arg1,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (x) ',npts=n1) then return
      if badpar(arg2,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (y) ',npts=n2) then return
      if badpar(arg3,[1,2,3],  1,CALLER='MARKDATA: (bad) ',npts=n3) then return
      if badpar(plottype,[0,1,2,3],0,CALLER='MARKDATA: (PLOTTYPE) ',default=1) then return
      if plottype ne 1 then begin
         print,'MARKDATA: warning, only plottype=1 is allowed'
         plottype=1
      endif
      npts=max([n1,n2,n3])
   endif

   if n_params() eq 4 then begin
      if badpar(arg1,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (x) '  ,npts=n1) then return
      if badpar(arg2,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (y) '  ,npts=n2) then return
      if badpar(arg3,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (err) ',npts=n3) then return
      if badpar(arg4,[1,2,3],  1,CALLER='MARKDATA: (bad) ',npts=n4) then return
      if badpar(plottype,[0,1,2,3],0,CALLER='MARKDATA: (PLOTTYPE) ',default=2) then return
      if plottype le 0 or plottype eq 3 or plottype ge 6 then begin
         print,'MARKDATA: illegal plottype, revert to 2'
         plottype=2
      endif
      npts=max([n1,n2,n3,n4])
   endif

   if n_params() eq 5 then begin
      if badpar(arg1,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (x) '   ,npts=n1) then return
      if badpar(arg2,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (xerr) ',npts=n2) then return
      if badpar(arg3,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (y) '   ,npts=n3) then return
      if badpar(arg4,[2,3,4,5,12,13,14,15],1,CALLER='MARKDATA: (yerr) ',npts=n4) then return
      if badpar(arg5,[1,2,3],  1,CALLER='MARKDATA: (bad) ' ,npts=n5) then return
      if badpar(plottype,[0,1,2,3],0,CALLER='MARKDATA: (PLOTTYPE) ',default=3) then return
      if plottype eq 0 then begin
         print,'MARKDATA: illegal plottype, revert to 3'
         plottype=3
      endif
      rerr = sqrt(arg2^2+arg4^2)
      npts=max([n1,n2,n3,n4,n5])
   endif

   if badpar(title,[0,7],0,CALLER='MARKDATA: (TITLE) ', $
                           default='Data Editor') then return
   if badpar(xtitle,[0,7],0,CALLER='MARKDATA: (XTITLE) ', $
                           default='x') then return
   if badpar(ytitle,[0,7],0,CALLER='MARKDATA: (YTITLE) ', $
                           default='y') then return
   if badpar(ptitle,[0,7],0,CALLER='MARKDATA: (PTITLE) ', $
                           default='') then return
   if badpar(xsize,[0,2,3],0,CALLER='MARKDATA: (xsize) ', $
                           default=700) then return
   if badpar(ysize,[0,2,3],0,CALLER='MARKDATA: (ysize) ', $
                           default=450) then return
   if badpar(scaling,[0,1,2,3],0,CALLER='MARKDATA: (scaling) ', $
                           default=1) then return
   if badpar(xflip,[0,1,2,3],0,CALLER='MARKDATA: (xflip) ', $
                           default=0) then return
   if badpar(yflip,[0,1,2,3],0,CALLER='MARKDATA: (yflip) ', $
                           default=0) then return
   if badpar(connect,[0,1,2,3],0,CALLER='MARKDATA: (connect) ', $
                           default=0) then return

   if !d.n_colors le 256 then begin
      ncolors=!d.n_colors
      eightbit = 1
   endif else begin
      ncolors=256
      eightbit = 0
   endelse

   ; Save the current plotting window
   windo=!d.window
   tvlct,old_r,old_g,old_b,/get
   p_multi=!p.multi

   if connect eq 0 then connect = 1 else connect = -1

   ; Setup plotting colors
   if eightbit then begin
      r=[0,255,  0,  0,255,255,  0,255,148,255]
      g=[0,  0,255,131,255,  0,255,161,  0,255]
      b=[0,  0,  0,255,  0,255,255,  0,255,255]
      ; 0 - black
      ; 1 - red
      ; 2 - green
      ; 3 - blue
      ; 4 - yellow
      ; 5 - magenta
      ; 6 - cyan
      ; 7 - orange
      ; 8 - violet
      ; 9 - white
      color=indgen(10)
      tvlct,r,g,b
   endif else begin
      color = [ $
         '000000'xl, $ ; black
         '0000ff'xl, $ ; red
         '00ff00'xl, $ ; green
         'ff8300'xl, $ ; blue
         '00ffff'xl, $ ; yellow
         'ff00ff'xl, $ ; magenta
         'ffff00'xl, $ ; cyan
         '00b3ff'xl, $ ; orange
         'ff0094'xl, $ ; violet
         'ffffff'xl  $ ; white
         ]
   endelse
   !p.multi=0

   mainbase  = widget_base(TITLE=title, /COLUMN, $
                           uvalue=0, MBAR=bar, /tlb_size_events)

   menulist = ['1\Display',$
               '0\Flip X',$
               '0\Flip Y',$
               '0\Scale All', $
               '0\Scale Good', $
               '2\Toggle Lines', $
               '1\Tools',$
               '0\All Good', $
               '2\All Bad']

   if n_params() eq 4 then begin
      menulist = [ menulist, $
                   '1\Plot', $
                   '0\x-y', $
                   '0\x-y/ysig', $
                   '0\x-ysig', $
                   '2\y-ysig']
   endif else if n_params() eq 5 then begin
      menulist = [ menulist, $
                   '1\Plot', $
                   '0\x-y', $
                   '0\x-y/ysig', $
                   '0\x/xsig-y/ysig', $
                   '0\x-ysig', $
                   '0\y-ysig', $
                   '0\x-xsig', $
                   '0\y-xsig', $
                   '0\xsig-ysig', $
                   '0\y-x/xsig', $
                   '0\x-y/rsig', $
                   '0\x-rsig', $
                   '2\y-rsig']
   endif

   menu = CW_PdMenu(bar, /RETURN_NAME, menulist, uvalue='THE_MENU', /MBAR)

   drawid    = widget_draw(mainbase,XSIZE=xsize,YSIZE=ysize, $
                              /BUTTON_EVENTS,/MOTION_EVENTS, uvalue='Window')
   colbase   = widget_base(mainbase,/ROW)
   doneid    = widget_button(colbase,VALUE=' Done ', uvalue='Done')
   replot    = widget_button(colbase,value=' Replot ', uvalue='Replot')
   badid     = widget_button(colbase,VALUE=' Bad ', uvalue='Bad')
   goodid    = widget_button(colbase,VALUE=' Good ', uvalue='Good')
   worstid   = widget_button(colbase,VALUE=' Worst ', uvalue='Worst')
   worst10id = widget_button(colbase,VALUE=' 10 Worst ', uvalue='10 Worst')
   clearid   = widget_button(colbase,VALUE=' Clear ', uvalue='Clear')

   ; Define the state structure
   if n_params() eq 2 then begin

      state = ptr_new({ $
         bad:       arg2, $
         color:     color, $
         connect:   connect, $
         drag:      0, $
         eightbit:  eightbit, $
         lastx:     0, $
         lasty:     0, $
         ncolors:   ncolors, $
         new:       intarr(npts), $
         ptitle:    ptitle, $
         plottype:  plottype, $
         px:        findgen(npts), $
         py:        findgen(npts), $
         pxtitle:   xtitle, $
         pytitle:   ytitle, $
         scaling:   0, $
         x:         indgen(npts), $
         x1:        0, $
         x2:        0, $
         xflip:     xflip, $
         xtitle:    xtitle, $
         y:         arg1, $
         y1:        0, $
         y2:        0, $
         yflip:     yflip, $
         ytitle:    ytitle, $

         mainbase:  mainbase, $
         drawid:    drawid $
         })

   endif else if n_params() eq 3 then begin
      state = ptr_new({ $
         color:    color, $
         ncolors:  ncolors, $           ; Number of colors for display
         eightbit: eightbit, $          ; Flag, if true, 8-bit display
         scaling:  scaling,  $
         xflip:    xflip,  $
         yflip:    yflip,  $
         connect:  connect,  $
         xtitle:   xtitle,  $
         ytitle:   ytitle,  $
         pxtitle:  xtitle, $
         pytitle:  ytitle, $
         plottype: plottype, $
         px:       findgen(npts), $
         py:       findgen(npts), $
         ptitle:   ptitle, $
         x1:       0,  $
         x2:       0,  $
         y1:       0,  $
         y2:       0,  $
         lastx:    0,  $
         lasty:    0,  $
         drag:     0, $
         x:        arg1,  $
         y:        arg2,  $
         new:      intarr(npts), $
         bad:      arg3, $

         mainbase: mainbase, $
         drawid:   drawid $
         })

   endif else if n_params() eq 4 then begin
      state = ptr_new({ $
         color:    color, $
         ncolors:  ncolors, $            ; Number of colors for display
         eightbit: eightbit, $          ; Flag, if true, 8-bit display
         scaling:  scaling, $
         xflip:    xflip, $
         yflip:    yflip, $
         connect:  connect, $
         xtitle:   xtitle, $
         ytitle:   ytitle, $
         pxtitle:  xtitle, $
         pytitle:  ytitle, $
         plottype: plottype, $
         ptitle:   ptitle, $
         px:       findgen(npts), $
         py:       findgen(npts), $
         x1:       0, $
         x2:       0, $
         y1:       0, $
         y2:       0, $
         lastx:    0, $
         lasty:    0, $
         drag:     0, $
         x:        arg1, $
         y:        arg2, $
         yerr:     arg3, $
         new:      intarr(npts), $
         bad:      arg4, $
         mainbase: mainbase, $
         drawid:   drawid $
         })

   endif else begin
      state = ptr_new({ $
         color:    color, $
         ncolors:  ncolors, $            ; Number of colors for display
         eightbit: eightbit, $          ; Flag, if true, 8-bit display
         scaling:  scaling, $
         xflip:    xflip, $
         yflip:    yflip, $
         connect:  connect, $
         xtitle:   xtitle, $
         ytitle:   ytitle, $
         pxtitle:  xtitle, $
         pytitle:  ytitle, $
         plottype: plottype, $
         ptitle:   ptitle, $
         px:       findgen(npts), $
         py:       findgen(npts), $
         x1:       0, $
         x2:       0, $
         y1:       0, $
         y2:       0, $
         lastx:    0, $
         lasty:    0, $
         drag:     0, $
         x:        arg1, $
         xerr:     arg2, $
         y:        arg3, $
         yerr:     arg4, $
         rerr:     rerr, $
         new:      intarr(npts), $
         bad:      arg5, $
         mainbase: mainbase, $
         drawid:   drawid $
         })
   endelse

   ; Protection against NaN's
   z=where(finite((*state).y) eq 0 and (*state).bad eq 0,count)
   if count ne 0 then begin
      (*state).bad[z]=1
      print,'MARKDATA: warning, ',strcompress(count), $
         ' NaN values automatically marked bad.'
   endif

   ;Stash the state structure pointer.
   widget_control,mainbase,set_uvalue=state

   ;Realize the widget
   widget_control,mainbase,/REALIZE

   ;Set the plotting window
   widget_control,(*state).drawid,get_value=dwin
   wset,dwin

   ; setup the data to be plotted
   markdata_setdata,state

   ; plot the data
   markdata_plot,state,1

   xmanager,'markdata',mainbase,EVENT_HANDLER='markdata_eve', $
      GROUP_LEADER=group,/MODAL,CLEANUP='markdata_cleanup'

   if n_params() eq 2 then arg2=badvals
   if n_params() eq 3 then arg3=badvals
   if n_params() eq 4 then arg4=badvals
   if n_params() eq 5 then arg5=badvals

   wset,windo
   if eightbit then tvlct,old_r,old_g,old_b
   !p.multi=p_multi
   return

end

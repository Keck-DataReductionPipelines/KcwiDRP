;+
; NAME:
;  logmanip
; PURPOSE:
;  Widget for editing and manipulating photometry log files.
; DESCRIPTION:
; CATEGORY:
;  Widgets
; CALLING SEQUENCE:
;  logmanip,logfile
; INPUTS:
;  logfile - String, file name of log file to work on.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  GROUP:  The widget ID of the widget that calls LOGMANIP.  When this
;          ID is specified, a death of the caller results in the death of
;          the LOGMANIP widget application.
;
;  TITLE:  A scalar string to be used for the window title.  If it is
;          not specified, the default title is "Photometry Log File Editor"
;  JUSTCLEAN: Flag, if set supresses the interactive widget operation.  All
;                that is done is to scan for duplications and remove them.
;                Output is also sorted.
;  DELETEFILE: If supplied, will mark the first batch of observations in this
;                file for deletion.  (Remember, first column in log file is
;                the file name where the observation came from.)
;
; OUTPUTS:
;  modified log file is written out to logfile
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  This function initiates the XMANAGER if it is not already running.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/2 - Written by Marc W. Buie
;  97/04/06, MWB, added DELETEFILE keyword
;  00/08/30, MWB, extracted wrphalt portion of program.
;  2003/10/01, MWB, converted my Delfile call to system file_delete routine
;  2006/5/22,  Peter L. Collins, Lowell Observatory
;                added phot parameter for CCD readout noise (rdnoise)
;-

; ------------------------------------------------------------------------------
;       procedure logmanip_scrub
; ------------------------------------------------------------------------------
;  This checks for duplicated measurements and sorts the data, returning the
;    index for what to keep and in what order.
; ------------------------------------------------------------------------------
pro logmanip_scrub,fn,obj,jd,serial,idx
   pad='                 '
   tmp=strarr(n_elements(fn))
   FOR i=0,n_elements(fn)-1 DO BEGIN
      tmp[i] = string(fn[i]+pad,jd[i],obj[i]+pad,serial[i], $
                      format='(a12,1x,f13.5,1x,a12,1x,i4.4)')
   ENDFOR

   ; sort by filename, jd, object, serial number
   idx = uniq(tmp,sort(tmp))

   ; if sorted list is shorter, then must pick last of any duplicates
   if n_elements(idx) ne n_elements(jd) then begin
      for i=0,n_elements(idx)-1 do begin
         z=where(tmp eq tmp[idx[i]],count)
         idx[i]=z[count-1]
      endfor
   endif

end

; ------------------------------------------------------------------------------
;       procedure logmanip_update
; ------------------------------------------------------------------------------
;  This updates the text screen with information on the logfile
; ------------------------------------------------------------------------------

pro logmanip_update,state
   info=strarr(20)
   info[0]=string("Photometry log file:",state.logfile,format='(a,1x,a)')

   objlist = state.obj[uniq(state.obj,sort(state.obj))]

   info[2]=strcompress(string(n_elements(objlist),n_elements(state.jd), $
              format='(i3," objects with ",i5," total entries in file.")'))

   pad='        '
   limit=min([100,n_elements(objlist)]) ; show at most 100 objects
   line=3
   for i=0,limit-1 do begin
      if i mod 10 eq 0 then line=line+1
      info[line]=info[line]+strmid(objlist[i]+pad,0,9)+' '
   endfor

   line=line+2
   if max(state.kill) eq 0 then $
      info[line] = 'Nothing marked for deletion.' $
   else $
      info[line] = string(long(total(state.kill)))+' observations marked for deletion.'

   widget_control,state.infoid,SET_VALUE=info
end

; ------------------------------------------------------------------------------
;       procedure logmanip_finish
; ------------------------------------------------------------------------------
;  This is used to save the data just before exiting.
; ------------------------------------------------------------------------------

pro logmanip_finish,state
   if state.dirty or total(state.kill) gt 0 then begin
      z=where(state.kill eq 0,count)
      if count eq 0 then begin
         print,'Log file ',state.logfile,' is now empty, deleting.'
         file_delete,state.logfile,/quiet,/noexpand_path
      endif else begin
         print,'Updating ',state.logfile
         wrphalt,state.logfile, $
               state.filename[z], state.obj[z], state.fil[z], state.jd[z], $
               state.exptime[z], state.gain[z], state.rad[z], $
               state.sky1[z], state.sky2[z], state.serial[z], $
               state.xpos[z], state.ypos[z], state.fwhm[z], $
               state.maxcnt[z], state.sky[z], state.skyerr[z], $
               state.mag[z], state.err[z], state.bad[z], state.rdnoise[z]
      endelse
   endif
end

; ------------------------------------------------------------------------------
;       procedure logmanip_ev
; ------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
; ------------------------------------------------------------------------------
pro logmanip_ev, event

; Get the state
stash=widget_info(event.handler,/CHILD)
widget_control,stash,GET_UVALUE=state,/NO_COPY

case event.id of

   state.doneid: begin
         if total(state.kill) gt 0 then begin
            nkill=fix(total(state.kill))
            if nkill eq n_elements(state.jd) then $
               info='ALL of the entries' $
            else begin
               info=strtrim(string(nkill),2)
               if info eq '1' then $
                  info=info+' entry' $
               else $
                  info=info+' of the entries'
            endelse
            ok = qannounc( $
               ['You have marked '+info+' for deletion from', $
                 state.logfile+'.  If you choose delete, these entries', $
                 'will be DELETED from the file and cannot be retrieved.'], $
               group=event.top,TRUELABEL='Delete data', $
               FALSELABEL='  Abort  ')
         endif else ok=1
         if ok then begin
            logmanip_finish,state
            widget_control,stash,SET_UVALUE=state,/NO_COPY
            widget_control,event.top,/DESTROY
            return
         endif
      end

   state.cancid: begin
         widget_control,stash,SET_UVALUE=state,/NO_COPY
         widget_control,event.top,/DESTROY
         return
      end

   state.allid: begin
         state.kill[*]=0
      end

   state.killid: begin
         state.kill[*]=1
      end

   state.badid: begin
         objlist = state.obj[uniq(state.obj,sort(state.obj))]
         killobj = picker(objlist)
         z=where(killobj eq state.obj,count)
         if count ne 0 then state.kill[z]=1
      end

   state.goodid: begin
         objlist = state.obj[uniq(state.obj,sort(state.obj))]
         keepobj = picker(objlist)
         z=where(keepobj eq state.obj,count)
         if count ne 0 then state.kill[z]=0
      end

   state.pntid: begin
         tk=state.kill
         markdata,indgen(n_elements(state.jd)),state.mag,state.err, $
            tk, /yflip, xsize=800, $
            xtitle='Observation Number', $
            ytitle='Instrumental magnitude', $
            ptitle='Photometry from '+state.logfile, group=event.top
         state.kill=tk
      end

   else: begin
         message,'Unknown widget event',/info
      end

endcase

logmanip_update,state

; Restore the state
widget_control,stash,SET_UVALUE=state,/NO_COPY

end

; ------------------------------------------------------------------------------
;       procedure logmanip
; ------------------------------------------------------------------------------
;  This is the actual routine that creates the widget and registers it with the
;  Xmanager.
; ------------------------------------------------------------------------------
pro logmanip, logfile, $
     GROUP = group, TITLE = title, JUSTCLEAN=justclean, DELETEFILE=deletefile

if badpar(deletefile,[0,7],0,CALLER='LOGMANIP: (DELETEFILE) ', $
                           default='[[no file delete]]') then return

if (!D.FLAGS AND 256) NE 256 then message, $
  'ERROR - Current graphics device ' + !D.NAME + ' does not support windows'

if xregistered('logmanip') then return

if n_params() ne 1 then $
   message,'Usage: logmanip,logfile'

if badpar(logfile,7,0,CALLER='LOGMANIP: (logfile) ') then return
if badpar(title,[0,7],0,CALLER='MARKDATA: (TITLE) ',default='Data Editor') then return

if not exists(logfile) then message, $
   'ERROR - Logfile '+logfile+' does not exist'

photprmt,logfile
rdphalt,logfile,filename,obj,fil,jd,exptime,gain, $
   rad,sky1,sky2,serial,xpos,ypos,fwhm,maxcnt,sky,skyerr,mag,err,bad, $
   rdnoise

kill = intarr(n_elements(jd))

IF deletefile ne '[[no file delete]]' THEN BEGIN
   zkill = where(filename eq deletefile,countkill)
   IF countkill eq 1 THEN BEGIN
      kill[zkill] = 1
   ENDIF ELSE IF countkill gt 1 THEN BEGIN
      pos=1
      REPEAT BEGIN
         IF zkill[pos-1]+1 eq zkill[pos] THEN BEGIN
            kill[zkill[pos]] = 1
            pos=pos+1
         ENDIF ELSE BEGIN
            pos=countkill
         ENDELSE
      ENDREP UNTIL pos ge countkill
   ENDIF
ENDIF

logmanip_scrub,filename,obj,jd,serial,idx

kill=kill[idx]

state = { $
   infoid: 0L, doneid: 0L, cancid: 0L, badid: 0L, goodid: 0L, allid: 0L, $
   killid: 0L, pntid: 0L, logfile:  logfile, $
   filename: filename[idx], obj: obj[idx], fil: fil[idx], $
   jd: jd[idx], exptime: exptime[idx], gain: gain[idx], $
   rad: rad[idx], sky1: sky1[idx], sky2: sky2[idx], $
   serial: serial[idx], xpos: xpos[idx], ypos: ypos[idx], fwhm: fwhm[idx], $
   maxcnt: maxcnt[idx],rdnoise: rdnoise[idx],sky: sky[idx],skyerr: skyerr[idx],$
   mag: mag[idx], err: err[idx], bad: bad[idx], $
   dirty:0, kill: kill $
   }

if n_elements(kill) ne n_elements(jd) then state.dirty=1 $
else begin
   z=where(idx ne indgen(n_elements(jd)),count)
   if count ne 0 then state.dirty=1
endelse

if keyword_set(justclean) then begin
   logmanip_finish,state
endif else begin

   base         = widget_base(TITLE=title,/COLUMN)
   state.infoid = widget_text(base,XSIZE=80,YSIZE=20)
   colbase      = widget_base(base,/ROW)
   state.doneid = widget_button(colbase,VALUE=' Exit ')
   state.cancid = widget_button(colbase,VALUE=' Cancel ')
   state.badid  = widget_button(colbase,VALUE=' Remove Object ')
   state.killid = widget_button(colbase,VALUE=' Remove All')
   state.goodid = widget_button(colbase,VALUE=' Restore Object')
   state.allid  = widget_button(colbase,VALUE=' Restore All ')
   state.pntid  = widget_button(colbase,VALUE=' Edit By Point ')

   logmanip_update,state

   ;Set the state.
   stash = widget_info(base,/CHILD)
   widget_control,stash,SET_UVALUE=state,/NO_COPY

   ;Realize the widget
   widget_control,base,/REALIZE

   xmanager,'logmanip',base,EVENT_HANDLER='logmanip_ev', $
      GROUP_LEADER=group,/MODAL

endelse

end

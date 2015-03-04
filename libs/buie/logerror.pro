;+
; NAME:
;  logerror
; PURPOSE:
;  Simplified error logging program.
; DESCRIPTION:
;
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  logerror,file,msg
; INPUTS:
;  file  - string containing file name of error log file
;  msg   - string (scalar or array) to be written to log file
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  CLEARLOG - Flag, if set, causes log file to be cleared before writing.
;               The default is that all messages are appended to the file.
;               If the file doesn't already exist this flag has no effect.
;  QUIET    - 0 - default, message printed to screen also, with CALLER tag.
;             1 - nothing printed to screen
;  CALLER   - Optional id string that identifies the calling program.
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
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
;  Written by Marc W. Buie, Lowell Observatory, 2000/09/12
;  2001/04/18, MWB, changed systime call.
;-
pro logerror,file,msg,CLEARLOG=clearlog,QUIET=quiet,CALLER=caller

   if badpar(file,7,0,caller='LOGERROR: (file) ') then return
   if badpar(msg,7,[0,1],caller='LOGERROR: (file) ',npts=nlines) then return
   if badpar(clearlog,[0,1,2,3],0,caller='LOGERROR: (CLEARLOG) ',default=0) then return
   if badpar(quiet,[0,1,2,3],0,caller='LOGERROR: (QUIET) ',default=0) then return
   if badpar(caller,[0,7],0,caller='LOGERROR: (CALLER) ',default='') then return

   if clearlog then $
      openw,lun,file,/get_lun,width=132 $
   else $
      openw,lun,file,/get_lun,width=132,/append

   jdstr,systime(/julian,/utc),0,time

   for i=0,nlines-1 do begin
      str = msg[i]
      if caller ne '' then str = caller + ' ' + msg[i]
      if not quiet then print,str
      printf,lun,time,' ',str
   endfor

   free_lun,lun

end

;+
; NAME:
;     exists
; PURPOSE: (one line)
;     Check for file (or directory) existence.
; DESCRIPTION:
; CATEGORY:
;     File I/O
; CALLING SEQUENCE:
;     flag = exists(file)
; INPUTS:
;     file - string containing file name to look for.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;     Return value is 1 (true) if file exists.  0 if it doesn't.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;    93/03/29 - Written by Marc W. Buie, Lowell Observatory
;    96/10/17, MWB, modified to use OPENR for Unix
;    97/02/16, MWB, fixed DOS bug for dirs with trailing \
;    99/06/11, MWB, Dave Osip, added clause for Macintosh computers.
;    99/11/14, MWB, restructured code
;    2000/10/9, Roger J. Dejus (ANL), changed code for Unix to accomodate
;                 changed system response in IDL v5.3 and later.
;  2004/9/21, MWB, removed obsolete call to Findfile
;-
function exists,file

   if badpar(file,7,0,caller='EXISTS: (file) ') then return,0

   case !version.os_family OF

      'unix': begin
         openr,lun,file,error=error,/get_lun
         if error eq 0 then begin
            free_lun,lun
            return,1
         endif else begin
            if !error_state.sys_msg eq '' then return,1 else return,0
         endelse
      end

      'Windows': begin
         if strmid(file,strlen(file)-1,1) eq '\' then $
            ans = file_search(strmid(file,0,strlen(file)-1),count=count) $
         else $
            ans = file_search(file,count=count)
         if count ge 1 then return,1 else return,0
      end

      'MacOS': begin
         if strmid(file,strlen(file)-1,1) eq ':' then $
            ans = file_search(strmid(file,0,strlen(file)-1),count=count) $
         else $
            ans = file_search(file,count=count)
         if count ge 1 then return,1 else return,0
      end

      'vms': begin
         message,!version.os_family+' is currently unsupported.'
      end

      else: begin
         message,!version.os_family+' is an unrecognized OS type.'
      end

   endcase

end

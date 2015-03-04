;+
; NAME:
;  hardcopy
; PURPOSE:
;  Close printer or ps graphics device and spool output.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  hardcopy,file
; INPUTS:
;  file - File to print, default is idl.ps (relevant for ps device only)
; OPTIONAL INPUT PARAMETERS:
;  QUEUE   - String, name of printer (Unix only).
;  SILENT  - Flag, if set suppresses all diagnostic output.
;  DELFILE - Flag, if set and device is ps, delete file after printing.
; KEYWORD INPUT PARAMETERS:
;
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
;  Written by Marc W. Buie, Lowell Observatory, 1999 Nov 22.  MacOS portion
;    written by David Osip, MIT.
;  2003/10/01, MWB, converted my Delfile call to system file_delete routine
;-
pro hardcopy,QUEUE=queue,SILENT=silent,DELFILE=del_file

   if badpar(queue,[0,7],0,caller='HARDCOPY (QUEUE) ',default='') then return
   if badpar(silent,[0,1,2,3],0,caller='HARDCOPY (SILENT) ',default=0) then return
   if badpar(del_file,[0,1,2,3],0,caller='HARDCOPY (DELFILE) ',default=0) then return

   if !d.name eq 'PS' then begin
      ans=fstat(!d.unit)
      if not ans.open or ans.unit eq 0 then begin
         print,'HARDCOPY: No current plot pending, nothing to print.'
         return
      endif
      file = ans.name
      if not silent then print,'PS file printing to ',file
   endif

   case !version.os_family OF

      'unix': begin
         if !d.name ne 'PS' then begin
            print,'HARDCOPY: print operation not allowed for device ',!d.name
            return
         endif
         device,/close
         if queue ne '' then begin
            cmd = 'lp -d '+queue+' -o noduplex -o nobanner '+file
            if not silent then print,'Printing file '+file+' to print queue '+queue
         endif else begin
            cmd = 'lp -o nobanner '+file
            if not silent then print,'Printing file '+file+' to default print queue '
         endelse
         spawn,cmd
      end

      'Windows': begin
         device,/close
         if !d.name eq 'PS' then begin
            if queue ne '' then begin
               cmd = 'copy idl.ps '+queue
            endif else begin
               cmd = 'copy idl.ps lpt1:'
            endelse
            spawn,cmd
         endif
      end

      'MacOS': begin
         device,/close
         if !d.name eq 'PS' then begin
            scriptpr = [ $
               'tell current application to copy variable "filename" to file2print',$
               'set temp to characters 1 thru -2 of file2print',$
               'set temp to temp as string',$
               'tell application "Finder"',$
                    'activate',$
                    'select file temp',$
                    'set the creator type of selection to "R*ch"',$
                    'print selection',$
               'end tell']           

            do_apple_script, scriptpr
         endif
      end

      'vms': begin
         message,!version.os_family+' is currently unsupported.'
      end

      else: begin
         message,!version.os_family+' is an unrecognized OS type.'
      end

   endcase

   if !d.name eq 'PS' and del_file then begin
      file_delete,file,/quiet,/noexpand_path
   endif

end

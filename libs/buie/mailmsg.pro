;+
; NAME:
;  mailmsg
; PURPOSE:   (one line only)
;  Send an email message
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  mailmsg,toaddr,subject,text
; INPUTS:
;  toaddr - string, email address to send to.
;  subject - string, subject line for message
;  text - string (array or scalar), this is the body of the message to
;           be sent.  Each entry in the string array is sent as a separate
;           line.  No line processing is done but the line is checked to
;           make sure it won't trip special processing by the mail program.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  CCADDR - string (array or scalar) of additional addresses to send to.
;             default = no Cc: recipients
;  MAILCLIENT - name of external mail program, default='mail'
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  This program will create a temporary file named 'idl_tmpmail.txt' in the
;    current directory.  It will be deleted when done but the program must
;    have write permission to succeed.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2004/11/01
;-
pro mailmsg,toaddr,subject,text,CCADDR=in_ccaddr,MAILCLIENT=mailclient, $
   VERBOSE=verbose

   self="MAILMSG: "
   if badpar(toaddr,7,0,caller=self+'(toaddr) ') then return
   if badpar(subject,7,0,caller=self+'(subject) ') then return
   if badpar(text,7,[0,1],caller=self+'(text) ') then return
   if badpar(in_ccaddr,[0,7],[0,1],caller=self+'(CCADDR) ', $
                       default='') then return
   if badpar(mailclient,[0,7],0,caller=self+'(MAILCLIENT) ', $
                       default='/usr/bin/mail') then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ', $
                       default=0) then return

   cmd = mailclient
   if verbose then cmd = [cmd,'-v']
   cmd = [cmd,'-s',quote(subject)]

   if n_elements(in_ccaddr) gt 1 then $
      ccaddr = strjoin(strcompress(in_ccaddr,/remove_all),',') $
   else $
      ccaddr = strcompress(in_ccaddr,/remove_all)

   if ccaddr ne '' then $
      cmd = [cmd,'-c',ccaddr]

   cmd = [cmd,toaddr]

   cmd=strjoin(cmd,' ')

   openw,lun,'idl_tmpmail.txt',/get_lun
   for i=0,n_elements(text)-1 do $
      printf,lun,text[i]
   free_lun,lun

   cmd = cmd + ' < idl_tmpmail.txt'
   spawn,cmd,result
   file_delete,'idl_tmpmail.txt'

end

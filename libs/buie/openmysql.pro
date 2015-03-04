;+
; NAME:
;  openmysql
; PURPOSE:
;  Open a mySQL database for operations via a pipe.
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  openmysql,lun,dbname
; INPUTS:
;  dbname - Name of database to open at start.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  HOST - host name of the server to connect to for queries.  The default
;           is to use the host name specified in your .my.conf configuration
;           file.
;  USER - user name to be used for the connection.  This is the user name
;           as understood by mysql and has no relationship to the user name
;           of the calling process.  The default is to use the user name
;           specified in your .my.conf configuration file.
; OUTPUTS:
;  lun - the logical unit of the pipe (use free_lun to close).
;  error  - return value indicating if the open call succeeded.  If error is
;            zero, the open was good and the lun is ready for use.  If error
;            is not zero, the lun will not point to an open file and the
;            !error_state system variable will contain more information if
;            you want it.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Assumes that your .my.cnf file points to the correct mysql server and that
;    it includes the login information.  Also, the command 'mysql' must appear
;    in your default path.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/01/09
;  2006/12/08, MWB, added error output argument, now can indicate if open failed
;  2007/09/06, MWB, added HOST/USER keywords
;  2010/03/05, MWB, added the EXTRACONFIG keyword
;-
pro openmysql,lun,dbname,error,HOST=host,USER=user,EXTRACONFIG=extraconfig

   self = 'OPENMYSQL: '
   if badpar(lun,[0,2,3],0,caller=self+'(lun) ') then return
   if badpar(dbname,7,0,caller=self+'(dbname) ') then return
   if badpar(host,[0,7],0,caller=self+'(HOST) ',default='') then return
   if badpar(user,[0,7],0,caller=self+'(USER) ',default='') then return
   if badpar(extraconfig,[0,7],0,caller=self+'(EXTRACONFIG) ', $
                                 default='') then return

   cmd = 'mysql '
   if host ne '' then cmd += '-h '+host+' '
   if user ne '' then cmd += '-u '+user+' '
   if extraconfig ne '' then cmd += '--defaults-extra-file='+extraconfig+' '
   cmd += '-B -q -n '
   cmd += dbname
   spawn,cmd,unit=lun

   catch,error

   if error ne 0 then begin
      print,self,!error_state.msg
      print,self,'Unable to open database ',dbname,', closing pipe and quitting.'
      free_lun,lun
      catch,/cancel
      return
   endif

   ; This command is needed to trigger the catch and facilitate closing
   ;   of the lun.  We don't need these results, just the activity on the pipe.
   mysqlcmd,lun,'show tables;',result,nlines

end

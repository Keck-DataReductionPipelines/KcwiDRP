;+
; NAME:
;  mysqlcmd
; PURPOSE:
;  Send a command to open database and collect the answer.
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  mysqlcmd,lun,query,answer,nlines
; INPUTS:
;  lun - the logical unit of the pipe (opened by openmysql).
;  query - String (or array of strings) to send to pipe.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  answer - Returned information from query (possible string array).
;  nlines - Number of lines of returned information (may be zero).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/01/09
;  2010/04/10, MWB, upgraded internal loop variable to long
;-
pro mysqlcmd,lun,query,answer,nlines,DEBUG=debug

   if badpar(lun,[2,3],0,caller='mysqlcmd (lun) ') then return
   if badpar(query,7,[0,1],caller='mysqlcmd (query) ',default='') then return
   if badpar(debug,[0,1,2,3],0,caller='mysqlcmd (DEBUG) ',default=0) then return

   ; Send the query
   for i=0,n_elements(query)-1 do begin
      printf,lun,query[i]
      if debug then print,query[i]
   endfor

   ; Send a special null query that will return two lines of EOT
   printf,lun,"select 'EOT';"
;   if debug then print,"select 'EOT';"
   flush,lun

   ; Read from pipe until two lines of EOT are seen.
   line=''
   nlines=0L
   done=0
   repeat begin
      readf,lun,line,format='(a)'
      if debug then print,line
      if line eq 'EOT' then begin
         readf,lun,line,format='(a)'
         if line eq 'EOT' then begin
            break
         endif else begin
            if nlines eq 0 then $
               answer = ['EOT',line] $
            else $
               answer = [answer,'EOT',line]
            nlines=nlines+2
         endelse
      endif else begin
         if nlines eq 0 then $
            answer = line $
         else $
            answer = [answer,line]
         nlines=nlines+1
      endelse
   endrep until done

   if debug and nlines gt 0 then begin
      print,'Up to the first 5 lines of the query response'
      for i=0,5<(nlines-1) do print,answer[i]
   endif

   if debug then print,strn(nlines),' lines of information returned from query.'

end

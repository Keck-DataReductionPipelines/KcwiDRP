;+
; NAME:
;   mysqlquery
; PURPOSE:
;   Submit MySQL query and get response as vectors of data (like readcol).
; DESCRIPTION:
;   Submit a simple SQL query to MySQL server, using the connection
;   previously opened with openmysql.  Retrieve the result into
;   a row of variables, much as readcol does.  Return dimensionality
;   of result(s).
; CATEGORY:
;   Database
;
; CALLING SEQUENCE:
;   mysqlquery,lun,query,[varables...],[format='(a,f,...)']
;
; INPUTS:
;    lun    - The logical unit of the pipe (opened by openmysql).
;    query  - String (or array of strings) to send to pipe.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;    format  - Specify format of output variables (default is ascii).
;    cmd     - Flag indicating this is a command, not a query so
;                don't bother processing the output (but do report the
;                number of rows affected/warnings? - not implimented).
;    verbose - Flag turns on debugging output to standard out.
;
; OUTPUTS:
;    variables - A list of variables to recieve columns of output.
;                   Default type is ascii, but use the format keyword to
;                   specify other data types.
;
; KEYWORD OUTPUT PARAMETERS:
;    HEADS   - String to receive array of column heads.
;    NGOOD   - Number of valid lines found that were read.
;
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   Requires an open connection to MySQL server (established by
;   use of openmysql) as well as valid permissions for whatever
;   query or command is to be executed.
;
; PROCEDURE:
; MODIFICATION HISTORY:
;   2002-02-14 Will Grundy, adapted from earlier version called mysql.pro
;   2002-02-27 WG changed behavior so 'NULL' becomes NaN instead of
;                  making the line be ignored when it occurs in a numerical
;                  field.
;   2002-03-25 WG changed to split on tab instead of white space, so that
;       strings with internal spaces (but not tabs) can be retrieved.
;   2003/01/10, MWB, fixed multi-line query error.  Only one query per
;                       call is allowed.
;   2006/11/09, PLC, changed strsplit call to use /preserve_null flag
;                     this means a field can now return an empty string.
;                     The behavior for non-'a' type fields is not defined
;                     in this case.
;   2007/07/15, MWB, added NGOOD output keyword
;   2010/03/11, MWB, change behavior so that if there is only one valid
;                     line the result is returned as a scalar instead of
;                     a one-element vector.
;
; BUGS/WISH LIST:
;   Ought to verify connection to MySQL server.
;   Does nothing helpful with SQL command results.
;   Does nothing helpful to identify/report bad SQL syntax.
;-
pro mysqlquery,lun,query,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12, $
       v13,v14,v15,v16,v17,v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28, $
       FORMAT=fmt,HEADS=heads,CMD=cmd,VERBOSE=verbose,NGOOD=ngood

   ; Zero the output variables, so there's no risk of accidentally
   ; re-processing a previous result if something went wrong.
   heads = ''
   ncol = n_params() - 2
   if ncol gt 0 then begin
      vv = 'v' + strtrim( indgen(ncol)+1, 2)
      for k=0,ncol-1 do begin
         st = vv[k] + ' = ""'
         tst = execute(st)
      endfor
   endif

   ; lun and query string are manditory, as are at least one output
   ; variable (unless the /CMD flag is set)
   if (keyword_set(cmd) and n_params() lt 2) or $
      (not keyword_set(cmd) and n_params() lt 3) then begin
      print,'Usage: n = mysqlquery(lun,query,v1,[v2,v3,v4,...])'
      return
   endif

;   query = gettok(query,';')  ; keep only up to 1st ";"
;   mysqlcmd,lun,query+';',result,nlines,debug=verbose
   mysqlcmd,lun,query,result,nlines,debug=verbose

   if not keyword_set(cmd) then begin
      ; First digest the column headings (split on tabs)
      heads = strsplit(result[0],'	',/extract,/PRESERVE_NULL)
      ; Next process everything else into a series of variables, using
      ; code shamelessly lifted from the astro library's readcol.pro.
      ; (thank you kindly, Landsman et al.!)
      nskip = 0
      if N_elements(fmt) gt 0 then begin        ;FORMAT string supplied?
         ; Grind format string into usable form
         zparcheck, 'MYSQL', fmt, 2, 7, 0, 'FORMAT string'
         frmt = strupcase(strcompress(fmt,/REMOVE))
         remchar, frmt, '('
         remchar, frmt, ')'
         pos = strpos(frmt, 'X', 0)
         while pos ne -1 DO begin
            pos = strpos( frmt, 'X', pos+1)
            nskip = nskip + 1
         endwhile
      endif else begin
         ; Default is ascii format (least likely to fail)
         frmt = 'A'
         if ncol gt 1 then for i = 1,ncol-1 do frmt = frmt + ',A'
      endelse
      nfmt = ncol + nskip
      idltype = intarr(nfmt)
      ; Create output arrays according to specified formats
      k = 0L
      hex = bytarr(nfmt)
      for i = 0L, nfmt-1 DO begin
         fmt1 = gettok( frmt, ',' )
         if fmt1 eq '' then fmt1 = 'A'        ; Default is ascii format
            case strmid(fmt1,0,1) of
               'A': idltype[i] = 7
               'D': idltype[i] = 5
               'F': idltype[i] = 4
               'I': idltype[i] = 2
               'B': idltype[i] = 1
               'L': idltype[i] = 3
               'Z': begin 
                     idltype[i] = 3        ;Hexadecimal
                     hex[i] = 1b
                  end
               'X':  idltype[i] = 0
               else: message,'Illegal format '+fmt1+' in field '+strtrim(i,2)
            endcase
            ; Define output arrays
            if idltype[i] ne 0 then begin
                st = vv[k] + '= make_array(nlines,TYPE = idltype[i] )'
                tst = execute(st)
                k = k+1
            endif
      endfor    
      ngood = 0L
      temp = '	'
      for j=1L,nlines-1 DO begin        ; Skip first line (headers)
         k = 0
         temp = strtrim(result[j],1)
         var = strsplit(temp,'	',/extract,/PRESERVE_NULL)
         for i = 0L,nfmt-1 DO begin
            if ( idltype[i] ne 0 ) then begin        ;Expecting data?
               if i+1 gt n_elements(var) then begin
                  ngood=ngood-1
                  goto, badline
               endif
               if ( idltype[i] ne 7 ) then begin        ;Check for numeric data
                  tst = strnumber(var[i],val,hex=hex[i])        ;Valid number?
                  ; Instead of failing on 'NULL', need to return 'NaN'
                  if strmatch(var[i],'NULL') then begin
                     var[i] = 'NaN'
                     tst = 1
                  endif
                  if tst eq 0 then begin                ;If not, skip this line
                    ngood = ngood-1
                    goto, BADLINE
                  endif
                  st = vv[k] + '[ngood] = val'
               endif else st = vv[k] + '[ngood] = strtrim(var[i],2)'
               tst = execute(st)
               k = k + 1
            endif
         endfor

BADLINE: ngood = ngood+1

      endfor
      if ngood eq 0 then begin
         message,'ERROR - No valid lines found for specified format',/INFORM
         return
      endif else begin
         ; Compress arrays to match actual number of valid lines
         for i = 0,ncol-1 DO tst = execute(vv[i] + '=trimrank('+ vv[i]+ '[0:ngood-1])')
      endelse
   endif else begin
      ; Executed a command that wasn't a query, just return number of
      ; lines affected
      message,'WARNING - cmd executed, but not checked',/INFORM
   endelse

end

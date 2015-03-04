;+
; NAME:
;  reduc
; PURPOSE:
;  Photometry reduction widget for using reductor.
; DESCRIPTION:
; Locates reduction directory and builds and/or edits the
; reduc.inf file. It allows user to run reductor when desired
; and appropriate. Reduc allows one 1 rule line only to be run
; at a time.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  reduc
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; BROWSE-     Flag, if set, will not allow editing of reduc.inf or running
;                reductor (for any reason), but will display all information,
;                with edit windows disabled.
; DATATABLE-  Alternate table for data table access in the photometry
;                data base ('phot'). If not specified the 'data' table is used.
;                This keyword is also passed to reductor and through to dbphot.
;                It is a string- if it is of the form 'd.t' where d and t
;                are non-null substrings, d is taken to be a database name
;                and t is the table. If t is a null string, it is individually
;                defaulted as phot.
; DIR -       Directory for reduc to run. This should be a directory with
;                rundate (yymmdd) directories beneath. If not specified, the
;                default is '/net/frakir/raid/buie/Reduced'.
; INSTDB   -  Alternate database for instrument queries and updates. A string 
;                giving an alternate database to query for the runstat and
;                image table.
;                This is a single data base to be usd for all instruments.
;                If it is set to a string with an embedded dot 'd.t'
;                the table name searched is changed to t, and the data
;                base is taken as d. If it is of the form
;                {a,b,c} where a, b, c ... are either of the form 'd'
;                or 'd.t' then it represents a list of databases/tables
;                to be searched in order.
;                If not specified, the runstat table is accessed in the
;                instrument databases roboccd, pccdobs and pccd2obs.
;                ie, equivalent to 
;                '{pccd2obs.runstat,pccdobs.runstat,roboccd.runstat]'
;                The image table is always 'x.image' where x is the 
;                database where the rundate was found with the INSTDB
;                search.
; INSTDEFTABLE-Alternate table for instdef table access in the photometry
;               data base ('phot'). If  unspecified the 'instdef' table is used.
;               The 'd.t' format (see DATATABLE keyword) is also supported.
; NOTESXSIZE- Size of notes widget in columns, default 80.
; NOTESYSIZE- Size of notes widget in rows, default 5.
; TRANSFTABLE-Alternate table for transformation table access in the photometry
;               data base ('phot'). If not specified the 'transf' table is used.
;               The 'd.t' format (see DATATABLE keyword) is also supported.
; 
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; Reduc rewrites the reduc.inf file in the reduction directory to which
; it is directed, and also runs reductor, under control of the user.
; It may update the runstat table in one or more of the instrument data bases.
; RESTRICTIONS:
; Code needs some internal reorganization.
; Reduc is not well protected against badly formed reduc.inf lines.
; The search for rundate in the data bases controlled by INSTDB is
; amended in the case of pccdobs only- the select will include
; INST = 'L'  AND'd with the usual RUNDATE = 'xxxxxx'
; If a data base name in INSTDB is nondefault, the instrument pertaining
; is assumed to be PCCD.
;
; Reduc/reductor don't allow the same rundate for different instruments.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Peter L. Collins, 2006/09/07, Lowell Observatory
;  2006/09/18, PLC, some fixes, especially to add/delete lines from menu bar.
;  2006/10/03,PLC, many fixes and additions.
;  2006/10/05,PLC, small changes to run reductor buttons and display of
;                  transf data base in the detail (per rule) window.
;  2006/10/18,PLC, many changes including runstat (db) edit and selection.
;  2006/10/19,PLC, adding reports.
;  2006/10/23, PLC, transf and object observation reports, 'database only' mode.
;  2006/11/03,PLC,  modified Prev/Next and Enter Rundate to search for best
;                   available rundate based on search criteria and context.
;  2006/11/04,PLC,  added 'Not' qualifier to Autostep criteria.
;  2006/11/06,PLC,  fixes for non existent reduc directory when trying to write
;                   reduc.log for runstat edits, and non-existent ddir check
;                   and modifier for "no cal directory" message. New img report.
;  2006/11/07,PLC,  allow notes update to function normally in browse mode. Add
;                   print for edit runstat query (ie, all db updates get print).
;  2006/11/09,PLC,  add all observations report and incorporate mysqlquery fix
;                   in imglistreport.
;  2006/12/07,PLC,  slightly rationalized color filter handling, fixed calls
;                   to qinput and qannounc to enforce modality, changes for 
;                   new phot.data format.
;  2006/12/09,PLC,  add DATATABLE keyword.
;  2006/12/12,PLC,  fix bug in run reduc so force pulldown menus preserve
;                   state, change menu for hardcopy to dropdown near buttons,
;                   rationalize browse/edit modes.
;  2006/12/14,PLC,  some bug fixes and added reduc_nightbad to remove data
;                   records and mark transf records bad when a night marked 
;                   bad (sky non-phot or status bad). Add default color term
;                   forcing widget buttons and menu bar edit options. Add
;                   sky and stat outputs to transf report.
;  2006/12/16,PLC,  big rearrangement of code- alphabetize, remove secondary and
;                   subevents.
;  2006/12/19, PLC, Added support for /SAVEALLPLOTS option to reductor
;                   and for transition to 'reduced' runstatus.
;  2006/12/30,PLC,  further rationalized handling of rundate and status 
;                   processing, current directory management, and extended the 
;                   definition of the database table keywords. Recomissioned
;                   the run reductor buttons in upper gui and removed /NORUN.
;                   Added maintenance bar with options for verbosity. Allowed
;                   "Recompute Log1 File" to always work as long as rundir is
;                   not read-only. It is the same as the regular run reductor
;                   button except that all rules are disabled and /NOSAVE is
;                   not allowed. 
;  2007/01/04,PLC,  Changed 'Enter Rundate' menu operation to use autostep 
;                   criteria when picking rundates to go to when the rundate
;                   entered is not available.
;                   Upgraded rundate selection idl screen error output.
;                   Modified order of directory evaluation when changing
;                   rundate to avoid accessing rundir when runstat is such
;                   that the directory will be hidden.
;                   Added a qannounc for rundate transitioning to reduced 
;                   status prior to running the rules in SAVEALLPLOTS mode.
; 2007/01/05,PLC,   Added call to plpedit to edit log1 bd flags via the edit
;                   menu bar, and
;                   commented out raw->cal processing stuff in reduc_setstatsky,
;                   at least for now.
; 2007/01/10,PLC,   fixed a logic hole that prevented the runstat Stat and Sky
;                   from being displayed if a rule was selected while editing
;                   former. The fix was in reduc_newrundisp.
; 2007/01/20,PLC,   Several fixes to get queries and updates to  db 'pccdobs' 
;                   to use the 1 letter 'inst' code correctly. The major
;                   one was to reduc_dbstr2hook to strtrim database name.
; 2007/02/15, PLC,  Modify queries to phot.data to reflect addition of Color
;                   column.
; 2007/02/20, MWB, Added DP rule clump
; 2007/02/21, MWB, Further cleanup and exception handling.
; 2008/06/19, MWB, repaired a logic problem with 'sl' rule display information.
;                    Remember, there may be more in phot.data than what comes
;                    from this program!
; 2008/09/15, MWB, image list broken for roboccd, fixed mysql query
;                   
;-

pro reduc_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, GET_UVALUE=state

   if ptr_valid((*state).oprule) then ptr_free,(*state).oprule
   if ptr_valid((*state).rulestr) then ptr_free,(*state).rulestr
   if ptr_valid((*state).rulecodes) then ptr_free,(*state).rulecodes
   if ptr_valid((*state).notes) then ptr_free,(*state).notes

   ; Free up the state structure.
   ptr_free, state

end

; Functions

; return portion of mysql query that implements the autostep selection criteria.
; table is prepended to column names if specified- currently this is 
; applied to the first occurence of 'Sky' or 'Inst' in stepquerysky and
; the 'L' clause only, respectively, to meet current needs- (specifically a
; join being used in report generation).
function reduc_autostepquery, state,db,all, TABLE=table
   if n_elements(table) eq 0 then table = ''
   
   if  ( ( (*state).stepquerySky eq '' and (*state).stepqueryStatus eq '') or $
        all) and   db ne 'pccdobs'  then return, ''
   query =  (*state).stepquerySky 
   if all then query = ''
   if table ne '' then begin
      n = strpos(query, 'Sky')
      if n ge 0 then begin
         if n gt 0 then query = strmid(query,0,n) + table + '.' +  $
                        strmid(query,n) $
         else query = table + '.' + query
      endif
   endif
      
   if (*state).stepqueryStatus ne '' and (not all) then begin
      if query ne '' then query +=  ' and '
      query += (*state).stepqueryStatus
   endif
   if (*state).vrbsqueries then print, 'autostep query, db is ', db
   if db eq 'pccdobs' then begin
      if query ne '' then  query += ' and '
      if table ne '' then query += (' ' + table + '.inst = ') $
      else query += ' inst = ' 
      query +=  quote('L') 
   endif
   query = ' ( ' + query + ' ) '
   if (*state).vrbsqueries then print, 'autostep query fragment: ', query
   return, query
end

; find best choice for rundate out of pattern given targdate.
; targdate is guaranteed not in pattern. If it is not a valid
; 6 digit rundate, bestdate returns ''. 
; in_pattern has at least 1 member.
; if direction is 'Next' find closest above targdate.
; if direction is 'Prev' find closest below targdate.
; if direction is '' and the targdate is the first or 0th of some month,
; direction reverts to 'Next'. If direction is '' otherwise, find the
; closest, favoring the later if 2 equal choices.
function reduc_bestdate, state,targdate, in_direction, in_pattern
   if not stregex(targdate,'[0-9][0-9][0-9][0-9][0-9][0-9]',/BOOLEAN) then $
      return, ''

   direction = in_direction
   if direction ne 'Next' and direction ne 'Prev' then direction=''

   day=strmid(targdate,strlen(targdate)-2,2)
   if day eq '01' or day eq '00' then $
      if direction eq '' then direction='Next'

   ; need to parse targdate as jd to get closest day in either direction right.
   ; since 2000 is a century year with a leap year it is permissible to move
   ; all the dates, including the targdate, fwd 10 years for openers
   ; so everything is in the same century.

   ; it's just easier to tack on target at the begining.
   pattern = [targdate, in_pattern]

   ; ugly expression- needed for jdparse- strn would be nicer but it
   ; doesn't handle arrays.
   datepat = '20' +  $    ; next line just adds 1 to the decade
            string(((fix(strmid(pattern,0,1)) + 1) mod 10),FORMAT='(I1)') + $
            strmid(pattern,1,1) + '/' + strmid(pattern,2,2) + '/' + $
            strmid(pattern,4,2)
   metric = jdparse(datepat)
   metric = metric -  metric[0]
   metric=metric[1:*]

   count = n_elements(metric)
   if direction eq 'Next' then z = where(metric gt 0.0, count) $
   else if direction eq 'Prev' then z = where(metric lt 0.0, count) $ 
   else z = indgen(count)
   
   if count le 0 then return,''

   q = min( abs(metric[z]))
   y = where( abs(metric[z]) eq q, count) ; guaranteed (but why not sort?)
   ; to favor the later date.
   if count gt 1 and direction eq '' then if z[y[1]] gt z[y[0]] then y[0] = y[1]

   if (*state).vrbsrundates then print, ' bestdate target ', targdate,  $
                     ' found: ',in_pattern[z[y[0]]] 
   return, in_pattern[z[y[0]]]
end

; convert scalar data base option string to an array of strings,
; [0] db [1] table [2] db [3] table  ...
; SINGLE allows one db/table pair only.
function reduc_dbstr2hook,keyword,SINGLE=single,ERRSTR=errstr,DB=db,TABLE=table
   if n_elements(single) eq 0 then single=0
   errstr=''
   dbhook = strcompress(strtrim(keyword,2))
   if strmid(dbhook,0,1) eq '}' then dbhook = strmid( dbhook,1,strlen(dbhook)-2)
   dbhook = strsplit(dbhook, ',',/EXTRACT)
   for j=0,n_elements(dbhook)-1 do begin
      piece = strsplit(dbhook[j], '.',/EXTRACT)
      piece = strtrim(piece,2)
      if n_elements(piece) eq 1 then begin
         if strmid(dbhook[j], strlen(dbhook[j])-1) eq '.' then $
            piece = [ piece[0],table] $
         else piece = [ db, piece[0]]
      endif else piece = piece[0:1]
      dbhook= [dbhook, piece] ; append processed items to the end.
   endfor
   dbhook=dbhook[j:j*3-1]
   if single and n_elements(dbhook) ne (2) then errstr='only one allowed'
   return, dbhook
end

; strn style output for 'F'  where the string is further truncated of
; superfluous zeros right of decimal pt.
function reduc_fstrn,v,FORMAT=f
  if v ne 0.0 then return, strn(v, FORMAT=f)
  return, '0.0' ; well, that's all we do for now!
end

; generate a list of the forcing terms for a rule, return as a string.
function reduc_infoforcing, state,oprule
   forced = [ oprule.ctforce,oprule.k2force,oprule.kforce,oprule.ktforce, $
              oprule.tdforce]
   z = where(forced eq 1 )
   if z[0] ge 0 then begin
      forcestr = strjoin((*state).forcenm[z],', ')
      return, 'Forcing terms: ' + forcestr
   endif else return, 'No forcing terms'
end

; maps either way, between numeric (Landolt) standard filter codes
; and 1 character names  UBVRI [01234]. If the input is not recognized
; a null string is returned. Currently both input and return value are
; strings.
function reduc_mapfilcod, state, f

   if strnumber(f,v) and strlen(f) eq 1  then begin
      v = fix(v) 
      if v ge 0 and v lt n_elements((*state).stdcolors) then $
         return, (*state).stdcolors[v] 
   endif
   z = where( f eq  (*state).stdcolors)
   if z[0] ge 0 then return, strn(z[0])
   if (*state).vrbsrulesedit then print, 'reduc_mapfilcod bad filter: ', f
   return,''

end

; return 1 or 0 as the run directory in (*state).rundate does not or does exist.
; The keywords are outputs, corresponding to state variables similarly named.
; reduchidden (which is another name for the return value) means the 
; path reducpath/rundate does not exist, is not readable or is not a
; directory; however, if rundate is a null string, reduchidden is always
; true.
;
; infoexists means reduchidden false and 'reduc.inf' exists in the directory
; as a readable file.
;
; ddirexists means infoexists true and the ddir specified in the current
; info file exists and is a readable directory.
;
; calibexists means ddirexists true  and another directory 'calib' exists and 
; is readable in the data directory. Note that the ddir is taken literally
; from the info file and reductor-style relocation is not done.
;
; writeable means reduchidden false and the directory is writeable, and, 
; any of ['reduc.inf', 'reduc.log'] that already exist in the directory are 
; also writeable. (this list will grow).
function reduc_norundir, state, INFOEXISTS=infoexists,WRITEABLE=writeable, $
         CALIBEXISTS=calibexists,DDIREXISTS=ddirexists,DDIRECTORY=ddirectory
                         
   infoexists = 0
   writeable = 0
   calibexists = 0
   ddirexists = 0
   ddirectory = ''
   rundate = (*state).rundate
   if rundate ne '' then begin
      reddir = (*state).reducpath + rundate 
      ; would exists( addslash(reddir)) do the whole job? other platforms?
      if not exists(reddir) or not file_test(reddir,/DIRECTORY,/READ) then begin
         if (*state).vrbsrundates then print, 'the directory for ' + rundate + $
                ' is missing, setting reduc hidden mode.'
      reduchidden = 1
      endif else begin
         reduchidden = 0
         redinfo=addslash(reddir)+(*state).infofile
         infoexists = file_test( redinfo,/READ)  ; should check is a file.
         ; may promote which will throw an error if not writeable.
         if infoexists then rdreduc,redinfo,duminst,ddirectory,dumrundate, $
                             dumrad, $
                             dumsky1,dumsky2,dumgain,dumrdnoise,oplines,/GUI
         ddirexists= ddirectory ne '' and file_test(addslash(ddirectory) +  $
                                                    rundate, /READ,/DIRECTORY) 
         calibexists = ddirexists and  $
                        file_test(addslash(addslash(ddirectory) + rundate) + $
                                   'calib', /READ,/DIRECTORY) 
         ; also tests the directory itself.
         writeables = [ '', 'reduc.inf', 'reduc.log'] + $
                        addslash(reddir); put this in state?
         z = where( file_test( writeables)) ; guaranteed because '.' exists.
         writeable =  min(file_test(  writeables[z],/WRITE)) gt 0
      endelse
   endif else reduchidden = 1

   return, reduchidden

end

function reduc_quizverbosity,state, verbosetype
   msg = [ 'You currently have verbosity enabled for', '', $
           ' Minimal verbosity for certain errors' ]
   if (*state).vrbsqueries then msg = [ msg, ' Database select queries']
   if (*state).vrbsupdate then msg = [ msg, ' Database update commands']
   if (*state).vrbsrundates then msg = [ msg, ' Rundate transitions']
   if (*state).vrbsdbedit then msg = [ msg, ' Database general edit']
   if (*state).vrbsrules then msg = [ msg, ' General rules processing']
   if (*state).vrbsrulesedit then msg = [ msg, ' Detailed rules editing']
   if (*state).vrbsreductor then msg = [ msg, ' Reductor calls']
   if (*state).vrbsdbphot then msg = [ msg, ' Dbphot detail']
   r = qannounc(msg, FALSELABEL='Cancel', TRUELABEL=verbosetype, $
                        YSIZE=10,GROUP_LEADER=(*state).mainbase)
   return,r
end

; Procedures

pro reduc_addonerule, state, rulesample
   if (*state).infoexists then  begin
      reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                       dumsky1,dumsky2,dumgain,dumrdnoise,oplines

       if oplines[0] eq '' then oplines = [ rulesample] $
       else oplines = [ oplines, rulesample]

      (*state).nrules = n_elements(oplines)
      ruleselindex = (*state).nrules - 1
      (*state).ruleselected = 1
      oplines += ' ok'
      if (*state).vrbsrules then begin
         print, ' writing ', (*state).infofile, ' to add rule line ', $
                             ruleselindex
         print, ' rule line is: ', rulesample
      endif
      wrreduc,addslash((*state).reducpath + (*state).rundate) + $ 
              (*state).infofile,duminst,dumddir,dumrundate, $
         dumrad, dumsky1,dumsky2,dumgain,dumrdnoise,oplines,/GUI
      reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                    dumsky1,dumsky2,dumgain,dumrdnoise,oplines, $
                    SAVELINENO=ruleselindex
      if (*state).vrbsrules then print, ' new rule line moved after sort to',  $
                                 ruleselindex
      (*state).ruleselindex = ruleselindex
      reduc_toplinesdisp, state,/SELECT_RULE
      reduc_opnulldisplay,state
   endif
end

; add a new rule to reduc.inf. Substr is the rule type and is one of
; [ 'TR', 'SL', '2C','LC', 'DP' ]
pro reduc_addrule, state, subevent
   if not (*state).infoexists then $
      r = qannounc( 'There is no rules files to edit!', $
                    FALSELABEL='',GROUP_LEADER=(*state).mainbase) $
   else begin
      if not (*state).rdonly and not (*state).reduchidden then begin
         ; guaranteed because house controls the subevents.
         ; This scheme depends on no rule name being substring of another.
         z=where(strcmp(subevent,(*state).newrules,strlen(subevent),/FOLD_CASE))
         reduc_addonerule,state, (*state).newrules[z[0]]
         reduc_selrule,state,(*state).ruleselindex
      endif else r = qannounc('Can''t add rules-read only access!', $
                                FALSELABEL='', $
                                GROUP_LEADER=(*state).mainbase)
   endelse
end

pro reduc_addruleclump, state
   if (*state).infoexists then  begin
      reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                       dumsky1,dumsky2,dumgain,dumrdnoise,oplines

      rulesample = [ $
         'dp 9 M BV SAO_120107 0 0.0 SAO_160066 ct 0.0 0.0 k2 0.0 0.0', $
         'dp 9 M BV SAO_140688 0 0.0 SAO_160066 ct 0.0 0.0 k2 0.0 0.0', $
         'dp 9 M BV SAO_140956 0 0.0 SAO_160066 ct 0.0 0.0 k2 0.0 0.0', $
         'dp 9 M BV SAO_141142 0 0.0 SAO_160066 ct 0.0 0.0 k2 0.0 0.0' $
         ]

       if oplines[0] eq '' then oplines = [ rulesample] $
       else oplines = [ oplines, rulesample]

      (*state).nrules = n_elements(oplines)
      ruleselindex = (*state).nrules - 1
      (*state).ruleselected = 1
      oplines += ' ok'
      if (*state).vrbsrules then begin
         print, ' writing ', (*state).infofile, ' to add rule lines ', $
                             ruleselindex
         print, ' rule line is: ', rulesample
      endif
      wrreduc,addslash((*state).reducpath + (*state).rundate) + $ 
              (*state).infofile,duminst,dumddir,dumrundate, $
         dumrad, dumsky1,dumsky2,dumgain,dumrdnoise,oplines,/GUI
      reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                    dumsky1,dumsky2,dumgain,dumrdnoise,oplines, $
                    SAVELINENO=ruleselindex
      if (*state).vrbsrules then print, ' new rule line moved after sort to',  $
                                 ruleselindex
      (*state).ruleselindex = ruleselindex
      reduc_toplinesdisp, state,/SELECT_RULE
      reduc_opnulldisplay,state
   endif
end

      

; find a default value and error for ct and kt. This is done by looking up
; the desideratum (ct and then k2), filter, color (from the oprule which is
; now always a 'tr' rule) and the rundate 
; in the instdef database table. The rundate must be in [date1,date2].
; If there is a match, the error and value are assigned- otherwise,
; [0.,0.] is assigned. Value-error pairs are returned in ctvec and k2vec.
pro reduc_defct,state,ctvec,k2vec
   oprule = (*(*state).oprule)
   filt = reduc_mapfilcod(state,oprule.stdfil1)
   rundate = (*state).rundate
   color=oprule.color1
   ctvec = [0.0, 0.0]
   k2vec = [0.0, 0.0]
   
   db = (*state).instdefdbhook[0] ; usually 'phot'
   table = (*state).instdefdbhook[1] ; usually 'instdef'
   openmysql,dblun,db

   ; create sql date from rundate
   runUTdate = '20' +strmid(rundate,0,2) + '-' + strmid(rundate,2,2) + $
               '-' + strmid(rundate,4)
   if strmid(rundate,0,1) eq '9' then runUTdate = '19' + strmid(runUTdate,2)

   query = [' select termname, value,error,comments from ' + table, $
            ' where filter = ' + quote(filt), $
            ' and color1 = '+quote(color[0])+'and color2 = '+quote(color[1]), $
            ' and instrument = ' + quote((*state).inst), $ $
            ' and to_days( date1) <= to_days( ' + quote(runUTDate) + ') ', $
            ' and to_days( date2) >= to_days( ' + quote(runUTDate) + ') ' ]
   query = [ query, ';' ]
   if (*state).vrbsqueries then print, ' query for ct,k2 default: ', query
   mysqlquery,dblun, query, term, v, e, c, FORMAT='a,f,f,a'
   free_lun, dblun

   c1 = 0
   c2 = 0
   if term[0] ne '' then begin
      ; note that there could be more than one match. This would constitute
      ; an error in the instdef table contents. In such a case
      ; the first one is taken, arbitrarily.
      z = where( term eq 'ct', c1 )
      if z[0] ge 0 then ctvec = [ v[z[0]], e[z[0]] ]
      z = where( term eq 'k2',c2 )
      if z[0] ge 0 then k2vec = [ v[z[0]], e[z[0]] ]
   endif
   if (*state).vrbsqueries then print, $
          ' query for ct, ', c1, ' kt, ', c2, ' answers'
end

pro reduc_displayrules,state, oplines, SET_LIST_SELECT=select, SENSITIVE=sens

   ; supply the 7 character 'grabber' field for each rule. This provides
   ; the single most important fact about the rule, bearing in particular
   ; on whether it has already run, and with what result.
   ; Data base queries are the sole means employed to answer these questions.
   grabbers = replicate('   -   ',n_elements(oplines)) ; meaning, has not run.
   (*state).unrun = 0
   oprule = 0
   openmysql,dblun,(*state).transfdbhook[0]
   for j=0,n_elements(oplines)-1 do begin
      reduc_oplinesplit, state, oplines[j],LOCALOPRULE=oprule
      rundate = (*state).rundate
      inst = (*state).inst
      if oprule.valid then begin
         if oprule.op ne 'tr' then  rulequerytype = 'lctype' $
         else rulequerytype = oprule.op

         case rulequerytype of
            'tr': begin
               query=['select chi2,quality from ' + (*state).transfdbhook[1], $
                          ' where  rundate = ' + quote( rundate), $
                         ' and  instrument = ' + quote(inst) ]

               query = [query, ' and  filter =  ' + $ 
                               quote(reduc_mapfilcod(state,oprule.stdfil1)) ]
               query = [query, ' and color1 =  ' +  $
                                      quote(oprule.color1[0]) ]
               query = [query, ' and color2 =  ' + quote(oprule.color1[1]) ]
               query = [query, ';' ]
               if (*state).vrbsqueries then print, $
                    'query for tr rule display: ', query
         
               mysqlquery, dblun, query, chi2, quality, format='f,a'
               if chi2[0] ne '' then grabbers[j] = $
                  string(chi2[0],FORMAT="(F4.1)")+strmid(quality[0],0,1)+'  ' $ 
               else (*state).unrun++
            end

            'lctype': begin
            ; get an int: count of obs for object and filter or (both) filters.
               query=['select ObjName, filter,color from '+(*state).datadbhook[1]+ $
                         ' where ', ' RefID = ' + quote( rundate + '-' + inst) ]
               if oprule.op ne 'sl' then $
                  query = [query, ' and  ObjName =  ' + $
                                  quote(repchar(oprule.object,'_',' '))] $
               else $
                  query = [query,' and ObjName is not NULL', $
                                 ' and ObjName not like '+quote('CAT:%')]

               query = [query, ' and   ( ']
               query = [query, ' filter =  ' + $ 
                                      quote(oprule.filnam1) ]
               colortest = '-'
               if oprule.op eq 'lc' or oprule.op eq '2c' then $
                  colortest = oprule.color1[0] + '-' + oprule.color1[1]
               if oprule.op eq 'sl' then colortest = oprule.filnam1 + '-' + $
                  oprule.filnam2
               if oprule.op eq 'dp' and oprule.filnam1 ne 'M' then $
                  colortest = strmid(oprule.dpcolor,0,1) + '-' + $
                              strmid(oprule.dpcolor,1,1)
               if oprule.op eq '2c' or oprule.op eq 'sl'  then $
                  query = [query, ' or filter =  ' + $ 
                                         quote(oprule.filnam2) ]
               query = [query, ' ) ']
               query = [ query, ' and  color = ' + quote(colortest)]
               query = [query, '  order by ObjName ']

               query = [query, ';' ]
               if (*state).vrbsqueries then print, $
                    'query for lctype rule display: ', query
      
               mysqlquery, dblun, query, object, filter, color, format='(a,a,a)'
               if object[0] ne '' then begin
                  if oprule.op eq 'sl' then begin
                     slcnt = 0
                     ; get first character of each obj name.
                     indicator = strmid(object, 0, 1)
                     ; this criterion matches reductor.
                     ; It's been suggested 'e' can also denote asteroids.
                     stars= where ( indicator ne 'a' and indicator ne 'e' and $
                                    indicator ne 'c' and indicator ne 'p')
                     nstars = n_elements(stars)
                     if stars[0] ge 0 then begin
                        ; find stars in each filter (and the right color).
                        starsf1 = where( filter[stars] eq oprule.filnam1 and $
                                         color[stars] eq colortest)
                        starsf2 = where( filter[stars] eq oprule.filnam2 and $
                                         color[stars] eq colortest)
                        nf1 = n_elements(starsf1)
                        nf2 = n_elements(starsf2)
                        if starsf1[0] ge 0 and starsf2[0] ge 0 then begin
                           intrsect, object[stars[starsf1]], $
                                     object[stars[starsf2]], stars2f
                           ; count all obs belonging to stars in intersection.
                           if n_elements(stars2f) gt 0 then $
                              for i = 0,n_elements(stars2f)-1 do begin
                                 ; is color enough?
                                 z = where(stars2f[i] eq object[stars] and $
                                           colortest eq color[stars],count)
                                 slcnt += count
                              endfor
                        endif
                     endif
                     if slcnt gt 0 then $
                        grabbers[j]=string(slcnt,FORMAT="(I5)") +  '  ' $
                     else (*state).unrun++
                  endif else begin
                     ; need to exclude unmatching serial #s here (perhaps).
                     grabbers[j]=string(n_elements(object),FORMAT="(I5)") + $
                     '  '   
                  endelse
               endif else (*state).unrun++
            end

            else:
         endcase
      endif else (*state).unrun++
   endfor
   free_lun, dblun

   widget_control,(*state).ruleselectid, SET_VALUE=grabbers + oplines, $
                  SET_LIST_SELECT= select, SENSITIVE=sens
end

pro reduc_editrunstat, state, column, val
   db = (*state).runstatdb
   if db ne '' then begin
      table = (*state).instdbhook[1] ; normally 'runstat'
      openmysql,dblun,db
      selector= [' update ' +  table, $
                 ' set ' + column + ' = ' + quote(val), $
                 ' where   rundate = ' + quote((*state).rundate)]
      if db eq 'pccdobs' then selector=[ selector, ' and inst = ' + quote('L')]
      selector = [ selector, ';' ]
      if (*state).vrbsupdate then print, 'Updating runstat: ', selector
      mysqlcmd, dblun, selector,updateresp,nlines
      free_lun, dblun

      prevval = (*state).runstatstat
      if column eq 'Sky' then prevval = (*state).runstatsky

      ; write reductor log. Since the dir may be hidden a special 
      ; ' reducwriteable' may be needed.
      r = reduc_norundir(state, WRITEABLE=reducwriteable)
      if not (*state).setbrowse and reducwriteable then begin
         infologfile='reduc.log'
         jdstr, systime(/JULIAN), 0, tis
         spawn, '/usr/bin/whoami', whois
         openw, loglun, addslash((*state).reducpath + (*state).rundate) + $
                                   infologfile,  /GET_LUN,/APPEND
         printf, loglun, tis + ' '  +  whois + ' ' +  'update runstat ' +   $
                 column + ' from ' + prevval +  ' to ' + val
         if (*state).vrbsreductor then print, 'update reduc.log at ', tis
         free_lun, loglun
      endif
   endif
end

; assumes we are in the right state and ready to go.
pro reduc_editquality, state,val, filter, colors
   db = (*state).transfdbhook[0] ; usually 'phot'
   table = (*state).transfdbhook[1] ; usually 'transf'
   cmd = ' update ' +  table + ' set quality = ' + quote(val)
   cmd = [cmd, $
            ' where filter='+quote(filter)+' and ' + $
                    'color1='+quote(colors[0])+' and ' + $
                    'color2='+quote(colors[1])+' and', $
                    'instrument=' + quote((*state).inst) + ' and ', $
                  '  RunDate = ' + quote((*state).rundate) ]
   cmd = [cmd, ';']
   openmysql,dblun,db
   if (*state).vrbsupdate then print, 'Updating '+db+'.'+table,['', cmd ]
   mysqlcmd, dblun, cmd, updateresp,nlines
   free_lun, dblun
end

pro reduc_imglistreport, state, results
   imgtable = 'image' ; standard search table
   insttable = 'instrument' ; standard search table, need for filtname,exptime.
   tables = imgtable +',' + insttable
   instdb = (*state).runstatdb
   rundate = (*state).rundate
   results=['']
   c=','

   query = [ 'Select ', $
             imgtable + '.filename,object,type,' ]
   if instdb eq 'roboccd' then $
      query = [query,'concat_ws('+quote('+')+c+'filtname1,filtname2) as filt,'] $
   else $
      query = [query,'filtname as filt,']

   query = [query, $
             'exptime as etime,jdmid,midtime,fwhm,peak,xpk,ypk,sky,', $
             'skysig' , ' from ' + tables, $  
              ' where ' + imgtable + '.filename = ' + insttable + '.filename', $
             ' and  ' + imgtable + '.filename like ' ]
;   if instdb eq 'nasacam' then query=[ query, quote(  '20' + rundate + '%')] $
;   else query=[ query, quote(rundate + '%')]
   query=[ query, quote(rundate + '%')]
   if instdb eq 'pccdobs' then begin
      query  =[ query,  ' and ' + imgtable + '.inst = ' + quote('L') + $
                       ' and ' + insttable + '.inst = ' + quote('L') ]
   endif
   query = [ query, ';' ]
   if (*state).vrbsqueries then $
      print, 'query for imglist report on ', instdb,' is ', query

   openmysql,dblun,instdb
   mysqlquery, dblun, query, fn, object, type, filt, etime, jdmid, $
                      midtime, fwhm, peak, $
                      xpk,ypk,sky,skysig,FORMAT='a,a,a,a,a,a,a,a,a,a,a,a,a', $
                      HEADS=colhdrs
   results=['']                
   free_lun, dblun
   if fn[0] ne '' then begin
      results = [ 'use ' + instdb + ' ;' , query, ' ' ]
      widths = [ $
      max(strlen(strtrim(fn))),max(strlen(strtrim(object))), $
      max(strlen(strtrim(type))), $ 
      max(strlen(strtrim(filt))),max(strlen(strtrim(etime))), $
      max(strlen(strtrim(jdmid))),max(strlen(strtrim(midtime))), $
      max(strlen(strtrim(fwhm))),max(strlen(strtrim(peak))), $
      max(strlen(strtrim(xpk))),  $
      max(strlen(strtrim(ypk))),max(strlen(strtrim(sky))), $
      max(strlen(strtrim(skysig))) ] 

      widths = widths > [ strlen(colhdrs) + 3]

      fmt = '('
      for j=0, n_elements(widths)-1 do begin
         if  j gt 0 then fmt += ','
         fmt += 'a' + strn(widths[j]) +',2x'
      endfor
      fmt += ')'
      
      results = [results, string(colhdrs, format=fmt)]
      for j=0, n_elements(fn)-1 do begin
         results = [ results, string(fn[j], object[j], type[j], $
                     filt[j],etime[j],jdmid[j], $
                     midtime[j], fwhm[j], peak[j], $
                     xpk[j], ypk[j], sky[j], skysig[j],format=fmt) ]
      endfor
      jdstr, systime(/JULIAN), 0, tis
      footer = [' ', tis + string( rundate, instdb, n_elements(fn), $
                FORMAT="(1x,a, ': ',a, i5, ' total lines')"),  ' ' ]
      results = [  results, footer]
      endif
end

; this displays the variable parts of the first line of the widget.
; It does not mess
; with state of the label under the prev/next, since that doesn't
; change as a function of rundate.
; It also displays any rules in the list widget, leaving it
; unselected.
; It also blanks the short info display to the right of the rules list
; (this function may move somewhere else later).
; It reads reduc.inf if it already exists.
; it sets state fields for the big 5 photometry parameters.
pro reduc_infodisp, state
   rundate = (*state).rundate
   if (*state).vrbsrules then print, 'infodisp: ',rundate
   reduc_runstatdisp, state 
   widget_control, (*state).rundateid, SET_VALUE=rundate + ':'
   widget_control, (*state).instid, SET_VALUE=(*state).inst
   if not (*state).reduchidden then begin
      if (*state).infoexists then begin
         rdreduc, addslash((*state).reducpath + (*state).rundate) + $ 
                   (*state).infofile,inst,ddir,inforundate,rad,sky1,sky2,gain, $
                   rdnoise, oplines,/GUI

         ;need consistency check for rundate and instrument (or in newrunstates)
         nops = n_elements(oplines)
         if nops gt 0 and oplines[0] eq '' then nops = 0
         reducstatus = 'Rules file: ' +  strn(nops) +   ' rules' 
         (*state).ddir = ddir 
         (*state).rad = rad 
         (*state).sky1 = sky1 
         (*state).sky2 = sky2 
         (*state).gain = gain 
         (*state).rdnoise = rdnoise 
         (*state).nrules = nops
         ; re-read reduc.inf to handle oks and sort rule lines.
         reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                         dumsky1,dumsky2,dumgain,dumrdnoise,oplines
         reduc_displayrules, state,oplines, SENSITIVE=(nops gt 0)
         (*state).ruleselindex = nops
         widget_control, (*state).mrid, SENSITIVE=0
      endif else begin
         reducstatus = 'No Rules file'
         (*state).nrules = 0
         widget_control, (*state).mrid, SENSITIVE=((*state).rdonly eq 0)
      endelse
         widget_control, (*state).reducstatusid, SET_VALUE=reducstatus
   endif
end

; clear the whole widget (when changing rundates).
pro reduc_newdirclrdisp, state
   widget_control, (*state).objradid, SENSITIVE = 0, SET_VALUE=''
   widget_control, (*state).skyrad1id, SENSITIVE = 0, SET_VALUE=''
   widget_control, (*state).skyrad2id, SENSITIVE = 0, SET_VALUE=''
   widget_control, (*state).gainid, SENSITIVE = 0, SET_VALUE=''
   widget_control, (*state).rdnoiseid, SENSITIVE = 0, SET_VALUE=''
   widget_control, (*state).instid, SET_VALUE=''
   widget_control, (*state).dpathid, SET_VALUE=''
   widget_control, (*state).rpathid, SET_VALUE=''
   widget_control, (*state).rundateid, SET_VALUE=''
   widget_control, (*state).reducstatusid, SET_VALUE=''
   widget_control, (*state).mrid, SENSITIVE=0
   widget_control, (*state).ruleselectid, SET_VALUE=''
   widget_control, (*state).runreducsigid, SENSITIVE=0
   widget_control, (*state).runreducmagid, SENSITIVE=0
   widget_control, (*state).selhardcopyid, SENSITIVE=0
   widget_control, (*state).rephotid, SENSITIVE=0
   widget_control, (*state).info0id, SET_VALUE=''
   widget_control, (*state).info1id, SET_VALUE=''
   widget_control, (*state).info2id, SET_VALUE=''
   widget_control, (*state).info3id, SET_VALUE=''
   widget_control, (*state).ruleselectid, SENSITIVE=0
   if (*state).vrbsrulesedit then print, 'newdirclrdisp'
   reduc_opnulldisplay,state
end

; this sets up all displays when you land in a new directory
; or radical changes have occurred. It does require that
; you have a valid rundate and be already in that directory.
; Ordinarily you clear everything first with reduc_newdirclrdisp
pro reduc_newdirsetdisp, state
   reduc_toplinesdisp, state
   reduc_opnulldisplay,state ; clear the rule editor (widget bottom)
   reduc_notesrefresh,state
end

; this is invoked for a new rundate or a change in the state of the
; current rundate (such as a runstat edit, or enter browse) and its job  is to
; do any updates needed to the widget displays and sensitization.
; It doesn't handle changes to rules.
pro reduc_newrundisp,state
      rbmsg  = (*state).rbmsg
      if (*state).rdonly then  rbmsg = (*state).rbmsgno
     ; display save/nosave reductor status
      widget_control, (*state).rbid, SET_VALUE=rbmsg


      if not (*state).reduchidden  and  (*state).ruleselected then begin
         ; display runstat in case it changed.
         reduc_runstatdisp, state 
         ; a rule is already selected so just need to redisplay it.
         reduc_opnulldisplay,state
         reduc_opdisplay, state,(*(*state).oprule)
      endif else begin
         ; need to clear/redisplay the directory etc. This will
         ; change sensitization of the 2 run reductor buttons in upper widget.
         reduc_newdirclrdisp,state
         (*state).ruleselected = 0
          if (*state).rundate ne '' then reduc_notessave,state
          reduc_newdirsetdisp,state
      endelse
end


; this routine re-evaluates state variables based on a (new) rundate
; in (*state).rundate. State variables changed include
;
; reduchidden rdonly runstatsky runstatstat infoexists ddirexists
; ddir reducwriteable calibexists
;
; The routine reads the runstat table in (*state).runstatdb to find
; the values for stat and sky, and probes the reduc directory
; for the rundate in question.
; It requires that rundate,runstatdb, and inst be set correctly in state.
; Rundate must be valid and (*state).runstatexists must be true.
pro reduc_newrunstates,state
   table = (*state).instdbhook[1] ; normally 'runstat'
   db =(*state).runstatdb
   openmysql,dblun,db
   query = ' select Sky, Stat from ' + table + '  where rundate = ' +  $
             quote( (*state).rundate)
   if db eq 'pccdobs' then query=[query, ' and inst = ' + quote('L') ]
      query = [ query, ';' ]

   mysqlquery, dblun, query, sky, stat, format='a,a'
   free_lun, dblun
   if sky[0] eq '' then begin
      (*state).rundatexists = 0  ; " can't happen"
      r = qannounc('Unexpected database error in rundate' + $
                (*state).rundate,FALSELABEL='',GROUP_LEADER=(*state).mainbase)
   endif

   (*state).runstatsky = sky[0]
   (*state).runstatstat = stat[0]

   ; where is guaranteed.
   z = where( stat[0] eq (*state).runstatnames) 
   flag = (*state).runstatflags[z[0]]

   ; find out about any reduction directory that might exist- unless
   ; runstat precludes directory access.
   if flag ne 0 then begin 
      reduc_reducdirprobe,state

      ; work out rdonly.
      (*state).rdonly = (*state).setbrowse
      if not (*state).reducwriteable or (*state).reduchidden then $
         (*state).rdonly = 1
      if not (*state).reducwriteable and not (*state).reduchidden then $
         r = qannounc( ['Reduc directory or files not writeable.',  $
                  'You cannot edit rules or run reductor except in /NOSAVE'], $
                              FALSELABEL='', GROUP_LEADER=(*state).mainbase)
   endif else begin
      if (*state).vrbsrundates then print, 'dir hidden by runstat ', $
         stat[0]
      (*state).infoexists = 0
      (*state).ddirexists = 0
      (*state).calibexists = 0
   endelse


   ; amend state (in the restrictive direction) for runstat.
   if flag lt 2  then (*state).rdonly = 1 
   if flag eq 0 then (*state).reduchidden = 1
   if (*state).reduchidden then (*state).ruleselected=0

   if (*state).vrbsrundates then print, ' newrunstates: reduchidden ', $
      (*state).reduchidden, ' rdonly ' , (*state).rdonly, $
      ' ruleselected ',(*state).ruleselected, ' setbrowse ',(*state).setbrowse
end

; this routine deals with the situation of a night being marked bad,
; defined as runstat.Stat being changed to 'bad' or runstat.Sky going
; to 'non-phot'.
; It is then necessary to modify records in the data and transf tables-
; (currently this procedure  requires they be in the same database.)
; A night is currently considered to be "marked bad" if runstat.Stat is
; changed to bad from something else, or runstat.Sky is changed to  non-phot.
; The first thing to do is to count the number of records in data and transf
; that are affected- if none, simply return and let the runstat update proceed.
; Otherwise,  a qannounc is posted to give the user a chance to back out.
; If they decide to go ahead, all of the transf records for this rundate 
; and instrument are marked bad, and all of the data records are removed.
;
; reply is the reply from the user to qannounc, 1 if to go ahead, 0 if user
; backed out (in which case no data base update occurs.)
;
pro reduc_nightbad, state, reply
   ; first get count of affected records in transf
   reply = 1
   db=(*state).transfdbhook[0] ; usually 'phot'
   openmysql,dblun,db
   tabletr = (*state).transfdbhook[1] ; usually 'transf'
   trcount = 0
   ; querytr[1:*] must be selector only.
   querytr= [ ' select UTDate,instrument,filter,color1,color2,nobs,' + $
                'quality  from ' + tabletr , $
             ' where rundate = '  + quote((*state).rundate), $
             ' and instrument = ' + quote((*state).inst),  $
             ' and quality != ' + quote('bad'), ';' ] 
   ; full query to get a sample listing of records.
   if (*state).vrbsqueries then print, ' query for tr scrub records: ', query
   mysqlquery, dblun, querytr, utdate, instr, filtr, c1,c2,nobs,q, $
                format='a,a,a,a,a,a,a'
   if utdate[0] ne '' then trcount = n_elements(utdate)

   tabledt = (*state).datadbhook[1] ; usually 'data'

   ; querydt[1:*] must be selector only.
   dtcount = 0

   querydt= [ ' select refid, objname, filter, mag from ' + tabledt , $
             ' where RefID = ' +  $
               quote((*state).rundate + '-' +  (*state).inst),  $
               'order by objname,filter ', ';' ]
   ; full query to get a sample listing of records.
   if (*state).vrbsqueries then print, ' query for data scrub records: ', query
   mysqlquery, dblun, querydt, refid,objname,fildt,mag ,  format='a,a,a,a'
   if refid[0] ne '' then dtcount = n_elements(refid)
   s_tr = strarr(trcount > 1)
   for i=0,trcount-1 do $
   s_tr[i] = string(utdate[i], instr[i], filtr[i], c1[i],c2[i],nobs[i],q[i], $
                  FORMAT="(A20, 1x, A10,1x,A2,'  (',A2,'-',A2,')  ',A6,A10)")
   s_dt = strarr(dtcount > 1)
   for i=0,dtcount-1 do $
   s_dt[i] = string(refid[i], objname[i], fildt[i], mag[i], $
                  FORMAT="(A20, 1x, A20,1x,A6,F8.3)")
   if (dtcount + trcount) gt 0 then begin
   ; make it big enough to show some of the transf records at least.
      reply = qannounc( ['You wish to mark this night as having bad data.', $
                 'The data records will be removed from the database.', $
                 'The transformation records will be marked ''bad''.', $
                 'There are ' + strn(trcount) + ' transf records and ' + $
                 strn( dtcount) + ' data records affected.', $
                 'The only way to restore them would be  to change status', $
                 ' and rerun all rules.', $
                 'Do you want to continue with this operation?', $
                 '', db + '.' + tabletr + ' Records', s_tr, $
                 '', db + '.' + tabledt + ' Records', s_dt], $
                 TRUELABEL='Go Ahead and Edit DataBase', $
                 FALSELABEL='Abort operation', $
                 YSIZE=12,GROUP_LEADER=(*state).mainbase) 
      if reply then begin
         cmd=['update ' + tabletr,' set quality = '+quote('bad'), querytr[1:*]]
         if (*state).vrbsupdate then $
            print, ' Scrubbing transf with this command ', cmd
         mysqlcmd, dblun,cmd,insertresp, nlines
         cmd = ['delete from ' +  tabledt,  querydt[1:*]]
         if (*state).vrbsupdate then $
            print, ' Scrubbing data with this command ', cmd
         mysqlcmd, dblun,cmd,insertresp, nlines
      endif
   endif
   free_lun, dblun
end


; refresh notes text widget with new (or the same) rundate
pro reduc_notesrefresh, state
   (*state).notesdirty = 0
   widget_control,(*state).notesinstid, SET_VALUE='Notes: unmodified'
   db = (*state).runstatdb
   if db ne '' then begin
      table = (*state).instdbhook[1]; 'runstat' normally
      cmd= ' select Notes from ' + table+ ' where Rundate = ' + $
          quote((*state).rundate) 
      if db eq 'pccdobs' then cmd += ' and inst = ' + quote('L') 
      cmd += ' ;'
      if (*state).vrbsqueries then print,  'noterefresh query ', cmd
      openmysql,dblun,db
      mysqlcmd,dblun,cmd,oldnotes,nlines
      free_lun, dblun
      if nlines gt 1 then begin
         ; db query succeeded.
         if oldnotes[1] ne 'NULL' then $
            notes=dequote(oldnotes[1],/SELECTSQL) $
         else notes =''  ; NULL (Default) notes field.
         if ptr_valid((*state).notes) then ptr_free,(*state).notes
         (*state).notes = ptr_new(notes)
         widget_control, (*state).notestextid, SET_VALUE=notes,$
                          EDITABLE=not((*state).rdonly) 
      endif else begin
         widget_control,(*state).notesinstid, set_value=''
         widget_control,(*state).notestextid, set_value='',editable=0
         print, 'Unexpected data base error in query', (*state).bel
      endelse
   endif
end

; save current notes in notes text widget to db
pro reduc_notessave, state
   if (*state).notesdirty then begin
      if (*state).vrbsrundates then print, ' Saving notes for ',(*state).rundate
      db = (*state).runstatdb
      if db ne '' then begin
         table = (*state).instdbhook[1] ; 'runstat' normally
         openmysql,dblun,db
         widget_control, (*state).notestextid, GET_VALUE=notes
         cmd= ' update ' + table + ' set Notes = ' + quote(notes) + $
              ' where Rundate = ' + quote((*state).rundate)
         if db eq 'pccdobs' then cmd += ' and inst = ' + quote('L')
         cmd += ' ;'
       if (*state).vrbsupdate then print,  'noterefresh update ', cmd 
         mysqlcmd, dblun,cmd,insertresp, nlines
        free_lun,dblun
        (*state).notesdirty = 0
         widget_control,(*state).notesinstid, SET_VALUE= 'Notes: unmodified'
      endif
   endif
end

; this is a join- note that 'image' is a hard coded table name
; but the other is controlled by keywd.
pro reduc_objobsreport, state, object,results
   table =  (*state).instdbhook[1]; standard search table, normally 'runstat'
   results=['']
   totlines = 0
   ndbs = 0

   for i = 0, (n_elements((*state).instdbhook) - 1),2 do begin
      openmysql,dblun,(*state).instdbhook[i]
      makerund = ' substring(filename,1,6) '
;      if (*state).instdbhook[i] eq 'nasacam' then $
;         makerund = ' concat( ' + quote('0') + $
;            ' , substring(filename,4,5) )'
      query=[' select ' + makerund + ' as rund, count(*),' +table+'.sky,stat' ]
      query = [ query, 'from image,' + table + ' where ' ]
      if object ne 'All' then $
         query = [ query, ' object = ' + quote(object) + ' and ' + makerund + $
                          ' = rundate'] $
      else $
         query = [ query,  makerund + ' = rundate']

      if (*state).instdbhook[i] eq 'pccdobs' then $
         query[n_elements(query)-1] += ' and  image.inst =  ' + quote('L')
      asquery = reduc_autostepquery(state, (*state).instdbhook[i],0, $
                                    TABLE=table)
      if asquery ne '' then $
      query = [ query, ' and ' + asquery ]
      query = [ query, ' group by rund order by rund' + ';' ]
      if (*state).vrbsqueries then print, 'final query for objobs report on ', $
         (*state).instdbhook[i],' is ', query

      mysqlquery, dblun, query, rund, obscount, sky, stat, format='a,i,a,a', $
                  HEADS=colhdrs
      free_lun, dblun

      if rund[0] ne '' then  begin 
         ndbs++
         totlines += n_elements(rund)
         header = [ 'use ' + (*state).instdbhook[i] + ' ;' , query, ' ', $
                    string(colhdrs, FORMAT="(a6,4x,a5,4x,a7,2x,a12)"), ' ']
         jdstr, systime(/JULIAN), 0, tis
         footer = [' ', tis + string( object, n_elements(rund), $
                           FORMAT="(1x,a, ': ', i5, ' lines')"),  ' ' ]
         if results[0] eq '' then results=header $
         else results = [ results, header]
         for j=0,n_elements(rund)-1 do $
         results = [results, string(rund[j], obscount[j], sky[j], stat[j], $
                     FORMAT="(a6,4x,i5,4x,a8,2x,a12)") ]
         results = [  results, footer]
      endif
   endfor
   jdstr, systime(/JULIAN), 0, tis
   if totlines gt 0 and ndbs gt 1 then begin 
       footer = [' ', tis + string( object, totlines, $
                            FORMAT="(1x,a, ': ', i5, ' total lines')"),  ' ' ]
      results = [  results, footer]
   endif
end


pro reduc_opdisplay,state, oprule
   if (*state).vrbsrules then print, ' reduc_opdisplay ', (*state).rundate
   selnms = ['']
   if  oprule.object ne (*state).na_str and not (*state).rdonly then begin
      widget_control,(*state).objectid,SET_VALUE=oprule.object,/SENSITIVE
      widget_control, (*state).o1_lid, /SENSITIVE
   endif
   if  oprule.objser ne (*state).na_val and not (*state).rdonly then begin
      objserstr = string(oprule.objser)
      widget_control,(*state).objserid,SET_VALUE=strn(oprule.objser),/SENSITIVE
      widget_control, (*state).o2_lid, /SENSITIVE
   endif
   if  oprule.comp ne (*state).na_str and not (*state).rdonly then begin
      widget_control,(*state).compid,SET_VALUE=oprule.comp,/SENSITIVE
      widget_control, (*state).o3_lid, /SENSITIVE
   endif
   if  oprule.filcod1 ne (*state).na_str and not (*state).rdonly then begin
      filtcstr='Filter 1'
      if (*state).filtchoice ne 0  then begin
         filtcstr='Filter 2'
         fstrings = [oprule.filcod2, 'Filter 1']
      endif else begin
         fstrings = [oprule.filcod1]
         if oprule.filcod2 ne (*state).na_str then fstrings = [fstrings, 'Filter 2']
      endelse
      widget_control,(*state).filter2_1id,SET_VALUE=fstrings,/SENSITIVE
      widget_control,(*state).f2_1id,SET_VALUE=filtcstr,/SENSITIVE

      sstrings = ['U','B','V','R','I' ]
      if (*state).filtchoice ne 0  then begin
         curfilnm = oprule.filnam2 
         if oprule.stdfil2 ne (*state).na_str then begin
            for i=0, n_elements(sstrings)-1 do $
               sstrings[i]=sstrings[i] + '-' +reduc_mapfilcod(state,sstrings[i])
         endif
      endif else begin
         curfilnm = oprule.filnam1 
         if oprule.stdfil1 ne (*state).na_str then begin
            for i=0, n_elements(sstrings)-1 do $
               sstrings[i]=sstrings[i] + '-' +reduc_mapfilcod(state,sstrings[i])
         endif
      endelse
      if oprule.stdfil1 ne (*state).na_str then $
         z = where( curfilnm eq strmid(sstrings,0,1) and $
                strmid(sstrings,1,1) eq '-') $
      else $
         z = where( curfilnm eq strmid(sstrings,0,1))

      if z[0] lt 0 then sstrings = [curfilnm, sstrings]
      select = 0 > z[0]
      widget_control,(*state).filter2_2id,SET_VALUE=sstrings,/SENSITIVE, $
                      SET_COMBOBOX_SELECT=select

   endif

   if  oprule.color1[0] ne (*state).na_str and not (*state).rdonly then begin
      if oprule.op ne 'dp' then begin
         if (*state).colorchoice eq  0 then begin
            color= oprule.color1[0] + '-' + oprule.color1[1]
            othername='Color 2'
            widget_control, (*state).c11_lid, SET_VALUE='Color 1:',/SENSITIVE
         endif else begin
            color = oprule.color2[0] + '-' + oprule.color2[1]
            othername='Color 1'
            widget_control, (*state).c11_lid, SET_VALUE='Color 2:',/SENSITIVE
         endelse
         nc = n_elements((*state).colormenu)
         colorstrings = (*state).colormenu[0:nc-2]
         z = where( color eq (*state).colormenu)
         if z[0] lt 0 then begin
            colorstrings = [colorstrings, color]
            z[0] = n_elements(colorstrings)-1
         endif
         if oprule.color2[0] ne (*state).na_str then $
            colorstrings = [colorstrings,othername]
      
         widget_control,(*state).color1_id, SET_VALUE=colorstrings, $
                         SET_COMBOBOX_SELECT=z[0],/SENSITIVE
      endif else begin
         widget_control, (*state).c11_lid, SET_VALUE='Color:',/SENSITIVE
         widget_control,(*state).color1_id, /SENSITIVE
         nc = n_elements((*state).colormenu)
         ; note the wheres are not guaranteed.
         intrsect,where(strmid(oprule.dpcolor,0,1) eq  $
                                   strmid((*state).colormenu,0,1)), $
                      where(strmid(oprule.dpcolor,1,1) eq  $
                                   strmid((*state).colormenu,2,1)),z,nf
         ; if color is a member of colormenu use that, otherwise
         ; conjure up a new enry.
         colorstrings = [ (*state).colormenu[0:nc-2]]
         if nf le 0 or z[0] lt 0 then begin
            colorstrings = [ colorstrings, strmid(oprule.dpcolor,0,1) + $
                                         '-' +   strmid(oprule.dpcolor,1,1)]
            z[0]= nc -1
         endif
         widget_control,(*state).color1_id, SET_COMBOBOX_SELECT=z[0], $
         SET_VALUE=colorstrings,/SENSITIVE
      endelse
   endif
   if  oprule.colorvec[0] ne (*state).na_val and not (*state).rdonly then begin
      widget_control,(*state).colorvalid,SET_VALUE=strn(oprule.colorvec[0]), $
                      /SENSITIVE
      widget_control, (*state).cv1_lid, /SENSITIVE
   endif
   if  oprule.colorvec[1] ne (*state).na_val and not (*state).rdonly then begin
      widget_control,(*state).colorerrid,SET_VALUE=strn(oprule.colorvec[1]), $
                      /SENSITIVE
      widget_control, (*state).cv2_lid, /SENSITIVE
   endif

   ; this is somewhat adhoc for right now.
   if oprule.op eq 'tr' and not (*state).rdonly then begin
      widget_control, (*state).defbuttonid, /SENSITIVE
      widget_control, (*state).clrbuttonid, /SENSITIVE
   endif
   
   forced = [ oprule.ctforce,oprule.k2force,oprule.kforce,oprule.ktforce, $
               oprule.tdforce]
   forcedvalue = [ reduc_fstrn(oprule.ctvec[0],FORMAT="(F10.4)"), $
                   reduc_fstrn(oprule.k2vec[0],FORMAT="(F10.4)"), $
                   reduc_fstrn(oprule.kvec[0],FORMAT="(F10.4)"), $
                   '', oprule.tddate]
   forcederror = [ reduc_fstrn(oprule.ctvec[1],FORMAT="(F10.4)"), $
                   reduc_fstrn(oprule.k2vec[1],FORMAT="(F10.4)"), $
                   reduc_fstrn(oprule.kvec[1],FORMAT="(F10.4)"),$
                   '','']

   ; this reorders the forcing terms for convenience in the menu.
   ; selorder orders menu order for forcing terms -> canonical order,
   ; The canonical order for forcing terms is lexicographic like
   ; forced, above, or (*state).forcenm.
   selorder = oprule.selorder
   unselorder = oprule.unselorder

   ; find forcing terms that "exist" (for the rule) and activated, respectively.
   ; array exst and set are partial ordering arrays into canonical order
   exst= where( forced ne (*state).na_val)
   set= where( forced eq 1)
   if exst[0] ge 0 and not (*state).rdonly then begin
      ; selnms in menu order and is the subset applying to this rule.
      selnms = (*state).forcenm[unselorder[exst]]
      if set[0] ge 0 then begin
         ; mark active entries.
         selnms[unselorder[set]] = '*'+selnms[unselorder[set]] 
      endif
      widget_control, (*state).frcselid,SET_VALUE=selnms,/SENSITIVE, $ 
                         SET_DROPLIST_SELECT=(*state).frcselselect

      widget_control, (*state).frcactid, /SENSITIVE, SET_DROPLIST_SELECT= $
                                                   (*state).ifselactive

      ; Is the selected menu entry active?
      frcselindex= (*state).frcselindex
      z = where(frcselindex eq set)
      if z[0] ge 0 then begin
         (*state).ifselactive = 1
         ; entry activated so sensitize the error and value 
         ; windows as appropriate. 
         
         ; index of this entry in menu order.
         if (*state).forcedispl[frcselindex] gt 0 then begin
            widget_control, (*state).vid, /SENSITIVE, $
                           SET_VALUE= forcedvalue [frcselindex]
            widget_control, (*state).f41_lid, /SENSITIVE
         endif
         if (*state).forcedispl[frcselindex] gt 1 then begin
            widget_control, (*state).eid, /SENSITIVE, $
                          SET_VALUE= forcederror[frcselindex]
            widget_control, (*state).f42_lid, /SENSITIVE
         endif
      endif
   endif
   widget_control, (*state).runreducmagid, /SENSITIVE
   widget_control, (*state).runreducsigid, /SENSITIVE
   widget_control, (*state).selhardcopyid, /SENSITIVE
   ; db queries for detail display- note that this code is
   ; redundant with displayrules.
   ;
   openmysql,dblun,(*state).transfdbhook[0]
   if oprule.op eq 'tr' then begin
   ; detail display of the transf entry for tr rules
      (*state).seltrquality=''

      query = [ 'select * from ' + (*state).transfdbhook[1], $
                   ' where rundate = ' + quote( (*state).rundate), $
                ' and  instrument = ' + quote((*state).inst) ]

      query = [query, ' and  filter =  ' + $ 
                             quote(reduc_mapfilcod(state,oprule.stdfil1)) ]
      query = [query, ' and color1 =  ' +  $
                             quote(oprule.color1[0]) ]
      query = [query, ' and color2 =  ' +  $
                             quote(oprule.color1[1]) ]
      query = [query, ';' ]
      if (*state).vrbsqueries then print, ' query for detail tr: ', query
      mysqlquery, dblun, query,inst,rundate,utdate,f,c1,c2,k,k2,kc,zpt, $
                   kt,e_k,e_k2,e_kc,e_zpt,e_kt,jdref, $
                  f_k,f_k2,f_kc,f_zpt,f_kt,nobs,chi2,qual,comm, format= $
                  'a,a,a,a,a,a,f,f,f,f,f,f,f,f,f,f,d,a,a,a,a,a,i,f,a,a'
      if inst[0] ne '' then begin
         if f_k[0] eq 'fitted' then f_k = '' else f_k = 'f'
         if f_k2[0] eq 'fitted' then f_k2 = '' else f_k2 = 'f'
         if f_kc[0] eq 'fitted' then f_kc = '' else f_kc = 'f'
         if f_kt[0] eq 'fitted' then f_kt = '' else f_kt = 'f'
         if f_zpt[0] eq 'fitted' then f_zpt = '' else f_zpt = 'f'
         if comm[0] eq 'NULL' then comm=''

         (*state).seltrquality=qual

         detaildisp = [ (*state).inst + ' ' + (*state).transfdbhook[0] + '.' + $
                        (*state).transfdbhook[1] + ' record', $
                        utdate[0]+' in '+f[0]+' and '+c1[0]+'-'+c2[0], $
                        'K   ' + string(k[0], FORMAT="(F8.4)") + '  ' + $
                         string(e_k[0], FORMAT="(F8.4)") + '  ' + f_k, $
                        'K2  ' + string(k2[0], FORMAT="(F8.4)") + '  ' + $
                         string(e_k2[0], FORMAT="(F8.4)") + '  ' + f_k2, $
                        'CT  ' + string(kc[0], FORMAT="(F8.4)") + '  ' + $
                         string(e_kc[0], FORMAT="(F8.4)") + '  ' + f_kc, $
                        'ZPT ' + string(zpt[0], FORMAT="(F8.4)") + '  ' + $
                         string(e_zpt[0], FORMAT="(F8.4)") + '  ' + f_zpt, $
                        'KT  ' + string(kt[0], FORMAT="(F8.4)") + '  ' + $
                         string(e_kt[0], FORMAT="(F8.4)") + '  ' + f_kt, $
                        'JDREF:   ' + string(jdref[0], FORMAT="(D13.5)"), $
                         strn(nobs[0]) + ' obs ' + ' chi2 = ' + strn(chi2[0]), $
                        'Quality   ' + qual, $
                        comm ]
      endif else begin
         detaildisp = [ (*state).inst + ' phot.transf record', $
                         'No entry yet available', $
                         'for ' +(*state).rundate +' in '+ $
                           reduc_mapfilcod(state,oprule.stdfil1)+' and '+ $
                           oprule.color1[0]+  '-' + oprule.color1[1] ]
      endelse
      widget_control, (*state).detailid, SET_VALUE = detaildisp
   endif else begin
      ; get an int: count of obs for object and filter or (both) filters.
      query = [ 'select ObjName, filter, color from ' + (*state).datadbhook[1]   + $
                ' where ',  ' RefID = ' + quote( (*state).rundate + '-' + $
                                                   (*state).inst) ]
      colortest = '-'
      if oprule.op eq 'lc' or oprule.op eq '2c' then $
         colortest = oprule.color1[0] + '-' + oprule.color1[1]
      if oprule.op eq 'sl' then colortest = oprule.filnam1 + '-' + $
         oprule.filnam2
      if oprule.op eq 'dp' and oprule.filnam1 ne 'M' then $
         colortest = strmid(oprule.dpcolor,0,1) + '-' + $
                              strmid(oprule.dpcolor,1,1)
      if oprule.op ne 'sl' then $
         query = [query, ' and  ObjName =  ' + $
                         quote(repchar(oprule.object,'_',' '))] $
      else $
         query = [query,' and ObjName is not NULL', $
                        ' and ObjName not like '+quote('CAT:%')]

      query = [query, ' and   ( ']
      query = [query, ' filter =  ' + $ 
                             quote(oprule.filnam1) ]
      if oprule.op eq '2c' or oprule.op eq 'sl'  then $
         query = [query, ' or filter =  ' + $ 
                                   quote(oprule.filnam2) ]
      query = [query, ' ) ']
      query = [ query, ' and  color = ' + quote(colortest)]
      query = [query, '  order by ObjName ']

      query = [query, ';' ]

      if (*state).vrbsqueries then print, ' query for detail lctype: ', query
      mysqlquery, dblun, query, object, filter, color, format='(a,a)'
      slcnt = 0
      if object[0] ne '' then begin
         if oprule.op eq 'sl' then begin
            ; get first character of each obj name.
            indicator = strmid(object, 0, 1)
            ; this criterion matches reductor.
            ; It's been suggested 'e' can also denote asteroids.
            stars= where ( indicator ne 'a' and  $
                           indicator ne 'c' and indicator ne 'p')
            if stars[0] ge 0 then begin
               ; find stars in each filter.
               starsf1 = where( filter[stars] eq oprule.filnam1 and $
                                color[stars] eq colortest)
               starsf2 = where( filter[stars] eq oprule.filnam2 and $
                                color[stars] eq colortest)
               if starsf1[0] ge 0 and starsf2[0] ge 0 then begin
                  intrsect, object[starsf1],object[starsf2], stars2f
                  ; count all obs belonging to stars in intersection.
                  if n_elements(stars2f) gt 0 then begin
                     for i = 0,n_elements(stars2f)-1 do begin
                        ; is color enough?
                        z = where(stars2f[i] eq object[stars] and $
                                  colortest eq color[stars],count)
                        slcnt += count
                     endfor
                  endif
               endif
            endif
         endif else begin
            slcnt = n_elements(object)
         endelse
      endif
      detaildisp = [ (*state).inst + ' data table record' ]
      if slcnt gt 0 then begin
         detaildisp = [ detaildisp, strn(slcnt) + ' observations']
         if oprule.op eq 'sl' then $
            detaildisp=[ detaildisp, 'of '+strn(n_elements(stars2f))+ ' stars']
         detaildisp = [ detaildisp, 'already reduced.'] 
      endif else detaildisp = [ detaildisp, ' no observations reduced.'] 
      widget_control, (*state).detailid, SET_VALUE = detaildisp
   endelse
   free_lun, dblun
   
   ; the info display
   str0 = '(' + oprule.op + ') '
   str1 = ''
   str2 = ''
   str3 = ''
   case oprule.op of 
      'dp': begin
         str0 += ' Differential photometry'
         str1 += 'on ' + oprule.object
         if oprule.objser gt 0 then str1 += ':' + strn(oprule.objser)
         str1 +=  ' with ' + oprule.comp
         str2 += 'in ' + oprule.filnam1 + ' and ' + $
                strmid(oprule.dpcolor,0,1)+ '-' + strmid(oprule.dpcolor,1,1)
         str3 += reduc_infoforcing(state,oprule)
      end

      'tr': begin
          str0 += ' Landolt standards transformation'
          str1 += 'in ' + oprule.filnam1 + ' and ' + $
                 oprule.color1[0] + '-' + oprule.color1[1]
         str2 += reduc_infoforcing(state,oprule)
      end

      'lc': begin
          str0 += ' Single color lightcurve'
          str1 += 'on ' + oprule.object 
          if oprule.objser gt 0 then str1 += ':' + strn(oprule.objser)
          str1 += ' in ' + oprule.filnam1
          str2 += 'with ' + oprule.color1[0] + '-' + oprule.color1[1]
          str3='color is ' +reduc_fstrn(oprule.colorvec[0], FORMAT="(F8.3)") $
                 + ' , err ' + reduc_fstrn(oprule.colorvec[1], FORMAT="(F8.3)")
      end

      '2c': begin
          str0 += ' Two color lightcurve'
          str1 += 'on ' + oprule.object
          if oprule.objser gt 0 then str1 += ':' + strn(oprule.objser)
          str1 += ' in ' +oprule.filnam1 + ' and ' + $
                   oprule.filnam2
          str2 += 'with ' +  oprule.color1[0] + '-' + oprule.color1[1] + $
                  ' and ' + oprule.color2[0] + '-' + oprule.color2[1] 
      end

      'sl': begin
          str0 += ' Reducing stars to standard color'
          str1 += 'in '  + oprule.filnam1 + ' and ' + oprule.filnam2
      end

      else: begin
       end
   endcase

   widget_control, (*state).info0id, SET_VALUE=str0
   widget_control, (*state).info1id, SET_VALUE=str1
   widget_control, (*state).info2id, SET_VALUE=str2
   widget_control, (*state).info3id, SET_VALUE=str3
end

;  (*state).oprule is used to generate a set of rulestr
;  stored in  (*state).rulestr and these are further
;  translated to opline. It's important to note
;  the output rulestr are generated by somewhat different rules
;  and are not compatible with inputs. The main difference
;  is 
;  The output opline matches the input, on a split-join, save for precision
;  of float quantities and order of optional arguments.
pro reduc_oplinesjoin,state,opline
   oprule = (*(*state).oprule)
   rulestr = strarr(oprule.n_rulestr)

   i = 0
   ; note that if the field is n.a. it will be axed during the opline translat.
   rulestr[i]=oprule.op
   rulestr[++i]=oprule.object
   rulestr[++i]=strn(oprule.objser)
   rulestr[++i]=oprule.comp
   rulestr[++i]=oprule.filcod1
   rulestr[++i]=oprule.filnam1
   rulestr[++i]=oprule.stdfil1
   rulestr[++i]=oprule.filcod2
   rulestr[++i]=oprule.filnam2
   rulestr[++i]=oprule.stdfil2

   if oprule.op eq 'dp' then begin
      rulestr[++i]=oprule.dpcolor
      ++i
   endif else begin
      rulestr[++i]=reduc_mapfilcod(state,oprule.color1[0])
      rulestr[++i]=reduc_mapfilcod(state,oprule.color1[1])
   endelse

   if oprule.color2[0] eq (*state).na_str then i += 2 $
   else begin 
   rulestr[++i]=reduc_mapfilcod(state,oprule.color2[0])
   rulestr[++i]=reduc_mapfilcod(state,oprule.color2[1])
   endelse
   rulestr[++i]=reduc_fstrn(oprule.colorvec[0],FORMAT="(f10.4)")
   rulestr[++i]=reduc_fstrn(oprule.colorvec[1],FORMAT="(f10.4)")
   nmandatory = i+1
   if oprule.ctforce eq 1 then begin
      rulestr[++i]='ct'
      rulestr[++i]=reduc_fstrn(oprule.ctvec[0],FORMAT="(f10.4)")
      rulestr[++i]=reduc_fstrn(oprule.ctvec[1],FORMAT="(f10.4)")
   endif 
   if oprule.k2force eq 1 then begin
      rulestr[++i]='k2'
      rulestr[++i]=reduc_fstrn(oprule.k2vec[0],FORMAT="(f10.4)")
      rulestr[++i]=reduc_fstrn(oprule.k2vec[1],FORMAT="(f10.4)")
   endif
   if oprule.kforce eq 1 then begin
      rulestr[++i]='k'
      rulestr[++i]=reduc_fstrn(oprule.kvec[0],FORMAT="(f10.4)")
      rulestr[++i]=reduc_fstrn(oprule.kvec[1],FORMAT="(f10.4)")
   endif
   if oprule.ktforce eq 1 then rulestr[++i]='kt'
   if oprule.tdforce eq 1 then begin
      rulestr[++i]='td'
      rulestr[++i]=oprule.tddate
   endif
   noplast = i

   ; get out the mandatory arguments. (You can't decanonize any but
   ; these, since the optionals must be tested for flag = 1.)
   z = where(oprule.op eq (*state).ruleops) ; guaranteed because valid rule.
   ; the tranlate array orders rulestr->opstr.
   translate = sort(oprule.canoniz[0:nmandatory-1,z[0]])
   opstr = rulestr[translate]
   mandavail = nmandatory
   z = where(opstr eq (*state).na_str or opstr eq strn((*state).na_val))
   if z[0] ge 0 then mandavail = min([z, nmandatory])

   if noplast ge nmandatory then $
      opstr = [opstr[0:mandavail-1], rulestr[nmandatory:noplast]] $
   else $
      opstr = opstr[0:mandavail-1]
   opline = strjoin(opstr, ' ')

end


; split and canonize opline fields- rulestr is a strsplit(opline)
; that is, an output array of field strings from a single rule line
; processed with useful defaulting and reorganization.
; oprule is a similar output but organized as an anonymous structure
; with some numeric conversion. Oprule also holds a copy of
; the reordering arrays.
; Both of these outputs are generated in the parse and may both be useful.
;
; Basic observation is that the rules lines involve many of the same
; fields in different orders.  A raw strsplit of the opline can be reordered
; with a subscripts array according to the rule operation
; By choosing defaults such that one can distinguish
;      valid entry in a field appropriate to the command
;      invalid entry in a field appropriate to the command
;      unspecified entry in a field appropriate to the command
;      a field not appropriate to the command
; it is possible to parse, validate, and guide the widget processing
; in a relatively clean way.
;
; 2006/09/01  rulestr is now a member of state
; 2006/09/25  LOCALOPRULE keyword to return the oprule directly and leave
;             the copy in state alone. In fact, LOCALOPRULE obviates all
;             side effects to state.
; 
; 
pro reduc_oplinesplit, state, opline, LOCALOPRULE=localoprule
   ; mechanical split of opline
   opstr = strsplit(opline,/EXTRACT)
   nop = n_elements(opstr)
   ; oprule struct

   n_rulestr=50      ; a maximum dimension for rulestr.
   na = n_rulestr-1  ; this is a token meaning term does not apply to rule.
   opt = n_rulestr-2 ; this is a token meaning a forcing
                     ; term taken from the optional argument area

   ; set of reordering arrays themselves ordered by rule
   ; the first line for each rule is the mandatory arguments.
   ; The comments under the line correspond to the object field
   ; name. The value of each element is the field number in the
   ; reduc.inf file to populate that element of the rulestr
   ; the second and third only give, for each optional (forcing) field,
   ; whether or not the field is valid for the command. The 
   ; optargs and optarglen arrays defined below encode how to parse 
   ; the optional area.
   ; the canoniz array orders opstr->rulestr.
   ; NOTE: the rows of this matrix must be ordered like (*state).ruleops.
   canoniz = [ $
       ; TR   
            [ 0, na, na,   na,    1,2,3, na,na,na,   4,5,  na,na,    na,na, $
            ; op obj ser  comp    fil1*   fil2*      col1  col2      colvec
              opt,opt,opt,          opt,opt,opt,     na,na,na, $
            ; ctforce/vec           k2force/vec     kforce/vec  
                 opt,     na,na    ], $
            ;   ktforce  td/date
       ; SL   
            [ 0, na, na,   na,    1,3,5, 2,4,6,     na,na,  na,na,      na,na, $
            ; op obj ser  comp    fil1*   fil2*      col1     col2      colvec
              na,na,na,             na,na,na,       na,na,na, $
            ; ctforce/vec           k2force/vec     kforce/vec  
                 na,     na,na    ], $
            ;   ktforce  td/date
       ; 2C
            [ 0,  1, 2,    na,     3,4,5,  8,9,10, 6,7,   11,12,     na,na, $
            ; op obj ser  comp      fil1*  fil2*  col1   col2       colvec
                 na,na,na,              na,na,na,      na,na,na,  $
            ;   ctforce/vec           k2force/vec     kforce/vec 
               na,        na,na ], $
            ; ktforce    td/date
       ; LC 
            [ 0, 1,   2,   na,    3,4,5,  na,na,na,  6,7,  na,na,    8,9, $
            ; op obj ser  comp    fil1*   fil2*      col1  col2      colvec
              na,na,na,             na,na,na,       na,na,na,    $
            ; ctforce/vec           k2force/vec     kforce/vec  
                 na,     na,na    ], $
            ;   ktforce  td/date
       ; DP
            [ 0,  4, 5,     7,    1,2,na, na,na,na,  3,3,  na,na,    6,na, $
            ; op obj ser  comp    fil1*   fil2*      col1  col2      colvec
                opt,opt,opt,          opt,opt,opt,     opt,opt,opt, $
            ;   ctforce/vec           k2force/vec     kforce/vec  
                na,       opt,opt] $
            ;   ktforce  td/date
            ]

   ; this array gives the names of the forcing terms in canonical order,
   ; which also matches their order in oprule and rulestr.
   optargs=['ct','k2','k','kt','td']

   ; this array is in the same order as (*state).ruleops and indicates the 
   ; forcing argument in the optargs array to be pushed to the top in the
   ; forcing menu for that rule. In effect, the value from optops
   ; for a rule is used to index a forcing term which is swapped 
   ; woth the 0th term (CT).
   opttops = [3,0,0,0,4]


   ; forcing terms arg count, in canonical order.
   optarglen= [3,3,3,1,2]; arg itself plus vector size

   ; for each rule, selorder converts menu order to canonical order,
   ; ie selorder[i] is the index of the ith menu entry in the canonical
   ; table. unselorder is the inverse indexing array.
   
   selorder = indgen(5)
   unselorder = indgen(5)

   ; reinitialized always here.
   oprule = { $
   valid:1, $
   op:(*state).us_str, $
   object:(*state).na_str, $
   objser:(*state).na_val, $
   comp:(*state).na_str, $
   filcod1:(*state).na_str, $  ; string, instrument dependent filter code
   filnam1:(*state).na_str, $  ; string, standard 1 letter filter name, like 'V'
   stdfil1:(*state).na_str, $  ; string, standard filter number, like '2'
   filcod2:(*state).na_str, $  ; string, instrument dependent filter code
   filnam2:(*state).na_str, $  ; string, standard 1 letter filter name, like 'V'
   stdfil2:(*state).na_str, $  ; string, standard filter number, like '2'
   color1:[(*state).na_str,(*state).na_str], $ ; standard 1 letter strings,
                                             ; like [ 'B', 'V']
   color2:[(*state).na_str,(*state).na_str], $ ; standard 1 letter strings,
                                             ; like [ 'B', 'V']
   dpcolor:(*state).na_str, $
   colorvec:[float((*state).na_val),float((*state).na_val)], $
   ctforce:(*state).na_val, $
   ctvec:[float((*state).na_val),float((*state).na_val)], $
   k2force:(*state).na_val, $
   k2vec:[float((*state).na_val),float((*state).na_val)], $
   kforce:(*state).na_val, $
   kvec:[float((*state).na_val),float((*state).na_val)], $
   ktforce:(*state).na_val, $
   tdforce:(*state).na_val, $
   tddate:(*state).na_str, $
   n_rulestr:n_rulestr, $
   optargs:optargs, $
   optarglen:optarglen, $
   opttop:0, $
   obscount:0,$
   selorder:selorder, $
   unselorder:unselorder, $
   canoniz:canoniz $
   }
   
   z = where( opstr[0] eq (*state).ruleops)
   if z[0] ge 0 then begin
      mandop = -1
      oprule.opttop = opttops[z[0]]
      ; rulestr canonically set to length long enough for everything.
      rulestr = replicate((*state).us_str,n_rulestr )
      rulestr[0:nop-1]=opstr[0:nop-1]
      ; a "cat's paw' to set up n.a. defaults
      rulestr[na] = (*state).na_str
      rulestr[opt] = (*state).us_str
      ; this reorders and applies the na defaults
      thecanon = canoniz[*,z[0]]

      ; the indices of valid optional fields for this rule in rulestr
      theopts = where(thecanon eq opt)
      ; the smallest index for a valid optional field in rulestr
      opt0 = min(theopts)
      if opt0 gt 0 then begin 
      ; now turn the 'opt' fields in the canon to identity elements.
         thecanon[theopts]=theopts
         ; the largest mandatory argument index in opstr (NOT rulestr)
         ; the where is guaranteed because some part of every rule is non-na.
         optopt0 = max(thecanon[where(thecanon[0:opt0-1] ne na)]) + 1
         if nop gt optopt0 then begin
            optopstr = opstr[optopt0:nop-1]
            for i =0, n_elements(optargs)-1 do begin
               argend = optarglen[i]-1
               z = where(optargs[i] eq optopstr, count)
               if count gt 1 or $
               ( count eq 1 and (z[0] + argend) ge (nop - optopt0)) then begin
                  oprule.valid = 0 
                  print, ' bad opt args ', count, opt0
               endif else if count gt  0 then $
                  rulestr[opt0:opt0+argend]= optopstr[z[0]:z[0]+argend]
               opt0 += optarglen[i]
            endfor
         endif
      endif
      if oprule.valid then begin
         rulestr  = rulestr[thecanon]

         ; fill out the oprule structure
         i = 0
         na_str = (*state).na_str
         oprule.op=rulestr[i]
         if rulestr[++i] ne na_str then oprule.object=rulestr[i]
         if rulestr[++i] ne na_str then oprule.objser=fix(rulestr[i])
         if rulestr[++i] ne na_str then oprule.comp=rulestr[i]
         if rulestr[++i] ne na_str then oprule.filcod1=rulestr[i]
         if rulestr[++i] ne na_str then oprule.filnam1=rulestr[i]
         if rulestr[++i] ne na_str then oprule.stdfil1=rulestr[i]
         if rulestr[++i] ne na_str then oprule.filcod2=rulestr[i]
         if rulestr[++i] ne na_str then oprule.filnam2=rulestr[i]
         if rulestr[++i] ne na_str then oprule.stdfil2=rulestr[i]

         if oprule.op eq 'dp' then begin
            oprule.dpcolor = strmid(rulestr[++i],0,2)
            oprule.color1 = ' '   ; so it is not 'na'
         endif else begin
         if rulestr[++i] ne na_str then oprule.color1= $
            [reduc_mapfilcod(state,rulestr[i]), $
             reduc_mapfilcod(state,rulestr[i+1])]
         endelse
         ++i

         if rulestr[++i] ne na_str then oprule.color2= $
            [reduc_mapfilcod(state,rulestr[i]), $
             reduc_mapfilcod(state,rulestr[i+1])]
         ++i
         if rulestr[++i] ne na_str then oprule.colorvec[0]=float(rulestr[i])
         if rulestr[++i] ne na_str then oprule.colorvec[1]=float(rulestr[i])
         if rulestr[++i] ne na_str then oprule.ctforce=(rulestr[i] eq 'ct')
         if rulestr[++i] ne na_str then oprule.ctvec[0]=float(rulestr[i])
         if rulestr[++i] ne na_str then oprule.ctvec[1]=float(rulestr[i])
         if rulestr[++i] ne na_str then oprule.k2force=(rulestr[i] eq 'k2')
         if rulestr[++i] ne na_str then oprule.k2vec[0]=float(rulestr[i])
         if rulestr[++i] ne na_str then oprule.k2vec[1]=float(rulestr[i])
         if rulestr[++i] ne na_str then oprule.kforce=(rulestr[i] eq 'k')
         if rulestr[++i] ne na_str then oprule.kvec[0]=float(rulestr[i])
         if rulestr[++i] ne na_str then oprule.kvec[1]=float(rulestr[i])
         if rulestr[++i] ne na_str then oprule.ktforce=(rulestr[i] eq 'kt')
         if rulestr[++i] ne na_str then oprule.tdforce=(rulestr[i] eq 'td')
         if rulestr[++i] ne na_str then oprule.tddate=rulestr[i]
      endif
   endif  else  oprule.valid = 0

   ; set up selorder arrays
   ; this reorders the forcing terms for convenience in the menu.
   selorder = indgen(5)
   t = selorder[oprule.opttop]
   ; all we want to do is ensure the opttop-th entry is at the top,
   ; which of course is specified rule by rule. It is done currently as a
   ; single transposition.
   if oprule.opttop ne 0 then selorder[oprule.opttop] = selorder[0]
   selorder[0]=t

   unselorder = selorder
   ; this 'inverts' selorder but as it turns out they are equal (since single 
   ; transposition).
   for j=0,n_elements(selorder)-1 do begin
      z = where( j eq selorder) ; guaranteed because transposed indgen array.
      unselorder[j]=z[0]
   endfor

   oprule.selorder = selorder
   oprule.unselorder = unselorder

   if n_elements( localoprule) gt 0  then localoprule = oprule $
   else begin 
      if ptr_valid((*state).oprule) then ptr_free,(*state).oprule
      (*state).oprule=ptr_new(oprule)
      if ptr_valid((*state).rulestr) then ptr_free,(*state).rulestr
      ; these side effects should happen elsewhere maybe in 'List'
      (*state).rulestr=ptr_new(rulestr)
      (*state).frcselindex = selorder[0] 
      (*state).frcselselect = 0
      (*state).ifselactive = (oprule.tdforce gt 0  or oprule.ktforce gt 0)
      (*state).filtchoice = 0
      (*state).colorchoice = 0
      (*state).partialcolor = ''
   endelse
end

; update the oplines list widget and the reduc.inf file with new edit.
; if NOWRITE is set the widget is updated but the reduc.inf file
; is not written, only read; in this case, the OPLINE argument is ignored.
; NOWRITE is used when database changes occur affecting the list widget,
; not associated with edit of a rules line.
pro reduc_oplineupdate,state,opline,NOWRITE=nowrite
   if n_elements(nowrite) eq 0 then nowrite=0  ;use badpar!
   ; note that the same order of oplines is guaranteed since no changes made.
   reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                    dumsky1,dumsky2,dumgain,dumrdnoise,oplines
   if not nowrite then oplines[(*state).ruleselindex]= opline

   ; redisplay (and reselect) the list widget.
   reduc_displayrules, state,oplines, SET_LIST_SELECT=(*state).ruleselindex

   if not nowrite then begin
      oplines += ' ok'
      if (*state).vrbsrulesedit then begin
         print, ' writing ', (*state).infofile, $
                ' to update rule line ', (*state).ruleselindex
         print, ' new rule line is ', opline
      endif
      wrreduc,addslash((*state).reducpath + (*state).rundate) + $ 
              (*state).infofile,duminst,dumddir,dumrundate,dumrad, $
              dumsky1,dumsky2,dumgain,dumrdnoise,oplines,/GUI
   endif
end

pro reduc_opnulldisplay,state
  if (*state).vrbsrules then print, ' reduc_opnulldisplay ', (*state).rundate
   widget_control,(*state).objectid,SENSITIVE=0
   widget_control, (*state).o1_lid, SENSITIVE=0
   widget_control,(*state).objserid,SENSITIVE=0
   widget_control, (*state).o2_lid,SENSITIVE=0
   widget_control,(*state).compid,SENSITIVE=0
   widget_control, (*state).o3_lid,SENSITIVE=0
   widget_control, (*state).f2_1id,SENSITIVE=0
   widget_control, (*state).filter2_1id,SENSITIVE=0
   widget_control, (*state).filter2_2id,SENSITIVE=0
   widget_control,(*state).color1_id,SENSITIVE=0
   widget_control,(*state).c11_lid,SENSITIVE=0
   widget_control,(*state).colorvalid,SENSITIVE=0
   widget_control, (*state).cv1_lid, SENSITIVE=0
   widget_control,(*state).colorerrid,SENSITIVE=0
   widget_control, (*state).cv2_lid, SENSITIVE=0
   widget_control, (*state).frcselid, SENSITIVE=0
   widget_control, (*state).frcactid, SENSITIVE=0
   widget_control, (*state).vid,SENSITIVE=0
   widget_control, (*state).eid,SENSITIVE=0
   widget_control, (*state).defbuttonid,SENSITIVE=0
   widget_control, (*state).clrbuttonid,SENSITIVE=0
   widget_control, (*state).f41_lid,SENSITIVE=0
   widget_control, (*state).f42_lid, SENSITIVE=0
   ; run reductor button
   widget_control, (*state).runreducmagid, SENSITIVE=0
   widget_control, (*state).runreducsigid, SENSITIVE=0
   widget_control, (*state).selhardcopyid, SENSITIVE=0
   ; the info display
   widget_control, (*state).info0id, SET_VALUE=''
   widget_control, (*state).info1id, SET_VALUE=''
   widget_control, (*state).info2id, SET_VALUE=''
   widget_control, (*state).info3id, SET_VALUE=''
   widget_control, (*state).detailid, SET_VALUE=''
end


;  display big 5 photometry parameters in the params area of the 4th line of
; the widget. It  sensitizes the windows so the parameters can be viewed but
; but makes them editable only when (*state).rdonly is 0.
pro reduc_paramsdisp,state
   editable = (*state).rdonly eq 0
   widget_control, (*state).objradid,/SENSITIVE,SET_VALUE=strn((*state).rad), $
                   EDITABLE=editable
   widget_control,(*state).skyrad1id,/SENSITIVE,SET_VALUE=strn((*state).sky1), $
                   EDITABLE=editable
   widget_control,(*state).skyrad2id,/SENSITIVE,SET_VALUE=strn((*state).sky2), $
                   EDITABLE=editable
   widget_control, (*state).gainid, /SENSITIVE,SET_VALUE=strn((*state).gain), $
                   EDITABLE=editable
   widget_control, (*state).rdnoiseid, /SENSITIVE, $
                   SET_VALUE=strn((*state).rdnoise),EDITABLE=editable
end

; display contents of third line of widget give data and reduction
; full paths. Note that the data dir field in reduc.inf and used by
; reductor does not include the rundate.
pro reduc_pathdisp, state
   ddir=''
   rdir = 'REDUCTION dir: ' + addslash((*state).reducpath) + (*state).rundate 

   if (*state).infoexists  then begin
      ddir = 'DATA dir: ' + addslash((*state).ddir) + (*state).rundate
      if not (*state).ddirexists then ddir += ' (absent)'
   endif

   widget_control, (*state).dpathid, SET_VALUE=ddir
   widget_control, (*state).rpathid, SET_VALUE=rdir
end

; this routine reads oplines and unconditionally strips all trailing 'ok'
; strings. It also sorts by rule and builds the array of rulecodes.
; the SAVELINENO keyword is a scalar int initially set to the ordinal of a 
; specific line (as currently located in the file on disk) and is returned 
; with the ordinal in the output line array which may change due to sorting.
pro reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                      dumsky1,dumsky2,dumgain,dumrdnoise,oplines, $
                      SAVELINENO=savelineno
   rdreduc, addslash((*state).reducpath + (*state).rundate) + $ 
           (*state).infofile,duminst,dumddir,dumrundate,dumrad, $
           dumsky1,dumsky2,dumgain,dumrdnoise,oplines,/GUI
   if n_elements(oplines) eq 1 and oplines[0] eq '' then return
   rulecodes = intarr(n_elements(oplines))
   for j=0,n_elements(oplines)-1 do  begin
      ops = strsplit(oplines[j],/EXTRACT)

      ; save a rule type index.
      x = where ( ops[0] eq (*state).ruleops) 
      if x[0] lt 0 then x[0] = n_elements((*state).ruleops) ; if unknown rule
      rulecodes[j] = x[0]

      z = where (ops ne 'ok')
      e = max(z)
      if z[0] ge 0 then oplines[j] = strjoin(ops[0:e], ' ') $
      else oplines[j]=''
   endfor

   ; sort oplines in order by rulecode. Note that unparsable rules go to end.
   ; we also need need to do some arithmetic with the ordinal to insure
   ; input order is preserved since IDL won't do that for us.
    ordering = sort(indgen(n_elements(oplines)) + rulecodes*n_elements(oplines))
    oplines = oplines[ordering]
   if ptr_valid((*state).rulecodes) then ptr_free,(*state).rulecodes
   (*state).rulecodes = ptr_new(rulecodes[ordering])
   if n_elements(savelineno) eq 1 then begin
     z = where(ordering eq savelineno) ; guaranteed because savelineno in range.
     savelineno = z[0]
   endif
end


pro reduc_reducdirprobe,state
   (*state).reduchidden = reduc_norundir(state,INFOEXISTS=infoexists, $
                                         WRITEABLE=reducwriteable, $
                                         DDIREXISTS=ddirexists, $
                                         DDIRECTORY=ddir, $
                                         CALIBEXISTS=calibexists)
   (*state).infoexists = infoexists
   (*state).reducwriteable = reducwriteable
   (*state).ddirexists = ddirexists
   (*state).ddir= ddir
   (*state).calibexists= calibexists
end

; keyword ALL if 1 forces out all rundates in all databases,
; otherwise it's according to the autostep variables stepquerySky
; and stepqueryStat.
pro reduc_rundatelist, state, qpattern,ALL=all,DBLIST=dblist

   if n_elements(all) eq 0 then all=0
   if (*state).vrbsrundates then begin 
      if all then print, ' rundate list on all' $
      else print, ' rundate list on autostep criteria'
   endif

   ; Otherwise query the sequence of db's and generate a search pattern of the
   ; form { xxxxx, xxxxx, .... } for nextfile.
   table = (*state).instdbhook[1] ; standard search table, normally 'runstat'.
   dbnmptrs = indgen(n_elements((*state).instdbhook)/2)*2 
   allrundates= [ '']
   dblist = ['']
   for i = 0, n_elements((*state).instdbhook)-1,2 do begin
      openmysql,dblun,(*state).instdbhook[i]
      query = reduc_autostepquery(state, (*state).instdbhook[i],all)
      if query ne '' then query = [  ' where '  +   query]
      query = [query, ' ;']
      query = [ ' select rundate from ' + table, query ]

      if (*state).vrbsqueries then print, ' query for rundate list: ', query
      mysqlquery, dblun, query, rundates,  format='a'
      n = 0 
      if rundates[0] ne '' then begin
         dbs = replicate((*state).instdbhook[i],n_elements(rundates))
         dblist = [dblist, dbs]
         allrundates  = [allrundates,rundates]
         n = n_elements(rundates)
      endif
      if (*state).vrbsrundates then print, n, ' rundates on ', $
         (*state).instdbhook[i] + '.' + table
      s = sort(allrundates)
      allrundates = allrundates[s]
      dblist = dblist[s]
      free_lun, dblun
   endfor
   ; break the rundates into two groups 1990-1999,2000-2089 (inclusive)
   last = [-1]
   if n_elements(allrundates) gt 1 then begin
      ; remove the cats paws
      allrundates = allrundates[1:*] 
      dblist = dblist[1:*] 

      first = where(strmid(allrundates,0,1) ge '9')
      if first [0] ge 0 then $
         last =where(strmid(allrundates,0,1) lt '9')
      qpattern = allrundates
      if last[0] ge  0 then begin
         qpattern = [allrundates[first],allrundates[last]]
         dblist = [dblist[first],dblist[last]]
      endif
   endif
end


; subevent is either 'Mag' or 'Sig'
; there must be a selected rule.
pro reduc_runreductor,state,subevent, hardcopy
   ruleselindex = (*state).ruleselindex
   reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                    dumsky1,dumsky2,dumgain,dumrdnoise,oplines
   ii = indgen(n_elements(oplines))
   if n_elements(oplines) gt 1 then $
      oplines[where( ii ne ruleselindex)] += ' ok' ; guaranteed by indgen
   
   if (*state).vrbsreductor then print, ' writing ', (*state).infofile, $
         ' to run rule line ', ruleselindex, oplines[ruleselindex]
   (*state).ruleselindex = ruleselindex 
   wrreduc, addslash((*state).reducpath + (*state).rundate) + $ 
           (*state).infofile,duminst,dumddir,dumrundate,dumrad, $
           dumsky1,dumsky2,dumgain,dumrdnoise,oplines,/GUI
   magresid = subevent eq 'Mag'
   dverbose = 0
   if (*state).vrbsqueries or (*state).vrbsupdate then $
      dverbose = 1
   if (*state).vrbsdbphot then dverbose = 3
   if (*state).vrbsreductor then print, 'running reductor'
   pushd,  addslash((*state).reducpath) + (*state).rundate 
   ; db keywords are passed in orig form and expanded below.
   reductor,MAGRESID=magresid,ERRSTR=errstr, PRINT=hardcopy, $
            DATATABLE=(*state).datatable, $
            TRANSFTABLE=(*state).transftable, $
            NOSAVE=(*state).rdonly,/GUI,DVERBOSE=dverbose
   popd
   if errstr[0] ne '' then begin
      qsize = n_elements( errstr) <  (*state).maxqsize
      r = qannounc(errstr, FALSELABEL='', $
                           YSIZE=(qsize > (*state).minqsize), $
                           GROUP_LEADER=(*state).mainbase)
   endif

   ;widget_control, (*state).rbid, SET_VALUE=(*state).rbmsg
   ;(*state).ruleselindex = ruleselindex
   ; widget_control, (*state).rbid, SET_VALUE=(*state).rbmsg

   ; refresh displays with a selrule.
   ; save state of frc term select and activate.
   ; so the force term menu state is preserved across 'run reductor'
   frcselindex = (*state).frcselindex
   frcselselect = (*state).frcselselect
   ifselactive = (*state).ifselactive
   reduc_newrunstates,state
   reduc_toplinesdisp, state,/SELECT_RULE ; 
   ;reduc_selrule,state,ruleselindex ; so rule stays selected.
   (*state).frcselindex = frcselindex 
   (*state).frcselselect = frcselselect 
   (*state).ifselactive = ifselactive 
   ; rerefresh lower part of the gui.
   reduc_opdisplay,state,(*(*state).oprule)
end

pro reduc_runstatdisp, state
   if (*state).rundatexists  then $
      widget_control,(*state).runstatid,SET_VALUE='Sky: '+(*state).runstatsky+ $
      ' Status: '+(*state).runstatstat
end

; This selects a rule out of the already populated list widget,
; and sets up all the right displays. It is not assumed the rule is valid.
; This is the meat of the 'List' event.
pro reduc_selrule,state,ruleselindex
   (*state).ruleselindex = ruleselindex
   (*state).ruleselected = 1
   ; need to read info file first to get a copy of oplines
   ; need to get the cd's under control!
   reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                    dumsky1,dumsky2,dumgain,dumrdnoise,oplines
   if (*state).vrbsrules then print, 'Current opline ',oplines[ruleselindex]
   reduc_oplinesplit, state,oplines[ruleselindex]
   oprule = (*(*state).oprule)
   reduc_opnulldisplay,state

   if oprule.valid then reduc_opdisplay,state,oprule $
   else begin
      str0 = 'Rules line cannot be parsed'
      str1 = 'Use edit menu to delete current rule' 
      str2 = 'You may then add a new'
      str3 = 'version of the same type of rule if you like'
      widget_control, (*state).info0id, SET_VALUE=str0
      widget_control, (*state).info1id, SET_VALUE=str1
      widget_control, (*state).info2id, SET_VALUE=str2
      widget_control, (*state).info3id, SET_VALUE=str3
   endelse
end

; selects a new rundate based on subevent, which is among
; ['Enter','Choose','First','Last','Next','Prev']
; and sets state appropriately. For 'Enter' and 'Choose' it
; will post qinput and picker modal widgets, respectively.
; It does not redisplay anything in the reduc widget.
; (*state).reduchidden is cleared or set according to whether
; the reduction directory exists. Note that (*state).reduchidden
; may also be modified by runstatdisp when the display happens.
; new is set 1 if the operation succeeded and a new rundate was
; selected. (Which could be the same as the old- new is set zero 
; if the operation fails, like 'Next' on the last rundate.)
; Notes from any previous rundate are saved to database if a new 
; rundate is selected.
pro reduc_selrundate, state,subevent,new
   reduc_rundatelist,state, qpattern, ALL=(subevent eq 'Enter'),DBLIST=dblist
   rundate=''
   new = 0
   nq = n_elements(qpattern)
   if nq gt 0 then begin
      if subevent eq 'Enter' then begin 
         rundate= qinput(PROMPT='please type rundate',/STRING, $
                         GROUP_LEADER=(*state).mainbase)
         rundate = rundate[0]
         if rundate eq '' then return
         z = where(rundate eq qpattern)
         if z[0] lt 0 then begin
             ; the autostep criteria are needed for the bestdate function.
            reduc_rundatelist,state, qpattern,DBLIST=dblist
            if n_elements(qpattern) eq 0 then begin
               print, 'No rundates available!' + (*state).bel
               return
            endif
            rundate = reduc_bestdate(state,rundate,'',qpattern)
         endif
      endif
      if subevent eq 'Choose' then begin
         rundate = picker(qpattern + ' ' + dblist) 
         if (*state).vrbsrundates then print, ' choosing: ',rundate
         rundate = strsplit(rundate,' ',/EXTRACT)
         rundate = rundate[0]
      endif
      if subevent eq 'First' then rundate = qpattern[0] 
      if subevent eq 'Last' then rundate = qpattern[nq-1]  
      if subevent eq 'Next' or subevent eq 'Prev' then begin
         lastrundate = (*state).rundate
         if n_elements(qpattern) ne 0 then begin
            if lastrundate ne '' then begin
               z = where( lastrundate eq qpattern)
               if z[0] lt 0 then begin
                  rundate=reduc_bestdate(state,lastrundate,subevent,qpattern)
               endif else begin
                  if subevent eq 'Next' and $
                     z[0] lt n_elements(qpattern)-1 then $
                     rundate = qpattern[z[0] + 1] $
                  else if subevent eq 'Prev'  and z[0] gt 0 then $
                     rundate = qpattern[z[0] - 1] $
                     else rundate = ''  ; off the end
                  endelse
            endif else if subevent eq 'Prev' then $
               rundate = qpattern[n_elements(qpattern)- 1] $
               else rundate = qpattern[0]
         endif 
      endif
      ; this test is needed to filter out [[[CANCEL from picker.
      if not stregex(rundate,'[09].*', /BOOLEAN) and rundate ne '' then return
      if rundate ne '' then begin
         if (*state).rundate ne '' then reduc_notessave,state
         z = where(rundate eq qpattern) ; guaranteed - all rundates in qpattern
         (*state).runstatdb = dblist[z[0]]

         ; infer instrument from db name.
         z=where((*state).runstatdb eq (*state).dbnames);
         if z[0] ge 0 then (*state).inst = (*state).dbinst[z[0]] $
         else (*state).inst = (*state).dbinst[0] ; if custom dbname

         (*state).rundate = rundate
         (*state).rundatexists = 1
         (*state).ruleselected = 0
         reduc_newrunstates, state
         reduc_newrundisp,state
         new = 1
      endif
   endif
   if rundate eq '' then begin
      if subevent eq 'Next' then begin
         print, 'Already at last rundate!' + (*state).bel
      endif else begin 
         if subevent eq 'Prev' then $
            print, 'Already at first rundate!'+(*state).bel $
         else print, 'No rundates available!' + (*state).bel
      endelse
   endif
end

; WHAT is any of the valid column enum values for transf.quality
; The routine checks if you are in browse mode, and forbids the
; operation wih a dismissive qannounc if so. It forbids, in similar
; fashion, performing the operation without a tr rule selected
; that has a current transf table record in place.
; If the transition is into 'good' or 'bad'  you get a qannounc indicating the
; consequences and giving a chance to back out.
; The operation is performed with editquality followed by appropriate redisplay.
pro reduc_settransfqual, state,  what
   if (*state).vrbsdbedit then print, 'Set Transf.Quality: ',  ' to ', what
   if (*state).setbrowse then $
      r = qannounc('Can''t update transf in Browse Mode!',$
                   FALSELABEL='',GROUP_LEADER=(*state).mainbase) $
   else begin
      if (*state).reduchidden  or not (*state).ruleselected then begin
         r = qannounc((*state).no_trmsg_qual, FALSELABEL='', $
                       GROUP_LEADER=(*state).mainbase) 
      endif else begin
         oprule = (*(*state).oprule)
         if oprule.op ne 'tr' or (*state).seltrquality eq '' then $
            r = qannounc((*state).no_trmsg_qual, FALSELABEL='', $
                         GROUP_LEADER=(*state).mainbase) $
         else if (*state).seltrquality ne what then begin
            u = 1
            if what eq 'good' or what eq 'bad' then begin
               finalmsg= [ 'Setting transf.quality to ' + what + $
                          ' puts the transf record into a final state', $
                         'where it can no longer be updated by running tr.']
               if what eq 'bad' then finalmsg = [ finalmsg, $
                     ' As a bad record it will be invisible to other rules.']
               finalmsg = [ finalmsg, $
                          ' Do you still want to change the quality flag?' ]
               u = qannounc(finalmsg, TRUELABEL='Go ahead and update quality', $
                        FALSELABEL='No, abort this operation', $
                         GROUP_LEADER=(*state).mainbase)
            endif
            filt = reduc_mapfilcod(state,oprule.stdfil1)
            colors=oprule.color1
            if u then reduc_editquality,state,what,filt,colors
            reduc_oplineupdate,state,'',/NOWRITE
            reduc_opnulldisplay,state
            reduc_opdisplay,state,oprule
         endif else if (*state).vrbsdbedit then print, $
             'settransfqual: no change (' + what + ')'
      endelse
   endelse
end


; WHICH is in [Stat, Sky']
; WHAT is any of the valid column enum values that correspond.
; This routine performs various state transitions
; based on the new value, and (possibly) the
; previous value of the same field.
; the runstatdisp call at the end may cause other transitions,
; specifically, in (*state).reduchidden and (*state).rdonly.
pro reduc_setstatsky, state, which, what
   if (*state).vrbsdbedit then print, 'Attempted Set Runstat: ', which,  $
      ' to ', what
   badstatemsg = ''
   if not (*state).rundatexists then $
      badstatemsg = 'Can''t update ' +  which +  ' without a rundate!'
   if (*state).setbrowse then $
      badstatemsg = 'Can''t update ' +  which +  ' in Browse Mode!'
   if badstatemsg ne '' then $
      r = qannounc(badstatemsg, FALSELABEL='',GROUP_LEADER=(*state).mainbase) $
   else begin
      u = 1
      if which eq 'Stat' then begin
         if what eq 'bad' then reduc_nightbad, state,u
         ;if what eq 'cal' and (*state).runstatstat eq 'raw' then begin
         ;  calibmsg = 'There is no calib directory for this rundate.  Proceed?'
         ;   if (*state).ddirexists then begin
         ;      if not (*state).calibexists then u = 0
         ;   endif else begin
         ;      u = 0
         ;      calibmsg = 'Data directory absent for this rundate.  Proceed?'
         ;   endelse
         ;   if not u then u = qannounc( calibmsg, TRUELABEL='update runstat', $
         ;                     FALSELABEL="don't update runstat", $
         ;                     GROUP_LEADER=(*state).mainbase)
         ;endif
         if what eq 'reduced' and  $
             (*state).runstatstat ne 'reduced'  then begin
            if (*state).runstatstat ne 'in progress' then begin
               r = qannounc('Must transition to ''reduced''  via in progress', $
                          FALSELABEL='', $
                          GROUP_LEADER=(*state).mainbase)
               u = 0
            endif else begin
               infotxt = [  $
                  'You wish to mark this night reduced. This requires that', $
                  'all rules be rerun with /SAVEALLPLOTS without additional', $
                  'interaction. If any data points do not get processed or', $
                  'there are other errors flagged by reductor, you will get', $
                  'a chance to review them before the status is changed.']
               if (*state).reduchidden or (*state).unrun gt  0 then begin
                   u =  qannounc(['You wish to mark this night reduced.', $
                         'However, all reductor rules have not been run.', $
                         'Do not proceed unless you are sure all rules run.', $
                         '',infotxt], $
                     TRUELABEL = 'Yes, do final rules run for reduced status', $
                     FALSELABEL='No, abort operation, leave status unchanged', $
                     GROUP_LEADER=(*state).mainbase,ysize=n_elements(infotxt)+4)
               endif else begin
                  u = qannounc( infotxt,  $
                     TRUELABEL = 'Yes, do final rules run for reduced status', $
                     FALSELABEL='No, abort operation, leave status unchanged', $
                     GROUP_LEADER=(*state).mainbase,ysize=n_elements(infotxt))
               endelse
               if u then begin
                  dverbose = 0
                  if (*state).vrbsqueries or (*state).vrbsupdate then $
                     dverbose = 1
                  if (*state).vrbsdbphot then dverbose = 3
                  pushd, addslash((*state).reducpath) + (*state).rundate 
                  reductor,ERRSTR=errstr,DATATABLE=(*state).datatable, $
                           TRANSFTABLE=(*state).transftable, $
                          /SAVEALLPLOTS,/GUI,DVERBOSE=dverbose
                  (*state).ruleselected = 0 ; simplifies things.
                  statstr=  $
                   [ 'There were unexpected issues flagged by reductor',  $
                     'which was run in SAVEALLPLOTS mode. ' + $
                     'The reductor output is shown below.', $
                     'You must now decide if you still want to mark ', $
                      (*state).rundate + ' as reduced.', '']
                  qsize = n_elements([ errstr, statstr]) <  (*state).maxqsize
                  if errstr[0] ne '' then u = qannounc([ statstr, errstr],  $
                                         TRUELABEL='change Stat to reduced', $
                                         FALSELABEL= $
                                               'abort,leave Stat unchanged', $
                                         GROUP_LEADER=(*state).mainbase, $
                                         YSIZE=(qsize > (*state).minqsize))
                  if not u then print, 'user aborts runstat update!'
                  popd
               endif
            endelse
         endif
      endif else   $ ; 'Sky'
         if what eq 'non-phot' then reduc_nightbad, state, u
      if u then begin
         reduc_editrunstat, state, which, what
         reduc_newrunstates, state
         reduc_newrundisp,state
      endif
   endelse
end
; change the autostep mode for sky conditions.
; subevent is  in ['Not', 'Phot', 'Non-Phot', 'Unknown', 'Any']
; this code may be reworked in future. 
pro reduc_skystepmode,state, subevent
   if (*state).vrbsdbedit then  print, 'Sky Step Mode Event:', subevent

   ; update the label under the Next/Prev buttons.
   ; for sky, it is the string left of the coma
   widget_control,(*state).stepid,GET_VALUE=s
   stepstr = strsplit(s, ',',/EXTRACT)
   if subevent eq 'Not' then begin
      ; add 'Not' if 'Not' not already there.
      if strmid(stepstr[0],0,3) ne 'Not'  and $
         stepstr[0] ne 'Any' then $
         stepstr[0] = 'Not ' + stepstr[0]
   endif else stepstr[0] = subevent
   widget_control,(*state).stepid,SET_VALUE=strjoin(stepstr,',')

   ; update the saved sql query fragment for this field.
   if subevent eq 'Not' then begin
      stepquery = (*state).stepquerySky
      relat = strpos(stepquery,'=') ; location of relational op
      if relat lt 0 or strpos(stepquery,'!=') gt 0 then $
         print, " 'Not' not valid here!", (*state).bel $
      else stepquery = strmid(stepquery,0,relat) + '!' + $
                       strmid(stepquery,relat) ; turn = into !=
   endif else begin
      ; create query on this field or no query for 'Any'
      if subevent eq 'Any' then stepquery = '' $
      else stepquery = ' Sky = ' + quote(subevent) + ' '
   endelse
   (*state).stepquerySky = stepquery
end

; change the autostep mode for processing status.
; subevent is  in ['Not', 'Raw', 'Cal', 'In Progress', 'Reduced', 'Published',
;                  'Bad', 'Any']
; this code may be reworked in future. 
pro reduc_statstepmode,state, subevent
   if (*state).vrbsdbedit then  print, 'Status Step Mode Event:', subevent
   subeventstr = strsplit(subevent, ' ',/EXTRACT)

   ; update the label under the Next/Prev buttons.
   ; for status, it is the string right of the coma
   widget_control,(*state).stepid,GET_VALUE=s
   stepstr = strsplit(s, ',',/EXTRACT)
   if subevent eq 'Not' then begin
      ; add 'Not' if 'Not' not already there.
      if strmid(stepstr[1],0,3) ne 'Not'  and $
         stepstr[1] ne 'Any' then $
         stepstr[1] = 'Not ' + stepstr[1]
   endif else begin
      stepstr[1] = subeventstr[0]
      if n_elements(subeventstr) ge 2 then $
         if subeventstr[1] eq 'or' then $
            ; subevent is 'Cal or In Progress'
            stepstr[1] = '{'+stepstr[1]+':'+ $
                          strjoin(subeventstr[2:*],' ')+'}' $
         else $ ; subevent is ' In Progress'
            stepstr[1] += subeventstr[1]
   endelse
   widget_control,(*state).stepid,SET_VALUE=strjoin(stepstr,',')

   ; update the saved sql query fragment for this field.
   if subevent eq 'Not' then begin
      stepquery = (*state).stepqueryStatus
      stepquerystr= strsplit(stepquery, ' ',/EXTRACT)
      relat = where(stepquerystr eq '=') ;locations of relational op
      antirelat = where(stepquerystr eq '!=')
      if relat[0] lt 0 or antirelat[0] ge 0 then $
         print, " 'Not' not valid here!", (*state).bel $
      else begin
         ; invert query,DeMorgan strikes again.
         stepquerystr[relat] = '!='
         compound = where(stepquerystr eq 'or')
         if compound[0] ge 0 then stepquerystr[compound]  = 'and'
         stepquery=strjoin(stepquerystr,' ')
      endelse
   endif else begin
      if subevent eq 'Any' then stepquery = '' $
      else stepquery = ' Stat = ' + quote(subevent) + ' '
      compound = where( subeventstr eq 'or')
      if compound[0] ge 0 then begin  ; is not general, obviously
         stepquery = ' Stat = ' + quote('cal') + ' or '
         stepquery += ' Stat = ' + quote('in progress') 
         stepquery = ' ( ' + stepquery + ' ) '
      endif
   endelse
      (*state).stepqueryStatus = stepquery
end
                              


; redisplay the top lines of the widget  including the
; rules list and info display to its right. Note that
; the rules list is unconditionally deselected.
; The rule edit and detail display (widget bottom) is unaffected.
; You also must be in the reduction directory for this to work.
pro reduc_toplinesdisp, state,SELECT_RULE=select_rule
   ruleselindex = (*state).ruleselindex
   reduc_infodisp, state  ; this reads reduc.inf and sets up some state.
   if not (*state).reduchidden then begin
      widget_control, (*state).rephotid, $
                       SENSITIVE=((*state).rdonly eq 0 and (*state).infoexists)
      if n_elements(select_rule) gt 0 then begin
         (*state).ruleselindex=ruleselindex
          (*state).ruleselected = 1
          widget_control,(*state).ruleselectid, SET_LIST_SELECT=ruleselindex
      endif
      reduc_pathdisp, state
      if (*state).infoexists then reduc_paramsdisp, state
   endif
end
; this is a join on the transf and (current rundate)runstat tables.
pro reduc_transfreport, state, results
   results=['']
   db = (*state).transfdbhook[0] ; normally 'phot'
   trtable = (*state).transfdbhook[1] ; normally 'transf'
   rstable = (*state).runstatdb + '.' + (*state).instdbhook[1] ; 'runstat'
   datelimit = [-90,90]
   oprule = (*(*state).oprule)
   filt = reduc_mapfilcod(state,oprule.stdfil1)
   rundate = (*state).rundate
   colors=oprule.color1
   ; create sql date from rundate
   runUTdate = '20' +strmid(rundate,0,2) + '-' + strmid(rundate,2,2) + $
               '-' + strmid(rundate,4)
   if strmid(rundate,0,1) eq '9' then runUTdate = '19' + strmid(runUTdate,2)
   trlist =  trtable + '.rundate,k,k2,kcolor,zeropt,ktime,' + $
             'e_k,e_k2,e_kcolor,e_zeropt,e_ktime,jdref,' + $
             'f_k,f_k2,f_kcolor,f_zeropt,f_ktime,nobs,chi2,quality,sky,stat'

   openmysql,dblun,db
   query = ['select '+trlist, $
            ' from '+trtable+','+rstable]
               
   query = [query, $
            ' where filter='+quote(filt)+' and ' + $
                'color1='+quote(colors[0])+' and ' + $
                'color2='+quote(colors[1])+' and', $
                (*state).instdbhook[1] + '.rundate='+trtable+'.rundate and '+ $
                    'quality != '+quote('bad')+' and']

   if (*state).runstatdb eq 'pccdobs' then $
      query = [query, (*state).instdbhook[1] + '.inst='+quote('L')+' and']

   ; long line query
   query = [query,'  to_days(UTDate)-to_days('+quote(runUTdate)+')' + $
                    ' <= '+strn(datelimit[1])+' and '+ $
                    'to_days(UTDate)-to_days('+quote(runUTdate)+')' + $
                    ' >= '+strn(datelimit[0])+' and', $
                  '  instrument='+quote((*state).inst)+ $
                  ' order by jdref;']
   if (*state).vrbsqueries then print, ' query for transf report: ', query

   mysqlquery,dblun,query,date, $
              k,k2,kcolor,zeropt,ktime,e_k,e_k2,e_kcolor,e_zeropt,e_ktime, $
              jdref, f_k,f_k2, f_kcolor,f_zeropt, f_ktime,nobs,chi2, $
              quality, sky, stat, HEADS=colhdrs, FORMAT= $
              'a, f,f,f,f,f, f,f,f,f,f, d, a,a,a,a,a, i,f,a,a,a'
   free_lun, dblun

   if date[0] ne '' then  begin 
      header = [ 'use ' + db + ' ;' , query, ' ', $
                string( colhdrs[0:11],colhdrs[17:21],FORMAT= $
                "(a6,1x, 10(a8,2x), a13, 1x, a5, 1x,a5,1x,a7,a8,1x,a11)"), $
                 ' ']
      jdstr, systime(/JULIAN), 0, tis
      footer = [' ', tis +  ' TRANSF for ' + filt + ', ' + $
                colors[0] + '-' + colors[1] + $
                ' on ' + (*state).inst + ': ' + $
                strn(n_elements(date)) + ' entries.']
      results=[header]
      for j=0,n_elements(date)-1 do begin
         ; generate one character codes, blank or 'f', for the forcing terms.
         forcing = [f_k[j],f_k2[j],f_kcolor[j],f_zeropt[j],f_ktime[j]]
         fletr = replicate(' ', n_elements(forcing))
         z = where ( forcing eq 'forced')
         if z[0] ge 0 then fletr[z] = 'f'

         results = [results,string(date[j], k[j], fletr[0],k2[j], fletr[1], $
                                     kcolor[j], fletr[2],zeropt[j], fletr[3], $
                                     ktime[j], fletr[4],e_k[j],e_k2[j], $
                                   e_kcolor[j],e_zeropt[j],e_ktime[j], $
                                    jdref[j], nobs[j],chi2[j], $
                                   quality[j], sky[j],stat[j], FORMAT= $
          '(a6,1x, 5(f8.4,a1,1x), 5(f8.4,2x),d13.5, 1x, i5, 1x,f5.2,1x,a7,' + $
            '1x,a8,1x,a11)')]
      endfor
      results = [  results, footer]
   endif
end
pro reduc_eve, event

   ; get current state
   widget_control, event.top, GET_UVALUE=state

   (*state).top = event.top

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name,/HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   ; generally useful variables.
   if ptr_valid((*state).oprule) then oprule = (*(*state).oprule)
   bel = (*state).bel
   selhardcopy = (*state).selhardcopy
   (*state).selhardcopy = 0

   case event_name of

      'THE_MENU': begin
         case event.value of

            'Exit' : begin
               if (*state).rundate ne '' then reduc_notessave,state
               widget_control, event.top, /DESTROY
               return
            end

            'Add dbphot detail (long)': begin
               (*state).vrbsdbphot = reduc_quizverbosity(state,event.value)
            end

            'Add DB Edit': begin
               (*state).vrbsdbedit = reduc_quizverbosity(state,event.value)
            end

            'Add DB Queries': begin
               (*state).vrbsqueries = reduc_quizverbosity(state,event.value)
            end

            'Add DB Updates': begin
               (*state).vrbsupdate = reduc_quizverbosity(state,event.value)
            end

            'Add Reductor': begin
               (*state).vrbsreductor = reduc_quizverbosity(state,event.value)
            end

            'Add Rundates': begin
               (*state).vrbsrundates = reduc_quizverbosity(state,event.value)
            end

            'Add Rules': begin
               (*state).vrbsrules = reduc_quizverbosity(state,event.value)
            end

            'Add Rules Edit': begin
               (*state).vrbsrulesedit = reduc_quizverbosity(state,event.value)
            end

            'Add Rule: TR (Landolt transformation)': begin
               reduc_addrule,state, 'TR'
            end
         
            'Add Rule: SL (all sources color solution)': begin
               reduc_addrule,state, 'SL'
            end

            'Add Rule: 2C (2 color light curve)': begin
               reduc_addrule,state, '2C'
            end

            'Add Rule: LC (1 color light curve)': begin
               reduc_addrule,state, 'LC'
            end

            'Add Rule: DP (differential photometry)': begin
               reduc_addrule,state, 'DP'
            end

            'Add Rule: DP star clump': begin
               reduc_addruleclump,state
            end

            'All Observations (Autostep Criteria)': begin
                  reduc_objobsreport, state,'All',repstr
                  if repstr[0] eq '' then repstr='There were no results found!'
                  viewtext,repstr,printcmd='pspr'
            end

            'Basic Verbosity': begin
               (*state).vrbsrules = 1
               (*state).vrbsupdate = 1
               (*state).vrbsqueries = 0
               (*state).vrbsrulesedit = 0
               (*state).vrbsrundates = 1
               (*state).vrbsreductor = 1
               (*state).vrbsdbedit = 0
               (*state).vrbsdbphot = 0
            end

            'Minimal Verbosity': begin
               (*state).vrbsrules = 0
               (*state).vrbsupdate = 0
               (*state).vrbsqueries = 0
               (*state).vrbsrulesedit = 0
               (*state).vrbsrundates = 0
               (*state).vrbsreductor = 0
               (*state).vrbsdbphot = 0
            end



            'Choose Rundate': begin
               reduc_selrundate,state,'Choose'
            end

            'Clear CT': begin
               if (*state).reduchidden  or (*state).rdonly or $
                    not (*state).ruleselected then begin
                  msg = (*state).no_trmsg_def
                  if (*state).rdonly then msg = (*state).browsemsg_def
                  r = qannounc(msg,FALSELABEL='',GROUP_LEADER=(*state).mainbase)
               endif else begin
                  if (*(*state).oprule).op ne 'tr'  then $
                     r = qannounc((*state).no_trmsg_def, FALSELABEL='', $
                                  GROUP_LEADER=(*state).mainbase) $
                  else  begin
                     oprule.ctforce = 0
                     oprule.ctvec = [0.0, 0.0]
                     if  (*state).frcselindex eq  0 then $
                        (*state).ifselactive = 0
                     (*(*state).oprule) = oprule
                     reduc_oplinesjoin,state,opline
                     reduc_oplineupdate,state,opline
                     reduc_opnulldisplay,state
                     reduc_opdisplay,state,oprule
                  endelse
               endelse
            end

            'Clear K2': begin
               if (*state).reduchidden  or (*state).rdonly or $
                  not (*state).ruleselected then begin
                  msg = (*state).no_trmsg_def
                  if (*state).rdonly then msg = (*state).browsemsg_def
                  r = qannounc(msg,FALSELABEL='',GROUP_LEADER=(*state).mainbase)
               endif else begin
                  if (*(*state).oprule).op ne 'tr' then $
                     r = qannounc((*state).no_trmsg_def, FALSELABEL='', $
                                  GROUP_LEADER=(*state).mainbase) $
                  else  begin
                     oprule.k2force = 0
                     oprule.k2vec = [0.0, 0.0]
                     if  (*state).frcselindex eq  1 then $
                        (*state).ifselactive = 0
                     (*(*state).oprule) = oprule
                     reduc_oplinesjoin,state,opline
                     reduc_oplineupdate,state,opline
                     reduc_opnulldisplay,state
                     reduc_opdisplay,state,oprule
                  endelse
               endelse
            end

            'Default CT': begin
               if (*state).reduchidden  or (*state).rdonly or $
                  not (*state).ruleselected then begin
                  msg = (*state).no_trmsg_def
                  if (*state).rdonly then msg = (*state).browsemsg_def
                  r = qannounc(msg,FALSELABEL='',GROUP_LEADER=(*state).mainbase)
               endif else begin
                  if (*(*state).oprule).op ne 'tr' then $
                     r = qannounc((*state).no_trmsg_def, FALSELABEL='', $
                                  GROUP_LEADER=(*state).mainbase) $
                  else  begin
                     reduc_defct, state,ctvec,k2vec
                     oprule.ctforce = 1
                     oprule.ctvec = ctvec
                     if  (*state).frcselindex eq  0 then $
                        (*state).ifselactive = 1
                     (*(*state).oprule) = oprule
                     reduc_oplinesjoin,state,opline
                     reduc_oplineupdate,state,opline
                     reduc_opdisplay,state,oprule
                  endelse
               endelse
            end


            'Default K2': begin
               if (*state).reduchidden  or (*state).rdonly or $
                  not (*state).ruleselected then begin
                  msg = (*state).no_trmsg_def
                  if (*state).rdonly then msg = (*state).browsemsg_def
                  r = qannounc(msg,FALSELABEL='',GROUP_LEADER=(*state).mainbase)
               endif else begin
                  if (*(*state).oprule).op ne 'tr' then $
                     r = qannounc((*state).no_trmsg_def, FALSELABEL='', $
                                  GROUP_LEADER=(*state).mainbase) $
                  else  begin
                     reduc_defct, state,ctvec,k2vec
                     oprule.k2force = 1
                     oprule.k2vec = k2vec
                     if  (*state).frcselindex eq  1 then $
                        (*state).ifselactive = 1
                     (*(*state).oprule) = oprule
                     reduc_oplinesjoin,state,opline
                     reduc_oplineupdate,state,opline
                     reduc_opdisplay,state,oprule
                  endelse
               endelse
            end

            'Delete Current Rule' : begin
               if (*state).nrules eq 1 then  $
                  r = qannounc( 'Can''t remove last Rule', $
                               FALSELABEL='',GROUP_LEADER=(*state).mainbase) $
               else if not (*state).ruleselected then $
                  r = qannounc( 'No Rule Selected!', FALSELABEL='', $
                                GROUP_LEADER=(*state).mainbase) $
               else begin
                  if (*state).nrules gt 0 and not (*state).rdonly then begin
                     reduc_rdoplines, state,duminst,dumddir,dumrundate,dumrad, $
                                      dumsky1,dumsky2,dumgain,dumrdnoise,oplines
                     if (*state).nrules le 1 then begin
                        newoplines = [''] 
                     endif else begin 
                        iold = indgen((*state).nrules)
                        ; guaranteed by indgen array.
                        inew = where( iold ne (*state).ruleselindex)
                        newoplines = oplines[inew]
                     endelse
                     newoplines += ' ok'
                     if (*state).vrbsrules then $
                           print,' writing ',(*state).infofile, $
                           ' to delete rule line ',(*state).ruleselindex
                     wrreduc,addslash((*state).reducpath+(*state).rundate)+ $
                            (*state).infofile,duminst,dumddir,dumrundate, $
                            dumrad,dumsky1,dumsky2,dumgain,dumrdnoise, $
                            newoplines,/GUI
                     reduc_rdoplines, state,duminst,dumddir,dumrundate,dumrad, $
                                   dumsky1,dumsky2,dumgain,dumrdnoise,newoplines
                     (*state).nrules--
                     (*state).ruleselected = 0
                     ; redisplay (and unselect) the list widget and # of rules. 
                     reduc_infodisp, state
                     reduc_opnulldisplay,state
                  endif else r=qannounc( $
                                  'Can''t delete rules in browse/nosave mode!', $
                                            FALSELABEL='', $
                                            GROUP_LEADER=(*state).mainbase)
               endelse
            end

            'Disable KT Fit': begin
               if (*state).reduchidden  or (*state).rdonly or $
                  not (*state).ruleselected then begin
                  msg = (*state).no_trmsg_def
                  if (*state).rdonly then msg = (*state).browsemsg_def
                  r = qannounc(msg,FALSELABEL='',GROUP_LEADER=(*state).mainbase)
               endif else begin
                  if (*(*state).oprule).op ne 'tr' then $
                     r = qannounc((*state).no_trmsg_def, FALSELABEL='', $
                                  GROUP_LEADER=(*state).mainbase) $
                  else  begin
                     oprule.ktforce = 0
                     if  (*state).frcselindex eq  3 then $
                        (*state).ifselactive = 0
                     (*(*state).oprule) = oprule
                     reduc_oplinesjoin,state,opline
                     reduc_oplineupdate,state,opline
                     reduc_opdisplay,state,oprule
                  endelse
               endelse
            end
            
            'Display All Keywords': begin
               jdstr, systime(/JULIAN), 0, tis
          
               vk= [ string('REDUC keywords: ', tis), '', $
                     string('BROWSE= ', (*state).browse), $
                     string('DATATABLE= ', (*state).datatable), $
                     string('DIR= ', (*state).dir), $
                     string('INSTDB= ', (*state).instdb), $
                     string('INSTDEFTABLE= ', (*state).instdeftable), $
                     string('NOTESXSIZE= ', (*state).notesxsize), $
                     string('NOTESYSIZE= ', (*state).notesysize), $
                     string('TRANSFTABLE= ', (*state).transftable)]
               viewtext,vk, PRINTCMD='pspr'
            end

            'Display Sources Provenance': begin
               help, output=vh,/SOURCE_FILES
               jdstr, systime(/JULIAN), 0, tis
               vh = [string('REDUC sources provenance: ', tis), '', vh]
               viewtext,vh, PRINTCMD='pspr'
            end

            'Dump State': begin
               s = (*state)
               help, s,OUTPUT=vh,/STRUCTURE
               jdstr, systime(/JULIAN), 0, tis
               vh = [string('REDUC STATE structure: ', tis), '', vh]
               viewtext,vh, PRINTCMD='pspr'
            end

            'Dump Oprule': begin
               if ptr_valid((*state).oprule) then begin 
                  op = (*(*state).oprule)
                  help, op,OUTPUT=vh,/STRUCTURE,/FULL
               endif else vh = 'There is no oprule currently loaded.'
               jdstr, systime(/JULIAN), 0, tis
               vh = [string('REDUC OPRULE structure: ', tis), '', vh]
                  viewtext,vh, PRINTCMD='pspr'
            end

            'Edit Log1 Bad Flags': begin
               if (*state).reduchidden  or (*state).rdonly or $
                  not (*state).rundatexists then  $
                  r = qannounc( $
                      ['Can''t edit bad flags unless you are in edit mode, ', $
                       ' with runstat ''in progress'' and rundir writeable' ] $
                         ,FALSELABEL='',GROUP_LEADER=(*state).mainbase) $
               else begin
                  log1file = addslash((*state).reducpath + (*state).rundate) + $
                             (*state).rundate + '.log1'
                  if not file_test(log1file,/WRITE) then $
                     r = qannounc( $
                         'Can''t edit bad flags, log1 file not writeable: ' + $
                          log1file, $
                         FALSELABEL='',GROUP_LEADER=(*state).mainbase) $
                  else $
                     plpedit,/MODAL,FILE=log1file, GROUP=(*state).mainbase
               endelse
            end
               

            'Enable KT Fit': begin
               if (*state).reduchidden  or (*state).rdonly or $
                  not (*state).ruleselected then begin
                  msg = (*state).no_trmsg_def
                  if (*state).rdonly then msg = (*state).browsemsg_def
                  r = qannounc(msg,FALSELABEL='',GROUP_LEADER=(*state).mainbase)
               endif else begin
                  if (*(*state).oprule).op ne 'tr' then $
                     r = qannounc((*state).no_trmsg_def, FALSELABEL='', $
                                  GROUP_LEADER=(*state).mainbase) $
                  else  begin
                     oprule.ktforce = 1
                     if  (*state).frcselindex eq  3 then $
                        (*state).ifselactive = 1
                     (*(*state).oprule) = oprule
                     reduc_oplinesjoin,state,opline
                     reduc_oplineupdate,state,opline
                     reduc_opdisplay,state,oprule
                  endelse
               endelse
            end

            'Enter Browse Mode': begin
               (*state).setbrowse = 1
               (*state).rdonly = 1
               widget_control, (*state).browseid, SET_VALUE=(*state).browse_msg
              if (*state).rundatexists then begin
                  reduc_newrundisp,state
               endif
            end

            'Enter Edit Mode': begin
               if (*state).browse then begin
                  r = qannounc('Cannot enter edit mode with BROWSE keyword set', $
                  FALSELABEL='', GROUP_LEADER=(*state).mainbase) 
               endif else begin
                  (*state).setbrowse = 0
                  widget_control, (*state).browseid, SET_VALUE=(*state).edit_msg
                  if (*state).rundatexists then begin
                     reduc_newrunstates,state
                     reduc_newrundisp,state
                  endif
               endelse
            end

            
            'Enter Rundate': begin
               reduc_selrundate,state,'Enter'
            end

            'First Rundate': begin
               reduc_selrundate,state,'First'
            end

            'Image List (Current Rundate)': begin
               if (*state).rundate ne '' then begin
                  reduc_imglistreport,state,repstr
                  if repstr[0] eq '' then $
                     repstr='There were no results found!'
                  viewtext,repstr,printcmd='pspr'
               endif else $
                  r = qannounc('You must first select a rundate', $
                  FALSELABEL='', GROUP_LEADER=(*state).mainbase) 
            end


            'Last Rundate': begin
               reduc_selrundate,state,'Last'
            end

            'Observations By Object (Autostep Criteria)': begin
               object = qinput(PROMPT='Enter Object Name',/STRING, $
                               GROUP_LEADER=(*state).mainbase)
               if object[0]  ne '' then begin
                  reduc_objobsreport, state,object[0],repstr
                  if repstr[0] eq '' then repstr='There were no results found!'
                  viewtext,repstr,printcmd='pspr'
               endif
            end

            'Run Reductor(SAVEALLPLOTS)': begin
               if  (*state).runstatstat ne 'in progress'  or $
                   (*state).reduchidden or (*state).rdonly then begin
                   r = qannounc(['You cannot run reductor in this mode.', $
                             'The rundate must have a valid directory', $
                           'and be ''in progress'',', $
                             'and you  must be in edit mode' ], FALSELABEL='', $
                             GROUP_LEADER=(*state).mainbase)
               endif else begin
                  if (*state).unrun gt 0 then begin
                     r = qannounc( $
                            'All reductor rules must already be run', $
                             FALSELABEL='', $
                             GROUP_LEADER=(*state).mainbase)
                  endif else begin
                     dverbose = 0
                     if (*state).vrbsqueries or (*state).vrbsupdate then $
                        dverbose = 1
                     if (*state).vrbsdbphot then dverbose = 3
                     pushd, addslash((*state).reducpath) + (*state).rundate 
                     if (*state).vrbsreductor then $
                        print, ' run reductor saveallplots'
                     reductor,ERRSTR=errstr,DATATABLE=(*state).datatable, $
                              TRANSFTABLE=(*state).transftable, $
                             /SAVEALLPLOTS,/GUI,DVERBOSE=dverbose
                     (*state).ruleselected = 0 ; simplifies things.
                     if errstr[0] ne '' then begin
                       qsize = n_elements( errstr) <  (*state).maxqsize
                        r = qannounc(errstr,  FALSELABEL='', $
                                         YSIZE=(qsize > (*state).minqsize), $
                                         GROUP_LEADER=(*state).mainbase)
                     endif
                     popd
                  endelse
               endelse
            end

            'Set Quality to: unknown': begin
               reduc_settransfqual,state, 'unknown'
            end

            'Set Quality to: bad': begin
               reduc_settransfqual,state, 'bad'
            end

            'Set Quality to: suspect': begin
               reduc_settransfqual,state, 'suspect'
            end

            'Set Quality to: good': begin
               reduc_settransfqual,state, 'good'
            end

            'Set Sky to: phot': begin
               reduc_setstatsky,state,'Sky','phot'
            end

            'Set Sky to: non-phot': begin
               reduc_setstatsky,state,'Sky','non-phot'
            end

            'Set Sky to: unknown': begin
               reduc_setstatsky,state,'Sky','unknown'
            end

            'Set Stat to: raw': begin
               reduc_setstatsky,state,'Stat','raw'
            end

            'Set Stat to: cal': begin
               reduc_setstatsky,state,'Stat','cal'
            end

            'Set Stat to: in progress': begin
               reduc_setstatsky,state,'Stat','in progress'
            end

            'Set Stat to: reduced': begin
               reduc_setstatsky,state,'Stat','reduced'
            end

            'Set Stat to: published': begin
               reduc_setstatsky,state,'Stat','published'
            end

            'Set Stat to: bad': begin
               reduc_setstatsky,state,'Stat', 'bad'
            end

            'Sky Not': begin
               reduc_skystepmode, state, 'Not'
            end

            'Sky Phot': begin
               reduc_skystepmode, state, 'Phot'
            end

            'Sky Non-Phot': begin
               reduc_skystepmode, state, 'Non-Phot'
            end

            'Sky Unknown': begin
               reduc_skystepmode, state, 'Unknown'
            end

            'Sky Any': begin
               reduc_skystepmode, state, 'Any'
            end

            'Status Any': begin
               reduc_statstepmode,state,'Any'
            end

            'Status Raw': begin
               reduc_statstepmode,state,'Raw'
            end

            'Status Cal': begin
               reduc_statstepmode,state,'Cal'
            end

            'Status In Progress': begin
               reduc_statstepmode,state,'In Progress'
            end

            'Status Cal or In Progress': begin
               reduc_statstepmode,state,'Cal or In Progress'
            end

            'Status Reduced': begin
               reduc_statstepmode,state,'Reduced'
            end

            'Status Published': begin
               reduc_statstepmode,state,'Published'
            end

            'Status Bad': begin
               reduc_statstepmode,state, 'Bad'
            end

            'Status Not': begin
               reduc_statstepmode,state,'Not'
            end

            'Transformation Records (select TR first)': begin
               if (*state).reduchidden  or $
                  not (*state).ruleselected then begin
                  r = qannounc((*state).no_trmsg_rep, FALSELABEL='', $
                                GROUP_LEADER=(*state).mainbase) 
               endif else begin
                  if (*(*state).oprule).op ne 'tr' then $
                     r = qannounc((*state).no_trmsg_rep, FALSELABEL='', $
                                  GROUP_LEADER=(*state).mainbase) $
                  else  begin
                     reduc_transfreport, state,repstr
                     if repstr[0] eq '' then $
                        repstr='There were no results found!'
                     viewtext,repstr,printcmd='pspr'
                  endelse
               endelse
            end

            'Void Notes Modifications': begin
               if (*state).vrbsdbedit then print, ' Void Notes Modifications'
               if (*state).rundate ne '' then reduc_notesrefresh,state
            end


            else: begin

               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Mainbase': begin

         ; Use if you have other widgets on screen, need to take off their
         ;   size from the event x,y size.
;         info=widget_info((*state).colbaseid,/geometry)
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize


;        refresh window here
      end

      'Color': begin
         if event.index eq -1 then begin
            ; an edited color only works in 'dp' and if it is of the form
            ; xy or x-y otherwise no change is made.
            if oprule.op eq 'dp' then begin
               rawcol = strtrim(event.str)
               if strlen(rawcol) eq 2 then $
                  oprule.dpcolor = rawcol
               if strlen(rawcol) eq 3 and strmid(rawcol,1,1) eq '-' then $
                  oprule.dpcolor = strmid(rawcol,0,1) + strmid(rawcol,2,1)
               (*(*state).oprule) = oprule
               reduc_oplinesjoin,state,opline
               reduc_oplineupdate,state,opline
               reduc_opdisplay,state,oprule
            endif
         endif else begin
            widget_control, (*state).color1_id, GET_VALUE=colorstrings
            seltag = colorstrings[event.index]
            if strpos(seltag,'Color') eq 0 then begin
               (*state).colorchoice = 1- (*state).colorchoice
               reduc_opdisplay,state,oprule
            endif else begin 
               rightness = strmid(seltag,0,1) eq '-'
               leftness = strmid(seltag,0,1,/REVERSE_OFFSET) eq '-'
               if not rightness then x = strmid(seltag,0,1)
               if not leftness then y = strmid(seltag,0,1, /REVERSE_OFFSET)
               if oprule.op eq 'dp' then begin
                  if not rightness then oprule.dpcolor =  $
                                        strmid(seltag,0,1) + $
                                       strmid(oprule.dpcolor,1,1)
                  if not leftness then oprule.dpcolor = $
                             strmid(oprule.dpcolor,0,1) + $
                             strmid(seltag,0,1, /REVERSE_OFFSET)
               endif else begin
                  if (*state).colorchoice eq 0 then begin
                     if not rightness then oprule.color1[0] = x
                     if not leftness then oprule.color1[1] = y
                  endif else begin
                     if not rightness then oprule.color2[0] = x
                     if not leftness then oprule.color2[1] = y
                  endelse
               endelse
               (*(*state).oprule) = oprule
               reduc_oplinesjoin,state,opline
               reduc_oplineupdate,state,opline
               reduc_opdisplay,state,oprule
            endelse
         endelse
      end


      'Clear CT,K2': begin
         oprule.ctforce = 0
         oprule.k2force = 0
         oprule.ctvec = [0.0, 0.0]
         oprule.k2vec = [0.0, 0.0]
         if  (*state).frcselindex le  1 then (*state).ifselactive = 0
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opnulldisplay,state
         reduc_opdisplay,state,oprule
      end

      'ColorErr': begin
         widget_control, event.id,GET_VALUE=strnew
         new=float(strnew)
         widget_control, event.id,SET_VALUE=reduc_fstrn(new, FORMAT="(F10.4)")
         oprule.colorvec[1] = new
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
      end

      'ColorVal': begin
         widget_control, event.id,GET_VALUE=strnew
         new=float(strnew)
         widget_control, event.id,SET_VALUE=reduc_fstrn(new, FORMAT="(F10.4)")
         oprule.colorvec[0] = new
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
      end

      'Comp': begin
         widget_control, (*state).compid, GET_VALUE=comp
         ; process out the blanks HERE!
         oprule.comp = nobname(strtrim(comp,2))
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
         
      end

      'Default CT,K2': begin
         reduc_defct, state,ctvec,k2vec
         oprule.ctforce = 1
         oprule.k2force = 1
         oprule.ctvec = ctvec
         oprule.k2vec = k2vec
         if  (*state).frcselindex le  1 then (*state).ifselactive = 1
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
      end

      'FiltStd': begin
         if event.index eq -1 then begin
            if oprule.stdfil1 eq (*state).na_str  then begin
               oprule.filnam1 = event.str
            endif
         endif else begin
            widget_control, (*state).filter2_2id,GET_VALUE=stdstr
            if (*state).filtchoice eq 0 then begin
               if oprule.stdfil1 ne (*state).na_str then begin
                  oprule.filnam1 = reduc_mapfilcod(state,strn(event.index))
                  oprule.stdfil1 = strn(event.index)
               endif else begin
                  oprule.filnam1 = stdstr[event.index]
               endelse
            endif else  begin
               oprule.filnam2 = reduc_mapfilcod(state,strn(event.index))
               if oprule.stdfil2 ne (*state).na_str then $
                  oprule.stdfil2 = strn(event.index)
            endelse
         endelse
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
         end

      'FiltStr': begin
         if event.index eq -1 then begin
            if event.str ne '' then begin
               filnam = reduc_mapfilcod(state,event.str)
               if (*state).filtchoice eq 0 then begin
                  oprule.filcod1 = event.str 
                  if filnam ne '' then begin
                     oprule.filnam1 = filnam
                     if oprule.stdfil1 ne (*state).na_str then $
                        oprule.stdfil1 = reduc_mapfilcod(state,filnam) 
                  endif
               endif else begin
                  oprule.filcod2 = event.str
                  if filnam ne '' then begin
                     oprule.filnam2 = filnam
                     if oprule.stdfil2 ne (*state).na_str then $
                        oprule.stdfil2 = reduc_mapfilcod(state,filnam) 
                  endif
               endelse
            endif
         endif
         if event.index eq 1 then (*state).filtchoice=1-(*state).filtchoice

         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
      end

      'FrcAct': begin
         ;print, ' FrcAct: event index ', event.index, ' frc sel index ', $
         ;        (*state).frcselindex
         if  (*state).frcselindex eq  0 then oprule.ctforce = event.index eq 1
         if  (*state).frcselindex eq  1 then oprule.k2force = event.index eq 1
         if  (*state).frcselindex eq  2 then oprule.kforce = event.index eq 1
         if  (*state).frcselindex eq  3 then oprule.ktforce = event.index eq 1
         if  (*state).frcselindex eq  4 then oprule.tdforce = event.index eq 1
         (*state).ifselactive = event.index eq 1
         (*(*state).oprule) = oprule
         if not oprule.tdforce or oprule.tddate ne '' then begin
            reduc_oplinesjoin,state,opline
            reduc_oplineupdate,state,opline
         endif
         reduc_opnulldisplay,state
         reduc_opdisplay,state,oprule
      end

      'FrcError': begin
         widget_control, event.id,GET_VALUE=strnew
         new=float(strnew)
         widget_control, event.id,SET_VALUE=strn(new)
         if  (*state).frcselindex eq  0 then oprule.ctvec[1] = new
         if  (*state).frcselindex eq  1 then oprule.k2vec[1]=new
         if  (*state).frcselindex eq  2 then oprule.kvec[1]=new
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
      end


      'FrcSel': begin
         ; frcselselect is the selection index in the menu order
         ; frcselindex is the selection in canonical order.
         ; The canonical order for forcing terms is lexicographic like
         ; forced, below, or (*state).forcenm.
          forced = [ oprule.ctforce,oprule.k2force,oprule.kforce, $
                     oprule.ktforce, oprule.tdforce]
         (*state).frcselselect = event.index
         ; this reorders the forcing terms for convenience in the menu.
         selorder = oprule.selorder
         unselorder = oprule.unselorder

         ; array  exst in canonical order
         ; guaranteed because selection sensitized only for the right rules.
         exst= where( forced ne (*state).na_val) 

         ; to convert the menu index (ie, frcselselect) to canonical order
         ;print,'exst in menu order', selorder[exst]
         frcselindex = selorder[exst[event.index]]
         
         (*state).ifselactive = (forced[frcselindex] eq 1)
         (*state).frcselindex = frcselindex

         ; special case with td..
         ; if td was activated but the tddate wasn't set AND we've transitioned
         ; to another force item, remove the td activation.
         if oprule.tdforce eq 1 and oprule.tddate eq '' and $
            (*state).forcenm[frcselindex] ne 'TD' then oprule.tdforce = 0
         (*(*state).oprule) = oprule
         ;print, 'td' ,oprule.tdforce 
         reduc_opnulldisplay,state
         reduc_opdisplay,state,oprule
      end

      'FrcValue': begin
         widget_control, event.id,GET_VALUE=strnew
         if (*state).frcselindex le 2 then begin
            new=float(strnew)
            widget_control, event.id,SET_VALUE=strn(new)
         endif
         if  (*state).frcselindex eq  0 then oprule.ctvec[0] = new
         if  (*state).frcselindex eq  1 then oprule.k2vec[0]=new
         if  (*state).frcselindex eq  2 then oprule.kvec[0]=new
         if  (*state).frcselindex eq  4 then oprule.tddate[0]=strnew
         (*(*state).oprule) = oprule
         if not oprule.tdforce or oprule.tddate ne '' then begin
            reduc_oplinesjoin,state,opline
            reduc_oplineupdate,state,opline
         endif
         reduc_opdisplay,state,oprule
      end


      'Hardcopy': begin
         (*state).selhardcopy=event.index
      end

      'List': begin
         reduc_selrule,state,event.index
      end

      'Make Rules': begin
         ; make sure we are in the right directory
         pushd,  addslash((*state).reducpath) + (*state).rundate 

         if (*state).vrbsreductor then print, ' run reductor to make rules.'
         reductor,ERRSTR=errstr,/GUI   ; make default rules
         popd
         if errstr ne '' then begin
            qsize = n_elements( errstr) <  (*state).maxqsize
            r = qannounc( errstr, FALSELABEL='', $
                                  YSIZE=(qsize > (*state).minqsize), $
                                  GROUP_LEADER=(*state).mainbase)
         endif

         reduc_newrunstates,state
         reduc_toplinesdisp,state
         ; so the IDL window remains where it was
      end

      'Next': begin
         reduc_selrundate,state,'Next'
      end

      'Notetext': begin
         ; any time you touch the window at all.
         if not (*state).notesdirty then $
            widget_control,(*state).notesinstid, set_value='notes: [modified]'
         (*state).notesdirty  = 1 
         ; preserve the setting of hardcopy for reductor run.
         (*state).selhardcopy = selhardcopy
      end

      'Object': begin
         widget_control, (*state).objectid, GET_VALUE=obj
         objv = strsplit(obj,/EXTRACT)
         obj = strjoin(objv)
         oprule.object = strtrim(obj,2)
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
         
      end

      'Objser': begin
         widget_control, (*state).objserid, GET_VALUE=ser
         oprule.objser = fix(ser)
         (*(*state).oprule) = oprule
         reduc_oplinesjoin,state,opline
         reduc_oplineupdate,state,opline
         reduc_opdisplay,state,oprule
         
      end

      'Params': begin
         ; get new string from widget that changed.
         if (*state).vrbsrules then $
            print, 'Params: ruleselindex ', (*state).ruleselindex
         widget_control, event.id,GET_VALUE=strnew
         new=float(strnew)
         ; if new is 0.0 or neg, it is invalid, most likely because mistyped.
         if new le 0.0 then begin
            print, ' Bad param value typed: ', strnew
            reduc_paramsdisp,state
         endif else begin
            if event.id eq (*state).objradid then (*state).rad = new
            if event.id eq (*state).skyrad1id then (*state).sky1 = new
            if event.id eq (*state).skyrad2id then (*state).sky2 = new
            if event.id eq (*state).gainid then (*state).gain = new
            if event.id eq (*state).rdnoiseid then (*state).rdnoise = new

            if (*state).vrbsrules then begin
               print, 'Params: ruleselindex ', (*state).ruleselindex
               print, 'Rewriting ', (*state).infofile
               print, 'Params: ',(*state).rad, (*state).sky1, (*state).sky2, $
                                 (*state).gain, (*state).rdnoise
            endif
            ; need to read info file first to get a copy of oplines
            rdreduc, addslash((*state).reducpath + (*state).rundate) + $ 
                    (*state).infofile,duminst,dumddir,dumrundate,dumrad, $
                    dumsky1,dumsky2,dumgain,dumrdnoise,oplines,/GUI
            if duminst eq '' then begin
               print, (*state).infofile,' disappeared or curdled, this is bad!'
               reduc_newdirclrdisp,state
               reduc_newdirsetdisp,state
            endif else begin
               wrreduc, addslash((*state).reducpath + (*state).rundate) + $ 
                       (*state).infofile,(*state).inst,(*state).ddir, $
                       (*state).rundate,(*state).rad,(*state).sky1, $
                       (*state).sky2,  (*state).gain, (*state).rdnoise, $
                       oplines, /GUI
               reduc_paramsdisp,state
            endelse
         endelse
         end
         
   
      'Prev': begin
         reduc_selrundate,state,'Prev'
      end
   

      'Rephot': begin
         ; this needs to ok out all active rules
         ; (*state).rdonly must be false, infoexists and writable must be true. 
         ; This runs reductor with no rules enabled and a minimal
         ; set of flags. If the params have changed from the values in the
         ; .log1 file reductor will do a rephot- otherwise it will do nothing.
         ; If a rule is selected you can use the other run reductor controls
         ; and the same thing will happen- rephot will be done, 
         ; and then the rule.
         reduc_rdoplines, state, duminst,dumddir,dumrundate,dumrad, $
                          dumsky1,dumsky2,dumgain,dumrdnoise,oplines
         oplines += ' ok'
         
         if (*state).vrbsrules then $
            print, ' writing ', (*state).infofile, ' with no rules enabled '
         wrreduc,addslash((*state).reducpath + (*state).rundate) + $
                 (*state).infofile,duminst,dumddir,dumrundate,dumrad, $
                 dumsky1,dumsky2,dumgain,dumrdnoise,oplines,/GUI
         if (*state).vrbsreductor then print,'run reductor to do rephot.'

         pushd,  addslash((*state).reducpath) + (*state).rundate 
         reductor,ERRSTR=errstr,/GUI   ; promote the file
         popd
         if errstr ne '' then begin
            qsize = n_elements( errstr) <  (*state).maxqsize
            r = qannounc(errstr, FALSELABEL='', $
                                 YSIZE=(qsize > (*state).minqsize), $
                                 GROUP_LEADER=(*state).mainbase)
         endif

         ; save state of frc term select and activate.
         ; so the force term menu state is preserved across 'run reductor'
         frcselindex = (*state).frcselindex
         frcselselect = (*state).frcselselect
         ifselactive = (*state).ifselactive
         ruleselected = (*state).ruleselected
         reduc_newrunstates,state
         reduc_toplinesdisp, state,SELECT_RULE=ruleselected ; 
         if ruleselected then begin
            (*state).frcselindex = frcselindex 
            (*state).frcselselect = frcselselect 
            (*state).ifselactive = ifselactive 
            ; rerefresh lower part of the gui.
            reduc_opdisplay,state,(*(*state).oprule)
         endif
      end

       ; we will run one opline  (magresid is available flag)
      'Run Reductor Mag': begin
         reduc_runreductor,state,'Mag',selhardcopy
      end

       ; we will run one opline  (magresid is available flag)
      'Run Reductor Sig': begin
         reduc_runreductor,state,'Sig',selhardcopy
      end


      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase
   widget_control,(*state).selhardcopyid, $
                  SET_DROPLIST_SELECT=(*state).selhardcopy
end ; end of event handler



pro reduc, DIR=dir,BROWSE=browse,INSTDB=instdb, $
           DATATABLE=datatable,TRANSFTABLE=transftable, $
           INSTDEFTABLE=instdeftable, $
           NOTESXSIZE=notesxsize, NOTESYSIZE=notesysize
   
   self = 'reduc: '
   if xregistered('reduc') then begin
      print, self, 'widget already running'
      return
   endif

   if (!d.flags and 256) eq 0 then begin
      print, self, 'error- no windowing device, cannot be started.'
      return
   endif
   if badpar(dir,  [0,7],    0,   caller=self + '(DIR) ', $
             default=['']) then return
   if badpar(instdb,  [0,7],    0,   caller=self + '(INSTDB) ', $
                default='pccd2obs.runstat,pccdobs.runstat,'+ $
                        'roboccd.runstat') then return
   if badpar(datatable,  [0,7],    0,   caller=self + '(DATATABLE) ', $
             default='phot.data') then return
   if badpar(instdeftable,  [0,7],    0,   caller=self + '(INSTDEFTABLE) ', $
             default='phot.instdef') then return
   if badpar(transftable,  [0,7],    0,   caller=self + '(TRANSFTABLE) ', $
             default='phot.transf') then return
   if badpar(browse,  [0,1,2,3],    0,   caller=self + '(BROWSE) ', $
             default=0) then return
   if badpar(notesxsize,  [0,1,2,3],    0,   caller=self + '(NOTESXSIZE) ', $
             default=80) then return
   if badpar(notesysize,  [0,1,2,3],    0,   caller=self + '(NOTESYSIZE) ', $
             default=5) then return

   ; validate and expand database keywords.
   instdbhook = reduc_dbstr2hook(instdb,ERRSTR=hookerrstr,DB='pccd2obs', $
                                 TABLE='runstat')
   if hookerrstr ne '' then begin
      print, self + ' database/table syntax error in ', 'INSTDB'
      return
   endif
   datadbhook = reduc_dbstr2hook(datatable,/SINGLE,ERR=hookerrstr, DB='phot', $
                                 TABLE='data')
   if hookerrstr ne '' then begin
      print, self + ' database/table syntax error in ', 'DATATABLE'
      return
   endif
   instdefdbhook=reduc_dbstr2hook(instdeftable,/SINGLE,ERR=hookerrstr, $
                                 DB='phot', TABLE='data')
   if hookerrstr ne '' then begin
      print, self + ' database/table syntax error in ', 'INSTDEFTABLE'
      return
   endif
   transfdbhook=reduc_dbstr2hook(transftable,/SINGLE,ERR=hookerrstr,DB='phot', $
                                 TABLE='data')
   if hookerrstr ne '' then begin
      print, self + ' database/table syntax error in ', 'TRANSFTABLE'
      return
   endif

   ; implement the following restrictions on the db choices:
   ; 1)- the table names must be unique for runstat, image, transf, instdef
   ;     and data tables regardless of data base for these.
   ; 2)- data and transf must be on the same database.
   ; 3)- the data tables for all of the elements of instdb must have
   ;     the same name.
   ; These restrictions are not intrinsic but for simplicity of
   ; processing, especially with joins.
   runstat = instdbhook[1]
   tptrs = indgen(n_elements(instdbhook)/2)*2 + 1
   z = where( instdbhook[tptrs] ne runstat)
   if z[0] ge 0 then begin
      print, self + ' only one table name allowed in array for ', 'INSTDB'
      return
   endif
   if datadbhook[0] ne transfdbhook[0] then begin
      print, self + ' DATATABLE, TRANSFTABLE must use the same database'
      return
   endif
   tnames = ['image',instdbhook[1],instdefdbhook[1],datadbhook[1], $
              transfdbhook[1]]
   if n_elements( uniq(tnames[sort(tnames)])) ne n_elements(tnames) then begin
      print, self + $
            ' need unique table names for INSTDB, TRANSFTABLE, ' + $
              'DATATABLE, INSTDEFTABLE and ''IMAGE'''
      return
   endif

   cd, CURRENT=odir
   odir = addslash(strtrim(odir,2))
   if dir ne '' then pushd, dir $
   else pushd, '/net/frakir/raid/buie/Reduced'

   ; changing directory via pushd and using cd,CURRENT is done to get a
   ; fully qualified path name. (and we add the trailing slash.) 
   ; In general reduc stays in its original directory except when it is
   ; processing the rundate directory (eg, when running reductor)
   ; and should always be back in the original directory after exit.
   cd, CURRENT=reducpath
   reducpath = addslash(strtrim(reducpath,2))
   popd

   rundate = ''  ; not valid (ie you always start in no rundate state)

   ;Define the main base.
   mainbase = widget_base(TITLE='REDUC: Photometric Data Reduction ', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   newrules = [ $
      '0\Add Rule: TR (Landolt transformation)', $
      '0\Add Rule: SL (all sources color solution)', $
      '0\Add Rule: 2C (2 color light curve)', $
      '0\Add Rule: LC (1 color light curve)', $
      '0\Add Rule: DP (differential photometry)', $
      '2\Add Rule: DP star clump' $
   ]

   runstatusskyvalues = [  '0\Set Sky to: phot', '0\Set Sky to: non-phot', $
                           '2\Set Sky to: unknown']
   runstatusstatvalues = [ '0\Set Stat to: raw', '0\Set Stat to: cal', $
                           '0\Set Stat to: in progress', $
                           '0\Set Stat to: reduced', $
                           '0\Set Stat to: published', '2\Set Stat to: bad']
   transfqualvalues = [ '0\Set Quality to: unknown', $
                        '0\Set Quality to: bad', $
                        '0\Set Quality to: suspect', $
                        '2\Set Quality to: good' ]
   defaultcolortermoptions = [ '0\Clear CT', '0\Clear K2', $
                               '0\Default CT', '0\Default K2', $
                               '0\Disable KT Fit', '2\Enable KT Fit' ]

   runstatusskystepmodes = [  '0\Sky Not', '0\Sky Phot', '0\Sky Non-Phot', $
                              '0\Sky Unknown', '2\Sky Any']
   runstatusstatstepmodes = [ '0\Status Not', '0\Status Raw', '0\Status Cal', $
                             '0\Status In Progress', $
                             '0\Status Cal or In Progress',  $
                             '0\Status Reduced', '0\Status Published', $
                             '0\Status Bad', '2\Status Any']

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Choose Rundate',$
                     '0\Enter Rundate',$
                     '0\First Rundate', $
                     '0\Last Rundate', $
                     '1\AutoStep Mode',$
                     '1\By Sky Condition', $
                      runstatusskystepmodes, $
                     '3\By Processing status', $
                      runstatusstatstepmodes, $
                      '0\Run Reductor(SAVEALLPLOTS)', $
                      '0\Void Notes Modifications', $
                     '2\Exit',$
                     '1\Edit',$
                     '0\Delete Current Rule', $
                     '1\Add New Rule', $
                      newrules, $
                      '1\Edit RunStat: Sky', $
                      runstatusskyvalues, $
                      '1\Edit RunStat: Stat', $
                      runstatusstatvalues, $
                      '1\Edit Transf: Quality', $
                       transfqualvalues, $
                      '1\Default Color Term Options', $
                      defaultcolortermoptions, $
                      '0\Edit Log1 Bad Flags', $
                      '0\Enter Browse Mode', $
                      '2\Enter Edit Mode', $
                      '1\Reports', $
                      '0\Image List (Current Rundate)', $
                      '0\Observations By Object (Autostep Criteria)', $
                      '0\All Observations (Autostep Criteria)', $
                      '2\Transformation Records (select TR first)', $
                       '1\Maintenance', $
                       '0\Dump State', $
                       '0\Dump Oprule', $
                       '0\Display All Keywords', $
                       '0\Display Sources Provenance', $
                       '1\Set Verbosity Level', $
                       '0\Minimal Verbosity', $
                       '0\Basic Verbosity', $
                       '0\Add DB Edit', $
                       '0\Add DB Queries', $
                       '0\Add DB Updates', $
                       '0\Add dbphot detail (long)', $
                       '0\Add Rules', $
                       '0\Add Rules Edit', $
                       '0\Add Reductor', $ $
                       '2\Add Rundates', $
                       '2\Health (Consistency and Program Invariants) Check' $
                      ], UVALUE='THE_MENU', /MBAR)

   envirobase = widget_base(mainbase,/ROW)
   previd=widget_button(envirobase,VALUE='Prev',UVALUE='Prev',/SENSITIVE)
   nextid=widget_button(envirobase,VALUE='Next',UVALUE='Next',/SENSITIVE)
   tagbase=widget_base(envirobase,/ROW,FRAME=1)
   rundateid = widget_label(tagbase, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   instid =  widget_label(tagbase, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   reducstatusid = widget_label(envirobase, VALUE='',/DYNAMIC_RESIZE, $
                                /ALIGN_LEFT)
   setbrowse = browse
   rdonly = setbrowse
   browse_msg = '       browse mode'
   edit_msg = '       edit mode'
   no_trmsg_rep = 'To run this report, you must have a TR rule selected'
   no_trmsg_def = 'To set option, you must have a TR rule selected'
   no_trmsg_qual = $
          'To set quality, you need a TR rule selected with a transf record'
   browsemsg_def = 'Cannot use option in browse or /NOSAVE mode'
   mrmsg = 'Make Default Rules file'
   mrid = widget_button(envirobase, value=mrmsg, $
                         UVALUE='Make Rules', SENSITIVE=0)
   runstatid =  widget_label(envirobase, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)

   stepbase = widget_base(mainbase,/ROW)
   stepquerySky='Sky  =  ' + quote('Unknown')
   stepqueryStatus=' Stat = ' + quote('Cal')
   stepid= widget_label(stepbase,VALUE='Unknown,Cal',/DYNAMIC_RESIZE, $
                        /ALIGN_LEFT)
   browseid= widget_label(stepbase,VALUE=edit_msg,/DYNAMIC_RESIZE, $
                        /ALIGN_LEFT)
   if setbrowse then widget_control, browseid, SET_VALUE=browse_msg

   pathbase = widget_base(mainbase,/ROW)
   dpathid =  widget_label(pathbase, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   rpathid =  widget_label(pathbase, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)

   parambase = widget_base(mainbase,/ROW)
   p1 = widget_label(parambase, value = 'Rad');
   objradid = widget_text(parambase,/EDITABLE,XSIZE=5, UVALUE='Params', $
                          SENSITIVE=0)
   p2 = widget_label(parambase, value = 'Sky1');
   skyrad1id = widget_text(parambase,/EDITABLE,XSIZE=5, UVALUE='Params', $
                          SENSITIVE=0)
   p3 = widget_label(parambase, value = 'Sky2');
   skyrad2id = widget_text(parambase,/EDITABLE,XSIZE=5, UVALUE='Params', $
                          SENSITIVE=0)
   p4 = widget_label(parambase, value = 'Gain');
   gainid = widget_text(parambase,/EDITABLE,XSIZE=7, UVALUE='Params', $
                          SENSITIVE=0)
   p5 = widget_label(parambase, value = 'RdN');
   rdnoiseid = widget_text(parambase,/EDITABLE,XSIZE=7, UVALUE='Params', $
                          SENSITIVE=0)
   rephotmsg='Recompute log1 file'
   rephotid = widget_button(parambase,VALUE=rephotmsg, $
                           UVALUE='Rephot', SENSITIVE=0)
   rulebaseid = widget_base(mainbase,/ROW,FRAME=1)
   rulelistid = widget_base(rulebaseid,/COLUMN)
   ruleselectid = widget_list(rulelistid, YSIZE=5, XSIZE=60, UVALUE='List')
   infobaseid = widget_base(rulebaseid,/COLUMN,FRAME=1)
   info0id = widget_label (infobaseid, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   info1id = widget_label (infobaseid, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   info2id = widget_label (infobaseid, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   info3id = widget_label (infobaseid, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)

   editbase = widget_base(mainbase,/ROW)
   edit1base=widget_base(editbase,/COLUMN)
   detailbase=widget_base(editbase,/COLUMN)
   detailid = widget_text(detailbase, XSIZE=35,YSIZE=11)
   objectbase=widget_base(edit1base,/ROW,FRAME=1)
   o1_lid = widget_label(objectbase, value='Object', SENSITIVE=0)
   objectid = widget_text(objectbase,/EDITABLE,XSIZE=8, UVALUE='Object', $
                          SENSITIVE=0)
   o2_lid = widget_label(objectbase, value='Serial', SENSITIVE=0)
   objserid = widget_text(objectbase,/EDITABLE,XSIZE=4, UVALUE='Objser', $
                          SENSITIVE=0)
   o3_lid = widget_label(objectbase, value='Comp', SENSITIVE=0)
   compid = widget_text(objectbase,/EDITABLE,XSIZE=14, UVALUE='Comp', $
                          SENSITIVE=0)
   runbase= widget_base(detailbase,/ROW)
   rbmsg ='Run Reductor'
   rbmsgno ='Run Reductor /NOSAVE'
   rbid = widget_label( runbase, VALUE=rbmsg,/DYNAMIC_RESIZE,/ALIGN_LEFT)
   runreducmagid = widget_button(runbase, value = 'Mag', $
                         UVALUE='Run Reductor Mag', SENSITIVE=0)
   runreducsigid = widget_button(runbase, value = 'Sig', $
                         UVALUE='Run Reductor Sig', SENSITIVE=0)
   selhardcopyid = widget_droplist( runbase, VALUE=['Screen','Hardcopy'], $
                              UVALUE='Hardcopy', SENSITIVE=0,/DYNAMIC_RESIZE)

   colorbase=widget_base(edit1base,/ROW,FRAME=1)
   filter2base=widget_base(colorbase,/ROW,FRAME=1)
   f2_1id = widget_label(filter2base, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   filter2_1id = widget_combobox(filter2base,/DYNAMIC_RESIZE,UVALUE='FiltStr', $
                 VALUE=[' ','Filter 2'], SENSITIVE=0,/EDITABLE)
   filter2_2id = widget_combobox(filter2base,/DYNAMIC_RESIZE,UVALUE='FiltStd', $
                 VALUE=['U','B','V','R','I' ],/EDITABLE,SENSITIVE=0)
   color1base= widget_base(colorbase,/ROW,FRAME=1)
   c11_lid= widget_label(color1base, value='Color', /DYNAMIC_RESIZE,SENSITIVE=0)
   ; must end with a string starting 'Color' and the arrays of 'X-' and '-Y'
   ; must be that way.
   colormenu = ['B-V','V-R','R-I','U-','B-','V-','R-','I-', $
                    '-U', '-B','-V', '-R', '-I']
   color1_id = widget_combobox(color1base, VALUE=colormenu, UVALUE='Color', $
                               /DYNAMIC_RESIZE,/EDITABLE,SENSITIVE=0)
   colorvecbase = widget_base(edit1base,/ROW,FRAME=1)
   cv1_lid = widget_label(colorvecbase, value='Color Value', SENSITIVE=0)
   colorvalid=widget_text(colorvecbase,/EDITABLE,XSIZE=5,UVALUE='ColorVal', $
                          SENSITIVE=0)
   cv2_lid = widget_label(colorvecbase, value='Error ', SENSITIVE=0)
   colorerrid=widget_text(colorvecbase,/EDITABLE,XSIZE=5,UVALUE='ColorErr', $
                          SENSITIVE=0)
   force4base=widget_base(edit1base,/ROW,FRAME=1)
   forcenm = ['CT', 'K2', 'K','KT','TD']
   forcedispl = [2, 2,2,0,1] ; if 1 display value, 2 display value and error
   frcselid = widget_droplist(force4base, VALUE=[' '],UVALUE='FrcSel', $
                              SENSITIVE=0,/DYNAMIC_RESIZE)
   frcactid = widget_droplist(force4base, VALUE=['DeActivated','Activated'], $
                              UVALUE='FrcAct', SENSITIVE=0,/DYNAMIC_RESIZE)
   f41_lid = widget_label(force4base, value='Value', SENSITIVE=0)
   vid = widget_text(force4base,/EDITABLE,XSIZE=7,UVALUE='FrcValue', $
                          SENSITIVE=0)
   f42_lid = widget_label(force4base, value='Error', SENSITIVE=0)
   eid = widget_text(force4base,/EDITABLE,XSIZE=7,UVALUE='FrcError', $
                          SENSITIVE=0)
   notesbase = widget_base(mainbase,/ROW)
   frcnotesbase = widget_base(notesbase,/COLUMN)
   frcdefbase=widget_base(frcnotesbase,/COLUMN,FRAME=3)
   defbuttonid = widget_button(frcdefbase, value = 'Default', $
                         UVALUE='Default CT,K2', SENSITIVE=0)
   clrbuttonid = widget_button(frcdefbase, value = 'Clear', $
                         UVALUE='Clear CT,K2', SENSITIVE=0)
   deflabel = widget_label(frcdefbase, VALUE='force CT,K2    ',/ALIGN_LEFT)
   notesinstid = widget_label(frcnotesbase, VALUE='     ',  FRAME=5, $
                         /DYNAMIC_RESIZE,/ALIGN_LEFT)

   notestextid=widget_text(notesbase,VALUE='', XSIZE=notesxsize, $
                          YSIZE=notesysize, $
                          /SENSITIVE, /ALL_EVENTS,UVALUE='Notetext', $
                          EDITABLE=0,/SCROLL)


   bel=string(07B)
   infofile = 'reduc.inf'
   na_str = '0B' ;STRING meaning field does not apply to the op.(oprule,rulestr)
   na_val = -9999  ;int(float) meaning field does not apply to the op.(oprule)
   us_str=''  ;STRING meaning field is unspecified  for the op. (oprule,rulestr)
   us_val=-1 ;int(float) meaning field is unspecified for the op. (oprule)
   ; size limit for qannounc boxes (display errstr from reductor)
   minqsize = 6
   maxqsize=50

   ruleops = ['tr','sl','2c','lc','dp'] ; in the order to display..
   newrules = [ 'tr 3 R 3 2 3 k2 0. 0.', $
                'sl 2 3 V R 2 3', $
                '2c a???? 0 2 V 2 2 3 3 R 3 2 3', $
                'lc a???? 0 3 R 3 2 3 0.4 0.0', $
                'dp 1 B BV p9 0 0.842 SAO_160288' $
              ]

   ; index # (starting at 0) is the corresponding Landolt color code.
   stdcolors=['U','B','V','R','I']

   ; data bases to search for rundates. It  is currently a requirement
   ; that rundates be unique to a given database, ie, the databases will
   ; be searched in order for a rundate but each one found must exist
   ; in only 1 database. This requirement is imposed by the reduction
   ; directory conventions and not the database architecture.
   ; Each database indexed in dbnames corresponds to the dbinst instrument,
   ;
   ; When searching pccdobs the inst code within pccdobs.runstat and
   ; other tables must be 'L'.
   dbnames = ['pccd2obs','pccdobs','roboccd']
   dbinst = ['PCCD','PCCD','Nasacam']

   ; runstat processing ('Stat') values
   runstatnames = ['raw','cal','in progress','reduced','published','bad']
   ; 1 means display directory and run rules, 2 means edit and run rules without
   ; NOSAVE. 0 means can only edit the runstat flags and run certain reports.
   runstatflags =  [  0  ,  0  ,      3      ,   1     ,    1      ,  1  ]

   ; level of print (to screen) verbosity. ( set to minimal )
   vrbsrules = 0
   vrbsupdate = 0
   vrbsqueries = 0
   vrbsrulesedit = 0
   vrbsrundates = 0
   vrbsreductor = 0
   vrbsdbedit = 0
   vrbsdbphot = 0


   state = ptr_new({ $

      ; pointers
      notes:ptr_new(), $
      oprule:ptr_new(), $
      rulecodes:ptr_new(), $
      rulestr:ptr_new(), $

      ; Data and information in the widget
      bel:bel, $
      browse:browse, $   ;keyword
      browse_msg:browse_msg, $
      browsemsg_def:browsemsg_def, $
      calibexists:0, $
      colorchoice:0, $
      colormenu:colormenu, $
      datatable:datatable, $ ; keyword
      datadbhook:datadbhook, $
      dbnames:dbnames, $
      dbinst:dbinst, $
      ddir:'', $
      ddirexists:0, $
      dir:dir, $      ; keyword
      edit_msg:edit_msg, $
      filtchoice:0, $
      forcedispl:forcedispl, $
      forcenm:forcenm, $
      frcselselect:0, $
      gain:0.0, $
      ifselactive:0, $
      infoexists:0, $
      infofile:infofile, $
      inst:'', $
      instdb: instdb, $  ; keyword
      instdbhook:instdbhook, $
      instdeftable:instdeftable, $  ; keyword
      instdefdbhook:instdefdbhook, $
      maxqsize: maxqsize, $
      minqsize: minqsize, $
      mrmsg: mrmsg, $
      na_str: na_str, $
      na_val: na_val, $
      newrules: newrules, $
      no_trmsg_def:no_trmsg_def, $
      no_trmsg_qual:no_trmsg_qual, $
      no_trmsg_rep:no_trmsg_rep, $
      notesdirty:0, $
      notesxsize:notesxsize, $
      notesysize:notesysize, $
      nrules:0, $
      oldir: odir, $
      partialcolor:'', $
      printit:0, $
      rad:0.0, $
      rbmsg:rbmsg, $
      rbmsgno:rbmsgno, $
      rdnoise:0.0, $
      rdonly: rdonly, $
      reduchidden:0, $
      reducpath: reducpath, $
      reducwriteable: 0, $
      rephotmsg:rephotmsg, $
      ruleops:ruleops, $
      ruleselindex:0, $
      rundate: rundate, $
      rundatexists: 0, $
      runstatdb:'', $
      runstatflags:runstatflags, $
      runstatnames:runstatnames, $
      runstatsky:'', $
      runstatstat:'', $
      ruleselected:0, $
      seltrquality:'', $
      setbrowse: setbrowse, $ 
      sky1:0.0, $
      sky2:0.0, $
      stdcolors:stdcolors, $
      transftable:transftable, $  ; keyword
      transfdbhook:transfdbhook, $
      unrun: 0, $
      us_str: us_str, $
      us_val: us_val, $
      vrbsdbedit: vrbsdbedit, $
      vrbsdbphot: vrbsdbphot, $
      vrbsqueries: vrbsqueries, $
      vrbsreductor: vrbsreductor, $
      vrbsrules: vrbsrules, $
      vrbsrulesedit: vrbsrulesedit, $
      vrbsrundates: vrbsrundates, $
      vrbsupdate: vrbsupdate, $

      ; widget ids
      browseid:browseid, $
      c11_lid:c11_lid, $
      color1_id:color1_id, $
      colorvalid:colorvalid, $
      colorerrid:colorerrid, $
      compid:compid, $
      clrbuttonid:clrbuttonid, $
      cv1_lid:cv1_lid, $
      cv2_lid:cv2_lid, $
      defbuttonid:defbuttonid, $
      detailid:detailid, $
      dpathid:dpathid, $
      eid:eid, $
      f2_1id:f2_1id, $
      filter2_1id:filter2_1id, $
      filter2_2id:filter2_2id, $
      frcactid:frcactid, $
      frcselid:frcselid, $
      frcselindex:0, $
      f41_lid:f41_lid, $
      f42_lid:f42_lid, $
      gainid:gainid, $
      info0id:info0id, $
      info1id:info1id, $
      info2id:info2id, $
      info3id:info3id, $
      instid:instid, $
      mrid: mrid, $
      nextid:nextid, $
      notesinstid:notesinstid, $
      notestextid:notestextid, $
      o1_lid:o1_lid, $
      o2_lid:o2_lid, $
      o3_lid:o3_lid, $
      objectid:objectid, $
      objserid:objserid, $
      objradid:objradid, $
      previd:previd, $
      rdnoiseid:rdnoiseid, $
      reducstatusid: reducstatusid, $
      rpathid:rpathid, $
      rundateid: rundateid, $
      runstatid: runstatid, $
      skyrad1id:skyrad1id, $
      skyrad2id:skyrad2id, $
      stepid:stepid, $
      rbid:rbid, $
      rephotid:rephotid, $
      ruleselectid: ruleselectid, $
      runreducmagid: runreducmagid, $
      runreducsigid: runreducsigid, $
      selhardcopyid:selhardcopyid, $
      selhardcopy:0, $
      stepquerySky:stepquerySky, $
      stepqueryStatus:stepqueryStatus, $
      top:0, $
      vid:vid, $
      ;
      mainbase: mainbase $       ; ID of top level base.
      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'reduc', mainbase, $
             EVENT_HANDLER='reduc_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='reduc_cleanup'

end

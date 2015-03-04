;+
; NAME:
;  puttran
; PURPOSE:
;  Add or replace transformation solution for a given 
;  night-instrument-filter-color combination
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  puttran,inst,date,filter,color1,color2,tran,transig,jdref,nobs,replaced,
;           fitted,chi2,quality
;
; INPUTS:
;  inst    - Instrument code
;  date    - YYMMDD string of date of observation
;  filter  - Landolt filter code 01234 is UBVRI
;  color1  - filter code for first color
;  color2  - filter code for second color (uses color index of C1-C2)
;  tran    - 5 transformation coefficients, k, k2, kcolor, 0pt, ktime
;  transig - uncertainties corresponding to trans.
;  jdref   - Time reference point for extinction- non-zero if ktime used.
;  nobs    - number of observations
;
; OPTIONAL INPUT PARAMETERS:
;  fitted  - array of ints for trans- non-zero if the corresponding values
;               fitted, otherwise forced. If not provided the default is set, 
;               currently that all quantities are forced,
;               except k (tran[0]).
;  chi2    - chi-square value. If not provided the data base
;               default is used, currently 0.0
;  quality - quality flag, a string containing any of the enum values
;              recognized by the database, currently one of
;              'unknown','bad','suspect','good'
;              If not provided the data base default is used,currently 'unknown'
;              An additional special value is supported, 'default', that when
;              supplied causes the following actions:
;                 old quality    action
;                 -----------------------------
;                  bad           keep value
;                  good          keep value
;                  unknown       ask for new value
;                  suspect       ask for new value
;                 not posted yet ask for value
;
; KEYWORD INPUT PARAMETERS:
;
; DATABASE - Name of MYSQL database for transformation search. 
;               The default is 'phot'. 
;               If TABLENAME specifies a database via the '.' notation, this
;               keyword (as well as its default) is ignored.
; TABLENAME- Name of table in MYSQL database for transformation search. 
;               The default is 'transf'. 
;               If string is of the form 'a.b' then a is taken to be the
;               name of the database and b is the tablename. Note that
;               if neither TABLENAME nor DATABASE is specified the photometry
;               table is located at 'phot.transf'
; OVERWRITE- flag, if set, an existing record with the instrument, date,
;               filters and color will be replaced. Otherwise it is
;               flagged as an error.
; REMOVE   - flag, if set,  any record tagged by inst, date, filter, color1,
;               color2 is removed.
; SILENT   - ordinarily a flag, if set, failures are silent. 
;               If set to negative values, debugging verbosity is enabled.
;  VERBOSE  - flag, if set, prints information including all db queries.
;
; OUTPUTS:
; replaced - set to 1 if a previous entry for inst, date, filter, color1 and
;               color2 already existed and was replaced, 0 if a new entry was
;               created, -1 if there was an error reported internally by
;               puttran, -2 if there was an error reported by MYSQL.
;               If the REMOVE flag is set, it is set 0 if there was no entry
;               to remove, 1 if an entry was removed, and -1 if there was an
;               error reported by puttran, -2 if there was an error 
;               reported by MYSQL.
;
;  
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; Using a value in an enum field (currently only quality) that is not one
;    of the defined value strings creates a record that is hard to deal with.
; Comments cannot be added with PUTTRAN.
;
; PROCEDURE:
; MODIFICATION HISTORY:
; 2006/4/07 - Written by Peter L. Collins, Lowell Observatory
;             for MYSQL operation.
; 2006/10/04, PLC, modified to write all values to transf without
;                  reference to data base names. Previous matching
;                  errors  in the order of the fitted
;                  array, were corrected in puttran, gettran and the
;                  transf table of the phot database.
; 2006/12/19, MWB, modified behavior driven by value of quality
; 2006/12/27, PLC, rationalization of data base keywords.
; 2010/07/29, MWB, cleanup of mysql insert string.
;-
pro puttran,inst,date,filter,color1,color2,tran,transig,jdref,nobs,replaced, $
    fitted,chi2,quality, $
    DATABASE=database,TABLENAME=tablename,OVERWRITE=overwrite,SILENT=silent, $
    REMOVE=remove,VERBOSE=verbose

   self = 'PUTTRAN '
   replaced = -1

   if badpar(inst,  7,           0,caller=self+'(inst) ') then return
   if badpar(date,  7,           0,caller=self+'(date) ') then return
   if badpar(filter,[1,2,3],     0,caller=self+'(filter) ') then return
   if badpar(color1,[1,2,3],     0,caller=self+'(color1) ') then return
   if badpar(color2,[1,2,3],     0,caller=self+'(color2) ') then return
   if badpar(tran,4,             1,caller=self+'(tran) ',DIMEN=d) then return
   if ( d ne 5 ) then begin
      print, self + '(tran) ', 'bad stride:', d
      return
   endif
   if badpar(transig,4,          1,caller=self+'(transig) ',DIMEN=d) then return
   if ( d ne 5 ) then begin
      print, self + '(transig) ', 'bad stride:', d
      return
   endif
   if badpar(jdref,5,            0,caller=self+'(jdref) ') then return
   if badpar(nobs,[1,2,3],       0,caller=self+'(nobs) ') then return
   if badpar(fitted,[0,1,2,3],   1,caller=self+'(fitted) ', DIMEN=d) then return
   if ( size(fitted,/type) ne 0 and d ne 5 ) then begin
      print, self + '(fitted) ', 'bad stride:', d
      return
   endif
   if badpar(chi2,[0,4,5],         0,caller=self+'(chi2) ', $
               default=0.0) then return
   if badpar(quality,[0,7],      0,caller=self+'(quality) ', $
               default='unknown')then return
   if badpar(database,[0,7],     0,caller=self+'(database) ', $
                                   default='') then return
   if badpar(tablename,[0,7],    0,caller=self+'(tablename) ', $
                                   default='') then return
   if badpar(overwrite,[0,1,2,3],0,caller=self+'(overwrite) ', $
                                   default=0) then return
   if badpar(silent,[0,1,2,3],   0,caller=self+'(silent) ', $
                                   default=0) then return
   if badpar(remove,[0,1,2,3],   0,caller=self+'(remove) ', $
                                   default=0) then return
   if badpar(noverify,[0,1,2,3], 0,caller=self+'(noverify) ', $
                                   default=0) then return
   if badpar(verbose,[0,1,2,3], 0,caller=self+'(verbose) ', $
                                   default=0) then return
   if  size(fitted,/type) eq 0 then fitted = [1, 0 , 0, 0, 0]

   ; process database and tablename. A simplified version of
   ; that found in reduc_db2hook. The final db,table pair are
   ; stored as a string array in transfdbhook.
   transfdbhook =['phot','transf']
   if tablename eq '' then begin
      if database ne '' then transfdbhook = [ database, transfdbhook[1]]
   endif else begin 
      piece = strcompress(strtrim(tablename,2))
      piece = strsplit(piece, '.',/EXTRACT)
      if n_elements(piece) eq 1 then begin
         if strmid(piece, strlen(piece)-1) eq '.' then $
            transfdbhook = [ piece[0],transfdbhook[1]] $
         else transfdbhook = [ transfdbhook[0], piece[0]]
      endif else transfdbhook = piece[0:1]
   endelse

   sfnames = ['U','B','V','R','I']
   fil = sfnames[filter]
   c1  = sfnames[color1]
   c2  = sfnames[color2]

   an = ' AND '
   semico = ';'
   sqldatel = [ '', '', '' , '']
   runDatefirsts = [0, 0, 2,4]
   runDatelengths = [1,2,2,2];  Decade YYear  MM DD (year includes decade)
   commaspc = ', '
   dash = '-'

   if silent lt  0 then debug = -silent else debug = 0

   ; we simply pass keywords here.
   gettran,inst,date,filter,color1,color2,tr,trs,jdr,found, $
            oldfitted,oldnobs,oldchi2,oldquality, $
            /DB,DATABASE=database,TABLENAME=tablename,/SILENT,VERBOSE=verbose
   if found then begin
      if overwrite or remove then begin
         ; remove the entry first
         delquery = 'DELETE FROM '+transfdbhook[1]+' WHERE'+  $
                    ' instrument = '+quote(inst)+ an+' rundate = '+ $
                    quote(date)+an+' filter = '+quote(fil)+an+ $
                    ' color1 = '+ quote(c1)+an+'color2 = '+ quote(c2)+semico

         if debug lt 2 then begin
            if verbose then print, 'puttran, delete on ', transfdbhook, delquery
            openmysql,dblun,transfdbhook[0]
            mysqlcmd,dblun,delquery,delresp,nlines
            free_lun, dblun
         endif
         replaced=1
         if remove then return
      endif else begin
         print,'ERROR! entry already present, puttran not enabled to overwrite'
         if debug lt 2 then return
      endelse
   endif else if remove then begin
      print,'ERROR! entry to be removed not present'
      return
   endif else replaced = 0

   ; now insert the entry
   lparen = '('
   rparen = ')'
   inserttoken = ' insert into '
   valuestoken = ' values '

   ; check on the quality value
   if quality eq 'default' then begin
      if not found then begin
         ask=1
         defvalue='unknown'
      endif else begin
         if oldquality eq 'unknown' or oldquality eq 'suspect' then begin
            ask=1
            defvalue=oldquality
         endif else begin
            quality=oldquality
            ask=0
         endelse
      endelse
      if ask then begin
         ans=''
         read,prompt='Status of fit for DB?  (unknown,bad,suspect,good) '+ $
                     '(default='+defvalue+') ',ans
         case strmid(ans,0,1) of
            'b': begin
                  quality='bad'
               end
            's': begin
                  quality='suspect'
               end
            'g': begin
                  quality='good'
               end
            'u': begin
                  quality='unknown'
               end
            else: begin
                  quality=defvalue
               end
         endcase
      endif
   endif

   fittedst = ''
   if size(fitted,/type) ne 0 then begin 
      for i=0,4 do begin
         if fitted[i] ne 0 then begin
            fittedst= fittedst+commaspc+quote('fitted')
         endif else begin
            fittedst = fittedst+commaspc+quote('forced')
         endelse
      endfor
   endif

   chi2st =commaspc+string(chi2,FORMAT='(f)')
   qualityst=commaspc+quote(quality)
   commentst = commaspc+quote('NULL')


   valuelist = ''
   ; prologue for sql insert/replace
   ourinsert = inserttoken+transfdbhook[1]+valuelist+valuestoken

   ; instrument, date fields
   thevalues = lparen+quote(inst)+commaspc+quote(date)
   ; format sql date and add that
   sqldatel = strmid(date,runDatefirsts,runDatelengths)
   if ( sqldatel[0] eq '9') then UTdate = '19'+sqldatel[1] $
   else  UTdate = '20' + sqldatel[1]

   UTdate = UTdate+dash+sqldatel[2]+dash+sqldatel[3]
   thevalues = thevalues+commaspc+quote(UTdate)

   ; three color fields
   names = [fil,c1,c2]

   for i = 0, 2 do begin
      thevalues = thevalues+commaspc+quote(names[i])
   endfor

   ; extinction and zero pt term fields
   for i = 0,4 do begin
      thevalues = thevalues+commaspc+string(tran[i],FORMAT='(F9.5)')
   endfor

   ; extinction and zero pt error term fields
   for i = 0,4 do begin
      thevalues = thevalues+commaspc+string(transig[i],FORMAT='(F9.5)')
   endfor

   ; jd for time dependent extinction term
   thevalues = thevalues+commaspc+string(jdref,FORMAT='(d15.6)')

   ; nobs

   thevalues = thevalues+fittedst
   thevalues = thevalues+commaspc+strn(nobs)
   thevalues = thevalues+chi2st
   thevalues = thevalues+qualityst

   thevalues = thevalues + commentst

   ourinsert = ourinsert+thevalues+rparen+semico;

;   if debug ne 0 then print, ourinsert
   print, ourinsert
   if debug lt 2 then begin
      openmysql,dblun,transfdbhook[0]
      if verbose then print, 'puttran, insert on ', transfdbhook, ourinsert
      mysqlcmd, dblun,ourinsert,insertresp, nlines
      free_lun, dblun
   endif
   
end

;+
; NAME:
;  dbphot
; PURPOSE: 
;  Add or update observation entries in the photometry data base.
; DESCRIPTION:
;  Opens data table in the phot data base and adds observations.
;  Normally some or all of the observations already  in the database for the
;  same RefID will be deleted. This 'cleaning', controlled by keyword,
;  can cover all of the RefID, or all observations for the RefID matching the 
;  input set by a) object name and filter or (b) jd. Dbphot can also be used
;  to delete entries only.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;
;  dbphot, refid,obj,jdobs,filstr,mag,magerr
;
; INPUTS:
;  refid   - String (up to 20 chars) which identifies an observing run.  
;               It corresponds to the value of the RefID field in the db table.
;               All data added to or removed from the database in a call will 
;               pertain to this id, which must be a non-null string.
;  obj     - String or string array giving the object(s) names. A null string
;               will engender a NULL value for the data base ObjName field.
;               An object name may not exceed 20 characters in length.
;  jdobs   - JD time of observation or array of times, which is either a string
;               of Gregorian date and time suitable for jdparse, or a double JD
;               number. Jd values less than or equal to zero, or empty string,
;               will engender a NULL value for the data base jd field. 
;               Note: null jd often validly applies to catalog entries.
;  filstr  - String (up to 10 chars) or string array giving the filter(s) names.
;  mag     - Array of reduced magnitudes.
;  magerr  - Array of reduced magnitude errors (one sigma).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; BAD           - Array of bad flags, integers, by default all zero.
; CLEANBYJD     - Flag, if set, all previous entries in the data base
;                    with this RefID  and an exact match to any of the
;                    jdobs input values will be deleted.
;                    If set, CLEANBYOBJFIL, NOCLEAN and CLEANBYREFID must be 
;                    unset.
; CLEANBYOBJFIL - Flag, if set, all previous entries in the data base
;                    with this RefID  and an exact match to any of the
;                    filter, objname and color input value combinations
;                    will be deleted. If COLOR is not specified then the match 
;                    is to object, filter and the color '-'. If this keyword is 
;                    set, CLEANBYJD, NOCLEAN and CLEANBYREFID must be unset.
; CLEANBYREFID  - Flag, if set, all previous entries in the data base
;                    with this RefID will be deleted. If set, CLEANBYJD, NOCLEAN
;                     and CLEANBYOBJFIL must be unset.
; CLEANONLY     - Flag, if set, only the removal of previous entries as depicted
;                    above will be done and the input entries will not be added.
;                    It is still necessary to specify one or more input values
;                    and all the usual restrictions will be in effect.
; COLOR         - String, represents color against which the magnitudes were 
;                    reduced (if applicable). This is a scalar applied to all
;                    values, and placed in the Color column of the data table.
;                    Normally it is of the form (filter1-filter2), like 'B-V'.
;                    If unspecified, or a null string is used, it will be
;                    defaulted to '-'.
; DATABASE      - String, name of photometry MYSQL database, by default 'phot'
;                    If keyword TABLE specifies a database via the '.' notation, 
;                    DATABASE is ignored.
; DECL          - Scalar or array of double precision dec values in radians, or
;                    strings in sexagesimal format suitable for decparse.
;                    If not specified, or empty string, a NULL
;                    value will be used for the ra and decl columns in database.
; NOCLEAN       - Flag, if set, no previous values will be cleaned from the db.
;                    If set, CLEANBYJD, CLEANBYOBJFIL and CLEANBYREFID must be
;                    unset.
; RA            - Scalar or array of double precision ra values in radians, or
;                    strings in sexagesimal format suitable for raparse.
;                    If not specified, or empty string or 0.0, a NULL
;                    value will be used for the ra and decl columns in database.
; SILENT        - Flag, if set, dbphot prints no non-error output. 
;                    It is ignored if TEST set.
; TABLE         - String, name of photometry table, by default 'data' If the 
;                    string is of the form 'a.b' then a is taken to be the
;                    name of the database and b is the tablename. Note that
;                    if neither TABLE nor DATABASE is specified the photometry
;                    table is located at 'phot.data'
; TEST          - Flag, if set, the data base will not change, but the cmds that
;                    would have been generated will be printed. It queries the
;                    db to list which existing entries would have been removed.
; VERBOSE       - Flag, if set prints verbose debug output- in particular, what
;                    TEST prints, but does perform all the database operations.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; NREMOV   - Set to number of previous data rows removed, or, if TEST set,
;               would be removed.
;            
; COMMON BLOCKS:
; SIDE EFFECTS:
;     DBPHOT removes all existing db entries for refid, instrument, objects,
;        filters, and jd  according to the *CLEAN* flags.
;        As many data base entries will be added as there are
;        elements in the mag array unless CLEANONLY is specified.
;        Specifying both CLEANONLY and NOCLEAN is a no-op but can be
;        useful for validating an input set to DBPHOT.
; RESTRICTIONS:
;     The number of elements in the mag and magerr arrays must
;     be equal. The number of elements in the obj, jdobs, filstr, bad
;     and RA,DECL (if specified)
;     arrays must also be the same. However, if a scalar is specified
;     for jdobs, filstr, obj, bad, RA or DECL, this is a legal case and the 
;     value is replicated for all entries. It is legal for RA and DECL to be
;     unspecified, or obj to specified as a null string, however, for each
;     observation, there must be a specified value for either obj or RA,DECL.
;     It is legal to specify both obj and ra,decl for an observation but it
;     is not ordinarily done. If one of ra and decl is specified for an
;     observatory, they will be both posted as NULL.
;     If bad is unspecified, a bad flag value of 0 is replicated for all entries.
;     One and only one of the flags NOCLEAN, CLEANBYREFID, CLEANBYOBJFIL and
;     CLEANBYJD must be specified.
;     CLEANBYJD requires that all values of jdobs be non-null.
;     CLEANBYOBJFIL requires that all values of obj and filstr be non-null.
;
;     Be aware that if you are following the practice of removing all bad
;     data from your input prior to calling DBPHOT (and thus not using BAD),
;     you will need to call DBPHOT with /CLEANONLY if and when all your data 
;     are marked bad to make sure the previous db entries are scrubbed.
;
;     This routine only works with the photometry 'data' table- the reference
;     table is not modified or examined.
; PROCEDURE:
; MODIFICATION HISTORY:
;       2006/08/10, Written by Peter L. Collins, Lowell Observatory
;       2006/09/26, PLC, add support and keyword for INSTRUMENT field in
;                        database- phot.data.
;       2006/12/05, PLC, add support for RefID field in the data table,
;                        which replaces the former Rundate and Instrument
;                        fields. Also RA and DEC are available as keyword
;                        inputs. Richer set of supported formats, defaults 
;                        and validity checks.
;       2006/12/09, PLC, test for inaccessible data base via openmysql and
;                        length checking on RefID, obj, and filstr.
;       2006/12/27, PLC, rationalization of data base keywords, and recoding
;                        of VERBOSE keyword.
;       2007/01/12, PLC, add CLEANONLY keyword.
;       2007/01/23, PLC, add COLOR keyword (with change to database table to add
;                        Color column). Tweak add/delete prints to reflect clean
;                        flags.
;-

pro dbphot, refid,obj,jdobs, filstr,mag,magerr, $
            DATABASE=database,TABLE=table, BAD=bad,TEST=test,SILENT=silent, $
            RA=in_ra,DECL=in_decl, NOCLEAN=noclean, CLEANBYREFID=cleanbyrefid, $
            CLEANBYOBJFIL=cleanbyobjfil, CLEANBYJD=cleanbyjd, NREMOV=nremov, $
            CLEANONLY=cleanonly, COLOR=color,VERBOSE=verbose

   self = 'DBPHOT: '
   if badpar(refid,7,     0, caller=self + '(REFID)')            then return
   if badpar(obj,    7, [0,1], caller=self + '(OBJ) ',  npts=nobj)   then return
   if badpar(filstr, 7, [0,1], caller=self + '(FILSTR)',npts=nfil)   then return
   if badpar(jdobs,[5,7],[0,1], caller=self + '(JDOBS) ',npts=njd, $
                          TYPE=tjd ) then return 
   if badpar(mag, [4,5],[0,1], caller=self + '(MAG)',   npts=nmag)  then return
   if badpar(magerr,[4,5],[0,1],caller=self+ '(MAGERR)',npts=nmagerr)then return
   if badpar(database,[0,7],0,  caller=self + '(DATABASE)',  $
             default='') then return
   if badpar(table, [0,7],  0, caller=self + '(TABLE)',  $
             default='') then return
   if badpar(bad,[0,1,2,3], [0,1], caller=self + '(BAD)',   npts=nbad, $
             default=intarr(nmag)) then return
   if badpar(test,[0,1,2,3],0, caller=self + '(TEST)', $
             default=0) then return
   if badpar(verbose,[0,1,2,3],0, caller=self + '(VERBOSE)', $
             default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self + '(SILENT)', $
             default=0) then return
   if badpar(in_ra,[0,5,7],[0,1],caller=self + '(RA)', npts=nra, TYPE=tra, $
             default='') then return
   if badpar(in_decl,[0,5,7],[0,1],caller=self + '(DECL)', npts=ndecl,  $
             TYPE=tdecl, default='') then return
   if badpar(noclean,[0,1,2,3],0,caller=self + '(NOCLEAN)', $
             default=0) then return
   if badpar(cleanbyrefid,[0,1,2,3],0,caller=self + '(CLEANBYREFID)', $
             default=0) then return
   if badpar(cleanbyobjfil,[0,1,2,3],0,caller=self + '(CLEANBYOBJFIL)', $
             default=0) then return
   if badpar(cleanbyjd,[0,1,2,3],0,caller=self + '(CLEANBYJD)', $
             default=0) then return
   if badpar(cleanonly,[0,1,2,3],0,caller=self + '(CLEANONLY)', $
             default=0) then return
   if badpar(color,[0,7],0,  caller=self + '(COLOR)',  $
             default='-') then return

   ; secondary defaulting of color.
   if color eq '' then color = '-'

   ; process database and table. A simplified version of
   ; that found in reduc_db2hook. The final db,table pair are
   ; stored as a string array in photdbhook.
   photdbhook =['phot','data']
   if table eq '' then begin
      if database ne '' then photdbhook = [ database, photdbhook[1]]
   endif else begin 
      piece = strcompress(strtrim(table,2))
      piece = strsplit(piece, '.',/EXTRACT)
      if n_elements(piece) eq 1 then begin
         if strmid(piece, strlen(piece)-1) eq '.' then $
            photdbhook = [ piece[0],photdbhook[1]] $
         else photdbhook = [ photdbhook[0], piece[0]]
      endif else photdbhook = piece[0:1]
   endelse

   ; define the setting for long verbosity
   vlong = verbose gt 1


   nullposition = -10.0D
   nulljd = 0.0D

   ; validity check on the clean flags- one and only one should be on.
   cleans= [noclean , cleanbyrefid,cleanbyjd,cleanbyobjfil]
   cleanstr= ['noclean', 'cleanbyrefid','cleanbyjd','cleanbyobjfil']
   z = where((cleans and 1) eq 1, cleancnt)

   if cleancnt ne 1 then begin
      print, self + 'one and only one of: ' + strjoin(cleanstr, ', ') + $
             ' must be set'
      return
   end
   cs = cleanstr[z]

   ; promote ra to double precision radians as needed. Empty strings get 
   ; turned into -10.0D.
   if tra eq 7 then begin
      ra = replicate(nullposition, nra)
      p = where(in_ra ne '')
      if p[0] ge 0 then ra[p] = raparse(in_ra[p])
   endif else ra = in_ra

   ; promote dec to double precision radians. Empty strings get turned into
   ; -10.0D
   if tdecl eq 7 then begin
      decl = replicate(nullposition, ndecl)
      p = where(in_decl ne '')
      if p[0] ge 0 then decl[p] = decparse(in_decl[p])
   endif else decl = in_decl

   ; promote jd to double precision days. Empty strings get turned into
   ; 0.0D
   if tjd eq 7 then begin
      jd = replicate(nulljd, njd)
      p = where(jdobs eq '')
      if p[0] ge 0 then jd[p] = jdparse(jdobs[p])
   endif else jd = jdobs

   ; promote scalar object to vector as needed.
   object = obj
   if nobj eq 1 then begin
      object = replicate(object, nmag)
      nobj = nmag
   endif

   ; promote scalar filter to vector as needed.
   filter = filstr
   if nfil eq 1 then begin
      filter = replicate(filter, nmag)
      nfil = nmag
   endif 

   ; promote scalar ra to vector as needed.
   if nra eq 1 then begin
      ra = replicate(ra, nmag)
      nra = nmag
   endif 

   ; promote scalar decl to vector as needed.
   if ndecl eq 1 then begin
      decl = replicate(decl, nmag)
      ndecl = nmag
   endif

   ; promote scalar jd to vector as needed.
   if njd eq 1 then begin
      jd = replicate(jd, nmag)
      njd = nmag
   endif

   in_pts = [nobj,njd,nfil,nmag,nmagerr,nbad,nra,ndecl]
   if max(in_pts) ne min(in_pts) then begin
      print,self + 'Error!  obj,filstr,jd,serial,mag,err,{bad,ra,decl}' + $
            ' must be the same length.'
      return
   endif

   ; if either dec or ra is null for an entry the other must be made null.
   x = where(decl eq nullposition)
   if x[0] ge 0 then ra[x] = nullposition
   x = where(ra eq nullposition)
   if x[0] ge 0 then decl[x] = nullposition

   ; validate length of RefID string.
   if strlen(refid) gt 20 or strlen(refid) lt 1 then begin
      print, self + 'Error! Refid string length ', strlen(refid), ' illegal'
      return
   endif
      
   ; validate length of ObjName strings.
   if max( strlen(object)) gt 20 then begin
      print, self + 'Error! invalid string length(s) for object names'
      return
   endif

   ; validate length of filstr strings.
   if max( strlen(filstr)) gt 10 or min(strlen(filstr)) lt 1  then begin
      print, self + 'Error! invalid string length(s) for filters'
      return
   endif

   ; validate length of color string.
   if max( strlen(color)) gt 10 or min(strlen(color)) lt 1  then begin
      print, self + 'Error! invalid string length(s) for color'
      return
   endif

   ; make sure there is an object name or a position for everybody.
   z = where( object eq '' and ra eq nullposition)
   if z[0] ge 0 then begin
         print, self + 'Error! ', n_elements(z), $
                 ' entries lack both {ra,dec} and object name'
         return
   endif

   ; selective clean requires input selection fields specified.
   z = [-1]   ; predefine the right result.
   if cleanbyjd then $
      z = where(jd eq nulljd)
   if cleanbyobjfil then $
      z = where(object eq '' or filter eq '')
   if z[0] ge 0 then begin
      print, self, cs, ' fails due to null values in selector field(s)' 
      return
   end

   querstr=['']
   comma = ' , '
   nremov = 0
   
   ; open the data base (even in test mode).
   openmysql,dblun,photdbhook[0],nodb
   if nodb ne 0 then begin
      print, self, ' cannot open data base ',database
      return
   endif
   if not noclean then begin
      ; generate queries array to delete  the refid's rows or subset thereof.
      if cleanbyobjfil then begin
         ; generate the  unique combos for the remove operation 
         ; (filter, objname, color)
         ; where a predicate is set up for every element. 
         ; quote is not helpful here because it handles vectors as multilines.
         querstr = 'filter  = ' + "'" + (filter) +"'" + ' and objName = ' + $
                    "'" + object + "'"
         querstr = querstr + ' and Color = ' + quote(color)
      endif
      if cleanbyjd then $
         querstr = 'jd  = ' + string(jd,FORMAT='(D13.5)')
      ; distill down to a unique set of predicates.
      queries = uniq( querstr, sort(querstr))
      if test or vlong then print, 'cleaning predicate(s): ', querstr[queries]

      ;  remove the filters and objNames, or jd's  indicated,
      ; within the refid, if using CLEANBYOBJNAME or CLEANBYJD.
      ; If using CLEANBYREFID ,there is only one query to get the whole RefID.
      for i=0, n_elements(queries)-1 do begin
         selector=['', 'from ' + photdbhook[1],' where RefID = ' + quote(refid)]
         if querstr[queries[i]] ne '' then $
            selector= [selector, ' and '  + querstr[queries[i]] ]
                                                                             
         selector = [ selector, ';' ]
         selector[0] = 'SELECT ObjName, RefID,jd,filter,mag,err,bad '

         if test or vlong then print,  'selector ', strn(i),': ', selector

         ; do a query first with the selector  to see how many to remove (fyi).
         mysqlquery,dblun,selector, objremov,RunDateremov, jdremov, $
                       filteremov, magremov, magerremov,badremov, $
                       format='a,a,a,a,f,f,i'
         if filteremov[0]  ne '' then begin
            ; there were some.
            if test or vlong  then begin
            ; in test mode print what would have been removed.
               for j=0, n_elements(jdremov)-1 do begin
                  print,' remove ',objremov[j],' ',RunDateremov[j],' ', $
                        jdremov[j],' ',filteremov[j], ' ',magremov[j], ' ', $
                        magerremov[j],' ', badremov[j]
               endfor
            endif
            nremov += n_elements(filteremov) ; statistic
         endif else begin
           if test then print, self + ' no entries removed'
         endelse

         ; the actual delete
         if not test then begin   
            selector[0] = ' DELETE '
            mysqlcmd, dblun, selector,resp, nlines
         endif
      endfor
   endif

   if not cleanonly then begin
      for j=0, nmag-1 do begin
         insertor=[ ' INSERT INTO ' + photdbhook[1], $
                    ' (  RefID,filter,Color,mag,err,bad ', ' )', $
                    ' VALUES ( ', $
                    quote(refid) + comma + $
                   quote(filter[j]) + comma + $
                   quote(color) + comma + $
                   strn(mag[j],FORMAT="(D13.5)") + comma + $
                   strn(magerr[j],FORMAT="(D14.6)") + comma + $
                   strn(bad[j]), ' )' ] 
         if jd[j] ne nulljd then begin
            insertor[1] += ' , jd '
            insertor[4] += (comma + strn(jd[j], FORMAT="(D13.5)") )
         endif
         if ra[j] ne nullposition then begin
            insertor[1] += ' , ra, decl '
            insertor[4] += (comma + strn(ra[j], FORMAT="(D12.9)") + comma + $
                            strn(decl[j], FORMAT="(D13.9)"))
         endif
         if object[j] ne '' then begin
            insertor[1] += ' , objname '
            insertor[4] += (comma + quote(object[j]) )
         endif
         insertor = [ insertor, ';' ]
         if test or vlong then $
            print, 'the insert command is ', insertor
         if not test then mysqlcmd, dblun,insertor,resp, nlines
      endfor
   endif else njd = 0
   if not silent then begin 
      if noclean then begin 
         print,  njd, ' entries added' 
      endif else begin
         if cleanonly then print, nremov, ' entries removed '  $
         else print, nremov, ' entries removed, ', njd, ' entries added'
      endelse
   endif
   free_lun, dblun
end

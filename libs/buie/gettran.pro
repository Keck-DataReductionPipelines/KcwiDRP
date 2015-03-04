;+
; NAME:
;  gettran
; PURPOSE:
;  Find and return transformation solution for a given night and instrument
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  gettran,inst,date,filter,color1,color2,tran,transig,jdref,found,
;          fitted,nobs,chi2,quality, 
;          DB=db,DATABASE=database,TABLENAME=tablename,FILE=file,PATH=path,
;          SILENT=silent
;
; INPUTS:
;  inst     - Instrument code
;  date     - YYMMDD string of date of observation
;  filter   - Landolt filter code 01234 is UBVRI
;  color1   - filter code for first color
;  color2   - filter code for second color (uses color index of C1-C2)
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DB       - flag, if set will use MYSQL server and validly populate outputs
;                fitted, nobs, chi2, quality and comments.
;                By default, the program uses the legacy mode of using a file
;                of tabulated values for the transformation data. Only the
;                trans, transig, jdref and found outputs are populated.
;                The file name is of the form (e.g.) transf_b.bmv.
;  DATABASE - Name of MYSQL database for transformation search.
;                The default is 'PHOT'. Used only when DB set.
;                If TABLENAME specifies a database via the '.' notation, this
;                keyword (as well as its default) is ignored.
;  TABLENAME- Name of table in MYSQL database for transformation search.
;                The default is 'transf'. Used only when DB set.
;                If string is of the form 'a.b' then a is taken to be the
;                name of the database and b is the tablename. Note that
;                if neither TABLENAME nor DATABASE is specified the photometry
;                table is located at 'phot.transf'
;  FILEPATH - the single directory path for the file used for the 
;                transformation search in legacy mode. The default is 
;                '/net/frakir/raid/buie/Reduced/'. Used only when FILE set.
;
;  SILENT   - flag, if set, failures are silent
;  VERBOSE  - flag, if set, prints information including all db queries.
;
; OUTPUTS:
;  tran     - 5 transformation coefficients, k, k2, kcolor, 0pt, ktime
;  transig  - uncertainties corresponding to trans.
;  jdref    - Time reference point for extinction- non-zero if ktime used.
;  found    - 1 if valid data found for the input parameters.
;  fitted   - array of ints for trans- non-zero if the corresponding values
;                fitted, otherwise forced. Currently fitted is defined
;                only when gettran uses MYSQL.
;  nobs     - number of observations, currently defined only when gettran
;                uses MYSQL.
;  chi2     - chi-square value, currently defined only when gettrans 
;                uses MYSQL.
;  quality  - quality flag, enum string among "good" "bad" suspect" "unknown"
;                currently defined only when gettran uses MYSQL.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; if DB is set the data base selected is opened, query'd and closed.
; if FILE is set the file selected is opened for read, read and closed.
;
; RESTRICTIONS:
; GETTRAN currently fails if the enum strings in database corresponding to
;    the arguments fitted and quality have the 'error' value.
; GETTRAN cannot access the comments field in the MYSQL table.
;
; PROCEDURE:
; MODIFICATION HISTORY:
; 96/10/17 - Written by Marc W. Buie, Lowell Observatory
; 97/2/6, MWB, added time dependent extinction term
; 2004/02/09, MWB, changed path to transf files.
; 2006/4/03,  Peter L. Collins, Lowell observatory
;                extended for MYSQL operation and some additional fields.
; 2006/08/07, MWB, fixed problem with keywords
; 2006/10/04, PLC, get fitted array in the right order (ktime and zeropt 
;                  swapped). Previous matching errors in the order of the 
;                  fitted array were corrected in puttran, gettran and the
;                  transf table of the phot database.
; 2006/12/27, PLC, rationalization of data base keywords.
;
;-
pro gettran,inst,date,filter,color1,color2,tran,transig,jdref,found, $
    fitted,nobs,chi2,quality, $
    DB=db,DATABASE=database,TABLENAME=tablename,PATH=path, $
    SILENT=silent,VERBOSE=verbose

   self = 'GETTRAN: '

   if badpar(inst,  7,      0, caller=self+' (inst) ') then return
   if badpar(date,  7,      0, caller=self+' (date) ') then return
   if badpar(filter,[1,2,3],0, caller=self+' (filter) ') then return
   if badpar(color1,[1,2,3],0, caller=self+' (color1) ') then return
   if badpar(color2,[1,2,3],0, caller=self+' (color2) ') then return
   if badpar(db,[0,1,2,3],0,   caller=self+' (db) ', default=0) then return
   if badpar(database,[0,7],0, caller=self+' (database) ', $
                             default='') then return
   if badpar(tablename,[0,7],0, caller=self+' (tablename) ', $
                             default='') then return
   if badpar(path,[0,7],0,caller=self+' (path) ', $
                          default='/net/frakir/raid/buie/Reduced/') then return 
   if badpar(silent,[0,1,2,3],0,   caller=self+' (silent) ', $
                             default=0) then return
   if badpar(verbose,[0,1,2,3],0,   caller=self+' (verbose) ', $
                             default=0) then return

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

   sfnames=['U','B','V','R','I']
   ifnames=['u','b','v','r','i']


   fil = ifnames[filter]
   c1  = ifnames[color1]
   c2  = ifnames[color2]
   found=0

   blanks='          '
   tagdate=date+blanks
   taginst=inst+blanks
   tagdate=strmid(tagdate,0,6)
   taginst=strmid(taginst,0,10)
   tag=taginst+' '+tagdate


   if not db then begin
      histname=path+'transf_'+fil+'.'+c1+'m'+c2


      if not exists(histname) then begin
         if silent ne 0  then $
            print,'GETTRAN: ERROR! Transformation file ',histname,' not found'

         found=0
         return
      endif

      str1=''
      str2=''
      openr,lun,histname,/get_lun
      while(not eof(lun) and str1 ne tag) do begin
       readf,lun,str1,str2,format='(a17,a90)'
      endwhile
      free_lun,lun

      if str1 ne tag then begin
         if silent ne 0 then $
            print,'GETTRAN: ERROR! ',sfnames[filter], $
            ', (',sfnames[color1],'-',sfnames[color2], $
            ') transformation for [',tag,'] not found'
            print,' in file ',histtname

         found=0
         return
      endif
   
      r1 = 0.0d0
      reads,str2,v1,e1,v2,e2,v3,e3,v4,e4,v5,e5,r1, $
         format='(5(1x,f7.4,1x,f6.4),1x,d13.5)'

      tran    = [v1,v2,v3,v4,v5]
      transig = [e1,e2,e3,e4,e5]
      jdref   = r1

      found=1
   endif else begin
      semico=';'
      cmd=['select instrument,RunDate,filter,color1,color2,k,k2,kcolor,', $
                  'zeropt,ktime,e_k,e_k2,e_kcolor,e_zeropt,e_ktime,jdref,', $
                  'f_k,f_k2,f_kcolor,f_zeropt,f_ktime,nobs,chi2,quality' + $
                   ' from' + ' ' +  transfdbhook[1], $
                  ' where RunDate='+quote(date)+' and instrument='+ $
                                   quote(inst), $
                  ' and ', $
                  'filter='+quote(sfnames[filter])+' and', $
                  'color1='+quote(sfnames[color1])+' and', $
                  'color2='+quote(sfnames[color2])+semico ]
      
      ;open database
      openmysql,dblun,transfdbhook[0]
      if verbose then print, 'gettran, query on ', transfdbhook, cmd
      mysqlquery,dblun,cmd, dbinst,dbdate,dbfilt,dbcolor1,dbcolor2, $
                 t0,t1,t2,t3,t4,ts0,ts1,ts2,ts3,ts4, $
                 jdref, $
                 fs0,fs1, fs2,fs3, fs4, $
                 nobs,chi2, quality, $
                 format='a,a,a,a,a, f,f,f,f,f, f,f,f,f,f, d, a,a,a,a,a, i,f,a'
      free_lun, dblun

      fs = [fs0, fs1,fs2,fs3,fs4]
      tran = [t0,t1,t2,t3,t4]
      transig = [ts0,ts1,ts2,ts3,ts4]
      jdref = jdref[0]

      if n_elements(dbinst) ne 1 or dbinst[0] ne inst or dbdate[0] ne date or $
         dbfilt[0] ne sfnames[filter] or dbcolor1[0] ne sfnames[color1] $
         or dbcolor2[0] ne sfnames[color2] then begin

         if n_elements(dbinst) le 1 then begin
            if silent ne 0 then $
               print,'GETTRAN: ERROR! ',sfnames[filter], $
               ', (',sfnames[color1],'-',sfnames[color2], $
               ') transformation for [',tag,'] not found'
         endif else begin
            if n_elements(dbinst) gt 1 then $
               if silent ne 0  then $
                  print,'GETTRAN: ERROR! ',sfnames[filter], $
                  ', (',sfnames[color1],'-',sfnames[color2], $
                  ') transformation for [',tag,'] has multiple records'
;            if  n_elements(dbinst) eq 1 and silent ne 0 then $
;            print,'GETTRAN: ERROR! ',sfnames[filter], $
;               ', (',sfnames[color1],'-',sfnames[color2], $
;               ') transformation for [',tag,'] has internal db errors'
             print, ' dbinst', dbinst[0]
             print, ' dbinst', dbinst[1]
             help, dbinst
         endelse

         found=0
         return
      endif

      fitted = [0, 0, 0, 0, 0]

      i = where( fs eq 'fitted', count)
      if count ne 0 then fitted[i] = 1

      i = where( fs ne 'forced' and fs ne 'fitted', count)
      if   count gt 0 then $
            if silent ne 0  then $
            print, 'GETTRAN: ERROR! values in fitted:', fs[i]

      found = 1
      return
   endelse
end

;+
; NAME:
;  mysqldoc
; PURPOSE:   (one line only)
;  Build a documentation file from internal mySQL documentation.
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  mysqldoc,lun,table,outfile
; INPUTS:
;     lun     - The logical unit of the pipe (previously opened by openmysql).
;                   -or-
;               String with the name of the database to access.
;                 If string is supplied then the database is opened
;                 at the start and closed at the end.
;     table   - String containing name of mySQL table to document.
;                  Default is to process all tables in the database.
;                  If default is used, then outfile is ignored.
;                  If processing the entire database, then an extra file
;                  named "documentation.html" is created with a master index
;                  for the entire database.
;     outfile - String with file name for output (default=table+'.html')
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     DBNAME  - String with name of the database being examined.
;                  Default='' if the database is already open.  If the
;                  database name is provided in lun then the default is
;                  to use the value of lun.
;     OUTDIR  - String with the name of the directory to put the output files.
;                  Default=current directory
; OUTPUTS:
;     All output is to the output file.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   Requires access to a MySQL server (established by
;   use of openmysql) as well as valid permissions for reading
;   the database.
;
;   I don't yet know how to test and make sure the table sought actually
;     exists in the database.  For the moment, if you give a non-existent
;     table, the program will crash most unpleasantly.
; PROCEDURE:
;
;   Special information:
;     The documentation for a database resides in the "doc" table.
;     This table must be defined similar to this:
;       +-----------+-------------+------+-----+---------+-------+
;       | Field     | Type        | Null | Key | Default | Extra |
;       +-----------+-------------+------+-----+---------+-------+
;       | tablename | varchar(20) | YES  |     | NULL    |       |
;       | field     | varchar(20) | YES  |     | NULL    |       |
;       | units     | varchar(20) | YES  |     | NULL    |       |
;       | source    | varchar(80) | YES  |     | NULL    |       |
;       | info      | text        | YES  |     | NULL    |       |
;       +-----------+-------------+------+-----+---------+-------+
;     The exact lengths of the first four fields is not critical.
;     For a normal field in a database you will provide the tablename
;     and field along with its information in units,source,info.  An
;     indirect reference can be imbedded in the info field and looks
;     like <<key>> where key is replaced by a short string.  This string
;     will be searched for under the "field" field where tablename is
;     set to NULL.  The text found will replace <<key>> in the output
;     html file.  The general description of a table is stored in a
;     record with tablename="table" and field is NULL.  For these records
;     "source" will contain a short description that will populate the
;     master summary table and "info" will contain a detailed but general
;     description of the purpose and usage for the table.  There is also
;     one special record per doc database where tablename and field are
;     both NULL.  In this record, the info field will contain a general
;     description of the entire database which will appear on the master
;     index page.
;
;     In general, units, source, and info will all be filled in for every
;     database field.  Sometimes units make no sense for a field.  In this
;     case put [[none]] in the field to leave the units blank.  NULL is
;     reserved to indicate that no information has yet been provided.
;
;     If you want to suppress a documentation page for a table (ie., one
;     that is for internal testing purposes only), then set source to
;     xxHIDExx in the record where tablename="table" and field is NULL.
;   
; MODIFICATION HISTORY:
;   2005/01/19, Written by Marc W. Buie, Lowell Observatory
;   2005/02/01, MWB, special handling for enum types.
;   2005/10/10, MWB, added call to mysqldocscan validator
;-
pro mysqldoc,in_lun,table,outfile,DBNAME=dbname,OUTDIR=outdir

   self='mysqldoc: '
   if badpar(in_lun,[2,3,7],0,caller=self+'(in_lun) ',type=luntype) then return
   if badpar(table,[0,7],0,caller=self+'(table) ',default='') then return
   if badpar(outfile,[0,7],0,caller=self+'(outfile) ', $
                default=table+'.html') then return
   if luntype eq 7 then default_dbname=in_lun else default_dbname=''
   if badpar(dbname,[0,7],0,caller=self+'(DBNAME) ', $
                default=default_dbname) then return
   if badpar(outdir,[0,7],0,caller=self+'(OUTDIR) ', $
                default='') then return
   if outdir ne '' then outdir=addslash(outdir)

   if luntype eq 7 then begin
      openmysql,lun,in_lun
   endif else begin
      lun = in_lun
   endelse

   tab = string(byte(9))

   if table eq '' then begin

      mysqldocscan,lun

      cmd='show tables;'
      mysqlcmd,lun,cmd,result,ntables
      print,'Open    ',outdir+'documentation.html'
      openw,olun,outdir+'documentation.html',/get_lun
      printf,olun,'<html>'
      printf,olun,'<head>'
      printf,olun,'<title>Documentation index for database ',dbname,'</title>'
      printf,olun,'</head>'
      printf,olun,'<body>'
      printf,olun,'<h1>Documentation index for database <em>',dbname,'</em></h1>'

      ; pull general documentation on the database
      cmd='select info from doc where ' + $
          'tablename is null and field is null;'
      mysqlcmd,lun,cmd,info,nlines
      if nlines gt 1 then begin
         words=strsplit(info[1],'\\n',/extract,/regex)
         mysqlsub,lun,words,finalwords
         printf,olun,'<p>'
         printf,olun,finalwords
         printf,olun,'</p>'
      endif else begin
         printf,olun,'<p>No general description available.</p>'
      endelse
      printf,olun,''

      printf,olun,'<table border=1>'
      printf,olun,'<tr>'
      printf,olun,'<td align=center><b>Table</b></td>'
      printf,olun,'<td><b>&nbsp;Purpose</b></td>'
      printf,olun,'</tr>'
      for i=1,ntables-1 do begin
         table = result[i]
         if table eq 'doc' then continue
         cmd='select source from doc where ' + $
             'tablename='+quote(table)+' and field is null;'
         mysqlcmd,lun,cmd,ans,nlines
         if nlines eq 2 then begin
            if ans[1] eq 'xxHIDExx' then continue
         endif
         printf,olun,'<tr>'
         printf,olun,'<td align=center>'
         printf,olun,'<a href="',table,'.html">',table,'</a>'
         printf,olun,'</td>'
         if nlines eq 1 then begin
            printf,olun,'<td><small>No description available.</small></td>'
         endif else begin
            if ans[1] ne 'NULL' then $
               printf,olun,'<td>',ans[1],'</td>' $
            else $
               printf,olun,'<td><small>No description available.</small></td>'
         endelse
         printf,olun,'</tr>'
         mysqldoc,lun,table,table+'.html',dbname=dbname,outdir=outdir
      endfor
      printf,olun,'</table>'

      jd=systime(/julian)
      jdstr,jd,0,str
      printf,olun,'<hr>'
      printf,olun,'<address>'
      printf,olun,'Created by mysqldoc.pro ',str,' MST'
      printf,olun,'</address>'
      printf,olun,'</body>'
      printf,olun,'</html>'
      free_lun,olun
      print,'Close   ',outdir+'documentation.html'

   endif else begin

      print,'writing ',outdir+outfile
      openw,olun,outdir+outfile,/get_lun

      printf,olun,'<html>'
      printf,olun,'<head>'
      if dbname ne '' then $
         printf,olun,'<title>Documentation for table ',dbname,'.',table,'</title>' $
      else $
         printf,olun,'<title>Documentation for table ',table,'</title>'
      printf,olun,'</head>'
      printf,olun,'<body>'
      if dbname ne '' then $
         printf,olun,'<h1>Documentation for table <em>', $
                     dbname,'.',table,'</em></h1>' $
      else $
         printf,olun,'<h1>Documentation for table <em>',table,'</em></h1>'

      ; first pull general documentation on this table
      cmd='select info from doc where ' + $
          'tablename='+quote(table)+' and field is null;'
      mysqlcmd,lun,cmd,result,nlines
      if nlines gt 1 then begin
         words=strsplit(result[1],'\\n',/extract,/regex)
         mysqlsub,lun,words,finalwords
         printf,olun,'<p>'
         printf,olun,finalwords
         printf,olun,'</p>'
      endif else begin
         printf,olun,'<p>No general description available.</p>'
      endelse
      printf,olun,''

      cmd='describe '+table+';'
      mysqlcmd,lun,cmd,answer,nlines
      
      printf,olun,'<table border=1>'
      printf,olun,'<tr>'
      printf,olun,'<td align=center><b>Field<br>Name</b></td>'
      printf,olun,'<td align=center><b>data<br>type</b></td>'
      printf,olun,'<td align=center><b>units</b></td>'
      printf,olun,'<td align=center><b>source</b></td>'
      printf,olun,'<td><b>&nbsp;Description</b></td>'
      printf,olun,'</tr>'
      for i=1,nlines-1 do begin
         printf,olun,'<tr>'
         words=strsplit(answer[i],tab,/extract)
         printf,olun,'<td align=center>',words[0],'</td>'
         if strmid(words[1],0,4) eq 'enum' then begin
            parts = strsplit(words[1],',',/extract)
            printf,olun,'<td align=center>',strjoin(parts,', '),'</td>'
         endif else begin
            printf,olun,'<td align=center>',words[1],'</td>'
         endelse
         cmd='select tablename,field,units,source from doc where ' + $
             'tablename='+quote(table)+' and field='+quote(words[0])+';'
         mysqlcmd,lun,cmd,result,nlines
         if nlines eq 1 then begin
            printf,olun,'<td align=center>?</td>'
            printf,olun,'<td align=center>?</td>'
            printf,olun,'<td><small>No documentation available.</small></td>'
         endif else begin
            words=strsplit(result[1],tab,/extract)
            printf,olun,'<td align=center>'
            if words[2] eq 'NULL' then $
               printf,olun,'?' $
            else if words[2] eq '[[none]]' then $
               printf,olun,'&nbsp;' $
            else printf,olun,words[2]
            printf,olun,'</td>'
            printf,olun,'<td align=center>'
            if words[3] eq 'NULL' then $
               printf,olun,'?' $
            else if words[3] eq '[[none]]' then $
               printf,olun,'&nbsp;' $
            else printf,olun,words[3]
            printf,olun,'</td>'
            cmd='select info from doc where ' + $
                'tablename='+quote(words[0])+' and field='+quote(words[1])+';'
            mysqlcmd,lun,cmd,result,nlines
            if nlines gt 1 then begin
               words=strsplit(result[1],'\\n',/extract,/regex)
               mysqlsub,lun,words,finalwords
               printf,olun,'<td>'
               printf,olun,finalwords
               printf,olun,'</td>'
            endif else begin
               printf,olun,'<td>'
               printf,olun,'No documentation available.'
               printf,olun,'</td>'
            endelse
         endelse
         printf,olun,'</tr>'
         printf,olun,''
      endfor
      printf,olun,'</table>'

      jd=systime(/julian)
      jdstr,jd,0,str
      printf,olun,'<hr>'
      printf,olun,'<address>'
      printf,olun,'Created by mysqldoc.pro ',str,' MST'
      printf,olun,'</address>'
      printf,olun,'</body>'
      printf,olun,'</html>'
      free_lun,olun

   endelse

   if luntype eq 7 then free_lun,lun

end

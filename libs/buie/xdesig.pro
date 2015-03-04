;+
; NAME:
;  xdesig
; PURPOSE:
;  Digest and record Minor Planet Center designation cross references.
; DESCRIPTION:
;  This program reads one or more files containing cross reference information
;    from the Minor Planet Center.  These files contain pairs of columns where
;    the first column in the pair has the internal name as reported to the MPC
;    and the second column has the cross-reference designation returned by the
;    MPC.
;
;  Each object is located in the master cross reference file and then the
;    object data is updated to indicate the proper linkage.  Before saving any
;    changes, the file is copied to a file with a '.bak' appended to the file
;    name.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  xdesig,fnlist,objectfile
; INPUTS:
;  fnlist     - File name(s) to be read in with cross-references
;  objectfile - File name of master cross references, default='newobj.dat'
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  SWAP - Flag, if set reverses the order of the cross reference list.  The
;            normal order is to see a local designation in the first (or odd)
;            columns and the provisional designation is in the second (or
;            even) columns.  If this flag is set then the provisional
;            designation comes first.
;  OBLIST - File containing the list of pre-designation objects of interest.
;               Default=/net/frakir/raid/buie/kbo/oblist.dat
;             If the file exists, it is scanned and warnings are posted if
;             an object is seen in oblist.dat that is also in the
;             cross-reference files that are being processed.  Nothing is done
;             by this program.  It's up to the user to do something with
;             warning messages.
;  NODB    - Flag, if set will suppress saving new codes to des.newobj database.
;
;  NOSAVE  - Flag, if set will suppress any file or DB I/O to check program
;             operation.  This is not intended for dry-runs, just for program
;             debugging.
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
;  2000/05/03, Written by Marc W. Buie, Lowell Observatory
;  2001/10/03, MWB, upgraded to LONG loop counters.
;  2002/03/22, MWB, added SWAP keyword.
;  2002/09/03, MWB, changed Str_sep call to strsplit
;  2003/02/04, MWB, fixed minor bug in a warning statement
;  2003/08/15, MWB, added NODB keyword and database storage to des.newobj
;  2003/10/01, MWB, converted my Filecopy call to system file_copy routine
;  2004/02/09, MWB, change path to oblist.dat file.
;  2004/09/24, MWB, changed final file_copy to file_move for backup file
;  2006/12/05, MWB, added NOSAVE keyword, the creation of a backup of
;                      the newobj database, and suppress reposting
;                      cross-references that have already been posted.
;-
pro xdesig,fnlist,objectfile,SWAP=swap,OBLIST=oblist,NODB=nodb,NOSAVE=nosave

   self='XDESIG: '
   if badpar(fnlist,7,[0,1],CALLER=self+'(desigfile) ') then return
   if badpar(objectfile,[0,7],0,CALLER=self+'(objectfile) ', $
                default='newobj.dat') then return
   if badpar(swap,[0,1,2,3],0,CALLER=self+'(SWAP) ', default=0) then return
   if badpar(oblist,[0,7],0,CALLER=self+'(OBLIST) ', $
                default='/net/frakir/raid/buie/kbo/oblist.dat') then return
   if badpar(nodb,[0,1,2,3],0,CALLER=self+'(NODB) ', $
                default=0) then return
   if badpar(nosave,[0,1,2,3],0,CALLER=self+'(NOSAVE) ', $
                default=0) then return
   
   ; First see if master file exists.  Quit if no file.
   if not exists(objectfile) then begin
      print,'Master object list file, ',objectfile,' does not exist.  Aborting.'
      return
   endif

   ; Check on the oblist file
   chkoblist=1B
   if not exists(oblist) then begin
      print,'Oblist file ',oblist,' does not exist.'
      ans=''
      read,ans,prompt='Do you want to continue anyway? '
      if strmid(ans,0,1) ne 'y' then return
      chkoblist=0B
   endif
   if chkoblist then begin
      readcol,oblist,oblistid,discdat,assign,format='a,a,a'
   endif

   ; Next, make sure input files exist.
   ok=1
   nfiles=n_elements(fnlist)
   for i=0L,nfiles-1 do begin
      if not exists(fnlist[i]) then begin
         print,'Input file ',fnlist[i],' does not exist.'
         ok=0
      endif
   endfor
   if not ok then begin
      print,'Aborting.'
      return
   endif

   changed=0

   ; Load the master file
   openr,lun,objectfile,/get_lun
   line=''
   ncodes = 0L
   while not eof(lun) do begin
      readf,lun,line,format='(a1)'
      ncodes = ncodes+1
   endwhile
   point_lun,lun,0L
   tab = STRING( byte(9) )
   code=strarr(ncodes)
   disctag=strarr(ncodes)
   info=strarr(ncodes)
   lines=strarr(ncodes)
   for i=0L,ncodes-1 do begin
      readf,lun,line,format='(a)'
      lines[i] = line
      words=strsplit(line,tab,/extract)
      nwords=n_elements(words)
      code[i] = words[0]
      if nwords ge 2 then disctag[i]=words[1]
      if nwords ge 3 then info[i]=words[2]
   endfor
   free_lun,lun
   print,objectfile,' ',strn(ncodes),' entries.'

   ; open the database connection
   if not nodb then begin
      openmysql,dblun,'des'

      if not nosave then begin

         fnbak = '.newobj.bak/'+repstr(nobname(systime()),':','')+'.db.gz'
         print,'Backup database to ',fnbak

         cmd='mysqldump --opt des newobj | gzip > '+fnbak
         spawn,cmd,result,exit_status=exit_status
         if exit_status ne 0 then begin
            print,result
            print,'Backup failed, unable to proceed.'
            return
         endif

      endif

   endif

   for i=0L,nfiles-1 do begin
      first=1
      openr,lun,fnlist[i],/get_lun
      while not eof(lun) do begin
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         nwords=n_elements(words)
         if nwords/2*2 ne nwords then begin
            print,fnlist[i],'  [',line,']','  odd number of words.'
         endif else begin
            words=reform(words,2,nwords/2,/overwrite)
            if first then begin
               dlist = words
               first = 0
            endif else begin
               dlist = [[dlist],[words]]
            endelse
         endelse
      endwhile
      free_lun,lun

      ; Set the order of columns
      if swap then begin
        idx0 = 1
        idx1 = 0
      endif else begin
        idx0 = 0
        idx1 = 1
      endelse

      ids=dlist[idx1,*]
      ids=ids[*]
      dlist[idx1,*] = mpcdcvt(ids)
      dim=size(dlist,/dimen)
      if n_elements(dlist) eq 2 then $
         nlist = 1 $
      else $
         nlist = dim[1]
      print,fnlist[i],nlist,' entries.'
      for j=0L,nlist-1 do begin

         ; check against the object list file
         if chkoblist then begin
            z=where(oblistid eq dlist[idx0,j],count)
            if count ne 0 then begin
               z=z[0]
               print,'Warning!  ',oblistid[z],' found in the oblist file.'
               print,'oblist --->',oblistid[z],' ',discdat[z],' ',assign[z]
            endif
         endif

         goodval=0
         if not nodb then begin

            ; post the cross-reference to the des.newobj database
            localid = dlist[idx0,j]
            xref    = dlist[idx1,j]

            ; First, check to see if this entry has already been posted to
            ;  the database.
            cmd='select localid,lxref,desig,number,name,attrib from newobj' + $
                ' where localid='+quote(localid)+';'
            mysqlcmd,dblun,cmd,chkresult,nlines
            if nlines eq 2 then begin
               chkresult=chkresult[1]
            endif else if nlines gt 2 then begin
               print,'Multiple entries in database for ',localid
               print,'This should never happen!'
               chkresult=''
            endif else begin
               chkresult=''
            endelse

            ; break up the cross-reference and pull off parens
            xsec='Primary'
            if strmid(xref,0,1) eq '(' and $
               strmid(xref,0,1,/REVERSE_OFFSET)  eq ')' then begin
               xref=strmid(xref,1,length-2)
            endif else if strmid(xref,0,1) eq '(' then begin
               xref=strmid(xref,1)
               xsec='Secondary'
            endif

            ; See if "new" cross reference has already been posted to the
            ;   database in some form.  Only allow further modification of
            ;   database if this is a new designation.
            if strpos(chkresult,xref) lt 0 then begin
               packed = mpcdcvt(xref)
               ; if numbered, this is the test that passes
               if packed eq xref then begin
                  if stregex(xref,'.*[A-Z].*') ge 0 then $
                     cmd='update newobj set desig='+quote(xref) + $
                         ',attrib='+quote(xsec)+ $
                         ' where localid='+quote(localid)+';' $
                  else $
                     cmd='update newobj set number='+xref + $
                         ',attrib='+quote(xsec)+ $
                         ' where localid='+quote(localid)+';'
               endif else begin
                  ; is it one of the funny objects?
                  if stregex(xref,'.*(T[123]|PL)^') ge 0 then xref='E'+xref
                  cmd='update newobj set desig='+quote(xref) + $
                      ',attrib='+quote(xsec)+ $
                      ' where localid='+quote(localid)+';'
               endelse

               goodval=1
               print,cmd
               if not nosave then $
                  mysqlcmd,dblun,cmd,answer,nlines
            endif else begin
               print,xref,' already posted for ',localid,', skipping.'
            endelse

         endif

         ; modify the information that will be in the newobj.dat file
         z=where(code eq dlist[idx0,j],count)
         z=z[0]
         if count ne 1 then begin
            print,'Code ',dlist[idx0,j],' found ',strn(count),' times.'
         endif else begin
            if info[z] eq '' then begin
               changed=1
               info[z] = dlist[idx1,j]
               lines[z] = code[z]+tab+disctag[z]+tab+info[z]
               if nodb then print,lines[z]
            endif else begin
               pos=strpos(info[z],dlist[idx1,j],0)
               if pos lt 0 then begin
                  changed=1
                  info[z] = dlist[idx1,j]+' '+info[z]
                  lines[z] = code[z]+tab+disctag[z]+tab+info[z]
                  if nodb then print,lines[z]
               endif else begin
;                  print,'Code ',dlist[idx0,j],', redundant tag: ',info[z]
               endelse
            endelse
         endelse

      endfor

   endfor

   if not nosave then begin
      if changed then begin
         file_move,objectfile,objectfile+'.bak',/noexpand_path,/overwrite
         print,'Saving changes to ',objectfile
         openw,lun,objectfile,/get_lun
         for i=0L,ncodes-1 do $
            printf,lun,lines[i]
         free_lun,lun
      endif else begin
         print,'No new cross-references, no changes made to ',objectfile
      endelse
   endif

   if not nodb then free_lun,dblun

end

;+
; NAME: 
;  destat
; PURPOSE:   (one line only)
;  to gather pertinent DES statistics and put them into an html file
; DESCRIPTION:
;  DEStat reads the .obj files for an oberving night and creates an
;  html file with three tables.  The first table contains all the
;  objects that were eventually marked "yes" in the .obj files.  The
;  second table contains all the objects marked "no", and the third
;  contains all of the objects known to be salted objects.  The rows
;  of a table contain a team member's initials, and the columns
;  contain information as to where their initials were located in the
;  .obj string.  This indicates whether a person discovered the
;  object, confirmed an object discovered by the auto run, etc. The
;  total number of objects are in parentheses, with the number of KBOs
;  in front of them.  A fourth table contains the names of the
;  "lookers" for this night, with corresponding initials.  It also
;  contains the number of fields where their initials appeared.  The
;  bottom of the file is dated.  
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  destat,root
; INPUTS:
;  root     :String- Six digit date: 'yymmdd'. 
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  REDUCED  :String- The path to the reduced files.  This should not
;                    include the root. Default='/net/frakir/raid/reduced/'
;  OUTDIR   :String- The path for the html file.  (Default = '')
;  HTMLNAME :String- The name of the html file.  Default = root+'.html' 
;  QUIET    :Flag-   Set for no printed output to screen.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  04/07/27,Written by Maureen Teyssier, REU at Lowell Obs.
;  2004/08/24, MWB, incorporated into library.
;  2004/08/31, MWB, added code to handle "crud" objects
;  2004/11/19, MWB, added code to cross reference against newobj database
;  2005/1/20, MWB, fix error in final table generation
;-

pro destat,root,REDUCED=reduced,OUTDIR=outdir,HTMLNAME=htmlname,quiet=quiet

   self='DESTAT: '
   if badpar(root,7,0,CALLER=self+'(root)') then return
   if badpar(reduced,[0,7],0,CALLER=self+'(REDUCED)', $
                     def='/net/frakir/raid/reduced/') then return
   if badpar(htmlname,[0,7],0,CALLER=self+'(htmlname)', $
                      def=root+'.html') then return
   if badpar(outdir,[0,7],0,CALLER=self+'(OUTDIR)', def='') then return
   if badpar(quiet,[0,1,2,3],0,CAller=self+'(quiet)',def=0) then return

   rd=strmid(root,4,2)

   reduced=addslash(reduced)
   reddir=addslash(reduced+root)
   outdir=addslash(outdir)

   ;look for obj files
   fnobj=file_search(reddir+'*.obj',count=nfiles)
   if nfiles eq 0 then begin
      if not quiet then print,'This night does not contain data.'
      return
   endif
   if not quiet then print,strn(nfiles),' obj files were found'
   ;insert safety check for obj files
   if nfiles/8*8 ne nfiles then begin
      print,root,': ERROR: There are .obj files missing.'
      return
   endif

   ;open the database connection
   openmysql,dblun,'des'

   ;get the "interesting" list from the vclass database.
   yr=fix(strmid(root,0,2))
   if yr gt 90 then yr += 1900 else yr += 2000
   rundate=strn(yr)+'-'+strmid(root,2,2)+'-'+strmid(root,4,2)
   cmd=['select vclass.lookerid,objectid from vclass where vclass.des=1 and', $
          'rundate='+quote(rundate)+' order by lookerid;']

   cmd=['select vclass.lookerid,objectid', $
        'from vclass', $
        'left join xref on', $
        'vclass.lookerid=xref.lookerid and', $
        'vclass.rundate=xref.rundate', $
        'where vclass.des=1 and vclass.rundate='+quote(rundate), $
        'order by vclass.lookerid;']
   if not quiet then print,cmd
   mysqlquery,dblun,cmd,desobj,objectid,format='a,a'
   if not quiet then print,'Interesting objects:',desobj
   desidstr=strarr(n_elements(desobj))

   ;gather initials and names from the database
   cmd='select initials,name from looker;'
   if not quiet then print,cmd
   mysqlquery,dblun,cmd,dbinitials,dbnames,format='a,a'
   if not quiet then print,'Initials in database ',dbinitials

   ;gather initials from the looker id files
   if not exists(reddir+'looker.ids') then begin
      print,self,'Error! Cannot find ',reddir+'looker.ids'
      return
   endif
   rdmatch,reddir+'looker.ids',initials,names
   if not quiet then print,'Initials in looker.ids file ',initials

   ;gather initials from .obj files
   objinitials='auto'
   for i=0,nfiles-1 do begin
      rdoblist,fnobj[i],nobj,filelist,dt,offset,pos,flags,idstr
      for j=0,nobj-1 do begin
         words=strsplit(idstr[j],',',/extract)
         objinitials = [objinitials,words]
      endfor
      objinitials = objinitials[uniq(objinitials,sort(objinitials))]
   endfor
   if not quiet then print,'Initials in obj files ',objinitials
   nlookers = n_elements(objinitials)
   objnames=strarr(nlookers)

   ; scan the list of initials from the obj files against the initials
   ;   found in the looker.ids file and against the database.
   ;   Make sure to ignore 'auto' and 'salt'.
   foundit = bytarr(nlookers)
   for i=0,nlookers-1 do begin

      if objinitials[i] eq 'salt' then begin
         foundit[i] = 3
         objnames[i] = 'salt'
         continue
      endif
      
      if objinitials[i] eq 'auto' then begin
         foundit[i] = 3
         objnames[i] = 'auto'
         continue
      endif

      if objinitials[i] eq 'crud' then begin
         foundit[i] = 3
         objnames[i] = 'crud'
         continue
      endif

      ; is it in looker.ids file?
      z=where(objinitials[i] eq initials,count)
      if count ne 0 then begin
         foundit[i] = 1
         objnames[i] = names[z[0]]
      endif

      ; is it in the database?
      z=where(objinitials[i] eq dbinitials,count)
      if count ne 0 then begin
         foundit[i] += 2
         objnames[i] = dbnames[z[0]]
      endif

   endfor

   ;   If any are found in the database and not in the looker.ids file
   ;   add them to the existing looker.ids file.
   z=where(foundit eq 2,count)
   if count ne 0 then begin
      initials = [initials,objinitials[z]]
      names = [names,objnames[z]]
      if not quiet then print,'Adding to looker.ids: ',objinitials[z]
      wrmatch,initials,names,reddir+'looker.ids'
      foundit[z] = 3
   endif

   ; Are any of the entries in looker.ids not in the database?  If so,
   ;   squawk about it.  This should not be suppressed by the quiet flag.
   z=where(foundit eq 1,count)
   if count ne 0 then begin
      print,'WARNING!  There are ',strn(count),' initials found in the obj'
      print,root,'      files and looker.ids file that are not in the database'
      print,'   ',objinitials[z]
   endif

   ; Are there any entries missing in both the looker.ids file and the database?
   ;  If so, squawk, again, don't honor the quiet flag for this stuff
   z=where(foundit eq 0,count)
   if count ne 0 then begin
      print,'WARNING!  There are ',strn(count),' initials found in the obj'
      print,root,'       files that are not in the database'
      print,'   ',objinitials[z]
   endif

   ; sort the list by initials
   idx = sort(objinitials)
   objinitials = objinitials[idx]
   objnames    = objnames[idx]

   ; init arrays for information table
   uf   = lonarr(3,nlookers) ;; you first
   af   = lonarr(3,nlookers) ;; auto first
   au   = lonarr(3,nlookers) ;; auto, then you
   um   = lonarr(3,nlookers) ;; you in the middle
   ums  = lonarr(3,nlookers) ;; you in the middle, salt tag
   ul   = lonarr(3,nlookers) ;; you last
   uls  = lonarr(3,nlookers) ;; you last, salt tag
   kuf  = lonarr(3,nlookers) ;; KBO, you first
   kaf  = lonarr(3,nlookers) ;; KBO, auto first
   kau  = lonarr(3,nlookers) ;; KBO, auto, then you
   kum  = lonarr(3,nlookers) ;; KBO, you in the middle
   kums = lonarr(3,nlookers) ;; KBO, you in the middle, salt tag
   kul  = lonarr(3,nlookers) ;; KBO, you last
   kuls = lonarr(3,nlookers) ;; KBO, you last, salt tag
   nlk  = lonarr(nlookers)   ; number of CCDs you "touched"

   table=['Objects marked "yes"','Objects marked "no"','Salted objects']
   checked=['confirmed','checked','confirmed']
   flagit=['y','n','s']
   iy = 0
   in = 1
   is = 2

   ; now loop over all obj files and extract desired information
   for i=0,nfiles-1 do begin

      ; read the object list.
      rdoblist,fnobj[i],nobj,filelist,dt,offset,pos,flags,idstr

      fl=strsplit(filelist[0],'.',/extract)
      ob=strsplit(fnobj[i],'/',/extract)
      tob=strsplit(ob[n_elements(ob)-1],'.',/extract)
      fd=strsplit(tob[0],'x',/extract)

      ; get a unique list of looker initials from this obj file
      tmpi = 'auto'
      for j=0,nobj-1 do begin
         words=strsplit(idstr[j],',',/extract)
         tmpi = [tmpi,words]
      endfor
      tmpi = tmpi[uniq(tmpi,sort(tmpi))]

      ; increment nlk array for lookers present in tmpi
      for j=0,n_elements(tmpi)-1 do begin
         z=where(tmpi[j] eq objinitials)
         nlk[z[0]]++
      endfor

      ; now increment the various scores for each obj in the obj file
      for j=0,nobj-1 do begin
         sepid=strsplit(idstr[j],',',/extract,count=nsepid)

         ; this will be the first index into the tally arrays
         if flags[j] eq '?' then begin
            print,'FATAL ERROR!  ',root,' contains an obj file with a ?'
            return
         endif
         zf = where(flags[j] eq flagit)
         zf = zf[0]

         ; build the object name
         name = rd+fl[1]+fd[1]+strb36(j,pad=2)

         ;see if the name is on the "interesting" list
         ; if so, set the kbo flag to 'true'
         z=where(name eq desobj,desct)
         if desct eq 1 then begin
            kbo=1
            desidstr[z[0]] = idstr[j]
         endif else begin
            kbo=0
         endelse

         ; does this object have a salt tag in the id list?
         z=where(sepid eq 'salt',saltct)
         if saltct gt 0 then salt=1 else salt=0

         for k=0,nsepid-1 do begin
            zi = where(sepid[k] eq objinitials)
            zi = zi[0]

            if k eq 0 then begin
               uf[zf,zi]++
               if kbo then kuf[zf,zi]++
               if sepid[k] eq 'auto' then af[zf,zi]++
               if kbo and sepid[k] eq 'auto' then kaf[zf,zi]++
            endif else if k eq nsepid-1 then begin
               ul[zf,zi]++
               if kbo then kul[zf,zi]++
               if k eq 1 and sepid[0] eq 'auto' then au[zf,zi]++
               if salt then ums[zf,zi]++
               if kbo and k eq 1 and sepid[0] eq 'auto' then kau[zf,zi]++
               if kbo and salt then kums[zf,zi]++
            endif else begin
               um[zf,zi]++
               if kbo then kum[zf,zi]++
               if salt then uls[zf,zi]++
               if kbo and salt then kuls[zf,zi]++
            endelse
            
         endfor

      endfor

   endfor

   ;writing the html file
   openw,lun,outdir+htmlname,/get_lun
   printf,lun,'<html>'
   printf,lun,'<title>Statistics for the night of ',root,'</title>'
   printf,lun,'<body>'
   printf,lun,'<center>'
   printf,lun,'<h1> DES: Looker Statistics for the night of ',root,'</h1>'
   printf,lun,'</center>'
   printf,lun,'<hr>'
   printf,lun,'The nights results are organized into three tables,'
   printf,lun,' corresponding to a final flag of "y," "n," or "s"' 
   printf,lun,' (yes, no or salt) in the .obj file.'
   printf,lun,'<p>'
   printf,lun,'The counts in the columns marked as'
   printf,lun,' partial salted cases are the result of salted objects'
   printf,lun,' interfering with the detection of objects that would'
   printf,lun,' otherwise be counted yes or no.  For example, if a marked'
   printf,lun,' red-blue pair was half real object, and half salt, it'
   printf,lun,' would result in a'
   printf,lun,' count in the partial salt column in a "yes" table.'    
   printf,lun,'<p>'
   printf,lun,'The total number of objects are in parenthesis; the'   
   printf,lun,' number of KBOs are in front.'
   printf,lun,'<p>'
   printf,lun,'<hr>'

   ;construct three tables: y,n,s
   for itable=0,n_elements(table)-1 do begin 

      printf,lun,'<h2>',table[itable],'</h2>'
      printf,lun,'<table width=100% border=1>'
      printf,lun,'   <tr valign=top>'
      printf,lun,'      <td>Initials of the "looker"</td>'
      printf,lun,'      <td>object you found</td>';yes
      printf,lun,'      <td>object auto found</td>'
      printf,lun,'      <td>"auto object" you ',checked[itable],'</td>'
      printf,lun,'      <td>object you ',checked[itable],'</td>'

      ;don't make this column for the salted table
      if itable ne 2 then $
         printf,lun,'      <td>auto object you ',checked[itable], $
                    '- partial salted case</td>'

      printf,lun,'      <td>you were the last to check this object </td>'

      ;don't make this column for the salted table
      if itable ne 2 then $
         printf,lun,'      <td>you were the last to check this object-', $
                    ' partial salted case</td>'   

      printf,lun,'   </tr>'

      for i=0,nlookers-1 do begin
         if objinitials[i] eq 'salt' then continue

         printf,lun,'   <tr>'
         if flagit[itable] eq 'y' then begin
            ;build the rows of the 'y' table
            printf,lun,'<td align=center>',objinitials[i],'</td>'
            printf,lun,'<td align=center>',kuf[iy,i],'  (',strn(uf[iy,i]),')','</td>'
            printf,lun,'<td align=center>',kaf[iy,i],'  (',strn(af[iy,i]),')','</td>'
            printf,lun,'<td align=center>',kau[iy,i],'  (',strn(au[iy,i]),')','</td>'
            printf,lun,'<td align=center>',kum[iy,i],'  (',strn(um[iy,i]),')','</td>'
            printf,lun,'<td align=center>',kums[iy,i],'  (',strn(ums[iy,i]),')','</td>'
            printf,lun,'<td align=center>',kul[iy,i],'  (',strn(ul[iy,i]),')','</td>'
            printf,lun,'<td align=center>',kuls[iy,i],'  (',strn(uls[iy,i]),')','</td>'
         endif else begin
            if flagit[itable] eq 'n' then ik=in else ik=is
            ;build the rows of the other tables
            printf,lun,'<td align=center>',objinitials[i],'</td>'
            printf,lun,'<td align=center>',uf[ik,i],'</td>'
            printf,lun,'<td align=center>',af[ik,i],'</td>'
            printf,lun,'<td align=center>',au[ik,i],'</td>'
            printf,lun,'<td align=center>',um[ik,i],'</td>'
            ;don't make this column for the salted table
            if flagit[itable] ne 's' then $  
               printf,lun,'<td align=center>',ums[ik,i],'</td>'
            printf,lun,'<td align=center>',ul[ik,i],'</td>'
            ;don't make this column for the salted table
            if flagit[itable] ne 's' then $  
               printf,lun,'<td align=center>',uls[ik,i],'</td>'
         endelse
         printf,lun,'</tr>'
      endfor

      printf,lun,'   </table>'
      printf,lun,'<p>'
      printf,lun,'<hr>'

   endfor;table

   printf,lun,'<p>'
   printf,lun,'<h2>Looker team for this night</h2>'
   ;make a table with initials, name and # of fields
   printf,lun,'<table border=1>'
   printf,lun,'<tr>'
   printf,lun,'   <td>Initials of the "looker"</td>'
   printf,lun,'   <td>Name of the "looker"</td>'
   printf,lun,'   <td>Number of images marked</td>'
   printf,lun,'</tr>'
   for i=0,nlookers-1 do begin
      if objinitials[i] eq 'salt' then continue
      if objinitials[i] eq 'auto' then continue
      if objinitials[i] eq 'crud' then continue
      printf,lun,'<tr>'
      printf,lun,'   <td align=center>',objinitials[i],'</td>'
      printf,lun,'   <td align=center>',objnames[i],'</td>'
      printf,lun,'   <td align=center>',nlk[i],'</td>'
      printf,lun,'</tr>'
   endfor
   printf,lun,'</table>'
   printf,lun,'<p> <p>'

   printf,lun,'<h2>List of interesting objects discovered on this night</h2>'
   printf,lun,'<table border=1>'
   printf,lun,'<tr>'
   printf,lun,'   <td>Looker ID</td>'
   printf,lun,'   <td>Object ID</td>'
   printf,lun,'   <td>Record of lookers</td>'
   printf,lun,'</tr>'
   for i=0,n_elements(desobj)-1 do begin
      printf,lun,'<tr>'
      printf,lun,'<td>',desobj[i],'</td>'
      if objectid[i] eq 'NULL' then begin
         printf,lun,'   <td>&nbsp;</td>'
      endif else begin
         cmd='select desig,number,name from newobj where localid=' + $
             quote(objectid[i])+';'
         mysqlquery,dblun,cmd,desig,number,finalname,format='a,a,a'
         if number[0] ne '' and number[0] ne 'NULL' and $
            finalname[0] ne '' and finalname[0] ne 'NULL' then begin
            printf,lun,'<td align=center> (',number[0],') ',finalname[0],'</td>'
         endif else if number[0] ne '' and number[0] ne 'NULL' then begin
            printf,lun,'<td align=center> (',number[0],') ',desig[0],'</td>'
         endif else if desig[0] ne '' and desig[0] ne 'NULL' then begin
            printf,lun,'<td align=center>',desig[0],'</td>'
         endif else begin
            printf,lun,'<td align=center>',objectid[i],'</td>'
         endelse
      endelse
      printf,lun,'<td>',desidstr[i],'</td>'
      printf,lun,'</tr>'
   endfor
   printf,lun,'</table>'

   printf,lun,'<p>'

   ;put the date and the time on the bottom
   jdate=systime(/julian)
   jdstr,jdate,0,date
   printf,lun,'<i>This file was generated on ',date,'.</i>'
   printf,lun,'</body>'
   printf,lun,'</html>'

   free_lun,lun,dblun

end

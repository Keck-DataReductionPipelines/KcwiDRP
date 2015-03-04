;+
; NAME:
;  objcheck
; PURPOSE:
;  Validate and summarize object positions and astrometry
; DESCRIPTION:
;  Reads astrometry and obj files from the current directory looking for
;    irregularites.  Recommendations are provided that suggest running astrom
;    or looker on specific frames.  These recommendations always point to
;    problem areas but sometimes the fix is something that takes care of a
;    problem that astrom or looker had with that frame, rather than something
;    that can be fixed by just running the programs.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  objcheck
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  JUSTLOOK   - Flag, if set turns off some printouts.  This is not that useful
;                 and may well disappear in later versions.
;
;  KILLSINGLE - Flag, as the obj files are read, scan for objects marked 'y' that
;                 have zero or one valid positions.  For any found, mark them
;                 'n' and add "crud" to the id string.
;
; OUTPUTS:
;  The file, check.lst, is written with the same information that is sent
;    to the screen to facilitate cleaning up any problems found.
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
;  98/11/23 - Written by Marc W. Buie, Lowell Observatory
;  2000/09/19, MWB, added KILLSINGLE keyword
;  2002/09/03, MWB, changed Str_sep call to strsplit
;  2004/9/21, MWB, removed obsolete call to Findfile
;-
pro objcheck,JUSTLOOK=justlook,KILLSINGLE=killsingle
   
   objfiles = file_search('*.obj',count=nobjfile)
   if not keyword_set(justlook) then justlook=0
   if not keyword_set(killsingle) then killsingle=0

   nscale=0
   scaleid=['']
   scale=[0]
   nbad1=0
   nbad2=0
   rthresh1 = 0.034
   rthresh2 = 0.10

   if nobjfile eq 0 then begin
      print,'No object files found, unable to continue'
      return
   endif

   outfile='check.lst'
   openw,lunout,outfile,/get_lun
   ratefile='badrate.lst'
   openw,lunrate,ratefile,/get_lun

   astfiles = file_search('*.ast',count=nastfile)
   if nastfile eq 0 then begin
      print,'There are no astrometry files (.ast) to check, quitting.'
      return
   endif

   print,'Number of object files        ',nobjfile
   printf,lunout,'Number of object files        ',nobjfile

   print,'Number of raw astrometry files',nastfile
   printf,lunout,'Number of raw astrometry files',nastfile

   ; Load all the raw astrometry
   if nastfile ne 0 then begin
      print,'Loading raw astrometry data....'
      nobslist=intarr(nastfile)
      FOR i=0,nastfile-1 DO BEGIN
         obj=strsplit(astfiles[i],'.',/extract)
         obj=strupcase(obj[0])
         newobj=0

         ; Process the data for this object
         openr,lun,astfiles[i],/get_lun

         ; Collect all the observations for this file and add it to the master list.
         j=0
         line=''
         WHILE not eof(lun) DO BEGIN
            readf,lun,line,format='(a)'
            words=strsplit(line,' ',/extract)
            if i eq 0 and j eq 0 then begin
               astid=obj
               frid= words[0]
               jd  = double(words[1])
               ra  = words[2]
               dec = words[3]
               mag = float(words[4])
            endif else begin
               astid=[astid,obj]
               frid= [frid,words[0]]
               jd  = [jd,double(words[1])]
               ra  = [ra,words[2]]
               dec = [dec,words[3]]
               mag = [mag,float(words[4])]
            endelse
            j=j+1
         ENDWHILE
         nobslist[i] = j

         free_lun,lun
      ENDFOR
      astmatched = lonarr(n_elements(frid))
   endif

   ; Go through each obj file, read and compare against astrometry
   countunk = 0L
   tag=''
   for i=0,nobjfile-1 do begin
      rdoblist,objfiles[i],nobj,filelist,dt,offset,pos,flags,idstr,nfiles
      toffset=reform(offset,2,nfiles-1)
      xoffset=toffset[0,*]
      xoffset=xoffset[*]
      yoffset=toffset[1,*]
      yoffset=yoffset[*]

      if nobj ne 0 and nfiles ne 0 then begin
         words=strmid(objfiles[i],0,strlen(objfiles[i])-4)
         xpos = rstrpos(words,'x')
         if xpos ge 1 then begin
            field  = strmid(words,0,xpos)
            exttag = strmid(words,xpos+1,999)
         endif else begin
            field  = words
            exttag = ''
         endelse

         if killsingle then begin
            dirty=0
;print,objfiles[i],nobj,nfiles
            for j=0,nobj-1 do begin
               z=where(pos[*,j] lt 0.0,count)
               count = count/2
;if flags[j] eq 'y' and min(pos[*,j]) lt 0.0 then print,pos[*,j],count
               if flags[j] eq 'y' and nfiles-count le 1 then begin
                  dirty=1
                  flags[j] = 'n'
                  idstr[j] = idstr[j]+',crud'
               endif
            endfor
            if dirty then $
               wroblist,objfiles[i],nobj,filelist,dt,offset,pos,flags, $
                  idstr,nfiles
         endif

         z=where(flags eq '?',count)
         countunk = countunk+count
         if count ne 0 then begin
            print,count,' object marked as unknown on field ',field
            printf,lunout,count,' object marked as unknown on field ',field
         endif

         z=where(flags eq 'y',count)
         if count ne 0 then begin
            f1 = strsplit(filelist[0],'.',/extract)
            f1 = strmid(f1[0],strlen(f1[0])-2,2)
            fno = strsplit(filelist[0],'.',/extract)
            fno = fno[1]
            for j=0,count-1 do begin
               objid = strb36(z[j],pad=2)
               objname = f1+fno+exttag+objid
               for k=0,nfiles-1 do begin
                  tag=''
                  fname = filelist[k]+'x'+exttag
                  if nastfile ne 0 then begin
                     zz = where( fname eq frid and objname eq astid, countzz)
;                     zz = where( objname eq astid, countzz)
;                     zz = where( fname eq frid, countzz)
                     if countzz eq 0 then begin
                        if not justlook then tag = ' [ run astrom ]'
                     endif else if countzz eq 1 then begin
                        tag = ''
                        astmatched[zz] = astmatched[zz] + 1
                     endif else begin
                        tag = '!!!!!!!!'
                     endelse
                  endif
                  if pos[2*k,z[j]] lt 0.0 or pos[2*k+1,z[j]] lt 0.0 then $
                     tag = ' [ run looker ]'
                  if tag ne '' then begin
                     print,filelist[k],'  ',objname,' ',fname,' ',field,' ',tag
                     printf,lunout,filelist[k],'  ',objname,' ',fname,' ',field,' ',tag
                  endif
               endfor

               ; Final rate check of this object
               zz = where( objname eq astid, countzz)
               if countzz gt 1 then begin

                  ; compute average rate of motion from ra,dec
                  tjd  = jd[zz]
                  tra  = raparse(ra[zz])
                  tdec = decparse(dec[zz])
                  dist = sqrt( ((tra[1:*]-tra[0])*cos(tdec[0]))^2 + $
                               (tdec[1:*]-tdec[0])^2 ) * !radeg * 3600.0
                  time = (tjd[1:*]-tjd[0])*24.0
                  rdrate = dist/time
                  if n_elements(rdrate) gt 1 then rdrate = mean(rdrate)
                  rdrate=rdrate[0]

                  ; compute average rate of motion from x,y
                  ; z[j] is pointer to current object in obj file
                  tpos = pos[*,z[j]]
                  tpos = reform(tpos,2,nfiles)
                  xpos = tpos[0,*]
                  xpos = xpos[*]
                  ypos = tpos[1,*]
                  ypos = ypos[*]
                  zgood = where( xpos[1:*] ge 0.0 and ypos[1:*] ge 0.0, ngood)
                  xpos[1:*] = xpos[1:*] - xoffset
                  ypos[1:*] = ypos[1:*] - yoffset
                  dist = sqrt( (xpos[1:*]-xpos[0])^2 + $
                               (ypos[1:*]-ypos[0])^2 ) ; in pixels
                  ; get scale
                  zz = where(exttag eq scaleid,countzz)
                  if nscale eq 0 or countzz eq 0 then begin
                     finf = 'astrom'+exttag+'.inf'

                     version=''
                     openr,luninf,finf,/get_lun
                     readf,luninf,version,format='(a)'
                     if version ne 'ASTROM v1.0' then begin
                        print,'Illegal astrom.inf file, version tag is wrong.'
                        return
                     endif
                     readf,luninf,raoff,decoff
                     readf,luninf,pscale0
                     readf,luninf,rang0
                     xflip=1
                     yflip=1
                     readf,luninf,xflip,yflip,format='(i2,1x,i2)'
                     free_lun,luninf

                     if nscale eq 0 then begin
                        scaleid = exttag
                        scale   = pscale0
                        nscale  = 1
                     endif else begin
                        scaleid = [scaleid,exttag]
                        scale   = [scale,pscale0]
                        nscale  = nscale+1
                     endelse
                     zz=nscale-1

                  endif
                  xyrate = ((dist[zgood]*scale[zz[0]]) / dt)
                  if n_elements(xyrate) gt 1 then xyrate=mean(xyrate)
                  xyrate=xyrate[0]
                  rerr = abs(xyrate-rdrate)
                  rfrac = rerr / rdrate

                  if rfrac gt rthresh2 then begin
                     print,objname,xyrate,rdrate,rerr,rfrac, $
                     format='(a,1x,3(1x,f5.1),1x,f4.2)'
                     nbad2 = nbad2+1
                     btag = ' BBB'
                  endif else btag=''
                  if rfrac gt rthresh1 then begin
                     printf,lunrate,objname,xyrate,rdrate,rerr,rfrac,btag, $
                     format='(a,1x,3(1x,f5.1),1x,f4.2,a)'
                     nbad1 = nbad1+1
                  endif

               endif
               
            endfor ; end 'y' object loop

         endif ; block for 'y' objects

      endif ; process object list block

;if i eq 24 then return
   endfor ; end obj file loop

   if countunk ne 0 then begin
      print,countunk,' total objects marked as unknown.'
      printf,lunout,countunk,' total objects marked as unknown.'
   endif

   if nastfile ne 0 and not justlook then begin
      z = where(astmatched ne 1,count)
      if count ne 0 then begin
         print,'Unmatched or multiply matched astrometry (try rerunning astrom):'
         printf,lunout,'Unmatched or multiply matched astrometry (try rerunning astrom):'
         for i=0,count-1 do begin
            if astmatched[z[i]] eq 0 then tag=' no match' else tag=' multiple matches'
            print,astid[z[i]],' ',frid[z[i]],' ',tag
            printf,lunout,astid[z[i]],' ',frid[z[i]],' ',tag
         endfor
      endif
   endif

   free_lun,lunout,lunrate

   if nbad1 gt 1 then $
      print,strn(nbad1),' objects with discrepant rates (>', $
            strn(fix(rthresh1*100+0.5)),'%).'
   if nbad2 gt 1 then print,strn(nbad2),' objects with discrepant rates (>', $
            strn(fix(rthresh2*100+0.5)),'%).'

end

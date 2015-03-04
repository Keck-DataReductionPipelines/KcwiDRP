;+
; NAME:
;  linkobj
; PURPOSE:
;  Cross check three source lists from one field and identify moving objects.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  linkobj,tag,fna,fnb,fnc,CANDIDATE=cand,NODISPLAY=nodisplay
; INPUTS:
;  tag - name of the field, should not contain any blanks
;  fna - file name for the earliest image (Frame A)
;  fnb - file name for the middle image (Frame B)
;  fnc - file name for the last image (Frame C)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  BADCOL    - Array of column numbers, any source found within 2.5 pixels of
;                these columns will be discarded.  Default = no bad columns.
;
;  CANDIDATE - Flag, used for testing, when set will plot all detected sources
;                in all images.  If you think a triplet should have been picked
;                up, set this to see if all three images were actually in the
;                original source list.
;
;  KEYLIST=  - Name of a file containing a correspondence list. This list
;                 associates a set of standard names with the actual keyword
;                 names found in a FITS file header. If this keyword is
;                 omitted, a default list is used, as if a file with the
;                 following contents had been supplied:
;                  AIRMASS   K  AIRMASS
;                  DATE      K  DATE-OBS
;                  DATETMPL  T  DD-MM-YYYY
;                  EXPDELTA  V  0.0
;                  EXPTIME   K  EXPTIME
;                  FILTER    K  FILTERS
;                  FILENAME  K  CCDFNAME
;                  OBJECT    K  OBJECT
;                  UT        K  UT 
;                 The middle column is a flag. It may be K, for Keyword,
;                 T, for Template, or V, for Value. If it is V, the contents
;                 of the third field on that line should make sense for the
;                 name in the first field.
;
;  MAXRATE   - Maximum motion rate permitted for a tripet, in pixels/hour.
;                The default is 50.0 pixels/hour.
;
;  MINRATE   - Minimum motion rate permitted for a tripet, in pixels/hour.
;                The default is 1.0 pixels/hour.
;
;  NODISPLAY - Flag, when set will suppress all image display allowing program
;                to be run in background or batch mode.  This will be somewhat
;                faster as well.  The display steps take a small but non-trivial
;                amount of time.
;
;  PATH      - Optional path for original image directory.
;                If not specified, the current directory is used.
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
; MODifICATION HISTORY:
;   1998/03/11, Written by Marc W. Buie, Lowell Observatory
;   1999/03/22, extracted frmdxdy.pro
;   2010/07/19, MWB, minor tweak to accomodate change to frmdxdy and
;                    stylistic edits.
;-
pro linkobj,tag,fna,fnb,fnc,CANDIDATE=cand,NODISPLAY=nodisplay, $
       MAXRATE=maxrate, MINRATE=minrate, KEYLIST=keylist, PATH=path, $
       BADCOL=badcol,EXTLIST=extlist

   if n_params() eq 0 then begin
      doc_library,'linkobj'
      return
   endif

   self='LINKOBJ: '
   if badpar(tag,7,0,CALLER=self+'(tag) ') then return
   if badpar(fna,7,0,CALLER=self+'(fna) ') then return
   if badpar(fnb,7,0,CALLER=self+'(fnb) ') then return
   if badpar(fnc,7,0,CALLER=self+'(fnc) ') then return
   if badpar(keylist,[7,0],0,CALLER=self+'(KEYLIST) ', $
                             DEFAULT='[[DEFAULT]]') then return
   if badpar(maxrate,[0,2,3,4,5],0,CALLER=self+'(MAXRATE) ', $
                                   DEFAULT=50.0) then return
   if badpar(minrate,[0,2,3,4,5],0,CALLER=self+'(MINRATE) ', $
                                   DEFAULT=1.0) then return
   if badpar(badcol,[0,2,3],[0,1],CALLER=self+'(BADCOL) ', $
                                  DEFAULT=-1) then return
   if badpar(extlist,[0,1,2,3],[0,1],CALLER=self+'(EXTLIST) ', $
                                     default=-1) then return
   if badpar(path,[0,7],0,CALLER=self+'(PATH) ',DEFAULT='') then return
   if path ne '' then path=addslash(path)

   ;cputime,timea
   left='                              '

   fmt='($,a)'

   loadkeys,keylist,hdrlist

   disp = (!d.name eq 'X' or !d.name eq 'PS') and not keyword_set(nodisplay)

   ; Check header of image to see if it is a multi-extension image.
   hdr=headfits(path+fna)
   numext=sxpar(hdr,'NEXTEND')
   if numext eq 0 then begin
      extlist=0
   endif else begin
      if extlist[0] eq -1 then begin
         extlist=indgen(numext)+1
      endif else begin
         if max(extlist) gt numext then begin
            print,self+'Input extension list is incompatible', $
                       ' with the number of extensions'
            print,'in the file.  This file had ',numext,' extensions', $
                  'and the largest item in'
            print,'your list is ',max(extlist)
            print,'Aborting.'
            return
         endif else if min(extlist) le 0 then begin
            print,self+'Input extension list is invalid.  You have', $
                       ' one or more values less'
            print,'than or equal to zero.'
            print,'Aborting.'
            return
         endif
      endelse
   endelse
   numext=n_elements(extlist)

   for ext=0,numext-1 DO begin

      if extlist[ext] eq 0 then begin
         extstr = ''
         exttag = ''
      endif else begin
         extstr = strb36(extlist[ext])
         exttag = 'x'+extstr
      endelse

      fnobj=strlowcase(tag)+exttag+'.obj'

      ;cputime,time0
      print,tag+exttag+':',format=fmt
      hdra=headfits(path+fna,exten=extlist[ext])
      nx = sxpar(hdra,'NAXIS1')
      ny = sxpar(hdra,'NAXIS2')
      parsekey,hdra,hdrlist,infoa
      if disp then begin
         a=readfits(path+fna,exten_no=extlist[ext],/silent)
         setwin,0,xsize=nx,ysize=ny
         skysclim,a,lowval,hival,meanval,sigma
         alowval = meanval-7*sigma
         ahival  = meanval+16*sigma
         tv,bytscl(a,min=alowval,max=ahival,top=!d.n_colors-1)
      endif

      hdrb=headfits(path+fnb,exten=extlist[ext])
      parsekey,hdrb,hdrlist,infob
      if disp then begin
         b=readfits(path+fnb,exten_no=extlist[ext],/silent)
         setwin,1,xsize=nx,ysize=ny
         skysclim,b,lowval,hival,meanval,sigma
         blowval = meanval-7*sigma
         bhival  = meanval+16*sigma
         tv,bytscl(b,min=blowval,max=bhival,top=!d.n_colors-1)
      endif

      hdrc=headfits(path+fnc,exten=extlist[ext])
      parsekey,hdrc,hdrlist,infoc
      if disp then begin
         c=readfits(path+fnc,exten_no=extlist[ext],/silent)
         setwin,2,xsize=nx,ysize=ny
         skysclim,c,lowval,hival,meanval,sigma
         clowval = meanval-7*sigma
         chival  = meanval+16*sigma
         tv,bytscl(c,min=clowval,max=chival,top=!d.n_colors-1)
      endif
      ;cputime,time1
      ;print,time1-time0,' seconds'

      fnla = fna+'.src'+exttag
      fnlb = fnb+'.src'+exttag
      fnlc = fnc+'.src'+exttag

      ; Check to see if source list files are present
      if not exists(fnla) then begin
         print,''
         print,'Unable to locate source list ',fnla,', quitting.'
         return
      endif
      if not exists(fnlb) then begin
         print,''
         print,'Unable to locate source list ',fnlb,', quitting.'
         return
      endif
      if not exists(fnlc) then begin
         print,''
         print,'Unable to locate source list ',fnlc,', quitting.'
         return
      endif

      ;cputime,time0
      list =readfits(fnla,/silent)
      xa   =list[*,0]
      ya   =list[*,1]
      fwhma=list[*,2]
      maga =list[*,3]
      erra =list[*,4]
      list =readfits(fnlb,/silent)
      xb   =list[*,0]
      yb   =list[*,1]
      fwhmb=list[*,2]
      magb =list[*,3]
      errb =list[*,4]
      list =readfits(fnlc,/silent)
      xc   =list[*,0]
      yc   =list[*,1]
      fwhmc=list[*,2]
      magc =list[*,3]
      errc =list[*,4]

      idxa=sort(maga)
      idxb=sort(magb)
      idxc=sort(magc)
      ;cputime,time1
      ;print,time1-time0,' seconds'

      ; Select subset of objects
      if n_elements(xa) gt 1000 then begin
         sela=idxa[0:999]
         nsela=1000
      endif else begin
         nsela=n_elements(xa)
         sela=indgen(nsela)
      endelse

      if n_elements(xb) gt 1000 then begin
         selb=idxb[0:999]
         nselb=1000
      endif else begin
         nselb=n_elements(xb)
         selb=indgen(nselb)
      endelse

      if n_elements(xc) gt 1000 then begin
         selc=idxc[0:999]
         nselc=1000
      endif else begin
         nselc=n_elements(xc)
         selc=indgen(nselc)
      endelse

      ;cputime,time0
      print,' b-a ',format=fmt
      ; find offset to frame B from frame A
      frmdxdy,xa[sela],ya[sela],xb[selb],yb[selb],xoffb,yoffb,errorb
      ;cputime,time1
      ;print,time1-time0,' seconds'
      str=strcompress(string('(',xoffb,',',yoffb,')', $
                               format='(a,f8.1,a,f8.1,a)'),/remove_all)
      print,str,format=fmt

      ;cputime,time0
      print,' c-a ',format=fmt
      ; find offset to frame C from frame A
      frmdxdy,xa[sela],ya[sela],xc[selc],yc[selc],xoffc,yoffc,errorc
      ;cputime,time1
      ;print,time1-time0,' seconds'
      str=strcompress(string('(',xoffc,',',yoffc,')', $
                               format='(a,f8.1,a,f8.1,a)'),/remove_all)
      print,str,format=fmt

      if errorb ne 0 or errorc ne 0 then begin
         print,''
         print,'The fine tuned center is too far away from the original center.'
         print,'Something must be very wrong with this triplet.'
         return
      endif

      if exists(fnobj) then begin
         rdoblist,fnobj,nobj,filelist,dt,offset,pos,flags,idstr,nfiles
         if nfiles ne 3 then begin
            print,'Pre-existing obj file does not have 3 file entries.', $
                  '  Unable to proceed.'
            return
         endif
         if filelist[0] ne fna or filelist[1] ne fnb $
                               or filelist[2] ne fnc then begin
            print,'Current filelist ',fna,',',fnb,',',fnc,' does not match'
            print,'  the old filelist ',filelist,'  Unable to continue.'
            return
         endif
      endif else begin
         filelist=[fna,fnb,fnc]
         nfiles = 3
         nobj = 0
      endelse

      offset=[xoffb,yoffb,xoffc,yoffc]

      ; Apply offsets
      xa1=xa
      ya1=ya
      xb1=xb-xoffb
      yb1=yb-yoffb
      xc1=xc-xoffc
      yc1=yc-yoffc

      ; Create mate flag vectors
      amate=intarr(n_elements(xa))
      bmate=intarr(n_elements(xb))
      cmate=intarr(n_elements(xc))

      ; Scan through the object

      nzp=0
      ;cputime,time0
      print,' Xf ',format=fmt
      for i=0,n_elements(xa)-1 do begin

         ; Check B/A x match
         zb=where( xb1 lt xa1[i]+2.0 and xb1 gt xa1[i]-2.0, countb)
         if countb eq 0 then goto,nexta

         ; if any, check for B/A y match on those that matched in x
         zb1=where(yb1[zb] lt ya1[i]+2.0 and yb1[zb] gt ya1[i]-2.0, countb)
         if countb eq 0 then goto,nexta
         zb=zb[zb1]

         ; if any B/A matches, check for C/A matches
         zc=where( xc1 lt xa1[i]+2.0 and xc1 gt xa1[i]-2.0, countc)
         if countc eq 0 then goto,nexta

         zc1=where( yc1[zc] lt ya1[i]+2.0 and yc1[zc] gt ya1[i]-2.0, countc)
         if countc eq 0 then goto,nexta
         zc=zc[zc1]

         distb=(xb1[zb]-xa1[i])^2+(yb1[zb]-ya1[i])^2
         zb1=where(distb eq min(distb))
         distb=sqrt(distb[zb1[0]])
         zb=zb[zb1[0]]

         distc=(xc1[zc]-xa1[i])^2+(yc1[zc]-ya1[i])^2
         zc1=where(distc eq min(distc))
         distc=sqrt(distc[zc1[0]])
         zc=zc[zc1[0]]

         if distb lt 2.0 and abs(maga[i]-magb[zb]) lt 1.5 and $
            distc lt 2.0 and abs(maga[i]-magc[zc]) lt 1.5 then begin
            amate[i]=-1
            bmate[zb]=-1
            cmate[zc]=-1
            if nzp eq 0 then begin
               zpb=maga[i]-magb[zb]
               zpc=maga[i]-magc[zb]
               nzp=1
            endif else begin
               zpb=[zpb,maga[i]-magb[zb]]
               zpc=[zpb,maga[i]-magc[zc]]
               nzp=nzp+1
            endelse
         endif  ; final pos and mag check

nexta:
      endfor
      ;cputime,time1
      ;print,time1-time0,' seconds'

      z=where(amate eq -1)
      robomean,fwhma[z],3.0,0.5,mfwhma
      z=where(bmate eq -1)
      robomean,fwhmb[z],3.0,0.5,mfwhmb
      z=where(cmate eq -1)
      robomean,fwhmc[z],3.0,0.5,mfwhmc
      print,'fwhm ',format=fmt
      str=strcompress(string('(',mfwhma,',',mfwhmb,',',mfwhmc,')', $
                               format='(a,3(f8.1,a))'),/remove_all)
      print,str,format=fmt

      robomean,zpb,3.0,0.5,zpb0,stdmean=zpb0sig
      robomean,zpc,3.0,0.5,zpc0,stdmean=zpc0sig
      magb=magb+zpb0
      magc=magc+zpc0

      if badcol[0] ne -1 then begin
         for i=0,n_elements(badcol)-1 DO begin
            z=where(abs(xa-badcol[i]) lt 2.5,count)
            if count ne 0 then amate[z]=-2
            z=where(abs(xb-badcol[i]) lt 2.5,count)
            if count ne 0 then bmate[z]=-2
            z=where(abs(xc-badcol[i]) lt 2.5,count)
            if count ne 0 then cmate[z]=-2
         endfor
      endif

      hit=0
      ;cputime,time0
      print,' MM ...',format=fmt

      ; Compute time separation between frame pairs in hours.
      dtb=float(infob.jd-infoa.jd)*24.0
      dtc=float(infoc.jd-infoa.jd)*24.0
      dt=[dtb,dtc]

      ; Compute maximum distance possible between frame pairs
      maxdistb = maxrate * dtb
      maxdistc = maxrate * dtc

      dupnoauto = 0
      dupauto = 0

      for i=0,n_elements(xa)-1 do begin

         if amate[i] eq 0 then begin

            zb=where(bmate eq 0,countb)
            if countb eq 0 then goto,foundit

            ; selection on B frames

            ; X distance
            zb1=where( xb1[zb] gt xa1[i]-maxdistb and $
                       xb1[zb] lt xa1[i]+maxdistb, countb )
            if countb eq 0 then goto,foundit
            zb=zb[zb1]

            ; Y distance
            zb1=where( yb1[zb] gt ya1[i]-maxdistb and $
                       yb1[zb] lt ya1[i]+maxdistb, countb )
            if countb eq 0 then goto,foundit
            zb=zb[zb1]
            
            ; Rate limit check
            brate=sqrt((xb1[zb]-xa1[i])^2+(yb1[zb]-ya1[i])^2)/dtb
            zb1=where(brate gt minrate and brate lt maxrate,countb)
            if countb eq 0 then goto,foundit
            zb=zb[zb1]
            brate=brate[zb1]

            ; Compute direction of motion of surviving B sources
            bdir =atan(yb1[zb]-ya1[i],xb1[zb]-xa1[i])

            ; Now start trimming the C list.
            zc=where(cmate eq 0,countc)
            if countc eq 0 then goto,foundit

            ; X distance
            zc1=where( xc1[zc] gt xa1[i]-maxdistc and $
                       xc1[zc] lt xa1[i]+maxdistc, countc )
            if countc eq 0 then goto,foundit
            zc=zc[zc1]

            ; Y distance
            zc1=where( yc1[zc] gt ya1[i]-maxdistc and $
                       yc1[zc] lt ya1[i]+maxdistc, countc )
            if countc eq 0 then goto,foundit
            zc=zc[zc1]

            ; Rate limit check
            crate=sqrt((xc1[zc]-xa1[i])^2+(yc1[zc]-ya1[i])^2)/dtc
            zc1=where(crate gt minrate and crate lt maxrate,countc)
            if countc eq 0 then goto,foundit
            zc=zc[zc1]
            crate=crate[zc1]

            ; Compute predictions for location in frame C
            cdist  = brate*dtc
            cpredx = cdist*cos(bdir)+xa1[i]
            cpredy = cdist*sin(bdir)+ya1[i]

            for j=0,countb-1 do begin

               cdist = (cpredx[j]-xc1[zc])^2 + (cpredy[j]-yc1[zc])^2

               zm=where(cdist lt 4.0,countzm)

               if countzm eq 1 then begin
                  zm=zm[0]
                  cdist=sqrt(cdist[zm])

                  ; Enforce minimally consistent magnitudes
                  mags=[maga[i],magb[zb[j]],magc[zc[zm]]]
                  if max(mags)-min(mags) gt 1.5 then goto,skipit

                  ; Enforce consistent fwhm
                  fwhms=[fwhma[i],fwhmb[zb[j]],fwhmc[zc[zm]]]
                  if max(fwhms)-min(fwhms) gt 1.0 then goto,skipit

                  ; Check B position against A source list
                  check=(xa1-xb1[zb[j]])^2 + (ya1-yb1[zb[j]])^2
                  if min(check) lt 2.25 then goto,skipit

                  ; Check C position against A source list
                  check=(xa1-xc1[zc[zm]])^2 + (ya1-yc1[zc[zm]])^2
                  if min(check) lt 2.25 then goto,skipit

                  ; Check A position against B source list
                  check=(xa1[i]-xb1)^2 + (ya1[i]-yb1)^2
                  if min(check) lt 2.25 then goto,skipit

                  ; Check C position against B source list
                  check=(xb1-xc1[zc[zm]])^2 + (yb1-yc1[zc[zm]])^2
                  if min(check) lt 2.25 then goto,skipit

                  ; Check A position against C source list
                  check=(xa1[i]-xc1)^2 + (ya1[i]-yc1)^2
                  if min(check) lt 2.25 then goto,skipit

                  ; Check B position against C source list
                  check=(xb1[zb[j]]-xc1)^2 + (yb1[zb[j]]-yc1)^2
                  if min(check) lt 2.25 then goto,skipit

                  ; Compute direction of motion of surviving C source
                  cdir =atan(yc1[zc[zm]]-ya1[i],xc1[zc[zm]]-xa1[i])

                  newpos = [ xa[i],ya[i],xb[zb[j]],yb[zb[j]], $
                                         xc[zc[zm]],yc[zc[zm]] ]

                  ; Check to make sure this hit has not already been recorded.
                  if nobj ne 0 then begin
                     for ii=0,nobj-1 do begin
                        diff = abs(pos[*,ii]-newpos)
                        if max(diff) lt 1.5 then begin
                           if strpos(idstr[ii],'auto') ge 0 then $
                              dupauto = dupauto + (flags[ii] eq 'y') $
                           else $
                              dupnoauto = dupnoauto + (flags[ii] eq 'y')
                           goto,foundit
                        endif
                     endfor
                  endif

                  if hit eq 0 then print,''
                  print,hit,i,zb[j],zc[zm],':', $
                     brate[j],bdir[j]*!radeg, $
                     crate[zm],cdir*!radeg, $
                     cdist,maga[i],erra[i],magb[zb[j]],errb[zb[j]], $
                                           magc[zc[zm]],errc[zc[zm]], $
                     format='(i4,3(1x,i6),a,2(2x,f4.1,1x,f6.1),'+ $
                             '2x,f3.1,3(2x,f4.1,1x,f3.1))'

                  if nobj eq 0 then begin
                     pos = newpos
                     flags = '?'
                     idstr = 'auto'
                     nobj = 1
                  endif else begin
                     pos = [ [[pos]], [newpos] ]
                     flags = [flags,'?']
                     idstr = [idstr,'auto']
                     nobj = nobj+ 1
                  endelse

                  amate[i]      = i
                  bmate[zb[j]]  = i
                  cmate[zc[zm]] = i
                  hit=hit+1

                  goto,foundit
               endif else if countzm gt 1 then begin
                  print,i,j,countzm,' multiple hits!'
               endif

skipit:
            endfor
foundit:
         endif
      endfor
      ;cputime,time1
      ;print,time1-time0,' seconds'

      if hit eq 0 then print,' None!'

      if nobj gt 0 then $
         wroblist,fnobj,nobj,filelist,dt,offset,pos,flags,idstr,nfiles

      if dupnoauto ne 0 then $
         print,' --> ',dupnoauto,' triplets previously noted (not auto)'

      if dupauto ne 0 then $
         print,' --> ',dupauto,' triplets previously noted (auto)'

      za=where(amate gt 0,counta)
      zb=where(bmate gt 0,countb)
      zc=where(cmate gt 0,countc)
      if keyword_set(cand) then begin
         za=where(amate ne -1,counta)
         zb=where(bmate ne -1,countb)
         zc=where(cmate ne -1,countc)
      endif

      if disp then begin
         setusym,-1

         setwin,0
         if counta ne 0 then plots,xa[za],ya[za],psym=8,symsize=2.0,/device
         for i=0,counta-1 do $
            xyouts,xa[za[i]]+5,ya[za[i]], $
                   strcompress(string(i),/remove_all),/device
         if countb ne 0 and not keyword_set(cand) then $
            plots,xb[zb]-xoffb,yb[zb]-yoffb,psym=8,symsize=2.0,/device
         if countc ne 0 and not keyword_set(cand) then $
            plots,xc[zc]-xoffc,yc[zc]-yoffc,psym=8,symsize=2.0,/device

         setwin,1
         if counta ne 0 and not keyword_set(cand) then $
            plots,xa[za]+xoffb,ya[za]+yoffb,psym=8,symsize=2.0,/device
         if countb ne 0 then plots,xb[zb],yb[zb],psym=8,symsize=2.0,/device
         if countc ne 0 and not keyword_set(cand) then $
            plots,xc[zc]-xoffc+xoffb,yc[zc]-yoffc+yoffb, $
                  psym=8,symsize=2.0,/device

         setwin,2
         if counta ne 0 and not keyword_set(cand) then $
            plots,xa[za]+xoffc,ya[za]+yoffc,psym=8,symsize=2.0,/device
         if countb ne 0 and not keyword_set(cand) then $
            plots,xb[zb]-xoffb+xoffc,yb[zb]-yoffb+yoffc, $
                  psym=8,symsize=2.0,/device
         if countc ne 0 then plots,xc[zc],yc[zc],psym=8,symsize=2.0,/device

      endif

      ;cputime,timeb
      ;print,'Total execution time ',timeb-timea,' seconds'

   endfor  ; End of extension for loop block

end

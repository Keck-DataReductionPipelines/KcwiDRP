;+
; NAME:
;  astlinks
; PURPOSE:
;  Scan for linkages among a collection of asteroid astrometric measurements
; DESCRIPTION:
;
;  This program looks for linkages between moving object astrometric
;    measurements.  The main set of measurements that are scanned are all
;    of the .ast files in the current directory.  These measurements are
;    checked against all other objects in the same directory and against
;    all objects in another directory.  The linkages are established by pure
;    linear motion coincidence.
;
;  There are two files written as the program runs, links.log and astlink.xrft.
;    The file, links.log, is a copy of the copious information about each
;    link that is written to the screen.  Here you get to see the data that
;    lead up to the match.  The file, astlink.xrft, is a list of matches
;    only.  It's in the same format as lplast.xrft which is used by ASTCOL to
;    record the proper name in the output astrometry file.  If you have provided
;    "otherdir" and there is an lplast.xrft file in that directory, that file
;    is read and used to further resolve names.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astlinks,otherdir
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;  otherdir - If specified, the local .ast files are checked against this
;               directory too.
; KEYWORD INPUT PARAMETERS:
;  NOLOCAL - Flag, if set will suppress checking for linkages within the
;               astrometry from the current directory.
;
;  ONEFILE - Optional input of a single local .ast file to check against
;               another directory.  Specifying this option implies /NOLOCAL.
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
;  98/01/13, Written by Marc W. Buie, Lowell Observatory
;  2001/08/15, MWB, changed to use new rdlplast procedure.
;  2002/09/03, MWB, changed Str_sep call to strsplit
;  2004/9/21, MWB, removed obsolete call to Findfile
;
;-
PRO astlinks,in_otherdir,NOLOCAL=nolocal,ONEFILE=onefile

   if n_params() eq 0 then begin
      doc_library,'astlinks'
      return
   endif

   if badpar(in_otherdir,[0,7],0,caller='REDUCTOR: (DUMPOBJ) ', $
                                 default='.') then return
   if badpar(nolocal,[0,1,2,3],0,caller='REDUCTOR: (NOLOCAL) ', $
                                 default=0) then return
   if badpar(onefile,[0,7],[0,1],caller='REDUCTOR: (ONEFILE) ', $
                                 default='') then return

   if onefile[0] ne '' then nolocal=1

   otherdir=in_otherdir

   if onefile[0] eq '' then begin
      localfn = file_search('*.ast',count=nlocal)
      IF nlocal eq 0 THEN BEGIN
         print,'No astrometry files found in current directory.  Aborting.'
         return
      ENDIF
   endif else begin
      localfn = onefile
      nlocal  = n_elements(localfn)
   endelse

   logfile='links.log'
   xreffile='astlink.xrft'
   openw,lunlog,logfile,/get_lun
   openw,lunxref,xreffile,/get_lun

   print,nlocal,' astrometry files in current directory.'
   printf,lunlog,nlocal,' astrometry files in current directory.'

   blanks='                                             '

   IF otherdir ne '.' THEN BEGIN
      otherfn = file_search(addslash(otherdir)+'*.ast',count=nother)
      IF nother eq 0 THEN otherdir='.'
   ENDIF

   if otherdir ne '.' then begin
      print,nother,' astrometry files from ',otherdir
      printf,lunlog,nother,' astrometry files from ',otherdir
   endif

   cr = string("15b)  ;"
   rt = string("12b)  ;"
   form='($,a,a15,1x,i5)'

   print,'Loading all astrometry data from the current directory.'
   ast=ptrarr(nlocal)
   for i=0,nlocal-1 do begin
      rdrawast,localfn[i],fn0,jd0,ra0,dec0,mag0,nobs0
      words=strsplit(localfn[i],'.',/extract)
      ast[i]=ptr_new({ $
         fn:     localfn[i], $
         objnam: words[0], $
         jd:     jd0, $
         ra:     ra0, $
         dec:    dec0, $
         mag:    mag0, $
         nobs:   nobs0 $
         })
   endfor
   nast=nlocal
   rdlplast,cprevid,clinkedid,ncount,/silent
   clplast = ncount gt 0
   if clplast then $
      print,'Loaded local lplast.xrft file.'

   if otherdir ne '.' then begin
      print,'Loading all astrometry data from ',otherdir
      dirlen=strlen(addslash(otherdir))
      otherast=ptrarr(nother)
      for i=0,nother-1 do begin
         rdrawast,otherfn[i],fn0,jd0,ra0,dec0,mag0,nobs0
         words=strsplit(strmid(otherfn[i],dirlen,999),'.',/extract)
         otherast[i]=ptr_new({ $
            fn:     otherfn[i], $
            objnam: words[0], $
            jd:     jd0, $
            ra:     ra0, $
            dec:    dec0, $
            mag:    mag0, $
            nobs:   nobs0 $
            })
      endfor
      ast=[ast,otherast]
      nast=nast+nother
      rdlplast,previd,linkedid,ncount,/silent,path=otherdir
      lplast = ncount gt 0

      if lplast then $
         print,'Loaded lplast.xrft file from ',otherdir

   endif else lplast=0

   nlinks=0
   FOR i=0,nlocal-1 DO BEGIN

      print,form=form,cr,localfn[i]+blanks,nlocal-i
      jd0   = (*ast[i]).jd
      ra0   = (*ast[i]).ra
      dec0  = (*ast[i]).dec
      mag0  = (*ast[i]).mag
      nobs0 = (*ast[i]).nobs
;      rdrawast,localfn[i],fn0,jd0,ra0,dec0,mag0,nobs0

      ; Check to see if this object already has a cross-reference in the
      ;   local cross reference file.
      objnam = strmid((*ast[i]).objnam+blanks,0,8)

      if clplast then zl = where(objnam eq cprevid,count) $
      else count=0

      if count ne 0 then begin
         print,'  ',objnam,'<-->',strtrim(clinkedid[zl[0]],2)
      endif else begin
         ; Compute rate of first object
         objdir0 = atan(dec0[nobs0-1]-dec0[0],ra0[0]-ra0[nobs0-1])*!radeg
         hmotobj0=angsep(ra0[0],dec0[0],ra0[nobs0-1],dec0[nobs0-1]) $
              / (abs(jd0[nobs0-1]-jd0[0]) * 24.0 ) * !radeg * 3600.0

         if nolocal then begin
            j1=nlocal
            j2=nast-1
         endif else begin
            j1=i+1
            j2=nast-1
         endelse

         FOR j=j1,j2 DO BEGIN

            IF i ne j THEN BEGIN

               jd1   = (*ast[j]).jd
               ra1   = (*ast[j]).ra
               dec1  = (*ast[j]).dec
               mag1  = (*ast[j]).mag
               nobs1 = (*ast[j]).nobs
   ;            rdrawast,cklist[j],fn1,jd1,ra1,dec1,mag1,nobs1
               ; Compute rate of second object
               objdir1 = atan(dec1[nobs1-1]-dec1[0],ra1[0]-ra1[nobs1-1])*!radeg
               hmotobj1=angsep(ra1[0],dec1[0],ra1[nobs1-1],dec1[nobs1-1]) $
                    / (abs(jd1[nobs1-1]-jd1[0]) * 24.0 ) * !radeg * 3600.0

               ; Compute rate assuming both 1st measures are same object
               IF jd0[0] eq jd1[0] THEN BEGIN
                  hmotobj2=9999.0
               ENDIF ELSE BEGIN
                  hmotobj2=angsep(ra0[0],dec0[0],ra1[0],dec1[0]) $
                       / (abs(jd1[0]-jd0[0]) * 24.0 ) * !radeg * 3600.0
               ENDELSE

               closerate = abs(hmotobj0-hmotobj1) lt 2.0 and $
                           abs(hmotobj0-hmotobj2) lt 2.0

               closedir  = abs(objdir0-objdir1) lt 5.0

               IF not closedir and objdir0 lt 0.0 THEN $
                  closedir = abs((objdir0+360.0)-objdir1) lt 5.0

               IF closerate and closedir THEN BEGIN

                  nlinks = nlinks + 1

                  print,nlinks
                  print,(*ast[i]).fn+blanks,hmotobj0,objdir0,minmax(mag0), $
                     format='(2x,"[",a30,1x,f6.1,"aph ",f6.1,"d]",2(1x,f4.1))'
                  print,(*ast[j]).fn+blanks,hmotobj1,objdir1,minmax(mag0),hmotobj2, $
                     format='(2x,"[",a30,1x,f6.1,"aph ",f6.1,"d]",2(1x,f4.1),2x,f6.1,"aph")'
                  print,''

                  if nlinks eq 1 then printf,lunlog,''
                  printf,lunlog,(*ast[i]).fn+blanks,hmotobj0,objdir0,minmax(mag0), $
                     format='(2x,"[",a30,1x,f6.1,"aph ",f6.1,"d]",2(1x,f4.1))'
                  printf,lunlog,(*ast[j]).fn+blanks,hmotobj1,objdir1,minmax(mag0),hmotobj2, $
                     format='(2x,"[",a30,1x,f6.1,"aph ",f6.1,"d]",2(1x,f4.1),2x,f6.1,"aph")'
                  printf,lunlog,''

                  ; record the hit to the cross-reference log file in the same
                  ;   format as lplast.xrft
                  linkstr = '           '+(*ast[j]).objnam
                  if lplast then begin
                     zl = where((*ast[j]).objnam eq previd,count)
                     if count gt 0 then begin
                        linkstr = linkedid[zl[0]]
                     endif
                  endif
                  printf,lunxref,(*ast[i]).objnam+blanks,linkstr, $
                     format='(a8,1x,a)'

               ENDIF

            ENDIF

         ENDFOR

      endelse

   ENDFOR

   print,''
   free_lun,lunlog,lunxref

   ptr_free,ast

END

;+
; NAME:
;  kbompck
; PURPOSE:
;  Pre-check and summary for MPC data submission for KBO/Centaur observations
; DESCRIPTION:
;  Reads all the .ast files contained in the astrometry directory and sifts
;   through the information therein.  These are files that were generated
;   previously by ASTCOL.  It also needs to find a file that lists
;   all the non-designated objects of concern (oblist.dat).  These
;   non-designated objects are assumed to be KBO/Centaur objects.
;
;  Once all the observations are read in, EPHEM is used to get the orbital
;   elements of all objects.  Those objects with orbital elements indicative
;   of KBO/Centaur objects are called out for special mention in the summary.
;
;  After the list of interesting objects is generated, the observations
;   of DESIGNATED KBO/Centaur objects are written to 'new_special_ted' in
;   the current directory.  Note that the previous contents of this file
;   are deleted prior to saving the new file.  This file has the same format
;   as the special_ted file used at Lowell to include observations prior to
;   MPC publication.  Once the contents of the new file are checked and known
;   to be good, it is usual to APPEND these observations to the end of the
;   existing special_ted file.  Again, remember that this file is not relevant
;   for non-designated objects.
;
;  Finally, please note that this program really doesn't do much other than
;   summarize the observations.  It is not perfect and often this information
;   needs to be edited and/or reformatted before it's useful to forward to
;   the Minor Planet Center along with the submitted data.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  kbompck
; INPUTS:
;  None
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;
;  ASTDIR - Directory where pending astrometry is located, default is
;             /net/frakir/raid/buie/astrometry
;
;  KBODIR - Directory where master list of objects of interest are located
;             (file name is 'oblist.dat').  The default directory is
;             /net/frakir/raid/buie/kbo.
;
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/10/05
;  2001/11/01, MWB, added new category
;  2002/01/14, MWB, added ASTDIR and KBODIR keywords.
;  2003/10/01, MWB, converted my Delfile call to system file_delete routine
;  2004/02/09, MWB, changed defaults for ASTDIR and KBODIR
;  2004/9/21, MWB, removed obsolete call to Findfile
;  2004/10/27, MWB, fixed some output bugs
;  2005/05/25, MWB, changes to file handling in reponse to changes in
;                     ASTCOL and AST2TED for separating KBOs.
;-
pro kbompck,ASTDIR=astdir,KBODIR=kbodir

   if badpar(astdir,[0,7],0,caller='kbompck (ASTDIR)', $
               default='/net/frakir/raid/buie/astrometry/') then return

   if badpar(kbodir,[0,7],0,caller='kbompck (KBODIR)', $
               default='/net/frakir/raid/buie/kbo/') then return

   oblist = 'oblist.dat'

   fnast = file_search(astdir+'*.ast',count=nfnast)

   print,'Found ',strn(nfnast),' nights of astrometry waiting for submission.'
   print,''

   ; Read in all the data to be submitted
   for i=0,nfnast-1 do begin
      rdast,fnast[i],fn0,jd0,ra0,dec0,mag0,obs0,id0,nobs0
      print,'File ',fnast[i],',',nobs0,' observations found.'
      if i eq 0 then begin
         astf = replicate(fnast[i],nobs0)
         fn   = fn0
         jd   = jd0
         ra   = ra0
         dec  = dec0
         mag  = mag0
         obs  = obs0
         id   = id0
         nobs = nobs0
      endif else begin
         astf = [astf,replicate(fnast[i],nobs0)]
         fn   = [fn,fn0]
         jd   = [jd,jd0]
         ra   = [ra,ra0]
         dec  = [dec,dec0]
         mag  = [mag,mag0]
         obs  = [obs,obs0]
         id   = [id,id0]
         nobs = nobs+nobs0
      endelse
      ; figure out if there are separate files for KBO data and report
      words=strsplit(fnast[i],'.',/extract)
      if exists(words[0]+'.kted') then begin
         nkbo=file_lines(words[0]+'.kted')
         nmbo=file_lines(words[0]+'.ted')
         print,'  ',strn(nkbo),' observations of distant objects', $
               ' and ',strn(nmbo),' of other objects.'
      endif
   endfor

   print,''
   print,nobs,' total observations pending.'
   print,''
   
   ; sift through this list of astrometry and get non-local designations
   tag1=strmid(id,0,1)
   tag2=strmid(id,1,1)
   z = where((tag1 lt 'A' or tag1 gt 'Z') and (tag2 lt 'A' or tag2 gt 'Z'), ndesig)
   if ndesig ne 0 then begin
      nonlocal = id[z]
      nonlocal = nonlocal[uniq(nonlocal,sort(nonlocal))]
      ndesig = n_elements(nonlocal)
      print,'Observations of ',ndesig, $
         ' designated or numbered objects found.',format='(a,i5,a)'
   endif

   ; check for oblist.dat, if not found, don't continue
   if not exists(kbodir+oblist) then begin
      print,''
      print,'The file ',kbodir+oblist,' was not found.'
      print,''
      print,'This file contains a list of the non-designated but interesting'
      print,'objects to look for in the astrometry to be submitted.  Without'
      print,'this file you will NOT get any information about non-designated'
      print,'objects.'
      print,''
      nobjects = 0
      nlocalid = 0
   endif else begin

      ; Load the non-designated KBO list.
      readcol,kbodir+oblist,localid,dod,comment,format='a,a,a'
      nobjects = n_elements(localid)

      ; Weed out the non-designated objects not measured
      measured = bytarr(nobjects)
      for i=0,nobjects-1 do begin
         z = where(localid[i] eq id,count)
         measured[i] = count
      endfor
      z = where(measured gt 0,nlocalid)
      if nlocalid gt 0 then begin
         localid = localid[z]
         dod     = dod[z]
         comment = comment[z]
      endif

   endelse

   print,'                ',nlocalid,' non-designated KBO ids found.', $
      format='(a,i5,a)' 
   print,''

   ; Extract current orbital elements for designated objects
   if ndesig gt 0 then begin
      objects = 'e'+nonlocal
      ejd=replicate(systime(/julian),ndesig)
      ephem,ejd,500,11,objects,elements1
   endif

   ; Extract current orbital elements for non-designated objects
   if nlocalid gt 0 then begin
      objects = 'a'+localid
      ejd=replicate(systime(/julian),nlocalid)
      ephem,ejd,500,11,objects,elements2
   endif

   ; ephemeris index codes
   inc =  3
   ecc =  4
   a   =  5
   q   =  6
   Q   =  7
   arc = 15
   num = 16

   if ndesig gt 0 then begin
      ; Weed out the designation list to keep only KBO/Centaurs
      z = where(elements1[q,*] ge 6.0, count1)
      if count1 ne 0 then begin
         elements1 = elements1[*,z]
         nonlocal  = nonlocal[z]
      endif
      print,'                ',count1,' designated KBOs observed.', $
         format='(a,i5,a)' 
   endif

   ; Tally up observational history for the non-designated objects
   ;    in the data to be submitted.
   ; nobs2 - Number of new observation in data to be submitted.
   ; nmeas - Number of previously known observations.
   if nlocalid ne 0 then begin
      nobs2 = intarr(nlocalid)
      arclen= fltarr(nlocalid)
      nmeas = fltarr(nlocalid)
      for i=0,nlocalid-1 do begin
         z = where(localid[i] eq id,count)
         nobs2[i] = count
         arclen[i] = elements2[arc,i]
         nmeas[i]  = elements2[num,i]
      endfor
   endif

   if ndesig gt 0 then begin
      pushd,astdir
      z = where(elements1[q,*] ge 6.0, count1)
      ; Sift through the designated objects and tally how many observations there are
      if count1 gt 0 then begin
         print,''
         print,'Designated KBO observation summary.'
         nobs1 = intarr(count1)
         print,'   Object nobs   a    e     i    arc   tobs  date(s)'
         print,'------------------------------------------------------'
         file_delete,'new_special_ted',/quiet,/noexpand_path
         for i=0,count1-1 do begin
            z = where(nonlocal[i] eq id,count)
            nobs1[i] = count
            jdo = jd[z]
            jdstr,jdo,100,str
            str = str[uniq(str,sort(str))]
   ;         print,nonlocal[i],count,str, $
   ;            format='(a10,1x,i2,6(1x,a))'
            print,nonlocal[i],count, $
               elements1[a,i],elements1[ecc,i],elements1[inc,i], $
               elements1[arc,i],elements1[num,i],str, $
               format='(a10,1x,i2,1x,f6.1,1x,f4.2,1x,f5.1,1x,f6.1,1x,i3,6(1x,a))'
            spawn,'grep -h '+nonlocal[i]+' *.*ted >> new_special_ted'
         endfor
      endif
      popd
   endif

   if nlocalid ne 0 then begin
      ; check for errors
      z=where(nobs2 eq 1, count)
      if count ne 0 then begin
         print,count,' objects only measured once!'
         print,localid[z]
      endif

      ; observations of previously discovered new IDs
      z=where(nmeas-nobs2 gt 0 and arclen gt 0.5, count)
      if count ne 0 then begin
         print,''
         print,'Objects previously discovered that are now confirmed.'
         print,count,' objects that should now be designatable.'
         print,''
         print,'   Object nobs   a    e     i    arc tobs'
         print,'-----------------------------------------'
         for i=0,count-1 do begin
            k=z[i]
            print,localid[k],nobs2[k],elements2[a,k],elements2[ecc,k],elements2[inc,k], $
               arclen[k],nmeas[k], $
               format='(a10,1x,i2,1x,f6.1,1x,f4.2,1x,f5.1,1x,f5.1,1x,i2)'
         endfor
      endif

      ; One night stands
      z=where((nobs2 eq 2 and arclen le 0.5) or (arclen le 0.5), count)
      if count ne 0 then begin
         print,''
         print,'Only 2 observations in data to be submitted.'
         print,count,' objects measured on one night, not yet designatable.'
         print,''
         print,'   Object nobs   a    e     i    arc tobs'
         print,'-----------------------------------------'
         for i=0,count-1 do begin
            k=z[i]
            print,localid[k],nobs2[k],elements2[a,k],elements2[ecc,k],elements2[inc,k], $
               arclen[k],nmeas[k], $
               format='(a10,1x,i2,1x,f6.1,1x,f4.2,1x,f5.1,1x,f5.1,1x,i2)'
         endfor
      endif

      ; New IDs
      z=where(nobs2 gt 2 and arclen gt 0.5, count)
      if count ne 0 then begin
         print,''
         print,'More than 2 observations in data to be submitted.'
         print,count,' objects measured multiple nights, designatable.'
         print,''
         print,'   Object nobs   a    e     i    arc tobs'
         print,'-----------------------------------------'
         for i=0,count-1 do begin
            k=z[i]
            print,localid[k],nobs2[k],elements2[a,k],elements2[ecc,k],elements2[inc,k], $
               arclen[k],nmeas[k], $
               format='(a10,1x,i2,1x,f6.1,1x,f4.2,1x,f5.1,1x,f5.1,1x,i2)'
         endfor
      endif
   endif

   ; Previously reported IDs

end

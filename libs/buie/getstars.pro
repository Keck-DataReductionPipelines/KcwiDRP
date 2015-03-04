;+
; NAME:
;  getstars
; PURPOSE: (one line)
;  Retrieve coordinates from the master star catalog
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  getstars,name,ra,dec,found,EPOCH=epoch,EQUINOX=equinox
; INPUTS:
;     name   - Name of the star.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     FILE    - Name of star catalog file.  Default is:
;                 '/net/frakir/raid/buie/starcat/starcat.dat'
;
;     EPOCH   - Decimal year of epoch to correct for proper motion to.
;                 Default = catalog epoch.  (Scalar or vector).
;
;     B1950   - Coordinates should be referred to equinox of B1950.
;
;     J2000   - Coordinates should be referred to equinox of J2000 (default).
;
;     OFDATE  - Coordinates should be referred to equinox of date.
;                 If selected, EPOCH must be provided.
;
;     NOBLANKS - Flag, if set, search for the name ignores all blanks.
;
;     NOCASE  - Flag, if set, search for the name ignores case.
;
; OUTPUTS:
;     ra     - Right Ascension in radians. J2000
;     dec    - Declination in radians.     J2000
;     found  - Flags, 1=star found, 0=star not found
;
; KEYWORD OUTPUT PARAMETERS:
;     STDNAME - Actual standard name from file, resolves the aliases.
;
; COMMON BLOCKS:
;     MWB_STARCAT
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/21, Written by Marc W. Buie, Lowell Observatory
;  2002/09/03, MWB, changed Str_sep call to strsplit
;  2004/02/09, MWB, changed path to starcat.dat file.
;-
pro getstars,name,ra,dec,found, $
       FILE=file,EPOCH=epoch,B1950=b1950,J2000=j2000,OFDATE=ofdate, $
       STDNAME=stdname,NOBLANKS=noblanks,NOCASE=nocase

   common mwb_starcat, info

   if n_params() lt 3 then begin
      ;Show the calling sequence.
      print,'getstars,name,ra,dec,found,FILE=file'
      return
   endif

   loadstar,file=file

   if badpar(name,7,[0,1],CALLER='GETSTARS: (name) ',npts=n1) then return

   if badpar(epoch,[0,5],0,CALLER='RDLAND2: (epoch) ',default=-1) then return
   if badpar(file,[0,7],0,CALLER='GETSTARS: (file) ', $
         default='/net/frakir/raid/buie/starcat/starcat.dat') then return
   if badpar(b1950,[0,1,2,3],0,CALLER='GETSTARS: (B1950) ',default=0) then return
   if badpar(j2000,[0,1,2,3],0,CALLER='GETSTARS: (J2000) ',default=0) then return
   if badpar(ofdate,[0,1,2,3],0,CALLER='GETSTARS: (OFDATE) ',default=0) then return
   if badpar(noblanks,[0,1,2,3],0,CALLER='GETSTARS: (NOBLANKS) ',default=0) then return
   if badpar(nocase,[0,1,2,3],0,CALLER='GETSTARS: (NOCASE) ',default=0) then return

   set=b1950+j2000+ofdate
   if set eq 0 then j2000=1

   if set gt 1 then begin
      print,'GETSTARS: Error. Only one of B1950, J2000, or OFDATE can be selected.'
      return
   endif

   if ofdate and epoch lt 0. then begin
      print,'GETSTARS: Error. Epoch must be provided with OFDATE request.'
      return
   endif

   if info.nobj eq 0 then begin
      message,'No stars in catalog!  Cannot proceed',/info
      return
   endif

   ra=fltarr(n1)
   dec=fltarr(n1)
   found=replicate(-1,n1)
   stdname=strarr(n1)

   ; Step through the object list and collect the pointers for each name
   ;   into the positional database.
   for i=0,n_elements(name)-1 do begin

      lookname = name[i]
      if noblanks then lookname=strcompress(lookname,/remove_all)
      if nocase   then lookname=strupcase(lookname)

      if noblanks and nocase then $
         z=where(lookname eq strupcase(strcompress(info.name,/remove_all)),count) $
      else if nocase then $
         z=where(lookname eq strupcase(info.name),count) $
      else if noblanks then $
         z=where(lookname eq strcompress(info.name,/remove_all),count) $
      else $
         z=where(name[i] eq info.name,count)

      z=z[0]

      ; Primary name found
      if count ge 1 then begin
         found[i] = z

      ; Need to search through the aliases
      endif else begin
         ; This finds if the name string is anywhere in the aliases
         if noblanks and nocase then $
            aloc=strpos(strcompress(strupcase(info.alias),/remove_all),lookname) $
         else if nocase then $
            aloc=strpos(strupcase(info.alias),lookname) $
         else if noblanks then $
            aloc=strpos(strcompress(info.alias,/remove_all),lookname) $
         else $
            aloc=strpos(info.alias,lookname)
         z=where(aloc ge 0,count)

         ; Need to step through the potential alias list, each must be
         ;   broken apart into all the aliases to make sure the match is
         ;   is unique
         num=0
         for j=0,count-1 do begin
            altnames=strsplit(info.alias[z[j]],'|',/extract)
            if nocase   then altnames=strupcase(altnames)
            if noblanks then altnames=strcompress(altnames,/remove_all)
            zz=where(lookname eq altnames,new_count)
            if new_count ge 1 then begin
               found[i] = z[j]
               num=num+1
            endif
         endfor
         if num ge 1 then count=1 else count=0
         if num gt 1 then begin
            print,'%%GETSTARS: WARNING! alias ',name[i],' is duplicated.'
         endif
      endelse

      if count gt 1 then $
         print,'%%GETSTARS: WARNING! duplicate catalog entry found for ',name[i]

   endfor

   ; Select out those objects that were found
   z=where(found ge 0,count)
   if count gt 0 then begin
      tra  = info.ra[found[z]]
      tdec = info.dec[found[z]]
      trap = info.rap[found[z]]
      tdecp= info.decp[found[z]]

      ; Apply proper motions to epoch (if provided)
      if epoch gt 0 then begin
         dyr = (epoch - 2000.0)    ; years from 2000
         tra  = tra  + trap*dyr
         tdec = tdec + tdecp*dyr
      endif

      ; Now, convert to requested equinox.
      if b1950 then begin
         rad  = tra  * !radeg
         decd = tdec * !radeg
         bprecess, rad, decd, tra, tdec
         tra  = tra  / !radeg
         tdec = tdec / !radeg
      endif

      if ofdate then begin
         tra  = tra  * !radeg
         tdec = tdec * !radeg
         precess,tra,tdec,2000.0,epoch
         tra  = tra  / !radeg
         tdec = tdec / !radeg
      endif

      ; Copy final positions to output vectors
      ra[z]      = tra
      dec[z]     = tdec
      stdname[z] = info.name[found[z]]
      found[z]   = 1

   endif

   ; For anything not found, try to pull a coordinate from the object name.
   z=where(found lt 0,count)
   if count ne 0 then begin
      found[z]=0
      zz=where(strmid(name[z],0,3) eq 'NVt',count)
      if count ne 0 then begin
         for i=0,count-1 do begin
            sign=''
            reads,name[z[zz[i]]],h,m,s,sign,d,mn,sc, $
               format='(3x,i2,i2,i3,a1,i2,i2,i2)'
            hmstorad,h,m,float(s)/10.0,tra
            if sign ne '-' then sign = 1 else sign = -1
            dmstorad,sign,d,mn,sc,tdec
            ra[z[zz[i]]]      = tra
            dec[z[zz[i]]]     = tdec
            stdname[z[zz[i]]] = name[z[zz[i]]]
            found[z[zz[i]]]   = 1
         endfor
      endif
   endif

end

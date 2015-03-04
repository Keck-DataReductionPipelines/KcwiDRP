;+
; NAME:
;     coord
; PURPOSE: (one line)
;     Given a list of names and JD, return coordinates (RA and Dec).
; DESCRIPTION:
; CATEGORY:
;     Photometry
; CALLING SEQUENCE:
;     coord,jd,obs,object,namefile,ra,dec
; INPUTS:
;     jd       - Julian date (scalar or vector).
;     obs      - Observatory code, Marsden's IAUC codes (scalar).
;     object   - Object name (scalar string or string array).
;     namefile - Correspondence file to match up to standard names.
;                  If this variable is undefined or set to '[[STANDARD]]'
;                  then the object names are taken to be standard names
;                  already and the list is used as is.  In this case, the
;                  the return value of STANDARD is precisely equal to object.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     FILE    - Name of star catalog file.  Default is: (see getstars.pro)
;     B1950    - If set, will return B1950 equinox coordinates (def=of date).
;     J2000    - If set, will return J2000 equinox coordinates (def=of date).
;     OFDATE  - Coordinates should be referred to equinox of date.
;                 If selected, EPOCH must be provided.
;     CACHE   - Flag, if set causes ephemeris calculations to be saved to a file.
;     FNCACHE - Override of standard ephemeris cache file.
;     DEBUG   - Flag, if turned on causes all pipe I/O to be echoed to screen.
; OUTPUTS:
;     ra  - Right ascension of object in radians.
;     dec - Delination of object in radians.
; KEYWORD OUTPUT PARAMETERS:
;     STANDARD - string array of standard names that match up to object.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     3/31/93 - Written by Marc W. Buie, Lowell Observatory
;     10/5/93, MWB, added support for two Landolt catalogs.
;     99/02/23, MWB, allowed bypass of namefile specification.
;     2001/11/06, MWB, added CACHE, FNCACHE keywords to ephem call
;     2002/12/04, MWB, fixed obs to accept strings
;-
pro coord,jd,obs,object,namefile,ra,dec, $
          FILE=file, OFDATE=ofdate, STANDARD=standard, $
          B1950=b1950, J2000=j2000, CACHE=cache, FNCACHE=fncache, $
          DEBUG=debug

   ; First validation.
   if n_params() ne 6 then begin
      print,'Usage: coord,jd,obs,object,namefile,ra,dec'
      return
   endif

   if badpar(jd,5,[0,1],caller='COORD: (jd) ',npts=n1) then return
   if badpar(obs,[2,3,7],0,caller='COORD: (obs) ',type=codetype) then return
   if badpar(object,7,[0,1],caller='COORD: (object) ',npts=n2) then return
   if badpar(namefile,[0,7],0,caller='COORD: (namefile) ', $
                default='[[STANDARD]]') then return
   if badpar(debug,[0,1,2,3],0,caller='COORD: (DEBUG) ',default=0) then return

   if codetype ne 7 then begin
      obs = strn(obs,length=3,padchar='0')
   endif else begin
      obs = strupcase(obs)
   endelse

   if n1 ne n2 then begin
      print,'COORD: jd and object name lists must agree in length.'
      return
   endif

   if namefile ne '[[STANDARD]]' then begin
      rdmatch,namefile,proper,informal,err=err
      if err then return

      ; Check to make sure there are no blank (undefined) standard names
      z=where(strlen(proper) eq 0,count)
      if count ne 0 then begin
         print,'COORD: the following objects have undefined standard names.'
         for i=0,count-1 do begin
            print,informal[z[i]]
         endfor
         return
      endif

      matchobj,proper,informal,object,standard
   endif else begin
      standard=object
   endelse

   ; Extract the first character of each object
   type = strmid(standard,0,1)

   ; Make sure there aren't any unknown objects.
   z=where(type eq 'u',count)
   if count ne 0 then begin
      bad = object[z]
      bad = bad[uniq(bad,sort(bad))]
      print,'The following objects are undefined in the name list file ',namefile
      for i=0,n_elements(bad)-1 do print,'   ',bad[i]
      print,'Cannot continue, coordinates not generated.'
      return
   endif

   ;Set up output arrays
   ra  = dblarr(n1)
   dec = dblarr(n2)

   ;Separate object list into Fixed Objects (stars, etc.) and Moving Objects
   ft = where(type ne 'a' and type ne 'c' and type ne 'p',count_ft)
   mt = where(type eq 'a' or  type eq 'c' or  type eq 'p',count_mt)

   print,count_ft,' Stars in list.'
   print,count_mt,' Moving objects in list.'

   if count_mt ne 0 then begin
      print,'Computing ephemerides...',format='(a,$)'
      code=0
      if keyword_set(b1950) then code=1
      if keyword_set(j2000) then code=2
      code=code+50
      ephem,jd[mt],obs,code,standard[mt],eph, $
         DEBUG=debug,cache=cache,fncache=fncache
      ra[mt] = eph[0,*]
      dec[mt] = eph[1,*]
      print,'done.'
   endif

   if count_ft ne 0 then begin
      print,'Computing star positions...',format='(a,$)'
      if keyword_set(b1950) or keyword_set(j2000) then ofdate=0 else ofdate=1
      jd2year,jd[ft[0]],epoch
      getstars,standard[ft],ra_s,dec_s,found,epoch=epoch, $
         OFDATE=ofdate,B1950=b1950,J2000=j2000,FILE=file
      if n_elements(found) gt total(found) then $
         message,'Not all the stars were found!',/info
      ra[ft]  = ra_s
      dec[ft] = dec_s
      print,'done.'
   endif

end

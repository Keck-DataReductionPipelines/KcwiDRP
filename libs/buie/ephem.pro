;+
; NAME:
;     ephem
; PURPOSE: (one line)
;     Ephemeris generator for solar system objects.
; DESCRIPTION:
;     This program pipes the input data to an external FORTRAN program
;     written by Larry Wasserman.  The external program looks up the orbital
;     elements of each object and computes the positions requested by the
;     output code.  The output code options are:
;
;        Code  Meaning
;        -----------------------------------------------------------------------
;         0    Topocentric RA, Dec, of date of object.
;         1    Topocentric RA, Dec,  B1950  of object.
;         2    Topocentric RA, Dec,  J2000  of object.
;         3    Topocentric x, y, z, of date of object.
;         4    Topocentric x, y, z,  B1950  of object.
;         5    Topocentric x, y, z,  J2000  of object.
;         6    Topocentric x, y, z, of date of Sun.
;         7    Topocentric x, y, z,  B1950  of Sun.
;         8    Topocentric x, y, z,  J2000  of Sun.
;         9    B1950 x, y, z, of object with respect to solar system barycenter.
;        10    J2000 x, y, z, of object with respect to solar system barycenter.
;        11    J2000 osculating orbital elements for the object
;                0 - mean anomaly (M)
;                1 - argument of perihelion
;                2 - longitude of the asending node
;                3 - inclination (i)
;                4 - eccentricity (e)
;                5 - semi-major axis (a)
;                6 - perihelion distance (q)
;                7 - aphelion distance (Q)
;                8 - Orbital period
;                9 - epoch of elements
;               10 - line of variations (delta RA)
;               11 - line of variations (delta Dec)
;               12 - semi-major axis of error ellipse
;               13 - semi-minor axis of error ellipse
;               14 - Julian Date of last astrometric measurement
;               15 - Arc length of astrometry
;               16 - Number of astrometric observations
;
;                 VRA, VDec, line of variations for +1 deg change in M (radians)
;                 JDlast - date of last astrometric measurement (nearest day)
;                 arc - Arc length of astrometric observations (days)
;                 nobs - Number of observations used to generate orbit.
;        12    Errors on osculating orbital elements for the object
;                 M, arg peri, node, i, e, a, q, Q, Period,
;                 (9 values)
;
;        20    Code 0 and Code 3 and Code 6 information.
;        21    Code 1 and Code 4 and Code 7 information.
;        22    Code 2 and Code 5 and Code 8 information.
;        23    Code 2, 5, 8, and 11 information.
;
;           all of the following include errors for all quantities, the
;             errors are all together and follow the main quantities.
;        31    Barycentric ecliptic x,y,z and velocities, no light time, of date.
;        32    Barycentric ecliptic x,y,z and velocities, no light time, B1950.
;        33    Barycentric ecliptic x,y,z and velocities, no light time, J2000.
;        34    Heliocentric ecliptic x,y,z and velocities, no light time, of date.
;        35    Heliocentric ecliptic x,y,z and velocities, no light time, B1950.
;        36    Heliocentric ecliptic x,y,z and velocities, no light time, J2000.
;        37    Barycentric equatorial x,y,z and velocities, no light time, of date.
;        38    Barycentric equatorial x,y,z and velocities, no light time, B1950.
;        39    Barycentric equatorial x,y,z and velocities, no light time, J2000.
;        40    Heliocentric equatorial x,y,z and velocities, no light time, of date.
;        41    Heliocentric equatorial x,y,z and velocities, no light time, B1950.
;        42    Heliocentric equatorial x,y,z and velocities, no light time, J2000.
;        43    Invariable plane x,y,z and velocities, no light time, of date.
;        44    Invariable plane x,y,z and velocities, no light time, B1950.
;        45    Invariable plane x,y,z and velocities, no light time, J2000.
;        46    Same as 36, plus covariance matrix (48 values total).
;
;     The object code depends on the type of object requested.
;     All asteroids are preceeded by A, comets by C, planets and satellites
;     by P, and topocentric locations by T.  For asteroids, either the number
;     of the asteroid or its provisional designation will work.  Comets
;     are recognized by either provisional or final designations, not names.
;     Planets and satellites use the NAIF coding scheme for objects.  For
;     example, the Pluto-Charon barycenter is 9, Charon is 901 and the
;     center of Pluto is 999.  Topocentric locations use the same code as
;     for the observatory codes, for example, Anderson Mesa would be T688.
;     None of the names allow spaces.  If the object isn't recognized or
;     supported by the external databases, RA and Dec values (if requested)
;     are returned as -99 and the x,y,z values are returned as 0.  Elements
;     will return as zero for invalid input.
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;     ephem,jd,obs,code,object,ephemeris
; INPUTS:
;     jd     - Julian Date (UT) of position to calculate (scalar or vector).
;     obs    - Observatory code, Marsden's IAUC codes (scalar).
;     code   - Output format code (see description) (scalar).
;     object - Object code (see description) (string or string array).
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;     CACHE  - Flag, if set, indicates that the ephemeris calculations should
;                 be saved in a file for possible later use.  The file name
;                 used will be code+'.eph'.  The file will be written
;                 to the current directory.  If the cache file exists in
;                 the current directory and its length matches the length
;                 of the input ephemeris generation request, then the file
;                 will be read instead of making the call to the external
;                 program, geteph.  If there is no file or if the file isn't
;                 the right length, the ephemeris generation will proceed
;                 normally.
;     FNCACHE - If you need to provide a better (or more unique) file name
;                 than the default algorithm, set this keyword to a string
;                 to be used as the file name for the cached data.
;     PIPE    - If you pass in a valid lun for the geteph pipe, then
;                 ephem will use this pipe rather than open up a new one.
;                 This can dramatically decrease execution time for numerous
;                 "short" calls to ephem.  In this case, the calling program\
;                 must open and close the pipe itself.
; OUTPUTS:
;     ephemeris - 2-D double precision array of values requested by code.
;
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     3/24/93 - IDL interface written by Marc W. Buie, Lowell Observatory.
;     99/07/25, MWB, added CACHE and FNCACHE keyword
;     01/04/06, DBT, added PIPE keyword
;     01/04/03, MWB, changed to support two error numbers in information
;     02/06/28, MWB, added support for codes 31-45
;     02/07/01, MWB, added support for code 12
;     2002/09/09, MWB, added support for string obscode values
;     2002/10/31, MWB, added support for code 46
;     2003/04/25, MWB, changed test for bad return on code 12
;-
pro ephem,jd,obs,code,object,ephemeris, DEBUG=debug, CACHE=cache, $
       FNCACHE=in_cachefile, PIPE=pipe

   ;Validate number of parameters.
   if n_params() ne 5 then begin
      print,'Usage:  ephem,jd,obs,code,object,ephemeris'
      return
   end

   ; Validate input arguments.
   if badpar(jd,5,[0,1],caller='EPHEM: (jd) ' ,npts=npts) then return
   if badpar(obs,[1,2,3,7],[0,1],caller='EPHEM: (obs) ',npts=nobs,type=codetype) then return
   if badpar(code,2,0,caller='EPHEM: (code) ') then return
   if badpar(object,7,[0,1], $
               caller='EPHEM: (object) ',RANK=objrank,NPTS=nobj) then return

   if badpar(debug,[0,1,2,3],0,caller='EPHEM: (DEBUG) ',default=0) then return
   if badpar(cache,[0,1,2,3],0,caller='EPHEM: (CACHE) ',default=0) then return
   if badpar(in_cachefile,[0,7],0,caller='EPHEM: (CACHEFILE) ',default='') then return

   if codetype ne 7 then begin
      obs = strn(obs,length=3,padchar='0')
   endif else begin
      obs = strupcase(obs)
   endelse

   ; Ensure match between object list and time.
   if nobj ne 1 and npts ne nobj then begin
      print,'EPHEM: Error.  The length of object must be 1 or match the length of JD.'
      return
   endif

   ; Ensure match between observatory list and time.
   if nobs ne 1 and npts ne nobs then begin
      print,'EPHEM: Error.  The length of obs must be 1 or match the length of JD.'
      return
   endif

   ; Validate output code requested.
   if code ge 50 then chk_code=code-50 else chk_code=code
   case chk_code of 
      0 : nvals = 2
      1 : nvals = 2
      2 : nvals = 2
      3 : nvals = 3
      4 : nvals = 3
      5 : nvals = 3
      6 : nvals = 3
      7 : nvals = 3
      8 : nvals = 3
      9 : nvals = 3
     10 : nvals = 3
     11 : nvals = 17
     12 : nvals = 9
     20 : nvals = 8
     21 : nvals = 8
     22 : nvals = 8
     23 : nvals = 25
     31 : nvals = 12
     32 : nvals = 12
     33 : nvals = 12
     34 : nvals = 12
     35 : nvals = 12
     36 : nvals = 12
     37 : nvals = 12
     38 : nvals = 12
     39 : nvals = 12
     40 : nvals = 12
     41 : nvals = 12
     42 : nvals = 12
     43 : nvals = 12
     44 : nvals = 12
     45 : nvals = 12
     46 : nvals = 48
     else : nvals = 0
   endcase

   if nvals eq 0 then begin
      print,'EPHEM: Error.  ',code,' is an illegal output code value.'
      return
   endif

   ; predeclare string for reading
   line=' '

   ; Caching operation, look for file, if found read it and return
   if cache then begin
      if in_cachefile eq '' then begin
         if nobj eq 1 then $
            cachefile = object+'_'+strn(code)+'.eph' $
         else $
            cachefile = strn(code)+'.eph'
      endif else begin
         cachefile = in_cachefile
      endelse

      if exists(cachefile) then begin
         openr,clun,cachefile,/get_lun
         nlines = 0

         while not eof(clun) do begin
            readf,clun,line,format='(a)'
            nlines=nlines+1
         endwhile

         if nlines eq npts then begin
            point_lun,clun,0
            goodcache=1
         endif else begin
            free_lun,clun
            goodcache=0
         endelse
      endif else begin
         goodcache=0
      endelse
   endif else begin
      goodcache=0
   endelse

   on_ioerror,bail_out

   ; Start up Larry's ephemeris generator (this assumes it's in the path).
   if not goodcache then begin
      ;cache the pipe to speed up multiple calls
      if n_elements(pipe) ne 0 then begin
         cache_pipe = 1
         ;if pipe not already opend, open it
         if pipe eq '' then begin
            spawn,'geteph',unit=pipe
         endif
      endif else begin
         spawn,'geteph',unit=pipe
      endelse
   endif

   ; Create output and temporary arrays.
   ephemeris=dblarr(nvals,npts)
   tmp = dblarr(nvals)

   if not goodcache and cache then $
      openw,clun,cachefile,/get_lun

   for i=0L,npts-1 do begin

      if goodcache then begin
         readf,clun,line
      endif else begin
         if nobj eq 1 then obj=object else obj=object[i]
         if nobs eq 1 then obse=obs else obse=obs[i]

         if debug then begin
            print,jd[i],obse,code,obj,format='(f13.5,1x,a3,1x,i2,1x,a)'
         endif

         printf,pipe,jd[i],obse,code,obj,format='(f13.5,1x,a3,1x,i2,1x,a)'
         flush,pipe
         readf,pipe,line

         if cache then printf,clun,line

         if debug then begin
            print,line
         endif
      endelse

      reads,line,tmp
      ephemeris[*,i] = tmp

   endfor

   ; Scan the data to see if valid ephemerides were returned for all.
   badflags = 0
   if chk_code eq 0 or chk_code eq 1 or chk_code eq 2 or $
      chk_code eq 20 or chk_code eq 21 or chk_code eq 22 then $
      z = where(ephemeris[0,*] lt -90.0, badflags) $
   else if chk_code eq 12 then $
      z = where(ephemeris[0,*] lt 0.0, badflags) $
   else $
      z = where(ephemeris[0,*] eq 0.0 and $
                ephemeris[1,*] eq 0.0 and $
                ephemeris[2,*] eq 0.0, badflags)

   if badflags ne 0 then begin
      bad = object[z]
      bad = bad[uniq(bad,sort(bad))]
      if n_elements(bad) eq 1 then begin
         print,'EPHEM:  Warning!  Object code ',bad[0], $
               ' did not return a valid ephemeris.'
         print,'   output code=',strn(code)
      endif else begin
         print,'EPHEM:  Warning!  No ephemeris was returned ', $
               'for the following object codes.'
         for i=0L,n_elements(bad)-1 do print,'   ',bad[i]
         print,'   output code=',strn(code)
      endelse
   endif

   if goodcache then begin
      free_lun,clun
   endif else begin
      ;if no pipe chache, close pipe
      if n_elements(cache_pipe) eq 0 then begin
         free_lun,pipe
         pipe = ''
      endif
      if cache then free_lun,clun
   endelse
   return

   ; This for unexpected output from geteph
   bail_out:
   print,jd[i],obse,code,obj,format='(f13.5,1x,a3,1x,i2,1x,a)'
   print,'EPHEM:  Illegal information returned by geteph:'
   print,line

   if n_elements(cache_pipe) eq 0 then begin
      free_lun,pipe
      pipe = ''
   endif
   return
end

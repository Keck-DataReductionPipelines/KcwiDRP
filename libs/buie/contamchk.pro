;+
; NAME:
;  contamchk
; PURPOSE:
;  Check solar system object for contamination from field star.
; DESCRIPTION:
;  Checks against the UNSO A2.0 catalog for appulse distances with field
;    stars.  Motion of the object is assumed to be linear over the requested
;    time range.  Usually, the time ranges are small (on the order of an hour
;    or so).  If a star brighter than the threshold is present within the
;    minimum separation distance during the time window, then the contamination
;    flag is set.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  bad = contamchk(jd1,jd2,object,septhresh)
; INPUTS:
;  jd1       - Julian date of starting time of window
;  jd2       - Julian date of ending time of window
;  object    - Ephem code for solar system object
;  septhresh - Separation threshold (arcsec), closer than this is considered
;                to be contamination if star is brighter than magthresh.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OBSCODE   - Observatory code (default=500, geocentric)
;  MAGTHRESH - Threshold magnitude (default=20), anything fainter is ignored.
;  STARFILE  - If provided, the USNO sub-catalog file is created and then not
;                 deleted at the end.  If the file already exists and the
;                 name is provided, then the file is used without calling
;                 refnet again.
; OUTPUTS:
;  return value is a flag, if set means there is a star too close in the window.
;
; KEYWORD OUTPUT PARAMETERS:
;  ERROR  - Set on return to indicate an error condition (usually this is
;              cause by a bad object code).
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Note that if you use the starfile keyword to save the sub-catalog, it is
;    up to you to get rid of the file when it is no longer relevant.  The saved
;    file will usually be no good if the date changes much or if you need to
;    change the magthresh to a fainter limit.  This option can, if used
;    properly, help speed things up but the calling program must take on more
;    responsibility to get the right answer.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/03/10
;  2002/09/09, MWB, added support for string obscode values
;-
function contamchk,jd1,jd2,object,septhresh, $
       OBSCODE=obscode,MAGTHRESH=magthresh,STARFILE=starfile,ERROR=error

   contam = 0
   error  = 1

   if badpar(jd1,5,0,caller='CONTAMCHK: (jd1) ') then return,contam
   if badpar(jd2,5,0,caller='CONTAMCHK: (jd2) ') then return,contam
   if badpar(object,7,0,caller='CONTAMCHK: (object) ') then return,contam
   if badpar(septhresh,[2,3,4,5],0, $
               caller='CONTAMCHK: (septhresh) ') then return,contam

   if badpar(obscode,[0,2,3],0,CALLER='CONTAMCHK: (OBSCODE) ', $
                               default='500',type=codetype) then return,contam
   if badpar(starfile,[0,7],0,caller='CONTAMCHK: (STARFILE) ', $
                default='contamchk.cat') then return,contam
   if badpar(magthresh,[0,2,3,4,5],0,caller='CONTAMCHK: (MAGTHRESH) ', $
                default=20.0) then return,contam

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   error  = 0

   ; Possible cleanup from last call.
   if exists('contamchk.cat') then file_delete,'contamchk.cat'

   ; Generate ephemeris for object at start and stop of window.
   jd=[jd1,jd2]
   ephem,jd,obscode,2,object,eph

   ; Check for valid ephemeris
   if max(eph) lt -90 then begin
      error=1
      return,contam
   endif

   ra1  = eph[0,0]
   dec1 = eph[1,0]
   ra2  = eph[0,1]
   dec2 = eph[1,1]

   dist = angsep(ra1,dec1,ra2,dec2)*!radeg*3600.0

   ; Generate the USNO A2.0 sub-catalog (if not already there)
   if not exists(starfile) then begin
      wid = 20+dist+septhresh*2
      refnet,ra1,dec1,wid,wid,magthresh+5,magthresh,starfile
   endif

   ; Make sure the file got generated, if not, quit.
   if not exists(starfile) then begin
      error=1
      return,contam
   endif

   ; Load the USNO sub-catalog
   rdstarc,starfile,ra,dec,bmag,rmag,nstars

   ; If there are no stars, we're done.
   if nstars eq 0 then return,contam

   ; Filter out stars that are too faint to count.
   z=where(rmag le magthresh,count)
   if count eq 0 then begin
      return,contam
   endif else begin
      ra=ra[z]
      dec=dec[z]
      bmag=bmag[z]
      rmag=rmag[z]
      nstars = count
   endelse

   ; Simple test for small motions
   if dist lt septhresh/2.0 then begin
      sep1 = angsep(ra1,dec1,ra,dec)*!radeg*3600.0
      sep2 = angsep(ra2,dec2,ra,dec)*!radeg*3600.0
      if min(sep1) le septhresh or min(sep2) le septhresh then contam=1

   ; More work if the distance traversed is large (compared to septhresh)
   endif else begin
      ; convert to tangent plane coords
      astrd2sn,ra,dec,ra1,dec1,x,y
      astrd2sn,ra2,dec2,ra1,dec1,x2,y2
      x  = x*!radeg*3600.0d0
      y  = y*!radeg*3600.0d0
      x2 = x2*!radeg*3600.0d0
      y2 = y2*!radeg*3600.0d0
      a1 = atan(y2,x2)
      a2 = atan(y,x)
      b = sqrt(x2^2+y2^2)
      d1= sqrt(x^2+y^2)
      d2= sqrt((x-x2)^2+(y-y2)^2)
      c = abs(d1*cos(a2-a1))
      dp = sqrt(d1^2-c^2)
      mind = dp
      lrmin = d1 < d2
      z = where(c gt b,count)
      if count ne 0 then mind[z] = lrmin[z]
      z=where(mind le septhresh,count)
;print,count,' close'
      if count ne 0 then contam = 1
;if count ne 0 then print,mind[z]
   endelse

   ; Possible cleanup from this call.
   if starfile eq 'contamchk.cat' then file_delete,starfile,/quiet

   return,contam

end

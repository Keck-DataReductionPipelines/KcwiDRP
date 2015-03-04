;+
; NAME:
;  lstlim
; PURPOSE:   (one line only)
;  Compute the LST range for time of observation
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  lstlim,ra,dec,answer,rlst,slst,quiet=quiet
; INPUTS:
;  ra  - RA of target (J2000) in radians or as a sexigesimal string
;  dec - Declination of target (J2000) in radians or as a sexigesimal string
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  AMCRIT - airmass at start and stop of LST range (default=2.5)
;  QUIET - flag if set will suppress printed output (echo of RA,Dec; the
;            lst string, answer, and the length of time above the limit in
;            hours)
; OUTPUTS:
;  answer - formatted string of the start and ending local sidereal time
;             to the nearest minute
;  rlst   - local sidereal time at the time of rising (2.5 airmasses)
;  slst   - local sidereal time at the time of setting (2.5 airmasses)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Only works for Lowell Observatory right now.  Airmass limit is hard-coded.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/04/13 - Marc W. Buie, Lowell Observatory, first very rough version.
;  2004/10/26, MWB, added AMCRIT and some input validation.
;  2008/04/08, MWB, modified formatting of printed output.
;-
pro lstlim,ra,dec,answer,rlst,slst,QUIET=quiet,AMCRIT=amcrit

   self='LSTLIM: '
   if badpar(ra,[4,5,7],0,caller=self+'(ra) ',type=ratype) then return
   if badpar(dec,[4,5,7],0,caller=self+'(dec) ',type=dectype) then return
   if badpar(amcrit,[0,2,3,4,5],0,caller=self+'(AMCRIT) ',default=2.5) then return

   if ratype  eq 7 then ra_r  = raparse(ra)   else ra_r =ra
   if dectype eq 7 then dec_r = decparse(dec) else dec_r=dec

   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi

   crital = 0.5*!pi - acos(1.0/amcrit)

   jd = systime(/julian)

   bestalt = !pi/2.0-abs(lat - dec_r)
   zenith = !pi/2.0-bestalt
   cz = cos(zenith)
   am = sqrt(235225.0*cz*cz + 970.0 + 1.0) - 485*cz

   lsidtim,jd,lon,lst
   hatojd,0.0d0,ra_r,lst,jd,jdtrans
   altoha,crital,dec_r,lat,horzha,type
   if type eq 0 then begin
      jdrise  = jdtrans - horzha/2.0d0/!dpi
      jdset   = jdtrans + horzha/2.0d0/!dpi
   endif

   lsidtim,jdrise,lon,rlst
   lsidtim,jdset,lon,slst

   rastr,rlst,-2,rlststr
   rastr,slst,-2,slststr

   if not keyword_set(quiet) then begin
      rastr,horzha,-2,has
      print,''
      print,'Position ',ra,' ',dec,' and critical airmass of ',amcrit, $
         format='(a,a,a,a,a,f3.1)'
      print,'LST range is ',rlststr,'-',slststr,', HA at critical airmass ',has
      print,'Altitude at transit ',bestalt*!radeg, $
            ' deg, airmass at transit ',am, $
         format='(a,f4.1,a,f4.2)'
      print,''
   endif

   answer = rlststr+'-'+slststr

end

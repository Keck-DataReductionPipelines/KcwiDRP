;+
; NAME:
;  obswind
; PURPOSE:
;  Compute possible observing window for a celestial object.
; DESCRIPTION:
;
; This routines determines the possible observing window for an object.
; The times of sunrise and sunset are left as input parameters to help
; speed up the computations.  It is expected that this routine will be called
; many times for a given date (LST0), but for different objects.
; RTIME and STIME denote the earliest time and the latest time that the object
; can be observed.  Note that if RTIME > STIME then the range passes through
; 0 hours.  RKIND and SKIND tell what limits the ends of the window as follows:
;        RKIND  will be either 'sunset' or 'rises' for TYPE = 0
;        RKIND  will be 'sunset' for TYPE =  1
;        RKIND  is undefined for TYPE = -1
;        SKIND  will be either 'sunrise' or 'sets' for TYPE = 0
;        SKIND  will be 'sunrise' for TYPE = 1
;        SKIND  is undefined for TYPE = -1
;        if TYPE = 2 then object is not available when sun is down.
;
; CATEGORY:
;  Astronomy
;
; CALLING SEQUENCE:
;  obswind,lst0,lat,ra,dec,srise,sset,rtime,rkind,stime,skind,type
; INPUTS:
;  lst0  - Local Sidereal Time at local midnight (radians).
;  lat   - Latitude of observatory (radians).
;  ra    - right ascension of object (radians), should be of date.
;  dec   - declination of object (radans), should be of date.
;  srise - Time of sunrise (JD).
;  sset  - Time of sunset (JD).
;  
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  AMLIMIT - Air Mass limit to denote rise and set of object.  Default=3.0
;
; OUTPUTS:
;  rtime - Time of opening of observing window (at rise or sunset)
;  rkind - String that identifies limit, either 'sunset' or 'rises'
;  stime - Time of closing of observing window (at rise or sunset)
;  skind - String that identifies limit, either 'sunset' or 'rises'
;  type  - Indicator of type of window
;           2 - Object not available when sun is down.
;           1 - Object always up.
;           0 - Object rises and sets.
;          -1 - Object always down.
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
;  1997/9/3, Written by Marc W. Buie, Lowell Observatory
;  2002/03/27, MWB, fixed AMLIMIT bug.
;
;-
PRO obswind,lst0,lat,ra,dec,srise,sset,rtime,rkind,stime,skind,type, $
   AMLIMIT=amlimit

   if badpar(lst0, [4,5],[0,1],CALLER='OBSWIND: (lst0) ' ) then return
   if badpar(lat,  [4,5],0,    CALLER='OBSWIND: (lat) '  ) then return
   if badpar(ra,   [4,5],[0,1],CALLER='OBSWIND: (ra) '   ) then return
   if badpar(dec,  [4,5],[0,1],CALLER='OBSWIND: (dec) '  ) then return
   if badpar(srise,[4,5],[0,1],CALLER='OBSWIND: (srise) ') then return
   if badpar(sset, [4,5],[0,1],CALLER='OBSWIND: (sset) ' ) then return

   ; Set the critical altitude and airmass for observability.
   if keyword_set(amlimit) then $
      amcrit=amlimit $
   else $
      amcrit=3.0
   crital = 0.5*!pi - acos(1.0/amcrit)
   altoha,crital,dec,lat,ha,type

   IF type eq 1 THEN BEGIN
      rtime = sset
      rkind = 'sunset'
      stime = srise
      skind = 'sunrise'
   ENDIF ELSE IF type eq 0 THEN BEGIN
      jdlclmid = (srise+sset)/2.0d0
      hatojd,0.0d0,ra,lst0,jdlclmid,jdtrans
      rtime = jdtrans - ha/2.0d0/!dpi
      stime = jdtrans + ha/2.0d0/!dpi
      rkind = 'rises'
      skind = 'sets'
      IF srise GE sset THEN BEGIN

         IF rtime LE stime THEN BEGIN
            IF rtime LT sset THEN BEGIN
               IF stime LT sset THEN BEGIN
                  type=2
               ENDIF ELSE IF stime GT srise THEN BEGIN
                  rtime=sset
                  rkind='sunset'
                  stime=srise
                  skind='sunrise'
               ENDIF ELSE BEGIN
                  rtime=sset
                  rkind='sunset'
               ENDELSE
            ENDIF ELSE IF rtime GT srise THEN BEGIN
               type=2
            ENDIF ELSE BEGIN
               IF stime GT srise THEN BEGIN
                  stime=srise
                  skind='sunrise'
               ENDIF
            ENDELSE
         ENDIF ELSE BEGIN
            IF rtime LT sset THEN BEGIN
               rtime=sset
               rkind='sunset'
               stime=srise
               skind='sunrise'
            ENDIF ELSE IF rtime GT srise THEN BEGIN
               IF stime LT sset THEN BEGIN
                  type=2
               ENDIF ELSE IF stime GT srise THEN BEGIN
                  rtime=sset
                  rkind='sunset'
                  stime=srise
                  skind='sunrise'
               ENDIF ELSE BEGIN
                  rtime=sset
                  rkind='sunset'
               ENDELSE
            ENDIF ELSE BEGIN
               stime=srise
               skind='sunrise'
            ENDELSE
         ENDELSE
      ENDIF ELSE BEGIN
         IF rtime LE stime THEN BEGIN
            IF rtime LT srise THEN BEGIN
               IF stime GT srise THEN BEGIN
                  stime=srise
                  skind='sunrise'
               ENDIF
            ENDIF ELSE IF rtime GT sset THEN BEGIN
               ; nop
            ENDIF ELSE BEGIN
               IF stime GE sset THEN BEGIN
                  rtime=sset
                  rkind='sunset'
               ENDIF ELSE BEGIN
                  type=2
               ENDELSE
            ENDELSE
         ENDIF ELSE BEGIN
            IF rtime LT srise THEN BEGIN
               sset=srise
            ENDIF ELSE IF rtime GT sset THEN BEGIN
               IF stime GT srise THEN BEGIN
                  stime=srise
                  skind='sunrise'
               ENDIF
            ENDIF ELSE BEGIN
               IF stime GT srise THEN BEGIN
                  stime=srise
                  skind='sunrise'
                  rtime=sset
                  rkind='sunset'
               ENDIF ELSE BEGIN
                  rtime=sset
                  rkind='sunset'
               ENDELSE
            ENDELSE
         ENDELSE
      ENDELSE
   ENDIF

END



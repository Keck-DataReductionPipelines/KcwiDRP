;+
; NAME:
;  wrphot
; PURPOSE: (one line)
;  Write a standard raw photometry data file (Pluto format).
; DESCRIPTION:
;  This creates a .001 format Pluto-Charon format file from the input
;    photometry.  As a suggestion, use .000 as a suffix if these are NOT
;    standard magnitudes (such as differential to some star), and use
;    .001 if they are magnitudes on a standard system.
;
;      File format: (one blank separates each field)
;
;         Orbit Flag . . . . . . . . . . I1      1 -> Search for new orbital
;                                                     elements that most
;            This is a flag to the program            closely match the time
;            PLEPH that controls which set            of observation.
;            of orbital elements to use          0 -> Use the same set of
;            for the Pluto ephemeris.                 orbital elements as used
;                                                     for the previous point.
;        This is always set to zero in the output from this program.
;
;         JD of observation (UT) . . . . F13.5   Julian date of the observation.
;
;         Filter ID  . . . . . . . . . . A2      Two character id of the filter
;                                                used for the observation.
;                                                Consult the filter documentation
;                                                to get the ID's that are
;                                                currently in use.
;        Only the first two characters of the filter field are saved to the file.
;
;         Observed Magnitude . . . . . . F7.4    Observed magnitude reduced to
;                                                some standard system.  The
;                                                filter ID must reflect this
;                                                system.
;
;         Uncertainty (sigma)  . . . . . F6.4    Error bar for observation.
;
;         Source . . . . . . . . . . . . A45     Free format field to reference
;                                                the observer and the telescope
;                                                used for the observation.
;        This is left blank by this program.
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  wrphot,jd,filter,mag,err,file
; INPUTS:
;  jd     - Julian date of observation (double precision).
;  filter - String array of filter names.
;  mag    - Observed magnitude.
;  err    - Uncertainty on the observed magnitude.
;  file   - String with name of file to save photometry to.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  APPEND - if set appends data to the named file.
;  BAD    - if supplied, must be a vector of flags, 0=good, 1=bad
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1993 Jun 26
;  96/11/03, MWB, added BAD input
;  2006/08/10, Peter L. Collins, Lowell Observatory, added SELF and
;              fixed small bug where nfil unchecked if not 1.
;-
pro wrphot,jd,in_filter,mag,err,file,APPEND=append,BAD=bad

   self = 'WRPHOT '
   if badpar(jd,5,       [0,1],caller=self +' (jd) ',npts=njd) then return
   if badpar(in_filter,7,[0,1],caller=self +' (filter) ',npts=nfil) then return
   if badpar(mag,[4,5],  [0,1],caller=self +' (mag) ',npts=nmag) then return
   if badpar(err,[4,5],  [0,1],caller=self +' (err) ',npts=nerr) then return
   if badpar(file,7,         0,caller=self +' (file) ') then return
   if badpar(bad,[0,2,3],[0,1],caller=self +' (BAD) ',  npts=nbad, $
             default=intarr(njd)) then return

   if nfil eq 1 then  begin 
      filter = replicate(in_filter,njd) 
      nfil = njd
   endif else  filter = in_filter

   in_pts = [njd,nmag,nerr,nbad,nfil]
   if max(in_pts) ne min(in_pts) then begin
      print,'WRPHOT: All inputs must have the same length.  Aborting.'
      return
   endif

   if keyword_set(append) then $
      openw, lun, file, append=append, /get_lun $
   else $
      openw, lun, file, /get_lun

   for i=0,njd-1 do $
      printf,lun,bad[i],jd[i],strmid(filter[i],0,2)+'  ',mag[i],err[i], $
         format='(i1,1x,f13.5,1x,a2,1x,f7.4,1x,f6.4)'

   free_lun,lun
end

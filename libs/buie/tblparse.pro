;+
; NAME:
;  tblparse
; PURPOSE:
;  Determine properties and problems from the table file with OSIRIS data
; DESCRIPTION:
;  The beginning and ending spectra files for a group of spectra are determined
;  for later processing.  Spectra which need a mate and have probably not been
;  processed by xdspec are flagged.  Spectra sets that have inequal exposure
;  are flagged as inhomogenous.
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  tblparse,tbl,oblist,badset
; INPUTS:
;  tbl - Structure containing the data from the table file
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  oblist - Array containing the beginning and ending filenames for each set of spectra as well
;           as the flagged, mateless spectra.  It has the format:
;           oblist[0,*] - Index for start of group
;           oblist[1,*] - Index for end of group
;           oblist[2,*] - Flag which if true indicates that the image is missing a mate.  One
;                         missing mate may require redoing the full set.
;  badset - The flag that indicates if there is one or more sets of spectra with inequal
;           exposure times.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  98/06/10 - Written by Chris Dalla Piazza, Lycoming College; extracted from xdspec
;  98/06/30, MWB, some logic cleanup on "deleted" spectra flags.
;-
pro tblparse,tbl,oblist,badset

if n_params() eq 0 then begin
   print,'tblparse,tbl,oblist,badset'
   return
endif

if badpar(tbl,8,1,caller='TBLPARSE: (tbl) ') then return

; Scan list, find groups of object data and tag if any need image mates
ng=0
oldob=''
objstart=-1
objend=-1
FOR i=0,tbl.nobs-1 DO BEGIN

   ; Deal with objects here
   IF tbl.imtype[i] eq 'o' THEN BEGIN

      ; If the object name changes, or the end of the list is
      ;   encountered, record the start/end of an object set.
      IF tbl.objnam[i] ne oldob or i eq tbl.nobs-1 THEN BEGIN

         ; Change is significant only if the old object name wasn't blank.
         IF oldob ne '' THEN BEGIN

            ; Three cases to deal with here: (1) Object group just
            ;    changed in the middle of the list, (2) Object group
            ;    ends at the end of the list, (3) List ends with a
            ;    singleton.
            IF i ne tbl.nobs-1 THEN BEGIN
               objend = i-1
            ENDIF ELSE BEGIN
              IF tbl.objnam[i] ne oldob THEN BEGIN
                 objend = i-1
              ENDIF ELSE BEGIN
                 objend = i
              ENDELSE
            ENDELSE

            ; First indication of needing a mate is that it is not flagged 'x'
            ;   and its mate is -1
            zz=where(tbl.imflag[objstart:objend] ne 'x' and $
                     tbl.mate[objstart:objend] eq -1,countzz)
            needmate = countzz gt 0

            ; Second indication of needing a mate is that a spectrum marked 'x'
            ;   has a listed mate.
            IF not needmate THEN BEGIN
               zz=where(tbl.imflag[objstart:objend] eq 'x' and $
                        tbl.mate[objstart:objend] ne -1,countzz)
               needmate = countzz gt 0
            ENDIF

            ; Third indication of needing a mate is that a mate is marked 'x'
            IF not needmate THEN BEGIN
               for j=objstart,objend do begin
                  if tbl.mate[j] ne -1 then begin
                     zz = where(tbl.mate[j] eq tbl.imnum)
                     if zz[0] eq -1 then begin
                        needmate = 1
                     endif else if tbl.imflag[zz[0]] eq 'x' then begin
                        needmate = 1
                     endif
                     if needmate then begin
                        tbl.mate[j] = -1
                     endif
                  endif
               endfor
            ENDIF

            ; Save the info for this group.
            IF ng eq 0 THEN BEGIN
               oblist = [objstart,objend,needmate]
            ENDIF ELSE BEGIN
               oblist = [[oblist],[objstart,objend,needmate]]
            ENDELSE

            ng=ng+1
         ENDIF
         objstart = i
      ENDIF
      oldob = tbl.objnam[i]

   ; Deal with non-objects (various calibration products).
   ENDIF ELSE BEGIN
      IF oldob ne '' THEN BEGIN
         if i ne tbl.nobs-1 then objend = i-1 else objend=i
         zz=where(tbl.imflag[objstart:objend] ne 'x' and $
                  tbl.mate[objstart:objend] eq -1,countzz)
         needmate = countzz gt 0
         IF not needmate THEN BEGIN
            zz=where(tbl.imflag[objstart:objend] eq 'x' and $
                     tbl.mate[objstart:objend] ne -1,countzz)
            needmate = countzz gt 0
         ENDIF
         IF ng eq 0 THEN BEGIN
            oblist = [objstart,objend,needmate]
         ENDIF ELSE BEGIN
            oblist = [[oblist],[objstart,objend,needmate]]
         ENDELSE
         ng=ng+1
      ENDIF
      oldob = ''
   ENDELSE

ENDFOR

; This traps a singleton on the end.  This is not marked for a needed
;   mate in case more will come later.
IF objstart ne objend and oldob ne '' THEN BEGIN
   IF ng eq 0 THEN BEGIN
      oblist = [objstart,objstart,0]
   ENDIF ELSE BEGIN
      oblist = [[oblist],[objstart,objstart,0]]
      print,'TBLPARSE: Warning: There is a solitary object frame at the end of'
      print,'the data stream.'
   ENDELSE
   ng=ng+1
   tbl.imflag[objstart] = 't'
ENDIF

; To check for inhomogeneous sets, the exposure time must first be rounded to
;   the nearest second to account for the jitter in exposure time.  Assume
;   a nominal 0.9 second time offset to commanded exposure times.
exptime = fix(tbl.exptim-0.9+0.5)

; Check groups for null sets, ie., all spectra excluded.  If any found,
; flag them with -2 so they can later be removed from oblist.
badset=0
FOR i=0,ng-1 DO BEGIN
   ; Count the number of good spectra
   zg=where(tbl.imflag[oblist[0,i]:oblist[1,i]] ne 'x',countzg)

   ; If none good, mark for deletion from oblist.
   IF countzg eq 0 THEN BEGIN
      oblist[*,i]=[-1,-1,-2]

   ; Okay, at least one is good, final check to see if the set is
   ;   homogenous.  'x' spectra don't count.
   ENDIF ELSE BEGIN
      idx=indgen(oblist[1,i]-oblist[0,i]+1)+oblist[0,i]
      IF min(exptime[idx[zg]]) ne max(exptime[idx[zg]]) or $
         min(tbl.avego[idx[zg]])  ne max(tbl.avego[idx[zg]])  THEN BEGIN
         print,'TBLPARSE: WARNING: Inhomogeneous Set: frames', $
               tbl.imnum[oblist[0,i]],'to',tbl.imnum[oblist[1,i]], $
               '-->',tbl.objnam[idx[0]], $
               format='(a,1x,i3.3,1x,a,1x,i3.3,1x,a,1x,a)'
         badset=1
      ENDIF
   ENDELSE
ENDFOR

; Now trim the sets that need deleting
z=where(oblist[2,*] ne -2,countgood)
if countgood ne ng then oblist = oblist[*,z]

end

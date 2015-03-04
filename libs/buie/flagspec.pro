;+
; NAME: 
;  flagspec
; PURPOSE: 
;  Interactive marking of bad pixels in an OSIRIS XD spectrum.
; DESCRIPTION:
;  Plots one order at a time and allows marking bad pixels.  See MARKDATA
;    for operation of the widget used to mark data.  No attempt is made to
;    repair the spectrum.  MARKDATA is called on each order.
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  flagspec,calib,root,fileno
; INPUTS:
;  calib- Anonymous structure containing all pertinent calibration
;           information.  This structure is usually loaded beforehand using
;           the routine, "ldcalir"
;  root   - Root of file name(s) (no . at the end, may include path).
;  fileno - Number of spectrum (suffix).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  The bad flags file for this spectrum is potentially modified.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  97/03/23, Written by Marc W. Buie, Lowell Observatory
;-
pro flagspec,calib,root,fileno

if badpar(calib,8,1,CALLER='flagspec (calib) ') then return
if badpar(root, 7,0,CALLER='flagspec (root) ') then return
if badpar(fileno,[2,3],[0,1],CALLER='cleanspec (fileno) ',npts=nfiles) then return

FOR j=0,nfiles-1 DO BEGIN

   fname = root+'.'+string(fileno[j],format='(i3.3)')
   spec  = readfits(fname,/silent)
   IF strmid(root,strlen(root)-1,1) eq 's' THEN $
      bname = strmid(root,0,strlen(root)-1) $
   ELSE $
      bname = root
   bname = bname+'b.'+string(fileno[j],format='(i3.3)')
   bad=bytarr(n_elements(spec))
   IF exists(bname) THEN BEGIN
      openr,lun,bname,/get_lun
      readu,lun,bad
      free_lun,lun
   ENDIF
   oldbad=bad

   for i=0,calib.nor-1 do begin

      tmpbad = bad[calib.o[i,0]:calib.o[i,1]]
      markdata,calib.w[calib.o[i,0]:calib.o[i,1]], $
               spec[calib.o[i,0]:calib.o[i,1]],tmpbad, $
               XTITLE='Wavelength (microns)', $
               YTITLE='Arbitrary Flux', $
               TITLE='Editing order '+strtrim(string(i),2), $
               PTITLE=fname+': Order '+strtrim(string(i),2),/connect
      bad[calib.o[i,0]:calib.o[i,1]] = tmpbad

   endfor

   z=where(bad ne oldbad,count)
   IF count ne 0 THEN BEGIN
      print,'Updating bad flags: ',bname
      openw,lun,bname,/get_lun
      writeu,lun,bad
      free_lun,lun
   ENDIF

ENDFOR

end


;+
; NAME:
;  rdstarc
; PURPOSE:
;  Read refnet based star catalog files.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  rdstarc,starfile,ra,dec,bmag,rmag,nstars
; INPUTS:
;  starfile - Name of file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag, if set suppresses all non-error output.
; OUTPUTS:
;  ra   - right ascension in radians (J2000)
;  dec  - declination in radians (J2000)
;  bmag - Blue magnitude
;  rmag - Red magnitude  (see the USNO A2.0 catalog docs to see what this means)
;  nstars - Number of stars found (can be zero- in this case other 
;          outputs not defined).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; Read star catalog file- if it is ascii (as output by refnet) use starcprmt to
; convert to v1.0 binary format.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/01/07
;  02/03/10 - MWB, added nstars return value
;  2007/11/23- Peter L. Collins, Lowell Observatory, 
;    convert to v1.0 binary fmt and add promote call.
;  2008/10/21, MWB, added SILENT keyword
;-
pro rdstarc,starfile,ra,dec,bmag,rmag,nstars,SILENT=silent

   supported='STARC v1.0'
   self='RDSTARC: '
   nstars=0L

   if badpar(starfile,7,0,caller=self+'(starfile) ') then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return

   version = supported ; just needs to be a string of the right length.
   starcprmt, starfile, SILENT=silent
   if exists(starfile) then begin
      info=file_info(starfile)
      if info.size le strlen(version)  then begin
      if not silent then print,'Star catalog file ',starfile,' is too short.'
      return
      endif
      openr, slun,starfile,/GET_LUN
      version_act = strlowcase(version)
      readu, slun, version_act  ; already promoted, we hope.
      if version ne  version_act then begin
         ; This means promote failed.
         print,'Star catalog file ',starfile,' has invalid contents.'
         free_lun, slun
         return
      endif
      readu, slun, nstars
      swap_endian_inplace,nstars,/SWAP_IF_LITTLE_ENDIAN
      if nstars gt 0 then begin
         ra = dblarr(nstars)
         dec = dblarr(nstars)
         bmag = fltarr(nstars)
         rmag = fltarr(nstars)
         ; ra vector, double of length nstars.
         readu, slun,ra
         swap_endian_inplace, ra, /SWAP_IF_LITTLE_ENDIAN
         ; dec vector, double of length nstars.
         readu, slun,dec
         swap_endian_inplace, dec, /SWAP_IF_LITTLE_ENDIAN
         ; bmag vector, double of length nstars.
         readu, slun,bmag
         swap_endian_inplace, bmag, /SWAP_IF_LITTLE_ENDIAN
         ; rmag vector, double of length nstars.
         readu, slun,rmag
         swap_endian_inplace, rmag, /SWAP_IF_LITTLE_ENDIAN
      endif
      free_lun, slun
   endif else begin
      print,'Star catalog file ',starfile,' cannot be found.'
   endelse
end

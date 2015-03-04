;+
; NAME:
;	rdpoint
; PURPOSE: (one line)
;	Read a raw ASCII format pointing data file as produced by MOVE.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;	rdpoint,filename,raobs,decobs,ratrue,dectrue,ppmnum,flag
; INPUTS:
;	filename - String containing the name of file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	raobs   - Observed right ascension in degrees.
;	decobs  - Observed declination in degrees.
;	ratrue  - Catalog right ascension in degrees.
;	dectrue - Catalog declination in degrees.
;	ppmnum  - PPM # of star pointed to.  (may not be useful)
;  flag    - T or F
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	4/26/93 - Written by Marc W. Buie, Lowell Observatory
;-
pro rdpoint1,filename,haobs,decobs,hatrue,dectrue,ppmnum,flag

readcol,filename,haobs,decobs,hatrue,dectrue,ppmnum,flag,format='(f,f,f,f,l,a)'

nobs=n_elements(haobs)

print,nobs,' objects in file.'

haobs   = haobs   * 15.0d0
hatrue  = hatrue  * 15.0d0

end

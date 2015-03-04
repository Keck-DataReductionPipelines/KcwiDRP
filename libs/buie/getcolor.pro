;+
; NAME:
;     getcolor
; PURPOSE: (one line)
;     Given a list of names and JD, return B-V and V-R colors.
; DESCRIPTION:
; CATEGORY:
;     Photometry
; CALLING SEQUENCE:
;     getcolor,stand,filler,bmv,vmr
; INPUTS:
;     stand    - Standard object name (scalar string or string array).
;     filler   - Color values to insert if object not in star catalogs.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     LAND1   - Optional name for the Landolt 1983 Standards Catalog.
;     LAND2   - Optional name for the Landolt 1992 Standards Catalog.
;     PRICAT  - Name of private star catalog file for X star catalog.
;                 Default = 'private.cat'  in current directory.
; OUTPUTS:
;     bmv  - Catalog B-V color of object.
;     vmr  - Catalog V-R color of object.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;    93/10/12 - Written by Marc W. Buie, Lowell Observatory
;-
pro getcolor,stand,filler,bmv,vmr, $
          LAND1=landfile1, LAND2=landfile2, PRICAT=pricat

; First validation.
if n_params() ne 4 then begin
   print,'Usage: getcolor,stand,filler,bmv,vmr, [LAND1,LAND2,PRICAT]'
   return
endif

if badpar(stand,7,[0,1],caller='GETCOLOR: (stand) ',npts=n1) then return
if badpar(filler,[4,5],1,caller='GETCOLOR: (filler) ',npts=n2) then return

if n2 ne 2 then begin
   print,'GETCOLOR: filler must contain two elements, B-V and V-R'
   return
endif

; Extract the first character of each object
type = strmid(stand,0,2)

;Set up output arrays
bmv = replicate(filler[0],n1)
vmr = replicate(filler[1],n1)

;Find all three types of catalog objects
land1 = where(type eq 'SL',nland1)
land2 = where(type eq 'SM',nland2)
priv  = where(type eq 'SX',npriv)

if nland1 ne 0 then begin
   codes = long(strmid(stand[land1],2,99))
   rdland,83,id,ra,dec,mags,colors,FILE=landfile1
   bmv[land1] = colors[1,codes]
   vmr[land1] = colors[2,codes]
endif

if nland2 ne 0 then begin
   codes = long(strmid(stand[land2],2,99))
   rdland,92,id,ra,dec,mags,colors,FILE=landfile2
   bmv[land2] = colors[1,codes]
   vmr[land2] = colors[2,codes]
endif

if npriv ne 0 then begin
   codes = long(strmid(stand[priv],2,99))
   rdpricat,id,name,ra,dec,mags,FILE=pricat
   ; convert id codes into indexing pointers
   for i=0,n_elements(codes)-1 do codes[i] = where(codes[i] eq id)
   bmv[priv] = mags[0,codes] - mags[1,codes]
   vmr[priv] = mags[1,codes] - mags[2,codes]
endif

end

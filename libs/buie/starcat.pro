;+
; NAME:
;     starcat
; PURPOSE: (one line)
;     Retreive coordinates from the star catalogs.
; DESCRIPTION:
; CATEGORY:
;     Astronomy
; CALLING SEQUENCE:
;     starcat,object,ra,dec
; INPUTS:
;     object - String (array or scalar) of a standard Star name
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     EPOCH   - JD of epoch to correct for proper motion to.
;                 Default = catalog epoch.  (Scalar or vector).
;     LAND1   - Optional file name for Landolt 1983 Standards Catalog
;     LAND2   - Optional file name for Landolt 1992 Standards Catalog
;     PRICAT  - Name of private star catalog file for X star catalog.
;                 Default = 'private.cat'  in current directory.
;
;     Only one of the following is allowed to be set.  If none are set,
;        the coordinates will be returned in the equinox of the catalog.
;     B1950   - Coordinates should be referred to equinox of B1950.
;     J2000   - Coordinates should be referred to equinox of J2000.
;     OFDATE  - Coordinates should be referred to equinox of date.
;                 If selected, EPOCH must be provided.
;
; OUTPUTS:
;     ra  - Right ascension in radians for object (scalar or vector).
;     dec - Declination in radians for object (scalar or vector).
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  97/06/21, Written by Marc W. Buie, Lowell Observatory
;  2000/11/08, MWB, removed use of obsolete ()
;-
pro starcat,object,ra,dec, $
             EPOCH=in_epoch, LAND1=landfile1, LAND2=landfile2, PRICAT=pricat, $
             B1950=B1950, J2000=J2000, OFDATE=OFDATE

;on_error,1

; First validation.
if n_params() ne 3 then begin
   print,'Usage: starcat,object,ra,dec'
   return
endif

if badpar(object,7,[0,1],caller='STARCAT: (object) ',npts=nobj) then return
if badpar(in_epoch,[0,2,3,4,5],[0,1],CALLER='STARCAT: (epoch) ',npts=n_epoch) then return
if badpar(pricat,[0,7],0,CALLER='STARCAT: (pricat) ',default='private.cat') then return
if badpar(B1950,[0,1,2,3],0,CALLER='STARCAT: (B1950) ') then return
if badpar(J2000,[0,1,2,3],0,CALLER='STARCAT: (J2000) ') then return
if badpar(OFDATE,[0,1,2,3],0,CALLER='STARCAT: (OFDATE) ') then return

if keyword_set(OFDATE) and not keyword_set(in_epoch) then $
   message,'Error. Epoch must be provided with OFDATE request.'

if keyword_set(in_epoch) and (n_epoch ne nobj and n_epoch ne 1) then $
   message,'Error. Non-scalar epoch must match length of object list.'

if not exists(pricat) then begin
   print,'STARCAT: Private star catalog ',pricat,' could not be found.  Aborting.'
   return
endif

if keyword_set(OFDATE) then begin
   if n_epoch eq 1 then $
      epoch = replicate(in_epoch,nobj) $
   else $
      epoch = in_epoch
endif

; Extract the first character of each object, and make sure all are S
type = strmid(object,0,1)
z=where(type ne 'S',count)
if count ne 0 then begin
   print,'STARCAT: Error, non star codes entered.'
   print,'The first one is ',object[z[0]]
   return
endif

;Initialize the output arrays
ra  = replicate(-1.0d0,nobj)
dec = replicate(-99.0d0,nobj)

; Extract the second character of each object, this identifies the catalog
type = strmid(object,1,1)

;Separate the list into pointers for each catalog
land1 = where(type eq 'L',count_land1)
land2 = where(type eq 'M',count_land2)
priv  = where(type eq 'X',count_priv)

if count_land1 ne 0 then begin
print,count_land1,' references to stars from the 1983 Landolt catalog found.'
   codes = long(strmid(object[land1],2,99))
   if keyword_set(OFDATE) then begin
      rdland,83,id,ra_l,dec_l,file=landfile1
      rad  = ra_l  * 180.0d0 / !dpi
      decd = dec_l * 180.0d0 / !dpi
      jd2year,epoch[land1],year
      for i=0,n_elements(codes)-1 do begin
         new_ra  = rad[codes[i]]
         new_dec = decd[codes[i]]
         precess,new_ra,new_dec,1985.0,year[i]
         ra[land1[i]]   = new_ra  / 180.0d0 * !dpi
         dec[land1[i]]  = new_dec / 180.0d0 * !dpi
      endfor
   endif else if keyword_set(B1950) then begin
      rdland,83,id,ra_l,dec_l,file=landfile1,/B1950
      ra[land1]  = ra_l[codes]
      dec[land1] = dec_l[codes]
   endif else begin
      rdland,83,id,ra_l,dec_l,file=landfile1,/J2000
      ra[land1]  = ra_l[codes]
      dec[land1] = dec_l[codes]
   endelse
endif

if count_land2 ne 0 then begin
print,count_land2,' references to stars from the 1992 Landolt catalog found.'
   codes = long(strmid(object[land2],2,99))
   if keyword_set(OFDATE) then begin
      rdland,92,id,ra_l,dec_l,file=landfile2
      rad  = ra_l  * 180.0d0 / !dpi
      decd = dec_l * 180.0d0 / !dpi
      jd2year,epoch[land2],year
      for i=0,n_elements(codes)-1 do begin
         bprecess, rad[codes[i]], decd[codes[i]], new_ra, new_dec
         precess,new_ra,new_dec,1950.0,year[i]
         ra[land2[i]]  = new_ra  / 180.0d0 * !dpi
         dec[land2[i]] = new_dec / 180.0d0 * !dpi
      endfor
   endif else if keyword_set(B1950) then begin
      rdland,92,id,ra_l,dec_l,file=landfile2,/B1950
      ra[land2] = ra_l[codes]
      dec[land2] = dec_l[codes]
   endif else begin
      rdland,92,id,ra_l,dec_l,file=landfile2,/J2000
      ra[land2] = ra_l[codes]
      dec[land2] = dec_l[codes]
   endelse
endif

if count_priv ne 0 then begin
print,count_priv,' references to stars from a private catalog found.'
   codes = long(strmid(object[priv],2,99))


   if keyword_set(J2000) then begin
      f_epoch = 2451544.5d0
      rdpricat,id,name,ra_l,dec_l,file=pricat, $
                  DRA=dra_l,DDEC=ddec_l,/J2000,epoch=f_epoch
   endif else begin
      f_epoch = 2433282.5d0
      rdpricat,id,name,ra_l,dec_l,file=pricat, $
                  DRA=dra_l,DDEC=ddec_l,/B1950,epoch=f_epoch
   endelse

   ; Convert proper motion vectors to radians per century.
   dra  = dra_l  /  43200.0d0 * !dpi
   ddec = ddec_l / 648000.0d0 * !dpi

   ; convert id codes into indexing pointers
   for i=0,n_elements(codes)-1 do codes[i] = where(codes[i] eq id)
   o_ra = ra_l[codes]
   o_dec = dec_l[codes]

   ; correct for proper motion
   if keyword_set(in_epoch) then begin
      dt  = (epoch[priv] - f_epoch) / 36525.0d ; time difference in centuries.
      o_ra  = o_ra  + dra[codes]  * dt
      o_dec = o_dec + ddec[codes] * dt
   endif

   ; precess to epoch if needed.
   if keyword_set(OFDATE) then begin
      o_ra  = o_ra  * 180.0d0 / !dpi
      o_dec = o_dec * 180.0d0 / !dpi
      jd2year,epoch[priv],year
      for i=0,count_priv-1 do begin
         new_ra   = o_ra[i]
         new_dec  = o_dec[i]
         precess,new_ra,new_dec,1950.0,year[i]
         o_ra[i]  = new_ra
         o_dec[i] = new_dec
      endfor
      o_ra  = o_ra  / 180.0d0 * !dpi
      o_dec = o_dec / 180.0d0 * !dpi
   endif

   ; copy to output vectors
   ra[priv]  = o_ra
   dec[priv] = o_dec
endif

z=where(ra lt 0.0d0,count)
if count ne 0 then $
   print,'STARCAT: Warning, ',count,' star codes are not in recognized catalogs.'

end

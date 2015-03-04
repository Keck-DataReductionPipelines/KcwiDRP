;+
; NAME:
;  sc_pstar
;
; PURPOSE:
;  Search a star catalog for an isolated star near a given position.
;
; DESCRIPTION:
;  Search a star catalog for a reference star that is within a given
;  arc (RADIUS) from a point in the sky and has no brighter stars nearby within
;  a given distance (SUB_RADIUS).
;
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  sc_pstar,cra,cdec,radius,id,ra,dec,dra,ddec,mag
; 
; INPUTS:
;  cra    - ra of search location  (radians)
;  cdec   - dec of search location  (radians)
;  radius - maximum disance from cra/cdec to look  (radians)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  MAG_MIN    - lower bound of star magnitude to return (default= 12.0)
;  MAG_MAX    - upper bound of star magnitude to return (default= -3.0)
;  SCLASS     - Spectral classification, as a regexp (default=all types)
;                   (eg. [ab]0 for a0 and b0 class)
;  DBNAME     - Name of DB to access (default='obs')
;  TBLNAME    - Name of table to query in the DB (default='ppm_catalog')
;  SUB_RADIUS - region around reference star where no brighter star is
;                 allowed (radians, default=5 arcmin).
;
; OUTPUTS: Returns a suitable reference star
;  id   - name of star
;  ra   - ra  (radians)
;  dec  - dec  (radians)
;  dra  - ra proper motion
;  ddec - dec proper motion
;  mag  - magnitude of star
;  
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  02/02/19, Writen by David Tucker
;-

pro sc_pstar, ra, dec, radius,$
     ret_id, ret_ra, ret_dec, ret_dra, ret_ddec, ret_mag, $
     MAG_MIN=mag_min, MAG_MAX=mag_max, SCLASS=sclass, $
     DBNAME=dbname, TBLNAME=tblname, SUB_RADIUS=sub_radius

   if badpar(ra,[4,5],0,caller='sc_pstar (ra) ') then return
   if badpar(dec,[4,5],0,caller='sc_pstar (dec) ') then return
   if badpar(radius,[4,5],0,caller='sc_pstar (radius) ') then return

   ; check to see that the parameters are valid
   if badpar(dbname,[0,7],0,caller='sc_pstar:(DBNAME) ', $
            default='obs') then return
   ;indicate waht table to query in the db
   if badpar(tblname,[0,7],0,caller='sc_pstar:(TBLNAME) ', $
            default='ppm_catalog') then return
   ;Area next to a refrence star that needs to be clear
   if badpar(sub_radius,[0,1,2,3,4,5,6],0,caller='sc_pstar:(SUB_RADIUS) ', $
            default=5.0/60.0/!radeg) then return

   ;open the db
   openmysql, db, dbname

   ;query mysql db for bounding box
   sc_nearest, db, ra, dec, radius, $
      t_id, t_ra, t_dec, t_dra, t_ddec, t_mag, $
      MAG_MIN=mag_min, MAG_MAX=mag_max, SCLASS=sclass, $
      TBLNAME=tblname

   ; this will flag an empty return
   ret_id = -1

   ;query mysql db for star gte mag within n deg
   for i=0, n_elements(t_ra)-1 do begin
      sc_bcheck, db, t_ra[i], t_dec[i], sub_radius, t_mag[i], count, $
         TBLNAME=tblname
      if count le 1 then begin
         ret_id   = t_id[i]
         ret_ra   = t_ra[i]
         ret_dec  = t_dec[i]
         ret_dra  = t_dra[i]
         ret_ddec = t_ddec[i]
         ret_mag  = t_mag[i]
         break
      endif
   endfor

;what do we do if we dont find anything?

   ;close the db
   free_lun, db

end

;+
; NAME:
;  sc_nearest
;
; PURPOSE:
;  Find the nearest N stars from a point in a star catalog.
;
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  sc_nearest, db, ra, dec, radius, id, ra, dec, dra, ddec, mag
;
; INPUTS:
;  db     - handle to an open mysql ddatabase
;  ra     - ra of serach location (radians)
;  dec    - dec of serach location (radians)
;  radius - maximum disance from ra/dec to look (radians)
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MAG_MIN - lower bound of star magnatude to return (eg. 7.5)
;  MAG_MAX - upper bound of star magnatude to return (eg. 8.0)
;  SCLASS - Spectral classification, as a regexp
;             example: {AB}0 for B0 and B0 class
;                      {GK}.* for any G or K class star
;  MAX_COUNT - Maximum number of stars to return, defaults to 10
;  TBLNAME - Name of table to query in the DB
;
; OUTPUTS: Returns a series of arrays with up to n stars
;  id   - name of star
;  ra   - ra
;  dec  - dec
;  dra  - delta ra
;  ddec - delta dec
;  mag  - magnatude of stars
;  sc   - Spectral type of star
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  02/02/19, Writen by David Tucker
;  2004/03/30, Marc W. Buie, Lowell Observatory, added spectral class return
;-
pro sc_nearest, db, ra, dec, radius, $
    ret_id, ret_ra, ret_dec, ret_dra, ret_ddec, ret_mag, ret_sc, $
    MAG_MIN=mag_min, MAG_MAX=mag_max, SCLASS=sclass, $
    TBLNAME=tblname, MAX_COUNT=max_count

   if badpar(max_count,[0,1,2,3],0,CALLER='sc_nearest (MAX_COUNT) ',$
      default=10) then return
 
   ;query mysql db for bounding box
   sc_region, db, ra, dec, radius, tmptbl, $
      MAG_MIN=mag_min, MAG_MAX=mag_max, SCLASS=sclass, $
      TBLNAME=tblname

   ;query database
   query = 'SELECT id, ra, decl, dra, ddecl, mag, sc FROM '+$
      tmptbl+' ORDER by tangsep ASC '+$
      'LIMIT '+strn(max_count)+';'
   mysqlcmd, db, query, results, rcount

   if rcount gt 1 then begin

      ;parse data and return to caller
      results  = results[1:*]
      ret_id   = make_array(n_elements(results),/LONG)
      ret_ra   = make_array(n_elements(results),/DOUBLE)
      ret_dec  = make_array(n_elements(results),/DOUBLE)
      ret_dra  = make_array(n_elements(results),/DOUBLE)
      ret_ddec = make_array(n_elements(results),/DOUBLE)
      ret_mag  = make_array(n_elements(results),/DOUBLE)
      ret_sc   = make_array(n_elements(results),/STRING)

      for i=0, n_elements(results)-1 do begin
         words=strsplit(strcompress(results[i]),' ',/extract)
         ret_id[i]   = long(words[0])
         ret_ra[i]   = double(words[1])
         ret_dec[i]  = double(words[2])
         ret_dra[i]  = double(words[3])
         ret_ddec[i] = double(words[4])
         ret_mag[i]  = double(words[5])
         if n_elements(words) eq 7 then ret_sc[i]  = words[6]
      endfor

   endif else begin
      ret_id = -1L
   endelse

   ;close the temporary table
   query = 'DROP TABLE '+tmptbl+';'
   mysqlcmd, db, query, results, rcount

end


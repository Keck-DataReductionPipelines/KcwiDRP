;+
; NAME:
;  sc_bcheck
;
; PURPOSE:
;  Search a star catalog for the number of stars near a given position
;
; DESCRIPTION:
;  search the ppm catalog for the number of stars equal or brighter than
;  a given threshold within a fixed distance to a region of the sky.
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  sc_bcheck, db, ra, dec, radius, mag_min, count
;
; INPUTS:
;  db - handle to an open mysql ddatabase
;  ra - ra of serach location
;  dec - dec of serach location
;  radius - maximum disance from ra/dec to look
;  mag_min - faint bound of object magnatude (count everything brighter)
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  TBLNAME - Name of table to query in the DB
;
; OUTPUTS:
;  count - the total number of stars in the region gte mag_min
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  02/02/19, Writen by David Tucker
;  2004/03/30, Marc W. Buie, Lowell Observatory, fix sense of mag check
;-
pro sc_bcheck, db, ra, dec, radius, mag_min, count, $
    TBLNAME=tblname

   ;query mysql db for bounding box
   sc_region, db, ra, dec, radius, tmptbl, $
      TBLNAME=tblname

   count = 0
   ;query database
   query = 'SELECT COUNT(mag<='+string(mag_min, format='(E0)')+$
      ' AND tangsep<='+string(radius, format='(E0)')+')'+$
      ' FROM '+tmptbl+' ORDER by tangsep ASC ;'
   mysqlcmd, db, query, results, rcount

   if n_elements(results) gt 1 then begin
      count = results[1]
   endif

   ;close the temporary table
   query = 'DROP TABLE '+tmptbl+';'
   mysqlcmd, db, query, results, rcount

end

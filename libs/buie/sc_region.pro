;+
; NAME:
;  sc_region
;
; PURPOSE:
;  Extract a region of a star catalog about some location.
;
; DESCRIPTION:
;  Generates a temporary table in the database of stars that are in a
;  region of the sky centered on the input coordinates.  Also generates a new
;  field tangsep in the table that is the distance in radians from the
;  point specified by ra/dec to the object in the given record.
;
; CATEGORY:
;  Database
;
; CALLING SEQUENCE:
;  sc_region, db, ra, dec, radius, tmptbl
;
; INPUTS:
;  db     - handle to an open mysql ddatabase
;  ra     - ra of serach location  (radians)
;  dec    - dec of serach location  (radians)
;  radius - maximum disance from ra/dec to look (radians)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  MAG_MIN - faint limit of star magnitude to return (eg. 12.0)
;  MAG_MAX - bright limit of star magnitude to return (eg. -3.0)
;  SCLASS  - Spectral classification, as a regexp
;               (eg. [ab]0 for a0 and b0 class)
;               default is to return all types.
;  TBLNAME - Name of table to query in the DB, default='ppm_catalog'
;
; OUTPUTS:
;  tmptbl - name of temporary table that was created  
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  Send 'DROP TABLE '+tmptbl+';' to mysql to close the table when you
;   are done with it.  The temporary table will disappear once the handle 
;   to the database is droped.
; MODIFICATION HISTORY:
;  02/02/07, Writen by David Tucker
;  02/02/19, expanded to sc_region and sc_nearest
;-

pro sc_region, db, ra, dec, radius, tmptbl, $
       MAG_MIN=mag_min, MAG_MAX=mag_max, SCLASS=sclass, TBLNAME=tblname

   ; check to see that the parameters are valid
   if badpar(tblname,[0,7],0,caller='ppmcat:(TBLNAME)', $
            default='ppm_catalog') then return

   if badpar(mag_max,[0,1,2,3,4,5,6,9],0,caller='ppmcat:(MAG_MAX)', $
            default=-3.0) then return
   if badpar(mag_min,[0,1,2,3,4,5,6,9],0,caller='ppmcat:(MAG_MIN)', $
            default=12.0) then return
   if badpar(sclass,[0,7],0,caller='ppmcat:(SCLASS)', $
            default="") then return

   if mag_max gt mag_min then begin
      tmpval  = mag_min
      mag_min = mag_max
      mag_max = tmpval
   endif

   ;defines
   PI_2 = !dpi/2.0d0
   TWOPI = !dpi*2.0d0

   ;find bounding box for search
   decmax = dec+radius
   decmin = dec-radius

   if decmax GE PI_2 then begin
      decmax = PI_2
      ramin = 0.0d0
      ramax = TWOPI
   endif else begin
      if decmin LE -PI_2 then begin
         decmin = -PI_2
         ramin = 0.0d0
         ramax = TWOPI
      endif else begin
         maxdec = ABS(decmin)
         if ABS(decmax) GT maxdec then begin
            maxdec = ABS(decmax)
         endif
;*** FixMe, ppmcat, 6.0, -1.0, 0.5 chokes here
; problem is that (sin(radius)/cos(maxdec)) is to large for asin()
; but how do we wrap it (=/-2pi) or is there something more fundamentaly wrong
         delra = ASIN(SIN(radius)/COS(maxdec))

         ramin = prival(ra - delra)
         ramax = prival(ra + delra)
      endelse
   endelse

   ;name our temporary table
   tmptbl = 'sc_region_temp'
   ;generate query
;fix for delta dec/delta ra, or not???

   ;place the results in a ne temporary table
   query = 'CREATE TEMPORARY TABLE '+tmptbl+$ 
            ' (tangsep DOUBLE)'+$ ;add in an angle seperation column 
            ' SELECT id, ra, decl, dra, ddecl, mag, sc FROM '+tblname+' WHERE'

   ;wrap at 2pi for ra
   if ramin LE ramax then begin
      query = query + ' ra BETWEEN '+STRING(ramin, ramax,$
         FORMAT='(E0," AND ", E0)')
   endif else begin
      query = query + ' (ra BETWEEN '+STRING(ramin, TWOPI,$
         FORMAT='(E0," AND ", E0)')+'OR ra BETWEEN '+$
         STRING(0.0d0, ramax, FORMAT='(E0," AND ", E0)')+')'
   endelse

   query = query + ' AND decl BETWEEN '+STRING(decmin, decmax,$
      FORMAT='(E0," AND ", E0)') +' AND mag BETWEEN '+$
      STRING(mag_max, mag_min, FORMAT='(E0," AND ", E0)')
   if sclass ne "" then begin
      query = query+' AND sc REGEXP "['+sclass+']"'
   endif
   query = query+';'
   mysqlcmd, db, query, results, rcount

   ;Build angle index
   sdec = SIN(dec)
   cdec = COS(dec)
   query = 'UPDATE '+tmptbl+' SET tangsep=ACOS( SIN(decl)*'+STRING(sdec,$
      FORMAT='(E0)')+'+COS(decl)*'+STRING(cdec, FORMAT='(E0)')+$
      '* COS(ra-'+STRING(ra, FORMAT='(E0)')+') );'
   mysqlcmd, db, query, results, rcount

end


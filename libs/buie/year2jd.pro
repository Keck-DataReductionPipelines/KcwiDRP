;+
; NAME:
;    year2jd
; PURPOSE: (one line)
;    Convert decimal year to Julian date (reverse of jd2year).
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    year2jd, year, jd
; INPUTS:
;    year   : Decimal year
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    jd     : Julian Date (double precision ONLY)
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Marc Buie, Lowell Observatory, 2008/02/03
;-
pro year2jd, year, jd

   if n_params() ne 2 then begin
      print, 'year2jd2, year, jd'
      return
   endif

   self='YEAR2JD: '
   if badpar( year, 5, [0,1], CALLER=self+'(year) ' ) then return

   year0 = long(year)
   jd0 = jdparse(strn(year0)+'/01/01 00:00:00')

   jd = (year-year0)*365.25d0 + jd0

end

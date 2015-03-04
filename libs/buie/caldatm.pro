;+
; NAME:
;       caldatm
; PURPOSE:
;       Find year, month, day, hour, minute, second from Julian Date.
; CATEGORY:
;       Astronomy
; CALLING SEQUENCE:
;       caldatm, jd, y, m, d, hour, min, sec
; INPUTS:
;       jd = Julian day number (like 2447000.5).   in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       y = year (like 1987).                      out
;       m = month number (like 7).                 out
;       d = day of month (like 23).                out
;       hour = hour of day (like 12).              out
;       min = minute of day (like 0).              out
;       sec = second of day (liek 0.0).            out
; COMMON BLOCKS:
; NOTES:
;  This routine is based on the formulas given in "Astronomical Formulae for
;    Calculators," 2nd. ed., by Jean Meeus on pages 23-29.  This algorithm
;    works for any date in either the Julian calendar (before 1582 October 4)
;    or the Gregorian calendar except for negative Julian Day numbers.
;
; MODIFICATION HISTORY:
;       M. Buie, 1991 Oct 10, Lowell Observatory
;       DWL, August 6, 1993, Modifications to operate with vector input.
;-

pro caldatm,jd,year,month,day,hour,minute,second, help=hlp

if (n_params(0) lt 7) or keyword_set(hlp) then begin
   print,' Find year, month, day, hour, minute, second from julian day number.'
   print,' caldatm, jd, y, m, d, hour, minute, second'
   print,'   jd = Julian day number (like 2447000).     in'
   print,'   y = year (like 1987).                      out'
   print,'   m = month number (like 7).                 out'
   print,'   d = day of month (like 23).                out'
   print,'   hour = hour of day (like 12).              out'
   print,'   min = minute of day (like 0).              out'
   print,'   sec = second of day (like 0.0).            out'
   return
endif

z = long(jd + 0.5)
a = z

j = WHERE( z GE 2299161L, countj )
if countj ne 0 then begin
   alpha = long( ( double(z[j]) - 1867216.25d0 ) / 36524.25d0 )
   a[j] = z[j] + 1 + alpha - long( double(alpha) / 4.0d0 )
endif

b = a + 1524
c = long( ( double(b) - 122.1d0 ) / 365.25d0)
d = long( 365.25d0 * double(c) )
e = long( double(b-d) / 30.6001d0 )

year  = c
month = e
day = fix(b - d - long( 30.6001d0 * double(e) ))

i = where( e le 13, counti )
j = where( e gt 13, countj )
if counti ne 0 then month[i] = e[i] - 1
if countj ne 0 then month[j] = e[j] - 13
;if e le 13 then month = e - 1  else  month = e - 13

i = where( month ge 3, counti )
j = where( month lt 3, countj )
if counti ne 0 then year[i] = c[i] - 4716
if countj ne 0 then year[j] = c[j] - 4715
;if month ge 3 then year = c - 4716  else  year = c - 4715

year    = fix(year)
month   = fix(month)
frac    = (jd + 0.5d0 - z) * 24.0d0
hour    = fix(frac)
frac    = ( frac - hour ) * 60.0d0
minute  = fix(frac)
second  = ( frac - minute ) * 60.0d0

return
end

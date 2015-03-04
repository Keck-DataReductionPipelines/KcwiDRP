;+
; NAME:
;  etut
; PURPOSE:   (one line only)
;  Compute ET-UT time offset for a given Julian Date
; DESCRIPTION:
; CATEGORY:
; CALLING SEQUENCE:
;   ans = etut(jd)
; INPUTS:
;   jd - Julian Date (UTC), scalar or vector
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;   Return value which is the difference of ET-UT for the given JD.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   Needs to have access to the file, leapsec.dat.  This file is periodically
;     updated as new leap-seconds are announced.  This file is contained as
;     part of this library.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2010/12/02, Written by Marc W. Buie based on a routine provided by
;                Larry Wasserman which was in turn adapted from a routine
;                supplied to him by Myles Standish.
;-
function etut,jd,FILE=in_file

   self='ETUT: '
   if badpar(jd,[4,5],[0,1],caller=self+'(jd) ') then return,0
   if badpar(in_file,[0,7],0,caller=self+'(FILE) ', $
                             default='leapsec.dat') then return,0

   ;ET-UT for 1600 - 1820.5 from the Explanatory Supplement, pg. 90.
   dtz_yr=[1600.0, 1621.0, 1635.0, 1639.0, 1645.0, 1653.0, 1662.0, 1681.0, $
           1710.0, 1727.0, 1738.0, 1747.0, 1760.9, 1774.1, 1785.1, 1792.6, $
           1801.8, 1811.9, 1820.5]
   dtz=[100.00, 98.00, 38.00, -13.00, 13.00, -10.00, -5.00, -13.50, $
         12.00,  7.60,  2.90,  -0.40,  2.10,   6.60,  8.30,   7.40, $
          5.70,  4.70,  5.32]

   ;ET-UT for 1820.5 - 1948.5 from Brouwer: 1952, AJ, 57, pg. 132, Table 8a
   ;      for 1949.5 - 1963.5 from the 1994 Almanac
   dt=[5.32,4.80,4.32,3.88,3.49,3.15,2.85,2.59,2.37,2.20, $
       2.01,1.77,1.49,1.16,.79,.36,-.04,-.28,-.37,-.29, $
       -.06,.25,.56,.85,1.13,1.39,1.65,1.89,2.12,2.33, $
       2.53,2.72,2.90,3.06,3.22,3.35,3.46,3.50,3.51,3.43, $
       3.32,3.14,2.90,2.61,2.26,1.85,1.39,.87,.20,-.68, $
       -1.78,-3.09,-4.48,-5.65,-6.57,-7.24,-7.67,-7.87,-8.0,-8.09, $
       -8.14,-8.17,-8.17,-8.14,-8.07,-7.97,-7.84,-7.67,-7.58,-7.58, $
       -7.67,-7.85,-8.04,-8.07,-7.93,-7.63,-7.19,-6.57,-5.8,-4.87, $
       -3.79,-2.54,-1.13,.35,1.8,3.26,4.69,6.11,7.51,8.9, $
       10.28,11.64,12.95,14.18,15.31,16.39,17.37,18.27,19.08,19.83, $
       20.48,21.06,21.56,21.97,22.29,22.55,22.72,22.82,22.92,23.05, $
       23.18,23.34,23.50,23.60,23.64,23.63,23.58,23.63,23.76,23.99, $
       24.30,24.71,25.15,25.61,26.08,26.57,27.08,27.61,28.15, $
       28.93,29.36,29.77,30.17,30.54,30.90,31.21,31.52,31.93, $
       32.43,32.92,33.37,33.80,34.24,34.75]
   dt_yr=findgen(n_elements(dt))+1820.5

   ; Load the modern leap second data
   if not exists(in_file) then begin
      file=find_with_def(in_file,!path)
      if file eq '' then begin
         print,self+'File '+in_file+' does not exist, cannot continue.'
         return,0.0
      endif
   endif else begin
      file = in_file
   endelse
   readcol,file,yr,mn,dy,delta,format='a,a,a,f'
   date=jdparse(yr+'-'+mn+'-'+dy)

   ; pre-allocate return value
   etut=jd*0.0

   ;Decimal calendar year
   jd2year,jd,yr

   ;Not defined before 1600
   z=where(yr lt 1600.0,count)
   if count ne 0 then begin
      etut[z] = 1.0/0.0 ; give back something obviously useless
   endif

   ; 1600 - 1820 from table in dtz
   z=where(yr ge 1600.0 and yr lt 1820.5,count)
   if count ne 0 then begin
      interp,dtz_yr,dtz,yr[z],etut0
      etut[z]=etut0
   endif

   ; 1820 to 1963/1/1 from table in DT
   z=where(yr ge 1820.5 and jd lt date[0],count)
   if count ne 0 then begin
      interp,dt_yr,dt,yr[z],etut0
      etut[z]=etut0
   endif

   ; 1963/1/1 to 1972/1/1, interpolate
   z=where(jd ge date[0] and jd lt date[12],count)
   if count ne 0 then begin
      interp,date,delta,jd[z],etut0
      etut[z]=etut0+32.184
   endif

   z=where(jd ge date[12] and jd lt date[n_elements(date)-1],count)
   if count ne 0 then begin
      interp,date,delta,jd[z],etut0
      etut[z]=fix(etut0)+32.184
   endif

   z=where(jd ge date[n_elements(date)-1],count)
   if count ne 0 then begin
      etut[z]=delta[n_elements(date)-1]+32.184
   endif

   return,etut

end

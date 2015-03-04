;+
; NAME:
;  showelem
; PURPOSE:
;  Print out current osculating orbital elements for a solar system object.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  showelem,obj
; INPUTS:
;  obj - Standard object code (see EPHEM).  This will not work for planets
;          or satellites.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  1998 Dec 17, Written by Marc W. Buie, Lowell Observatory
;  2001/04/18, MWB, changed systime call.
;  2001/04/20, MWB, changed to support new version of geteph/ephem
;-
pro showelem,object,jd,DEBUG=debug,EDB=edb

   if badpar(object,7,0,caller='SHOWELEM: (object) ') then return
   if badpar(edb,[0,1,2,3],0,caller='SHOWELEM: (EDB) ',default=0) then return

   curjd   = systime(/julian,/utc)

   if badpar(jd,[0,2,3,4,5],[0,1],caller='SHOWELEM: (jd) ',default=curjd) then return

   if n_elements(jd) eq 6 then begin
      jdcnv,jd[0],jd[1],jd[2],jd[3]+jd[4]/60.0+jd[5]/3600.0,curjd
      timesrc='Manual Time Input'
   endif else begin
      if jd eq curjd then $
         timesrc='Auto/Current Time' $
      else $
         timesrc='Manual Time Input'
      curjd = jd
   endelse

   jdstr,curjd,0,curtimestr

   ephem,curjd,500,61,object,elem,DEBUG=debug
   jdstr,elem[14],100,laststr

   if elem[15] gt 365.0 then begin
      arc = elem[15] / 365.25
      arcunits = ' years'
   endif else if elem[15] gt 49.0 then begin
      arc = elem[15] / 30.5
      arcunits = ' months'
   endif else if elem[15] gt 12.0 then begin
      arc = elem[15] / 7.0
      arcunits = ' weeks'
   endif else begin
      arc = elem[15]
      arcunits = ' days'
   endelse

   as='"'
   fmt1='(4x,a,2x,f11.6,1x,a)'
   fmt2='(4x,a,2x,a)'
   fmt3='(4x,a,2x,f6.1,a)'
   fmt4='(4x,a,2x,i6,a)'

   if min(elem) ne 0.0 or max(elem) ne 0.0 then begin
      print,''
      print,'---------------------------------------------------------------------'
      print,'  Osculating J2000 orbital elements for ', $
                strmid(strupcase(object),1,99),'   (',timesrc,')'
      print,'Mean anomaly                    [M]',elem[0],'deg',format=fmt1
      print,'Argument of perihelion          [w]',elem[1],'deg',format=fmt1
      print,'Longitude of the ascending node [O]',elem[2],'deg',format=fmt1
      print,'Inclination                     [i]',elem[3],'deg',format=fmt1
      print,'Eccentricity                    [e]',elem[4],'deg',format=fmt1
      print,'Semi-major axis                 [a]',elem[5],'AU',format=fmt1
      print,'Perihelion distance             [q]',elem[6],'AU',format=fmt1
      print,'Aphelion distance               [Q]',elem[7],'AU',format=fmt1
      print,'Orbital period                  [P]',elem[8],'Years',format=fmt1
      print,'Epoch of elements                  ',curtimestr,format=fmt2
      if elem[12] ge 0.0 then begin
         print,''
         print,'Current ephemeris error  a=',elem[12],as,format=fmt3
         print,'                         b=',elem[13],as,format=fmt3
         print,'Date last measured         ',laststr,format=fmt2
         print,'Arc length                 ',arc,arcunits,format=fmt3
         print,'Number of observations     ',long(elem[16]),format=fmt4
      endif
      print,'---------------------------------------------------------------------'
      if edb then begin
         tmp=fix(strmid(curtimestr,8,2))+raparse(strmid(curtimestr,11,99))/(2.0*!pi)
         print,strmid(object,1,99),',e,', $
            strcompress(elem[3],/remove_all),',', $
            strcompress(elem[2],/remove_all),',', $
            strcompress(elem[1],/remove_all),',', $
            strcompress(elem[5],/remove_all),',', $
            ',', $
            strcompress(elem[4],/remove_all),',', $
            strcompress(elem[0],/remove_all),',', $
            strmid(curtimestr,5,2),'/', $
            strcompress(tmp,/remove_all),'/', $
            strmid(curtimestr,0,4),',', $
            '2000,H1,.1'
         print,'---------------------------------------------------------------------'
      endif
      print,''
   endif else begin
      print,''
      print,'Object ',strmid(strupcase(object),1,99), $
            ' is not in the orbital element database.'
      print,''
   endelse

end

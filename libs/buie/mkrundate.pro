;+
; NAME:
;  mkrundate
; PURPOSE:   (one line only)
;  Create a run date string given a Julian Date.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  rundate = mkrundate(jd)
; INPUTS:
;  jd - Julian Date (scalar)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is a 6-digit string that is the run date for the input JD
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2008/06/22, Written by Marc W. Buie, Southwest Research Institute
;-
function mkrundate,jd

   self='rundate: '
   if badpar(jd,[4,5],0,caller=self+'(jd) ') then return,''

   jdstr,jd,300,str
   return,strmid(str,2,6)
   
end

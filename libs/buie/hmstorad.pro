;+
; NAME:
;    hmstorad
; PURPOSE: (one line)
;    Convert from hours, minutes, and seconds of Right Ascension to radians.
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    hmstorad, hour, min, sec, radians
; INPUTS:
;    hour : Hour.    0   <= hour < 24.
;    min  : Minute.  0   <= min  < 60.
;    sec  : Second.  0.0 <= sec  < 60.0.
;    If more than one of these are vectors, they must be the same length.
;    A mixture of scalar and vector input parameters is equivalent to all three
; inputs being vectors: The scalar inputs are treated as replicated vectors.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    radians  : Converted angle in radians.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Copyright (C) 1987, by Marc W. Buie
;    Version dated 87/6/3
;    Ported by Doug Loucks, Lowell Observatory, August 12, 1993, from the
; C-Language version written by Marc Buie.
;   2005/08/23 - MWB, minor modification to ensure full double precision
;                      calculation regardless of input data type.
;-
pro hmstorad, hh, mm, ss, radians

   rad_per_hour = 0.26179938779914943654d0

   ; Check for correct number of parameters.
   if n_params() ne 4 then begin
      ; Display the calling sequence.
      print, 'hmstorad, hour, min, sec, radians'
      return
   endif

   self='HMSTORAD: '
   if badpar(hh,[2,3,4,5],[0,1],CALLER=self+'(hh) ',NPTS=hh_size) then return
   if badpar(mm,[2,3,4,5],[0,1],CALLER=self+'(mm) ',NPTS=mm_size) then return
   if badpar(ss,[2,3,4,5],[0,1],CALLER=self+'(ss) ',NPTS=ss_size) then return

   check = [hh_size,mm_size,ss_size]
   z = where(check ne 1, count)
   if count ne 0 then begin
      if min(check[z]) ne max(check[z]) then begin
         print,self,'Vector input parameters must be the same length.'
         return
      endif
   endif

   i = where(hh lt 0 or hh ge 24,count)
   if count ne 0 then begin
      print,self,'Parameter out of range:  0 <= hour < 24.'
      return
   endif

   i = where(mm lt 0 or mm ge 60,count)
   if count ne 0 then begin
      print,self,'Parameter out of range:  0 <= min < 60.'
      return
   endif

   i = where(ss lt 0.0 or ss ge 60.0,count)
   if count ne 0 then begin
      print,self,'Parameter out of range.  0.0 <= sec < 60.0.'
      return
   endif

   radians = double(hh) + ( double(mm) + double(ss) / 60.0d0 ) / 60.0d0
   radians = radians * rad_per_hour

end

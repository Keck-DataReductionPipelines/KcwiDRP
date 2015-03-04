;+
; NAME:
;  newanom
; PURPOSE:   (one line only)
;  Compute new mean anomaly from old given date.
; DESCRIPTION:
; CATEGORY:
;  Celestial Mechanics
; CALLING SEQUENCE:
;  newanom,jdepoch,m,newjd,newm
; INPUTS:
;  m       - Mean anomaly (radians)
;  jdepoch - Julian date of mean anomaly
;  a       - Semi-major axis (AU)
;  newjd   - Julian date of mean anomaly desired
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  newm    - New mean anomaly (radians)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2002/05/29 - Marc W. Buie, Lowell Observatory.  Ported from fortran routines
;    supplied by Larry Wasserman and Ted Bowell.
;-
pro newanom,jdepoch,m,a,newjd,newm

   gk=0.01720209895d0

   dt = newjd-jdepoch

   mean_motion = gk*a^(-1.5d0)

   newm = m + dt*mean_motion

   newm = newm mod (2.0d0*!dpi)

   z=where(newm lt 0.0d0, count)
   if count ne 0 then newm[z]=newm[z]+2.0d0*!dpi

end

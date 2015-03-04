;+
; NAME:
;	fxtm
; PURPOSE: (one line)
;	Fix bad time codes from Anderson Mesa CCD software.
; DESCRIPTION:
;	This is a special purpose routine for fixing bad times that are
;	generated from using MGO with the Hendon CCD control software at
;	Anderson Mesa.  For a string of times, this is what it looks like:
;	Frame      Stored time      Actual time
;         n           t(n+1)          t(n)
;	  n+1         t(n+2)          t(n+1)
;	  n+2         t(n+3)          t(n+2)
;	  n+3         t(n+3)          t(n+3)
;	for a four frame sequence.  The times for n to n+2 are actually the
;	times for the next frame, the n+3 time is correct.   The time for
;	the first frame is lost.
;
;	This program copies the times that are out of place and computes
;	the first time by backing up from the n+1 frame by the mean of the
;	difference between all the other frames.
; CATEGORY:
;	CCD data processing
; CALLING SEQUENCE:
; INPUTS:
;	vec - Time vector to be repaired (MODIFIED).
;	i1  - First point in sequence within the vector.
;	i2  - Last point in sequence within the vector.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	vec - Time vector with i1 to i2 points repaired.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1992 Jan 28, by Marc W. Buie, Lowell Observatory
;-
pro fxtm,vec,i1,i2
   tmp1=vec[i1:i2-2]
   tmp2=vec[i1+1:i2-1]
   tdif = tmp2-tmp1
   avgtdif = mean(tdif)
   vec[i1+1:i2-1]=tmp1
   vec[i1] = vec[i1+1]-avgtdif
end

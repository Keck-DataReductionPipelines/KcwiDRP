;+
; NAME: 
;  imqual
; PURPOSE:
;  Generate a ``grade'' for image quality of input image.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  imqual,im,thresh,smofac,grade,usmim
; INPUTS:
;  im     - The input image, 2-D only thus far.
;  thresh - Fraction of peak in image that is considered for grading.
;  smofac - Smoothing factor for the unsharp masked image.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  grade  - Grade, 0 is bad, bigger is better.  Units are arbitrary
;  usmim  - The unsharp masked image.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/08/16 - Written by Marc W. Buie, Lowell Observatory.
;-
pro imqual,im,thresh,smofac,grade,usmim, $
      NOPRINT=noprint

; Determine the threshold value for test.  THRESH sets a level between
;   the image min and max.  Any pixels above tval will be used in the grade.
   nx = minmax(im)
   tval = (nx[1]-nx[0])*thresh+nx[0]
   z = where(im ge tval, count)

; Unsharp mask the image, first smooth then subtract.
   smim = smooth(im,smofac)
   usmim = 3.0*im-2.5*smim

   if count gt 1 then begin
      grade = stdev(usmim[z])
   endif else begin
      grade = 0.0
   endelse

   if not keyword_set(noprint) then $
      print,count,grade,grade/sqrt(count),stdev(im[z])

end

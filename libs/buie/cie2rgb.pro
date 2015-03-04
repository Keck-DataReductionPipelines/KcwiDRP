;+
; NAME:
;  cie2rgb
; PURPOSE:   (one line only)
;  Convert from CIE Chromaticity coordinates to RGB color-space values
; DESCRIPTION:
;  Conversion from CIE Chromaticity (D65) to sRGB according to
;   IEC 61966-2-1:1999. Note that I'm calling RGB the same as sRGB,
;   after converting to 0-255 byte range instead of (0.0,1.0)
;
;  Also, since I really don't care for gamma (contrast) I have implemented
;   a simpler form than found in the IEC standard so that I can change
;   gamma to any value and the calculation is easy to perform.  The standard
;   formula includes a piecewise continuous function that is logrithmic
;   everywhere except for very low brightness.  At low brightness it uses
;   a linear ramp down to zero.  Instead, I just use the following for gamma:
;
;        out  = in^(1.0/gamma)
;
;   when converting from bigy (Y) to RGB (before converting from 0-1 to 0-255)
;   The default in this program is a gamma=1.0 and this formula is bypassed.
;   The standard gamma in the CIE prescription is "approximately 2.2".
;   The difference between my implementation and the prescription is unlikely
;   to be seen in the midst of large variations in monitors and printers
;   at the very low intensity range (bottom few percent).
;
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  cie2rgb,x,y,bigy,r,g,b
; INPUTS:
;  x,y   - scalar, vector, or arrays of Chromaticity (see spec2xyz)
;            must all be the same length
;  bigy  - intensity
;    Note: white is x=0.3127, y=0.3290, bigy=Y=1.0
;    (z=0.3583 because x+y+z=1)
; OPTIONAL INPUT PARAMETERS:
;  GAMMA - default=1
; KEYWORD INPUT PARAMETERS:
;  D65        - Flag, if set the output will be based on the D65 illuminant
;                 Otherwise, it will be corrected for Illuminant C
;  NORMALIZED - Flag, if set the output will be normalized floating point
;                 values (0-1) for RGB rather than byte values
; OUTPUTS:
;  r,g,b - same size and rank as input, RGB byte values
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2009/01/05
;  2009/01/15, MWB, added NORMALIZED keyword
;  2009/01/16, MWB, added D65 keyword
;-
pro cie2rgb,x,y,bigy,r,g,b,GAMMA=gamma,NORMALIZED=normalized,D65=d65

   self='CIE2RGB'
   if badpar(x,[4,5],[0,1,2],caller=self+'(x) ') then return
   if badpar(y,[4,5],[0,1,2],caller=self+'(y) ') then return
   if badpar(bigy,[4,5],[0,1,2],caller=self+'(bigy) ') then return
   if badpar(gamma,[0,2,3,4,5],0,caller=self+'(GAMMA) ',default=1.0) then return
   if badpar(normalized,[0,1,2,3],0,caller=self+'(NORMALIZED) ',default=0) then return
   if badpar(d65,[0,1,2,3],0,caller=self+'(D65) ',default=0) then return

   z = 1.0 - x - y

   r =  3.2406*x - 1.5372*y - 0.4986*z
   g = -0.9689*x + 1.8758*y + 0.0415*z
   b =  0.0557*x - 0.2040*y + 1.0570*z

   r = r*bigy/0.3290
   g = g*bigy/0.3290
   b = b*bigy/0.3290

   if not d65 then begin
      r = r/1.01039
      g = g/0.93728
      b = b/1.05666
   endif

   if gamma ne 1.0 then begin
      r = ((r > 0.0) < 1.0)
      g = ((g > 0.0) < 1.0)
      b = ((b > 0.0) < 1.0)
      r = r^(1.0/gamma)
      g = g^(1.0/gamma)
      b = b^(1.0/gamma)
      ;z1=where(r le 0.0031308, count1, complement=z2, ncomplement=count2)
      ;if count1 ne 0 then r[z1] = r[z1]*12.92
      ;if count2 ne 0 then r[z2] = 1.055*r[z2]^(1.0/gamma)-0.055
      ;z1=where(g le 0.0031308, count1, complement=z2, ncomplement=count2)
      ;if count1 ne 0 then g[z1] = g[z1]*12.92
      ;if count2 ne 0 then g[z2] = 1.055*g[z2]^(1.0/gamma)-0.055
      ;z1=where(r le 0.0031308, count1, complement=z2, ncomplement=count2)
      ;if count1 ne 0 then b[z1] = b[z1]*12.92
      ;if count2 ne 0 then b[z2] = 1.055*b[z2]^(1.0/gamma)-0.055
   endif

   if not normalized then begin
      r = ((r > 0.0) < 1.0)
      g = ((g > 0.0) < 1.0)
      b = ((b > 0.0) < 1.0)
      r = byte((fix(r*255+0.5) > 0) < 255)
      g = byte((fix(g*255+0.5) > 0) < 255)
      b = byte((fix(b*255+0.5) > 0) < 255)
   endif

end

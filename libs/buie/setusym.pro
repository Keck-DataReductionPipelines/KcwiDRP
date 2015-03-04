;+
; NAME:
;  setusym
; PURPOSE: (one line)
;  Set the user defined symbol to one of many shapes.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  setusym,sym
; INPUTS:
;  sym - Symbol to load into the user-defined type.
;          1 - Circle
;          2 - Plus sign
;          3 - Asterisk
;          4 - Diamond
;          5 - Triangle, point up.
;          6 - Square
;          7 - X
;          8 - Star
;          9 - Triangle, point down.
;         10 - Rotated square (45 degrees)
;  Positive ==> filled shape, negative ==> hollow shape.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  Redefines the user defined type (psym=8)
; RESTRICTIONS:
; MODIFICATION HISTORY:
;   Written by Marc W. Buie, Lowell Observatory, 1992 Sep 21
;   98/02/19, MWB, added new symbols 2-9
;-
pro setusym,in_sym

if in_sym lt 0 then fill = 0 else fill = 1
sym=abs(in_sym)

case sym of
   1: begin  ; Circle
         i=findgen(21)/20.*2*!pi
         x=sin(i)
         y=cos(i)
      end
   2: begin  ; Plus
         x=[0.0, 0.0,0.0,1.0,-1.0]
         y=[1.0,-1.0,0.0,0.0, 0.0]
         fill=0
      end
   3: begin  ; Asterisk
         ang = [180.,0.,0.,120.,300.,0.,60.,240.]/!radeg
         r   = [1.,1.,0.,1.,1.,0.,1.,1.]
         x   = r * cos(ang)
         y   = r * sin(ang)
         fill=0
      end
   4: begin  ; Diamond
         x = [0.,0.6,0.,-0.6,0.]
         y = [1.,0.,-1.,0.,1.]
      end
   5: begin  ; Triangle, point up.
         ang = 2.0*!pi*findgen(4)/3.0 + !pi/2.0
         x   = cos(ang)
         y   = sin(ang)
      end
   6: begin  ; Square
         ang = 2.0*!pi*findgen(5)/4.0 + !pi/4.0
         x   = cos(ang)
         y   = sin(ang)
      end
   7: begin  ; X
         ang = [135.,315.,0.,45.,225.]/!radeg
         r   = [1.,1.,0.,1.,1.]
         x   = r * cos(ang)
         y   = r * sin(ang)
         fill=0
      end
   8: begin  ; star
         ang = 2.0*!pi*findgen(11)/10.0 + 0.1*!pi
         r = replicate(1.0,11)
         r[indgen(5)*2+1] = 0.4
         x   = r*cos(ang)
         y   = r*sin(ang)
      end
   9: begin  ; Triangle, point down.
         ang = 2.0*!pi*findgen(4)/3.0 + 3.0*!pi/2.0
         x   = cos(ang)
         y   = sin(ang)
      end
  10: begin  ; Rotated square (45 degrees)
         x = [0.,1.,0.,-1.,0.]
         y = [1.,0.,-1.,0.,1.]
      end
   else: begin ; Error
      print,'SETUSYM: Illegal plot symbol value: ',in_sym
      return
      end
   endcase

usersym,x,y,fill=fill

end

;+
; $Id: kcwi_parse_imsec.pro,v 1.1 2013/05/13 21:43:03 neill Exp $
;
; KCWI_PARSE_IMSEC - return a 4 element integer vector with the
;	standard image section limits parsed
;-
function kcwi_parse_imsec,secstr
;
; trim brackets
tmp = secstr
junk = gettok(tmp,'[')
sec = gettok(tmp,']')
;
ar = intarr(4)
ar[0] = fix(gettok(sec,':'))
ar[1] = fix(gettok(sec,','))
ar[2] = fix(gettok(sec,':'))
ar[3] = fix(sec)
;
return,ar
end

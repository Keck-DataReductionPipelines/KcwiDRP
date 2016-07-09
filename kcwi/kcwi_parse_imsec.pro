;+
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

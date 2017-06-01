function date2unix,dtstr
;+
; date2unix - parse DTSTR and convert to Unix seconds
;-
tmp = dtstr
yr = long(gettok(tmp,'-'))
mo = long(gettok(tmp,'-'))
dy = long(gettok(tmp,'T'))
hr = long(gettok(tmp,':'))
mn = long(gettok(tmp,':'))
sc = double(tmp)

return, (julday(mo,dy,yr,hr,mn,sc) - julday(1,1,1970,0,0,0.)) * 24.d0 * 60.d0 * 60.d0
end

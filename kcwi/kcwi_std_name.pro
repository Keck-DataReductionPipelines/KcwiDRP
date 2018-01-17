;+
; NAME:
;	KCWI_STD_NAME
;-
function kcwi_std_name,name
	sname = strlowcase(strtrim(name,2))
	;
	; Handle BD stars
	if strpos(name,'bd') ge 0 then begin
		cat = 'bd'
		c = strpos(sname,'bd')
		sname = strmid(sname,c+2)
		while stregex(sname,'[^0-9]') eq 0 do $
			sname = strmid(sname,1)
		d = stregex(sname,'[^0-9]')
		deg = strmid(sname,0,d)
		sname = strmid(sname,d)
		while stregex(sname,'[^0-9]') eq 0 do $
			sname = strmid(sname,1)
		num=''
		while stregex(sname,'[0-9]') eq 0 do begin
			num += strmid(sname,0,1)
			sname = strmid(sname,1)
		endwhile
		sname = cat+deg+'d'+num
	;
	; Remove spaces from others
	endif else begin
		sname = strcompress(sname,/remove)
	endelse
	return,sname
end

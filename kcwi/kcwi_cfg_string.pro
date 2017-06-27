function kcwi_cfg_string,kcfg,long=long,delim=delim
	nc = n_elements(kcfg)
	cstr = strarr(nc)
	if keyword_set(delim) then $
		del = ':' $
	else	del = ''
	for i=0,nc-1 do begin
		cstr[i] = strn(kcfg[i].xbinsize)+strn(kcfg[i].ybinsize) + del
		if keyword_set(delim) then $
			cstr[i] += strtrim(kcfg[i].bgratnam,2) + del $
		else	cstr[i] += string(strtrim(kcfg[i].bgratnam,2), $
							format='(a-4)') + del
		cstr[i] += strmid(kcfg[i].ifunam,0,1) + del
		if keyword_set(long) then begin
			cstr[i] += strtrim(kcfg[i].calpnam,2) + del
			if keyword_set(delim) then $
				udel = del $
			else	udel = ' '
			cstr[i] += strtrim(string(kcfg[i].callang, $
							format='(f9.1)'),2)+udel
		endif
		cstr[i] += strtrim(string(kcfg[i].bcwave,format='(f10.1)'),2)
	endfor
	return, cstr
end

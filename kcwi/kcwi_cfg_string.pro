pro kcwi_cfg_string,kcfg
	nc = n_elements(kcfg)
	cstr = strarr(nc)
	for i=0,nc-1 do $
		cstr[i] = strn(kcfg[i].xbinsize)+strn(kcfg[i].ybinsize) + $
		string(strtrim(kcfg[i].bgratnam,2),format='(a-4)') + $
		strmid(kcfg[i].ifunam,0,1) + $
		strtrim(string(kcfg[i].bcwave,format='(f10.1)'),2)
	return, cstr
end

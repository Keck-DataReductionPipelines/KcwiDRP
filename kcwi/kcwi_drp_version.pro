function kcwi_drp_version, short=short
	cd,current=cwd
	cd,!KCWI_DATA
	cd,'..'
	verstring = 'KCWI DERP Version: 1.1.0 REL 2018/06/11'
	spawn,'git describe --tags --long', gitver, errmsg
	errlen = total(strlen(errmsg))
	if errlen le 0 then begin
		if keyword_set(short) then begin
			verstring = gitver
		endif else begin
			verstring = 'KCWI DERP Version: '+gitver
			spawn,'git log -1 --format=%cd', gitdate, errmsg
			if strlen(errmsg) le 0 then $
				verstring += ' ' + gitdate
		endelse
	endif else print,errmsg
	cd,cwd
	return, verstring
end

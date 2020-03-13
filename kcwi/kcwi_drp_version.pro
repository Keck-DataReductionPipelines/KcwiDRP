function kcwi_drp_version, short=short
	cd,current=cwd
	cd,!KCWI_DATA
	cd,'..'
	verstring = 'KCWI DERP Version: 1.2.1 DEV 2020/02/19'
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
	endif else print,'Warning: not a git repo'
	cd,cwd
	return, verstring
end

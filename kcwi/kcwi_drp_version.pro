function kcwi_drp_version
	cd,current=cwd
	cd,!KCWI_DATA
	cd,'..'
	verstring = 'KCWI DERP Version: 1.0.2 DEV 2018/04/00'
	spawn,'git describe --tags --long', gitver, errmsg
	errlen = total(strlen(errmsg))
	if errlen le 0 then begin
		verstring = 'KCWI DERP Version: '+gitver
		spawn,'git log -1 --format=%cd', gitdate, errmsg
		if strlen(errmsg) le 0 then $
			verstring += ' ' + gitdate
	endif else print,errmsg
	cd,cwd
	return, verstring
end

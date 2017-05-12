function kcwi_drp_version
	cd,current=cwd
	cd,!KCWI_DATA
	cd,'..'
	verstring = 'KCWI DERP Version: 0.4.1 REL 2017/05/12'
	spawn,'git describe --tags --long', gitver, errmsg
	if strlen(errmsg) le 0 then begin
		verstring = 'KCWI DERP Version: '+gitver
		spawn,'git log -1 --format=%cd', gitdate, errmsg
		if strlen(errmsg) le 0 then $
			verstring += ' ' + gitdate
	endif
	cd,cwd
	return, verstring
end

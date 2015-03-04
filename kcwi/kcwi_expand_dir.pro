function kcwi_expand_dir,dir
	odir = dir
	if !VERSION.OS_FAMILY EQ 'unix' then $
		if strpos(dir,'~') GE 0 then odir = expand_tilde(dir)

	odir = file_expand_path(odir)

	return,odir + '/'
end

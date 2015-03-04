; $Id: kcwi_get_imname.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
function kcwi_get_imname,ppar,imgnum,tail, $
	nodir = nodir, calib = calib, raw = raw, reduced = reduced, $
	master = master, help = help
	;
	; setup
	pre = 'KCWI_GET_IMNAME'
	;
	; help request
	if n_params(0) lt 2 or keyword_set(help) then begin
		print,pre+': Info - Usage: full_filename = '+pre+'( KcwiPpar, ImgNum, [Tail], /CALIB, /INPUT, /OUTPUT, /MASTER)'
		return,''
	endif
	;
	; verify ppar
	if kcwi_verify_ppar(ppar,/init) ne 0 then begin
		print,pre+': ERROR - invalid or uninitialized ppar struct'
		return,''
	endif
	;
	; test imgnum
	if imgnum lt 0 then begin
		kcwi_print_info,ppar,pre,'invalid image number',/error
		return,''
	endif
	;
	; check tail
	if n_params(0) lt 3 then $
		tail = ''
	;
	; default is raw dir
	dir = ppar.rawdir
	if keyword_set(reduced) then $
		dir = ppar.reddir
	if keyword_set(calib) then $
		dir = ppar.caldir
	if keyword_set(nodir) then $
		dir = ''
	;
	; image number and root image name strings
	;
	; are we a master file? (mbias, mdark, mflat)
	if keyword_set(master) then begin
		if strlen(master) gt 0 then begin
			rutstr = master
		endif else begin
			kcwi_print_info,ppar,pre,'invalid master file root',/error
			return,''
		endelse
	;
	; regular image file
	endif else begin
		rutstr = ppar.froot
	endelse
	imgstr = string(imgnum,'(i0'+strn(ppar.fdigits)+')')
	;
	; construct file name
	file = dir + rutstr + imgstr + tail + '.fits'
	;
	; all done
	return,file
	
end

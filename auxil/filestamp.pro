pro filestamp,lfile,verbose=verbose,archdir=archdir
;+
; filestamp - append a date stamp to the end of the current file
;
; keywords:
;	verbose - print operation
;	archdir - move stamped file into this directory
;-
; extract filename parts
fdecomp,lfile,disk,idir,file,ext
;
; check for error condition
if strlen(file) le 0 and strlen(idir) le 0 then begin
	print,'FILESTAMP: Error - no file or directory to stamp, nothing done.'
	return
endif
;
; if file is empty then we are stamping the bottom-most directory
if strlen(file) le 0 then begin
	sta=strsplit(idir,'/',/extract,count=nd)
	file = sta[nd-1]
	idir = ''
	for i=0,nd-2 do idir = idir + sta[i] + '/'
endif
;
; find out where to put the new file if needed
; user-specified archive directory
if keyword_set(archdir) then begin
	if size(archdir,/type) eq 7 then $
		arch = archdir $
	else	arch = 'arch'
	adir = idir + arch
	if not file_test(adir,/directory) then $
			file_mkdir,adir
	adir = adir
; just use current directory
endif else begin
	while strmid(idir,strlen(idir)-1) eq '/' do $
		idir = strmid(idir,0,strlen(idir)-1)
	adir = idir
endelse
; make sure we trail a slash unless we are null
if strlen(adir) gt 0 then begin
	if strmid(adir,strlen(adir)-1) ne '/' then $
		adir = adir + '/'
endif
; does the filename in question exist already?
if file_test(lfile) then begin
	finfo=file_info(lfile)
	if strlen(ext) gt 0 then $
		mfile=adir+file+'.'+ext+'_'+timestr(finfo.ctime) $
	else	mfile=adir+file+'_'+timestr(finfo.ctime)
	if file_test(mfile) then begin
		if strlen(ext) gt 0 then $
			mfile=adir+file+'.'+ext+'_'+timestr(finfo.ctime,/time) $
		else	mfile=adir+file+'_'+timestr(finfo.ctime,/time)
	endif
	file_move,lfile,mfile
	if keyword_set(verbose) then $
		print,'Moved '+lfile+' to '+mfile
endif
;
return
end

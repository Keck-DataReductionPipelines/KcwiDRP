; $Id: kcwi_read_ppar.pro,v 1.3 2015/02/21 00:18:38 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_READ_PPAR
;
; PURPOSE:
;	This function reads the pipeline parameter structure from a 
;	pipeline parameter file.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	Result = KCWI_READ_PPAR( PPAR_FILENAME )
;
; INPUTS:
;	ppfname	- pipeline parameter file (written with KCWI_PPAR_WRITE)
;
; KEYWORDS:
;	VERBOSE - set this to get extra screen output
;
; RETURNS:
;	KCWI DRP parameter struct (as defined in kcwi_ppar__define.pro)
;
; PROCEDURE:
;	Reads in pipeline parameters from a file with keyword value
;	pairs, one to a line with comments.
;	These lines have the following structure:
;		keyword up to 15 chars long
;		1 space
;		value up to 15 chars long unless:
;			it's a string in which case it is unlimited
;			if the string does not fit in 15 chars then
;			the comment field appears on the following line
;		1 space
;		the characters '/ ' leading a comment field of any length
;
;		E.g.:
;
;		INITIALIZED                   1 / pipeline struct initialized?
;		PPFNAME             'pipe.ppar' / pipeline parameter set name
;		RAWROOT         '/Users/kcwi/drp/run/14/12/20'
;		/ root dir of raw data
;		GAIN1                   0.14500 / gain for amp 1 in electrons/DN
;
;	The details of the processing steps can be found in the KCWI DRP 
;	documentation and are briefly described in KCWI_PIPE.PRO.
;
; EXAMPLE:
;
;	This will read in the file 'm82.ppar' containing all the pipeline
;	parameters:
;
;	m82par = KCWI_READ_PPAR('m82.ppar')
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-APR-12	Initial version
;-
function kcwi_read_ppar,ppfname,verbose=verbose
;
; initialize
	pre = 'KCWI_READ_PPAR'
	version = repstr('$Revision: 1.3 $ $Date: 2015/02/21 00:18:38 $','$','')

	A = {kcwi_ppar}		; get blank parameter struct
	ppar = struct_init(A)	; initialize it
;
; get current directory
	cd,cur=cwd
	ppar.curdir = cwd + '/'
;
; use defaults, if name not passed in
	if n_params(0) lt 1 or n_elements(ppfname) le 0 then begin
		;
		; check default reduced directory first
		ppfname = ppar.reddir + ppar.ppfname
		if not file_test(ppfname,/read) then begin
			;
			; now check current directory
			ppfname = ppar.curdir + ppar.ppfname
			if not file_test(ppfname,/read) then begin
				kcwi_print_info,ppar,pre,'default ppar file not found',/error
				return,ppar
			endif
		endif
	endif
;
; check file
	fi = file_info(ppfname)
	if not fi.exists or not fi.read or not fi.regular then begin
		print,pre+': Error - file not accessible: ',ppfname
		return,ppar
	endif
	ppar.ppfname = ppfname
;
; open file
	openr,il,ppar.ppfname,/get_lun
;
; get parameter struct tags
	keys = tag_names(ppar)
;
; read file until done
	rec = ''
	while not eof(il) do begin
;
; read next record
		readf,il,rec
;
; skip comments
		if strmid(strtrim(rec,2),0,1) ne '/' then begin
;
; get input key index
			ikey = gettok(rec,' ')
			i = where(keys eq ikey, ni)
;
; make sure it is found and unambiguous
			if ni eq 1 then begin
				i = i[0]
;
; get value string
				vstr = gettok(rec,' ')
;
; what type does parameter need?
				type = size(ppar.(i),/type)
;
; print if requested
				if keyword_set(verbose) then begin
					if strlen(vstr) gt 15 then $
						print,ikey,vstr, $
							form='(a-15,1x,a)' $
					else	print,ikey,vstr,rec, $
						format='(a-15,1x,a15,1x,a)'
				endif
;
; convert to parameter type
				case type of
					1:	ppar.(i) = byte(vstr)
					2:	ppar.(i) = fix(vstr)
					3:	ppar.(i) = long(vstr)
					4:	ppar.(i) = float(vstr)
					5:	ppar.(i) = double(vstr)
					7:	begin
						str = strsplit(vstr,"'",/extrac)
						ppar.(i) = str
						end
				endcase
;
; handle input key/struct key mis-match
			endif else begin
				print,pre+ ': Error - illegal keyword: ',ikey
;
; warn that struc is not fully initialized
				ppar.initialized = 0
				return,ppar
			endelse
;
; skip comments
		endif else begin
			if keyword_set(verbose) then print,rec
		endelse
;
; read until end of file
	endwhile
;
; close up
	free_lun,il
;
; reset loglun tag
	ppar.loglun = -1
;
; return the fully initialized, timestamped struct
	ppar.initialized = 1
	ppar.timestamp = double(fi.mtime)	; use file timestamp
;
	return,ppar
end

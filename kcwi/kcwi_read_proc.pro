;
; Copyright (c) 2017, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_READ_PROC
;
; PURPOSE:
;	This function reads the processing control from a proc file.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_READ_PROC, Ppar, ProcFilename, ImgNums
;
; INPUTS:
;	Ppar		- KCWI_PPAR pipeline parameter struct
;	ProcFilename	- proc file (written with KCWI_PREP)
;
; OUTPUTS:
;	ImgNums	- object images numbers
;
; KEYWORDS:
;	COUNT	- contains number of images read
;	VERBOSE - set this to get extra screen output
;
; RETURNS:
;	KCWI_PPAR struct array, one for each image in proc file
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2017-MAY-24	Initial version
;-
function kcwi_read_proc,ppar,procf,imgs, $
	verbose=verbose,count=count,select=select
;
; setup
	pre = 'KCWI_READ_PROC'
	count = 0
;
; check inputs
	if kcwi_verify_ppar(ppar,/init,/silent) ne 0 then begin
		ppar = { kcwi_ppar }
		kcwi_print_info,ppar,pre, $
			'Uninitialized ppar struct, returning',/error
		return,-1
	endif
;
; use defaults, if name not passed in
	if n_params(0) lt 1 or n_elements(procf) le 0 then begin
		;
		; check default reduced directory first
		procf = ppar.reddir + ppar.prfname
		if not file_test(procf,/read) then begin
			;
			; now check current directory
			procf = ppar.curdir + ppar.prfname
			if not file_test(procf,/read) then begin
				kcwi_print_info,ppar,pre, $
					'default proc file not found',/error
				return,-1
			endif
		endif
	endif
;
; check file
	fi = file_info(procf)
	if not fi.exists or not fi.read or not fi.regular then begin
		kcwi_print_info,ppar,pre,'file not accessible: ',procf,/error
		return,-1
	endif
;
; initialize
	opar = [ppar]
	imgs = [-1]
	keys = tag_names(ppar)
	nkeys = n_elements(keys)
;
; open proc file
	openr,il,procf,/get_lun
;
; skip past header to first image record
	rec = '#'
	while strpos(rec,'#') ge 0 do readf,il,rec
;
; loop over input
	while not eof(il) do begin
		count += 1
		;
		; image number for this record
		imgs = [imgs,fix(gettok(rec,' '))]
		;
		; get params for this record
		opar = [opar,ppar]
		if not eof(il) then begin
			;
			; read next record
			readf,il,rec
			;
			; process key,value pairs
			while strpos(rec,'=') ge 0 do begin
			    if strpos(rec,'#') lt 0 then begin
				;
				; parse keyword
				tag = strupcase(gettok(rec,'='))
				;
				; find tag in ppar struct
				ti = where(strcmp(keys,tag),nti)
				;
				; do we have an unambiguous match?
				if nti eq 1 then begin
					;
					; convert value based on type
					val = ''
					tty = size(opar[count].(ti),/type)
					case tty of
						1: val = byte(rec)
						2: val = fix(rec)
						3: val = long(rec)
						4: val = float(rec)
						5: val = double(rec)
						7: val = strtrim(rec,2)
						else: kcwi_print_info,ppar,pre,$
							'Bad tag type',tty
					endcase
					;
					; assign value to the tag
					opar[count].(ti) = val
				endif else if nti eq 0 then begin
					kcwi_print_info,ppar,pre, $
						'Unrecognized tag',tag,/warning
				endif else begin
					kcwi_print_info,ppar,pre, $
						'Ambiguous tag',tag,/warning
				endelse
			    endif	; not a comment line
			    if not eof(il) then $
					readf,il,rec $
			    else	rec = ''
			endwhile	; process key,value pairs
		endif	; not eof(il)
	endwhile	; not eof(il)
;
; trim arrays
	good = where(imgs ge 0, ngood)
	if ngood gt 0 then begin
		imgs = imgs[good]
		opar = opar[good]
	endif else begin
		opar = -1
		count = 0
	endelse
;
	return,opar
end

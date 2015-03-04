; $Id: kcwi_write_ppar.pro,v 1.9 2015/02/21 00:18:39 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_WRITE_PPAR
;
; PURPOSE:
;	This procedure writes the pipeline parameter structure to a file.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_WRITE_PPAR, PPAR_STRUCT
;
; INPUTS:
;	Ppar	- pipeline parameter structure
;
; KEYWORDS:
;	FORCE	- set this for force writing file, even if struct not init'ted
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	outputs pipeline parameter struct to file specified in Ppar.ppfname
;
; PROCEDURE:
;	Writes out the pipeline parameters as a series of keyword value
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
;		RAWDIR          '/Users/kcwi/drp/run/14/12/20'
;		/ root dir of raw data
;		GAIN1                   0.14500 / gain for amp 1 in electrons/DN
;
;	The details of the processing steps can be found in the KCWI DRP 
;	documentation and are briefly described in KCWI_PIPE.PRO.
;
; EXAMPLE:
;
;	This will write out the file 'm82.ppar' containing all the pipeline
;	parameters:
;
;	ppar.ppfname='m82.ppar'
;	KCWI_WRITE_PPAR,ppar
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-APR-12	Initial version
;	2013-MAY-03	Uses procdir + ppfname for output filename
;-
pro kcwi_write_ppar,ppar,force=force,archive=archive
;
; initialize
	pre = 'KCWI_WRITE_PPAR'
	legal_types = [1, 2, 3, 4, 5, 7]
;
; don't write out if the file name is not set
	if strlen(ppar.ppfname) le 0 then begin
		kcwi_print_info,ppar,pre,'PPFNAME tag empty',/error
		return
	endif
; don't write out if it isn't initialized
	if not ppar.initialized and not keyword_set(force) then begin
		kcwi_print_info,ppar,pre,'KCWI_PPAR struct not initialized',/error
		return
	endif
;
; test filename
	ofile = ppar.reddir + ppar.ppfname
	if file_test(ofile) then begin
		if keyword_set(archive) then begin
			filestamp,ofile,/arch
			kcwi_print_info,ppar,pre,'archived existing ppar file', $
				ofile,format='(a,a)'
		endif else begin
			file_delete,ofile,verbose=ppar.verbose
			kcwi_print_info,ppar,pre,'deleted existing ppar file', $
				ofile,format='(a,a)'
		endelse
	endif
;
; open file
	openw,ol,ofile,/get_lun
;
; instantiate a ppar structure for the pipeline
	A = {kcwi_ppar}
;
; get comments
	junk = struct_init(A,comments=cmnts)
;
; get struct tags
	keys = tag_names(ppar)
;
; store loglun
	loglun = ppar.loglun
	ppar.loglun = -1
;
; test timestamp, set it to now if not set
	if ppar.timestamp le 0 then ppar.timestamp = systime(1)
;
; loop over tags
	for i=0,n_tags(ppar)-1 do begin
;
; how many elements?
		nel = size(ppar.(i),/n_elements)
;
; what type?
		type = size(ppar.(i),/type)
		w = where(legal_types eq type, nw)
;
; only single element, basic types are legal
		legal = (nw eq 1 and nel eq 1)
;
; print only legal parameters
		if legal then begin
			printed = (1 eq 0)
;
; get parameter type
			case type of
			1:	fmt = '(a-15,1x,i15,a3,a)'
			2:	fmt = '(a-15,1x,i15,a3,a)'
			3:	fmt = '(a-15,1x,i15,a3,a)'
			4:	fmt = '(a-15,1x,f15.5,a3,a)'
			5:	fmt = '(a-15,1x,f15.3,a3,a)'
			7:	begin
				str = "'"+ppar.(i)+"'"
				if strlen(str) gt 15 then begin
					printf,ol,keys[i],str, $
						format='(a-15,1x,a)'
					printf,ol,'/ ',cmnts[i]
				endif else begin
					printf,ol,keys[i],str,' / ',cmnts[i], $
						format='(a-15,1x,a15,a,a)'
				endelse
				printed = (1 eq 1)
				end
			else:	fmt = ''
			endcase
			if not printed then $
			    printf,ol,keys[i],ppar.(i),' / ',cmnts[i],format=fmt
;
; not a legal parameter
		endif else $
			kcwi_print_info,ppar,pre,'not printing ',keys[i],/warning
	endfor
;
; close up
	free_lun,ol
	kcwi_print_info,ppar,pre,ppar.ppfname + ' written in directory ' + $
		ppar.reddir
;
; restore loglun
	ppar.loglun = loglun
;
	return
end

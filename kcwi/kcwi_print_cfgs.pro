; $Id: kcwi_print_cfgs.pro,v 1.15 2014/10/28 16:44:34 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_PRINT_CFGS
;
; PURPOSE:
;	This function prints a summary of the configurations passed, one
;	line per image.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_PRINT_CFGS,Kcfg
;
; INPUTS:
;	Kcfg	- An array of struct KCWI_CFG.
;
; OUTPUTS:
;	imsum	- image summary (string)
;
; KEYWORDS:
;	header	- set to print headings for the columns
;	silent	- just return string, do not print
;	outfile	- filename to print to
;
; PROCEDURE:
;	Prints a summary allowing comparison of configurations of each image.
;
; EXAMPLE:
;	Read in the stage one processed image data headers in directory 
;	'redux' and return an array of struct KCWI_CFG.  Find all the
;	continuum flats and print their configuration summary.
;
;	KCFG = KCWI_PRINT_CFGS('redux',filespec='*_int.fits')
;	KCWI_PRINT_CFG, KCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-08	Initial version
;	2013-NOV-13	Added outfile keyword
;-
pro kcwi_print_cfgs,kcfg,imsum,header=header,silent=silent,outfile=outfile
	;
	; setup
	pre = 'KCWI_PRINT_CFGS'
	version = repstr('$Revision: 1.15 $ $Date: 2014/10/28 16:44:34 $','$','')
	imsum = ''
	;
	; check inputs
	if kcwi_verify_cfg(kcfg) ne 0 then return
	;
	; outfile?
	if keyword_set(outfile) then begin
		filestamp,outfile,/arch
		openw,ol,outfile,/get_lun
		printf,ol,'# '+pre+'  '+version
		printf,ol,'# SSM = Sky, Shuffle, Mask: 0 - no, 1 - yes'
		printf,ol,'#  #/   N Bin AMPS SSM GRAT FILT   FM4pos    GRpos   CAMpos   FOCpos   Cwave JDobs         Expt Type     Imno   RA          Dec             PA    Object'
	endif
	;
	; header?
	if keyword_set(header) and not keyword_set(silent) then begin
		print,' SSM = Sky, Shuffle, Mask: 0 - no, 1 - yes'
		print,'   #/   N Bin AMPS SSM GRAT FILT   FM4pos    GRpos   CAMpos   FOCpos   Cwave JDobs         Expt Type     Imno   RA          Dec             PA    Object'
	endif
	;
	; current date
	cdate = 0.d0
	;
	; loop over elements
	n = n_elements(kcfg)
	for i=0,n-1l do begin
		;
		; prepare summary
		imsum = string(i+1,'/',n,kcfg[i].xbinsize,kcfg[i].ybinsize, $
			strtrim(kcfg[i].ampmode,2),kcfg[i].skyobs, $
			kcfg[i].shuffmod,kcfg[i].nasmask, $
			strtrim(kcfg[i].gratid,2),strtrim(kcfg[i].filter,2), $
			kcfg[i].fm4pos,kcfg[i].gratpos,kcfg[i].campos, $
			kcfg[i].focpos,kcfg[i].cwave,kcfg[i].juliandate, $
			kcfg[i].exptime,strtrim(kcfg[i].imgtype,2), $
			kcfg[i].imgnum,kcfg[i].ra,kcfg[i].dec,kcfg[i].rotpa, $
			format='(i4,a1,i4,2i2,1x,a-5,3i1,1x,a-4,1x,a-4,4i9,f8.1,f12.3,f7.1,1x,a-8,i5,2f13.8,2x,f7.2)')
		;
		; add object info
		if strpos(kcfg[i].imgtype,'object') ge 0 then begin
			imsum = imsum + string(strtrim(kcfg[i].object,2),form='(2x,a)')
		endif
		if not keyword_set(silent) then print,imsum
		if keyword_set(outfile) then begin
			if i gt 0 then $
				deljd = kcfg[i].juliandate - kcfg[i-1].juliandate $
			else	deljd = 1.0
			if deljd gt 0.25 and kcfg[i].juliandate-cdate gt 0.75 then begin
				cdate = kcfg[i].juliandate
				caldat,long(cdate),month,day,year
				printf,ol,'# Run: ',year-2000.,month,day, $
					format='(a,i02,i02,i02)'
			endif
			printf,ol,imsum
		endif
	endfor
	if keyword_set(outfile) then free_lun,ol
	;
	return
end

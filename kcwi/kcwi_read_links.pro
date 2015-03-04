; $Id: kcwi_read_links.pro,v 1.6 2015/02/21 00:18:36 neill Exp $
;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_READ_LINKS
;
; PURPOSE:
;	This function reads the processing links from a link file.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_READ_LINKS, Ppar, LinkFilename, ImgNums
;
; INPUTS:
;	Ppar		- KCWI_PPAR pipeline parameter struct
;	LinkFilename	- link file (written with KCWI_PREP)
;
; OUTPUTS:
;	ImgNums	- object images numbers
;
; OUTPUT KEYWORDS:
;	BIAS	- bias links
;	DARK	- dark links
;	FLAT	- flat links
;	CBAR	- continuum bar links
;	ARC	- arc links
;	PROF	- profile image links
;	SKY	- sky image links
;	RRSP	- relative response image links
;	STD	- standard star image links
;	COUNT	- contains number of images read
;	VERBOSE - set this to get extra screen output
;
; RETURNS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-APR-03	Initial version
;-
pro kcwi_read_links,ppar,linkf,imgs, $
	bias=bias,dark=dark,flat=flat,cbar=cbar,arc=arc,prof=prof,sky=sky, $
	rrsp=rrsp, std=std, verbose=verbose,count=count,select=select
;
; setup
	pre = 'KCWI_READ_LINKS'
	version = repstr('$Revision: 1.6 $ $Date: 2015/02/21 00:18:36 $','$','')
;
; check inputs
	if kcwi_verify_ppar(ppar,/silent) ne 0 then $
		ppar = { kcwi_ppar }
;
; initialize
	count = 0
	imgs = -1l
	bias = -1l
	dark = -1l
	flat = -1l
	cbar = -1l
	arc =  -1l
	prof = -1l
	sky  = -1l
	rrsp = -1l
	std  = -1l
;
; use defaults, if name not passed in
	if n_params(0) lt 1 or n_elements(linkf) le 0 then begin
		;
		; check default reduced directory first
		linkf = ppar.reddir + ppar.lnfname
		if not file_test(linkf,/read) then begin
			;
			; now check current directory
			linkf = ppar.curdir + ppar.lnfname
			if not file_test(linkf,/read) then begin
				kcwi_print_info,ppar,pre,'default link file not found',/error
				return
			endif
		endif
	endif
;
; check file
	fi = file_info(linkf)
	if not fi.exists or not fi.read or not fi.regular then begin
		kcwi_print_info,ppar,pre,'file not accessible: ',linkf,/error
		return
	endif
;
; read file
	readcol,linkf,imgs,bias,dark,flat,cbar,arc,prof,sky,rrsp,std, $
		format='l,l,l,l,l,l,l,l,l,l',/silent,comment='#'
	count = n_elements(imgs)
;
; select a single image?
	if keyword_set(select) then begin
		print,'      Img     Bias     Dark     Flat     Cbar      Arc     Prof      Sky     Rrsp      Std'
		forprint,imgs,bias,dark,flat,cbar,arc,prof,sky,rrsp,std, $
			format='(10i9)'
		print,'      Img     Bias     Dark     Flat     Cbar      Arc     Prof      Sky     Rrsp      Std'
		pick = -1l
		read,'Which Img?: ',pick
		t=(where(imgs eq pick, nt))[0]
		while nt ne 1 do begin
			read,'Which Img?: ',pick
			t=where(imgs eq pick, nt)
		endwhile
		;
		; pick 'em out
		imgs=imgs[t]
		bias=bias[t]
		dark=dark[t]
		flat=flat[t]
		cbar=cbar[t]
		arc = arc[t]
		prof=prof[t]
		sky = sky[t]
		rrsp=rrsp[t]
		std = std[t]
		count = 1l
	endif
;
	return
end

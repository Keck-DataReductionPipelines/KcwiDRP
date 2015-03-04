;$Id: cwi_fix_headers.pro,v 1.24 2015/01/13 18:19:16 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	CWI_FIX_HEADERS
;
; PURPOSE:
;	Fix CWI image data headers to work with KCWI DRP.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	CWI_FIX_HEADERS,Hmf
;
; OPTIONAL INPUTS:
;	Hmf	- header modifications file (see below), prompts for if missing
;
; KEYWORDS:
;	FROOT	- root for image files (def: 'image')
;	FDIGITS	- number of digits for image number (def: 4)
;	VERBOSE	- print extra output
;	UPDATE	- set to force updating, otherwise only updates those in need
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Modifies the headers of all files specified in the Hmf file.
;
; PROCEDURE:
;	This should only be performed on raw images.  The initial image list
;	is generated with the file spec that defaults to 'image????.fit*', but
;	can be modified using the FROOT and FDIGITS keywords.  It is always 
;	a good idea to copy the original data files into a working directory
;	in case anything bad happens.
;
;	The Hmf file that is read in by CWI_FIX_HEADERS has five SPACE
;	DELIMITED columns (no tabs):
;
;	Column 1: KEYWORD	this is the header keyword to update or add
;	Column 2: VALUE		this is the value for the keyword
;	Column 3: TYPE		this is the IDL type code for the input:
;				1 - for bytes
;				2 - for integers
;				3 - for long integers
;				4 - for reals
;				5 - for doubles
;				7 - for strings
;				other types are illegal
;	Column 4: AFTER		the keyword after which to insert a new keyword
;				must set to '-' if not needed
;	Column 5: IMGNOS	the range list of image numbers, examples are:
;				6972-7083
;				7040,7046,7055,7057,7069,7071
;				*
;				Any mix of ranges using a dash in between and
;				individual images using a comma in between
;				The '*' means apply to all images
;				IMPORTANT: use no spaces in these lists!
;
;	This file is read in and all relevant headers are modified.
;	In addition, two keywords are added recording the file used for the 
;	header modification and the time-stamp of the modification.
;
; HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-APR-16	Initial version
;	2013-AUG-06	Added '*' option for image list range, meaning all
;	2014-MAY-28	Added FROOT and FDIGITS keywords
;	2014-JUN-03	checks file digits automatically if FDIGITS not set
;	2014-SEP-24	removed cwave and gratanom calc: moved to cwi_format_info.pro
;	2014-OCT-16	now updates headers only once, added update keyword
;-
pro cwi_fix_headers,hmf,froot=froot,fdigits=fdigits,update=update, $
	verbose=verbose
;
; program setup
pre = 'CWI_FIX_HEADERS'
version = repstr('$Revision: 1.24 $ $Date: 2015/01/13 18:19:16 $','$','')
;
; check inputs
if n_params(0) lt 1 then begin
	hmf = ''
	read,'Header modification file: ',hmf
endif
;
; test header mod file
if not file_test(hmf) then begin
	print,'Header modification file not found: ',hmf
	return
endif
;
; get input file spec
if keyword_set(froot) then $
	frute = froot $
else	frute = 'image'
;
; get input number of digits
if keyword_set(fdigits) then $
	fdig = fdigits $
else	begin
	flist = file_search('*.fit*', count=nf)
	if nf le 0 then begin
		print,pre,' ERROR - no fits files found'
		return
	endif
	fdig = 0
	for i=0,nf-1 do begin
		ndig = kcwi_get_digits(flist[i])
		if ndig gt fdig then fdig = ndig
	endfor
endelse
print,pre,' INFO - assuming image numbers have this many digits: ',fdig
;
; file spec
fspec = frute + strjoin(replicate('?',fdig)) + '.fit*'
;
; get a list of files
flist=file_search(fspec,count=nf)
;
; check if we found anything
if nf le 0 then begin
	print,pre,' ERROR - no files found, try using froot and/or fdigits keywords'
	return
endif
print,pre,' INFO - number of images found: ',nf
;
; extract image numbers
imgnos = fix(stregex(flist,'[0-9]+',/extract))
;
; read in header modification file
readcol,hmf,mkeys,val,tcode,after,rng,form='a,a,i,a,a',comment='#',delim=' '
nmk = n_elements(mkeys)
;
print,pre,' INFO - number of keyword modifications: ',nmk
;
; update matrix
umat = lonarr(nmk,nf)
;
; loop over modification records
for i=0,n_elements(mkeys)-1 do begin
;
; check for asterisk (meaning all)
	if strpos(rng[i],'*') ge 0 then begin
		umat[i,*] = imgnos
;
; expand range par into image number list
	endif else begin
		rangepar,rng[i],mno
		nmno = n_elements(mno)
		umat[i,0:(nmno-1)] = mno
	endelse
endfor	; loop over modification records
;
; count number fixed
nfix = 0L
;
; loop over images (flist and imgnos)
for j=0,nf-1 do begin
;
; read in header
	h = headfits(flist[j],errmsg=errmsg)
	;
	; was header read correctly?
	if strlen(errmsg) le 0 then begin
		;
		; check for a needed update
		hfxfl = sxpar(h,'hfixfile',count=nhfxfl)
		;
		; default to no
		do_update = (1 eq 0)
		;
		; this one has not been fixed yet
		if nhfxfl eq 0 then do_update = (1 eq 1)
		;
		; this one was fixed, but with a different file
		if nhfxfl gt 0 and strcmp(hfxfl,hmf) eq 0 then do_update = (1 eq 1)
		;
		; this is how we force an update
		if keyword_set(update) then do_update = (1 eq 1)
		;
		; now do the update or not
		if do_update then begin
			if keyword_set(verbose) then $
				print,j+1,' / ',nf,'modifying ',flist[j], $
					format='(i,a,i,2x,a,a)'
			;
			; mod status
			did_mod = (1 eq 0)
			;
			; loop over modification records
			for i=0,n_elements(mkeys)-1 do begin
				;
				; does this image need this keyword modification?
				t=where(umat[i,*] eq imgnos[j], nt)
				if nt ge 1 then begin
					;
					; modded it
					did_mod = (1 eq 1)
					;
					; log it, if requested
					if keyword_set(verbose) then $
						print,mkeys[i],' to ',val[i], $
							form='(a-10,a,a-30)'
					;
					; convert value string to appropriate type
					case tcode[i] of
						1:	oval = byte(val[i])
						2:	oval = fix(val[i])
						3:	oval = long(val[i])
						4:	oval = float(val[i])
						5:	oval = double(val[i])
						7:	oval = repstr(val[i],'_',' ')	; handle blanks
						else:	begin
							print,'Error - illegal type code: ',tcode[i]
							return
							end
					endcase
					;
					; apply this keyword update
					if strtrim(after[i],2) ne '-' then $
						sxaddpar,h,mkeys[i],oval,after=after[i] $
					else	sxaddpar,h,mkeys[i],oval
				endif	; nt ge 1 (modify this keyword)
			endfor	; loop over modification records
			;
			; did we actually modify it?
			if did_mod then begin
				;
				; count it
				nfix += 1L
				;
				; log modification
				sxaddpar,h,'HFIXVER',version,' program version'
				sxaddpar,h,'HFIXFILE',hmf,' file used to modify header'
				sxaddpar,h,'HFIXDATE',systime(0),' header modify date'
				;
				; check to see that imgnum is in header
				ino = sxpar(h,'IMGNUM',count=nino)
				if nino le 0 then $
					sxaddpar,h,'IMGNUM',imgnos[j],after='IMGTYPE'
				;
				; update fits file with new header
				modfits,flist[j],0,h
			endif else if keyword_set(verbose) then $
				print,j+1,' / ',nf,'no mods   ',flist[j], $
					format = '(i,a,i,2x,a,a)'
			if keyword_set(verbose) then $
				print,' '
		;
		; do_update (modification for this image)
		endif else if keyword_set(verbose) then $
			print,j+1,' / ',nf,'skipping  ',flist[j], $
				format='(i,a,i,2x,a,a)'
	;
	; header not read properly
	endif else begin
		print,'Bad header, skipping ',flist[j]
	endelse
endfor	; loop over images
;
; report
print,'Fixed '+strn(nfix)+' headers'
;
return
end

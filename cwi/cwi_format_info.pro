; $Id: cwi_format_info.pro,v 1.38 2015/01/26 18:29:15 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology.  All rights reserved
;+
; NAME:
;	CWI_FORMAT_INFO
;
; PURPOSE:
;	Create header modification text file for input to CWI_FIX_HEADERS
;	from image????.info files.
;
; CALLING SEQUENCE:
;	CWI_FORMAT_INFO
;
; INPUTS:
;	None.
;
; KEYWORDS:
;	FROOT	- root for image files (def: 'image')
;	FDIGITS	- number of digits for image number (def: 4)
;	UPDATE	- set to use existing 'hdrfix.txt' file and only add
;			new image records to 'hdrfix.txt'
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Generates file 'hdrfix.txt' that contains the keyword processing
;	information required for CWI_FIX_HEADERS.PRO.
;
; PROCEDURE:
;	Gathers a list of images with the filespec that defaults to
;	'image????.fit*', but can be modified using the FROOT and FDIGITS
;	keywords, and looks for corresponding *.info files.  Parses the 
;	image header and the info file into the keyword specifications 
;	needed by CWI_FIX_HEADERS.PRO.
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-06	Initial version
;	2013-AUG-07	added parsing TELOBJ token for OBJECT name
;	2013-SEP-17	skip third junk coord for NAS obs
;	2013-OCT-02	conversion of Cass ring angle to PA
;	2013-OCT-07	interactive setting of some run keywords
;	2013-NOV-05	refined image number range inputs
;	2013-DEC-04	added DOMEHIGH handling for dome flats
;	2014-MAY-28	added FROOT and FDIGITS keywords
;	2014-JUN-03	checks file digits automatically if FDIGITS not set
;       2014-AUG-24     added yellow grating determination
;	2014-SEP-24	now queries image range for IR filter, calculates
;			cwave and gratanom keyword values
;	2015-JAN-26	errors out if allhdr.txt not found
;-
pro cwi_format_info,froot=froot,fdigits=fdigits,update=update
;
; setup
pre = 'CWI_FORMAT_INFO'
version = repstr('$Revision: 1.38 $ $Date: 2015/01/26 18:29:15 $','$','')
q = ''
;
; check allhdr.txt
afile = !CWI_DATA + 'allhdr.txt'
if not keyword_set(update) and not file_test(afile) then begin
	print,pre+': ERROR - allhdr.txt not found, looking here: ',!CWI_DATA
	print,pre+': Fix !CWI_DATA in startup.pro and rerun.'
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
; get list of raw images
flist = file_search(fspec, count=nf)
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
; archive output file
fnam = 'hdrfix.txt'
;
; check for an update
do_update = (1 eq 0)	; default: no update
lastimg = -1L		; last image in file
istart = 0L		; starting image
if keyword_set(update) then begin
	if file_test(fnam,/read,/write,/regular) then begin
		do_update = (1 eq 1)
		print,pre,' INFO - updating existing hdrfix.txt'
		;
		; read current file to find last image number
		readcol,fnam,mkeys,val,tcode,after,rng, $
			form='a,a,i,a,a',comment='#',delim=' '
		lastimg = fix(rng[n_elements(rng)-1])
		;
		; starting image
		newims = where(imgnos gt lastimg, nnew)
		istart = newims[0]
		;
		; check if there are any images to update
		if nnew le 0 then begin
			print,pre,' INFO - no new images'
			return
		endif
		;
		; open file for appending new info
		openw,ol,fnam,/get_lun,/append
	endif else begin
		print,pre,' ERROR - update keyword set but no previous hdrfix.txt'
		return
	endelse
;
; if we are not updating, archive old file
; and open a new copy
endif else begin
	filestamp,fnam,/arch
	openw,ol,fnam,/get_lun
endelse
printf,ol,'# '+pre+' '+version
printf,ol,'# Run on '+systime(0)
;
; Keyword processing info (see CWI_FIX_HEADERS.PRO for details)
printf,ol,'#KEYWRD  VALUE   TYPE    AFTER    RANGE'
;
; if we are not updating an existing hdrfix.txt we need this stuff
if not do_update then begin
	;
	; get selected run info
	val=''
	while strlen(strtrim(val,2)) eq 0 do $
		read,"Enter observer's last name: ",val
	val = strcompress(val,/remove_all)
	;
	; format and write out
	key='OBSERVER'
	ityp = 7
	after = '-'
	rng = '*'
	valfmtlen = strlen(val)>10
	spc = (14 - valfmtlen)>1
	fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,a)'
	printf,ol,key,val,ityp,after,rng,format=fmt
	;
	; read allhdr.txt (confirmed to exist above)
	rec = ''
	openr,il,afile,/get_lun
	while not eof(il) do begin
		readf,il,rec
		printf,ol,rec
	endwhile
	free_lun,il
	;
	; get image ranges for sky and NAS mask
	print,'For image number ranges use:'
	print,"* for all images in the night,"
	print,"(<cr>) for no images in the night,"
	print,"image number range list (e.g. '1234-1254,1265,1277')."
	;
	; SKY Observations
	skyrng=''
	read,'Enter sky observations     image number range: ',skyrng
	if strlen(skyrng) gt 0 then begin
		skyrng = strcompress(skyrng,/remove_all)
		if strlen(skyrng) gt 0 then begin
			key='SKYOBS'
			after='FILTER'
			val = 'T'
			valfmtlen = strlen(val)>10
			spc = (14 - valfmtlen)>1
			fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,a)'
			printf,ol,key,val,ityp,after,skyrng,format=fmt
		endif
	endif
	;
	; Twilight flats
	twirng=''
	read,'Enter good twilight flats  image number range: ',twirng
	if strlen(twirng) gt 0 then begin
		twirng = strcompress(twirng,/remove_all)
		if strlen(twirng) gt 0 then begin
			key='SKYOBS'
			after='FILTER'
			val = 'T'
			valfmtlen = strlen(val)>10
			spc = (14 - valfmtlen)>1
			fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,a)'
			printf,ol,key,val,ityp,after,twirng,format=fmt
		endif
		rangepar,twirng,twiran
	endif else twiran = -1
	;
	; NAS Mask
	maskrng=''
	print,'Please include all images (science and calibration) in the next query.'
	read,'Enter nod-and-shuffle mask image number range: ',maskrng
	maskrng = strcompress(maskrng,/remove_all)
	key='NASMASK'
	after='SKYOBS'
	fmt = '(a-8,1x,a-10,i2,4x,a-8,1x,a)'
	;
	; a real image number range
	if strlen(maskrng) gt 1 then begin
		val = 'F'
		printf,ol,key,val,ityp,after,'*',format=fmt
		val = 'T'
		printf,ol,key,val,ityp,after,maskrng,format=fmt
	;
	; an asterisk '*' (all)
	endif else if strlen(maskrng) eq 1 then begin
		val = 'T'
		printf,ol,key,val,ityp,after,maskrng,format=fmt
	;
	; a <cr> (none)
	endif else if strlen(maskrng) eq 0 then begin
		val = 'F'
		printf,ol,key,val,ityp,after,'*',format=fmt
	endif
	;
	; SHUFF status
	ssttrng=''
	key='SHFFSTAT'
	after='NASMASK'
	ityp=3
	;
	; check for aborted nod-and-shuffle obs
	if strlen(maskrng) ge 1 then begin
		read,'Enter aborted nod-and-shuffle   image numbers: ',ssttrng
		ssttrng=strcompress(ssttrng,/remove_all)
		;
		; a real image number range
		if strlen(ssttrng) gt 1 then begin
			val = '0'
			printf,ol,key,val,ityp,after,'*',format=fmt
			val = '1'
			printf,ol,key,val,ityp,after,ssttrng,format=fmt
		;
		; an asterisk '*' (all)
		endif else if strlen(ssttrng) eq 1 then begin
			val = '1'
			printf,ol,key,val,ityp,after,ssttrng,format=fmt
		;
		; a <cr> (none)
		endif else if strlen(ssttrng) eq 0 then begin
			val = '0'
			printf,ol,key,val,ityp,after,'*',format=fmt
		endif
	endif
	;
	; check for aborting during sky observation
	if strlen(ssttrng) ge 1 then begin
		skabrng=''
		after='-'
		read,'Enter aborted while on sky      image numbers: ',skabrng
		skabrng=strcompress(skabrng,/remove_all)
		;
		; a real image number range
		if strlen(skabrng) gt 1 then begin
			printf,ol,'NSSKYR0','686',3,'-',skabrng,format=fmt
			printf,ol,'NSSKYR1','1370',3,'-',skabrng,format=fmt
			printf,ol,'NSOBJR0','1371',3,'-',skabrng,format=fmt
			printf,ol,'NSOBJR1','2055',3,'-',skabrng,format=fmt
		endif 
	endif
endif	; not do_update
;
; do we have DOMEHIGH keywords in the info files?
domehicount = 0
redgratcount = 0
;
; loop over fits files
for i=istart,nf-1 do begin
	;
	; initialize image type data
	arc = (1 eq 0)
	cont = (1 eq 0)
	bias = (1 eq 0)
	dflat = (1 eq 0)
	tflat = (1 eq 0)
	fm4pos = -9.
	mmppos = -9.
	calxpos = -9.
	focpos = -9.
	campos = -9.
	gratpos = -9.
	angout = (1 eq 1)
	focout = (1 eq 1)
	tubout = (1 eq 1)
	shffmod = (1 eq 0)
	redgrat = (1 eq 0)
	;
	; get image number
	imgno = fix(stregex(flist[i],'[0-9]+',/extract))
	;
	; check against twilight flat list
	if twiran[0] ge 0 then begin
		t = where(twiran eq imgno, nt)
		if nt eq 1 then tflat = (1 eq 1)
	endif
	;
	; read in header and check for bias frame
	hdr = headfits(flist[i])
	;
	; check validity of header
	if n_elements(hdr) gt 1 then begin
		;
		; check if exposure time consistent with being a bias frame
		if sxpar(hdr,'EXPTIME') eq 0. then $
			bias = (1 eq 1)
		;
		; look for info file
		ifil = strmid(flist[i],0,strpos(flist[i],'.fit'))+'.info'
		;
		; if it exists, parse it
		if file_test(ifil,/read) then begin
			;
			; bools for where we are in the file
			back = (1 eq 0)
			targ = (1 eq 0)
			naso = (1 eq 0)
			;
			; open the info file
			openr,il,ifil,/get_lun
			;
			; read through each info file
			rec = ''
			while not eof(il) do begin
				readf,il,rec
				;
				; don't output every time through
				do_out = (1 eq 0)
				;
				; by default we don't care where the keywords go
				after = '-'
				;
				; find out what section of the file we are in
				if strpos(rec,'BACKGROUND') ge 0 then begin
					back = (1 eq 1)
					naso = (1 eq 1)
				endif
				if strpos(rec,'TARGET') ge 0 then begin
					targ = (1 eq 1)
					naso = (1 eq 1)
				endif
				;
				; start parsing each useful item
				if strpos(rec,'SHUFFMOD') ge 0 then begin
					;
					; get the keyword
					key = gettok(rec,' ')
					;
					; now what value for the keyword
					val = strtrim(rec,2)
					;
					; set the IDL type of the value
					ityp = 7
					;
					; after which keyword?
					after = 'NASMASK'
					;
					; let's write this one out
					do_out = (1 eq 1)
					;
					; test val because nod-and-shuffle obs have 0. exptime
					if val eq 'T' then begin
						bias = (1 eq 0)
						shffmod = (1 eq 1)
					endif
				endif
				;
				; same for each item below
				if strpos(rec,'TELOBJ') ge 0 then begin
					key = gettok(rec,' ')
					key = 'OBJECT'
					val = strtrim(rec,2)
					val = strcompress(rec,/remove)
					val = repstr(val,'_','')
					ityp = 7
					do_out = (1 eq 1)
				endif
				if strpos(rec,'EXPTIMNS') ge 0 then begin
					key = gettok(rec,' ')
					key = 'EXPTIME'
					val = strtrim(rec,2)
					ityp = 5
					do_out = (1 eq 1)
				endif
				if strpos(rec,'NSVERS') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					ityp = 7
					do_out = (1 eq 1)
				endif
				if strpos(rec,'MMPOS') ge 0 then begin
					key = gettok(rec,' ')
					key = 'MMPPOS'
					val = strtrim(rec,2)
					mmppos = float(val)	; save for imgtype decision
					ityp = 3
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'FOCPOS') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					focpos = float(val)
					ityp = 3
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'FM3POS') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					ityp = 3
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'FM4POS') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					fm4pos = float(val)
					ityp = 3
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'GRATPOS') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					gratpos = float(val)
					ityp = 3
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'CAMPOS') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					campos = float(val)
					ityp = 3
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'CALXPOS') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					calxpos = float(val)	; save for imgtype decision
					ityp = 3
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'CALYPOS') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					ityp = 3
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'ARC') ge 0 and $
				   strpos(rec,'DOME') lt 0 then begin
					key = gettok(rec,' ')
					jnk = gettok(rec,' ')
					val = strtrim(rec,2)
					;
					; was the arc lamp on?
					if fix(val) eq 1 then begin
						arc = (1 eq 1)
						val = 'T'
					endif else begin
						arc = (1 eq 0)
						val = 'F'
					endelse
					ityp = 7
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'CONT') ge 0 then begin
					key = gettok(rec,' ')
					jnk = gettok(rec,' ')
					val = strtrim(rec,2)
					;
					; was the continuum lamp on?
					if fix(val) eq 1 then begin
						cont = (1 eq 1)
						val = 'T'
					endif else begin
						cont = (1 eq 0)
						val = 'F'
					endelse
					ityp = 7
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'DOMEHIGH') ge 0 then begin
					key = gettok(rec,' ')
					val = strtrim(rec,2)
					domehicount += 1
					;
					; was the continuum lamp on?
					if strpos(val,'ON') ge 0 then begin
						dflat = (1 eq 1)
						val = 'T'
					endif else begin
						dflat = (1 eq 0)
						val = 'F'
					endelse
					ityp = 7
					after = 'IMGTYPE'
					do_out = (1 eq 1)
				endif
				if strpos(rec,'LST') ge 0 then begin
					rec = strtrim(rec,2)
					key = gettok(rec,' ')
					jnk = gettok(rec,' ')
					val = strtrim(rec,2)
					ityp = 7
					after = 'DATE'
					do_out = (1 eq 1)
					;
					; don't write these out for sub-images
					if back then do_out = (1 eq 0)
					if targ then do_out = (1 eq 0)
				endif
				if strpos(rec,'RA') eq 0 and strpos(rec,'offset') lt 0 and $
					strpos(rec,'rate') lt 0 then begin
					rec = strtrim(rec,2)
					key = gettok(rec,' ')
					;
					; adjust keyword for approprate sub-image
					if back then key = 'BACKRA'
					if targ then key = 'TARGRA'
					jnk = gettok(rec,' ')
					val = strtrim(rec,2)
					;
					; convert to decimal degrees
					sta = strsplit(val,':',/extract)
					ra = 15.d0 * ten(fix(sta[0]),fix(sta[1]),float(sta[2]))
					val = strtrim(string(ra,format='(f13.8)'),2)
					;
					ityp = 5
					;
					; don't emit last garbage coord from NAS obs
					if not naso or ( naso and (back or targ) ) then $
						do_out = (1 eq 1)
				endif
				if strpos(rec,'DEC') ge 0 and strpos(rec,'offset') lt 0 and $
					strpos(rec,'rate') lt 0 then begin
					rec = strtrim(rec,2)
					key = gettok(rec,' ')
					;
					; adjust keyword for approprate sub-image
					if back then key = 'BACKDEC'
					if targ then key = 'TARGDEC'
					jnk = gettok(rec,' ')
					val = strtrim(rec,2)
					;
					; convert to decimal degrees
					sta = strsplit(val,':',/extract)
					dec = ten(fix(sta[0]),fix(sta[1]),float(sta[2]))
					val = strtrim(string(dec,format='(f13.8)'),2)
					;
					ityp = 5
					;
					; don't emit last garbage coord from NAS obs
					if not naso or ( naso and (back or targ) ) then $
						do_out = (1 eq 1)
				endif
				if strpos(rec,'HA') ge 0 then begin
					rec = strtrim(rec,2)
					key = gettok(rec,' ')
					jnk = gettok(rec,' ')
					val = strtrim(rec,2)
					ityp = 7
					;
					; don't write these out for sub-images
					if not naso or ( naso and targ ) then $
						do_out = (1 eq 1)
				endif
				if strpos(rec,'air mass') ge 0 then begin
					rec = strtrim(rec,2)
					key = gettok(rec,' ')
					key = gettok(rec,' ')
					key = 'AIRMASS'
					jnk = gettok(rec,' ')
					val = strtrim(rec,2)
					ityp = 5
					do_out = (1 eq 1)
					;
					; air mass is last item in each section (for now)
					; so turn off the section when we get here
					; don't write these out for sub-images
					if back then begin
						back = (1 eq 0)
						do_out = (1 eq 0)
					endif
					if targ then begin
						targ = (1 eq 0)
						do_out = (1 eq 0)
					endif
				endif
				if strpos(rec,'focus') ge 0 then begin
					key = 'TELFOC'
					val = gettok(rec,' ')
					val = gettok(rec,' ')
					val = gettok(rec,' ')
					ityp = 5
					if focout then begin
						do_out = (1 eq 1)
						focout = (1 eq 0)
					endif
				endif
				if strpos(rec,'tube length') ge 0 then begin
					key = 'TUBELEN'
					val = gettok(rec,' ')
					val = gettok(rec,' ')
					val = gettok(rec,' ')
					val = gettok(rec,' ')
					ityp = 5
					if tubout then begin
						do_out = (1 eq 1)
						tubout = (1 eq 0)
					endif
				endif
				if strpos(rec,'Cass ring angle') ge 0 then begin
					key = 'ROTPA'
					val = gettok(rec,' ')
					val = gettok(rec,' ')
					val = gettok(rec,' ')
					val = gettok(rec,' ')
					val = float(rec) + 180.	; convert to PA on sky
					if val ge 360. then val = val - 360.
					if val le -360. then val = val + 360.
					val = strtrim(string(val,format='(f9.3)'),2)
					ityp = 5
					if angout then begin
						do_out = (1 eq 1)
						angout = (1 eq 0)
					endif
				endif
				;
				; did we want to write this out?
				if do_out then begin
					valfmtlen = strlen(val)>10
					spc = (14 - valfmtlen)>1
					fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,i)'
					printf,ol,key,val,ityp,after,imgno,format=fmt
					;
					; make sure we get RA and DEC from TARGRA and TARGDEC
					if strmatch(key,'TARGRA') then begin
						key='RA'
						printf,ol,key,val,ityp,after,imgno,format=fmt
					endif
					if strmatch(key,'TARGDEC') then begin
						key='DEC'
						printf,ol,key,val,ityp,after,imgno,format=fmt
					endif
				endif
			endwhile	; read through each info file
    			;
    			; close input info file
    			free_lun,il
		endif	; info file exists
		;
		; Now check image type
		key = 'IMGTYPE'
		val = 'test'	; default is a test image (can be skipped in DRP)
		ityp = 7
		after = '-'
		;
		; arc values
		if arc and not cont and calxpos ge 0. and calxpos lt 100. then $
			val = 'arc'
		;
		; continuum flat
		if cont and not arc and calxpos ge 0. and calxpos lt 100. then $
    			val = 'cflat'
		;
		; is the bar mask in?
		if cont and not arc and calxpos gt 164000. and calxpos lt 166000. then $
    			val = 'cbars'
		if arc and not cont and calxpos gt 164000. and calxpos lt 166000. then $
    			val = 'arcbars'
		;
		; how about darks?
		if not arc and not cont and calxpos gt 109000. and calxpos lt 111000. then $
    			val = 'dark'
		;
		; are we in object observing position?
		if not arc and not cont and mmppos gt 259000. and mmppos lt 261000. then $
    			val = 'object'
		;
		; are we a twilight flat?
		if tflat then $
			val = 'tflat'
		;
		; are we a dome flat?
		if dflat and mmppos gt 259000. and mmppos lt 261000. then $
    			val = 'dflat'
		;
		; check for test images
		if fm4pos le 0. and focpos le 0 and mmppos le 0 and $
			gratpos le 0 and campos le 0 then $
				val = 'test'
		;
		; are we a bias (from exposure time way above)
		if bias then $
			val = 'bias'
		;
		; format and write out
		valfmtlen = strlen(val)>10
		spc = (14 - valfmtlen)>1
		fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,i)'
		printf,ol,key,val,ityp,after,imgno,format=fmt
		;
		; check configuration for grating and filter setting
		if strmatch(val,'test') eq 0 and strmatch(val,'bias') eq 0 then begin
			if abs(gratpos) gt 50000L then begin
				grat = 'MEDREZ'
				filt = 'NONE'
			endif else begin
				if fm4pos lt 275 and fm4pos gt -500 then begin
					grat = 'BLUE'
					filt = grat
				endif else if fm4pos le -500 then begin
                           		grat = 'YELLOW'
					filt = 'RED'
				endif else begin
					grat = 'RED'
					filt = grat
					redgratcount += 1
					redgrat = (1 eq 1)
				endelse
			endelse
			valfmtlen = strlen(grat)>10
			spc = (14 - valfmtlen)>1
			fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,i)'
			key = 'GRATID'
			after = 'IMGTYPE'
			printf,ol,key,grat,ityp,after,imgno,format=fmt
			valfmtlen = strlen(filt)>10
			spc = (14 - valfmtlen)>1
			fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,i)'
			key = 'FILTER'
			after = 'GRATID'
			printf,ol,key,filt,ityp,after,imgno,format=fmt
			;
			; calculate central wavelength
			cwave = cwi_central_wave(grat,campos,gratpos)
			ityp = 5
			val = strtrim(string(cwave,format='(f9.2)'),2)
			valfmtlen = strlen(val)>10
			spc = (14 - valfmtlen)>1
			fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,i)'
			key = 'CWAVE'
			after = 'IMGTYPE'
			printf,ol,key,val,ityp,after,imgno,format=fmt
			;
			; calculate grating anomoly
			gratanom = cwi_gratanom(grat,gratpos,cwave)
			val = strtrim(string(gratanom,format='(f9.3)'),2)
			valfmtlen = strlen(val)>10
			spc = (14 - valfmtlen)>1
			fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,i)'
			key = 'GRATANOM'
			after = 'GRATPOS'
			printf,ol,key,val,ityp,after,imgno,format=fmt
		endif
		;
		; now do some checks if we are updating
		if do_update and not bias then begin
			;
			; print image
			print,' '
			print,'Image: ',imgnos[i]
			;
			; sky obs?
			key='SKYOBS'
			after='FILTER'
			val = 'F'
			read,'Is this a sky obs? <cr> - no, Y - yes: ',q
			if strupcase(strmid(q,0,1)) eq 'Y' then val = 'T'
			valfmtlen = strlen(val)>10
			spc = (14 - valfmtlen)>1
			fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,i)'
			printf,ol,key,val,7,after,imgnos[i],format=fmt
			;
			; NAS mask?
			key='NASMASK'
			after='SKYOBS'
			val = 'F'
			fmt = '(a-8,1x,a-10,i2,4x,a-8,1x,i)'
			read,'Is the N&S mask in? <cr> - no, Y - yes: ',q
			if strupcase(strmid(q,0,1)) eq 'Y' then begin
				val = 'T'
				printf,ol,key,val,7,after,imgnos[i],format=fmt
				;
				; was this N&S observation aborted?
				key='SHFFSTAT'
				after='NASMASK'
				val = '0'
				if shffmod then begin
					read,'Was this N&S aborted? <cr> - no, Y - yes: ',q
					if strupcase(strmid(q,0,1)) eq 'Y' then begin
						val = '1'
						read,'While on sky? <cr> - no, Y - yes: ',q
						if strupcase(strmid(q,0,1)) eq 'Y' then begin
							printf,ol,'NSSKYR0','686',3,'-',imgnos[i],format=fmt
							printf,ol,'NSSKYR1','1370',3,'-',imgnos[i],format=fmt
							printf,ol,'NSOBJR0','1371',3,'-',imgnos[i],format=fmt
							printf,ol,'NSOBJR1','2055',3,'-',imgnos[i],format=fmt
						endif
					endif	; Aborted N&S?
					printf,ol,key,val,3,after,imgnos[i],format=fmt
				endif	; SHUFFMOD true?
			endif else $	; N&S mask in?
				printf,ol,key,val,7,after,imgnos[i],format=fmt
			;
			; if we are red grating, did we use the IR filter?
			if redgrat then begin
				read,'Was the IR filter in? <cr> - no, Y - yes: ',q
				if strupcase(strmid(q,0,1)) eq 'Y' then begin
					key='FILTER'
					after='GRATID'
					val = 'IR'
					valfmtlen = strlen(val)>10
					spc = (14 - valfmtlen)>1
					fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,i)'
					printf,ol,key,val,7,after,imgnos[i],format=fmt
				endif
			endif
		endif	; are we updating?
	endif else begin ; check validity of header
		print,pre,': Error - bad fits header for ',flist[i]
	endelse
endfor	; loop over images
;
; now check for dome flats
if domehicount lt 5 and not do_update then begin
	dfltrng=''
	read,'Enter dome flat image number range: ',dfltrng
	if strlen(dfltrng) gt 0 then begin
		dfltrng = strcompress(dfltrng,/remove_all)
		key='IMGTYPE'
		after='-'
		ityp = 7
		val = 'dflat'
		valfmtlen = strlen(val)>10
		spc = (14 - valfmtlen)>1
		fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,a)'
		printf,ol,key,val,ityp,after,dfltrng,format=fmt
	endif
endif
;
; now check for non-default filters
print,''
print,'Default grating/filter pairings:'
print,'Grating Filter'
print,'------- -------------'
print,'BLUE    BLUE'
print,'YELLOW  RED'
print,'RED     RED'
print,'MEDREZ  NONE'
print,'IR filter available but not as a default'
print,''
;
; now check for IR filter
if redgratcount gt 0 and not do_update then begin
	irfltrng=''
	read,'Enter IR filter image number range: ',irfltrng
	if strlen(irfltrng) gt 0 then begin
		irfltrng = strcompress(irfltrng,/remove_all)
		key='FILTER'
		after='GRATID'
		ityp = 7
		val = 'IR'
		valfmtlen = strlen(val)>10
		spc = (14 - valfmtlen)>1
		fmt = '(a-8,1x,a-'+strn(valfmtlen)+',i2,'+strn(spc)+'x,a-8,1x,a)'
		printf,ol,key,val,ityp,after,irfltrng,format=fmt
	endif
endif
;
; close hdrfix.txt file
free_lun,ol
;
return
end

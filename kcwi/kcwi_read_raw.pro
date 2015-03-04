;$Id: kcwi_read_raw.pro,v 1.4 2015/02/21 00:18:38 neill Exp $
;
;+
;
;-
function kcwi_read_raw,ppar,imgno,header=header,silent=silent
	;
	; initialize
	pre = 'KCWI_READ_RAW'
	version = repstr('$Revision: 1.4 $ $Date: 2015/02/21 00:18:38 $','$','')
	;
	; check input
	if kcwi_verify_ppar(ppar,/init) ne 0 then return,-1
	;
	; create file name
	fspec = ppar.rawdir + ppar.froot + $
		string(imgno,form='(i0'+strn(ppar.fdigits)+')')+'.fit*'
	flist = file_search(fspec,count=nf)
	;
	; check results
	if nf ne 1 then begin
		print,pre+': Error - file not found or ambiguous: ',imgno
		header=''
		return,-1
	endif
	im = mrdfits(flist[0],0,header,silent=silent,/unsigned)
	;
	return,im
end

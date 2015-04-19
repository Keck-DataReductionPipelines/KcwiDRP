function timestr,secs,time=time
;+
;	TIMESTR - create a time stamp string of YYMMDD
;
; USEAGE:
;
;	timestr,secs
;
; INPUTS:
;
;	secs	- seconds since 1, January, 1970, if missing use current time
;
; KEYWORDS:
;
;	time	- add time to stamp
;
; RETURNS:
;
;	string of YYMMDD for time stamping
;
; HISTORY:
;
;	071019 jdn - initial revision
;-
if n_params(0) le 0 then $
	str = systime(0) $
else	str = systime(0,secs)
;
mos = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

for i=0,1 do mo = gettok(str,' ')

t=where(strpos(mos,mo) ge 0, n)
if n eq 1 then begin
	mstr = string(t(0)+1,format='(i02)')
endif else begin
	print,'Illegal month: ',mo
	return,''
endelse

day = fix(gettok(str,' '))
dstr= string(day,format='(i02)')

tstr= gettok(str,' ')

ystr = gettok(str,' ')

ystr = strmid(ystr,2,2)
;
if keyword_set(time) then $
	return,ystr+mstr+dstr+':'+tstr $
else	return,ystr+mstr+dstr
end

function struct_init, A, comments=comments
;+
; struct_init - initialize structure based on values in *__define.pro
;-
names = tag_names(A)
comments = strarr(n_tags(A))
sname = strlowcase(tag_names(A,/structure_name))+'__define'
resolve_routine,sname,/no_recompile
info = routine_info(sname,/source)
openr,il,info.path,/get_lun
while not eof(il) do begin
	irec=''
	readf,il,irec
	rec = gettok(irec,';')	; remove comments
	while strpos(rec,':') ge 0 do begin
		tag = strupcase(gettok(rec,':'))
		p   = where(strcmp(names,tag) eq 1, np)
		val = gettok(rec,',')
		if np le 0 then begin
			print,'Error - tag not found: ',tag
		endif else if np gt 1 then begin
			print,'Error - ambiguous tag: ',tag
		endif else begin
		    ;
		    ; stash comment
		    comments[p] = strtrim(irec,2)
		    ;
		    ; if not an array
		    if strpos(val,'[') lt 0 and strpos(val,'(') lt 0 then begin
			;
			; process string
			if strpos(val,"'") ge 0 or $
			   strpos(val,'"') ge 0 then begin
				jnk = gettok(val,"'")
				A.(p) = gettok(val,"'")
			;
			; if not a string
			endif else begin
				A.(p) = val
			endelse
		    endif	; not an array
		endelse		; tag found
	endwhile		; strpos(rec,':') ge 0
endwhile			; not eof(il)
free_lun,il
;
return,A
end

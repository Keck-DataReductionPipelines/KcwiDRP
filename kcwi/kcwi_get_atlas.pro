;+
;	kcwi_get_atlas - derive which atlas to use based on kcfg
;
; INPUTS:
;	kcfg	- KCWI_CFG struct
;
; OUTPUTS:
;	atlas	- atlas spectrum fits file
;	atname	- atlas name
;-
pro kcwi_get_atlas,kcfg,atlas,atname
	;
	; setup
	pre = 'KCWI_GET_ATLAS'
	;
	; verify Kcfg
	if kcwi_verify_cfg(kcfg,/silent) ne 0 then begin
		print,pre+': Error - malformed KCWI_CFG struct'
		return
	endif
	;
	; Init
	atlas = 0
	atname = 0
	;
	; check Lamp 0
	if kcfg.lmp0stat eq 1 and kcfg.lmp0shst eq 1 then begin
		atname = strtrim(kcfg.lmp0nam,2)
		atlas = strlowcase(atname)+'.fits'
	endif else if kcfg.lmp1stat eq 1 and kcfg.lmp1shst eq 1 then begin
		atname = strtrim(kcfg.lmp1nam,2)
		atlas = strlowcase(atname)+'.fits'
	endif
	;
	return
end

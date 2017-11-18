;
; Copyright (c) 2014, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_DRP_BATCH
;
; PURPOSE:
;	This procedure will run the pipeline in batch mode.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_DRP_BATCH, DirList
;
; INPUTS:
;	DirList	- list of run directories (string array)
;
; KEYWORDS:
;	DARK		- set to run KCWI_STAGE2DARK (def: NO)
;	MINOSCANPIX	- set to minimum pixels required for overscan subtraction
;	LASTSTAGE	- set to the last stage you want run
;	ONESTAGE	- set to a single stage you want run (overrides LASTSTAGE)
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Runs pipeline in each directory specified in DirList.
;
; EXAMPLE:
;	KCWI_DRP_BATCH,['140527','140528','140529']
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-JUN-03	Initial version
;	2014-OCT-23	Added onestage keyword
;	2014-NOV-07	Added stage7std to nominal run
;	2017-NOV-18	Accounting for re-structure
;-
pro kcwi_drp_batch,dirlist,dark=dark, $
	minoscanpix=minoscanpix, $
	laststage=laststage, $
	onestage=onestage
;
; check keywords
if keyword_set(laststage) then $
	last = laststage $
else	last = 8
;
if keyword_set(onestage) then $
	one = onestage $
else	one = 0
;
; how many directories?
ndir = n_elements(dirlist)
;
; get defaults from KCWI_PPAR struct
A = {kcwi_ppar}
ppar = struct_init(A)
;
; loop over directories
for i=0,ndir-1 do begin
	cd,dirlist[i]
	print,dirlist[i]
	;
	; check for one stage
	if one gt 0 then begin
		case one of
			1: kcwi_stage1
			2: kcwi_stage2dark
			3: kcwi_stage3geom
			4: kcwi_stage4flat
			5: kcwi_stage5sky
			6: kcwi_stage6cube
			7: kcwi_stage7dar
			8: kcwi_stage8std
			else: print,'Illegal stage: ',one
		endcase
	;
	; otherwise run up to last stage
	endif else begin
		;
		; archive any existing output directory
		filestamp,ppar.reddir,/verbose
		;
		; make a new output directory
		spawn,'mkdir '+ppar.reddir
		;
		; get the pipeline ready
		kcwi_prep,/verbose,/display,minoscanpix=minoscanpix
		;
		; do basic ccd image reduction
		kcwi_stage1
		if last le 1 then goto,done
		;
		; if requested do dark subtraction
		if keyword_set(dark) then $
			kcwi_stage2dark
		if last le 2 then goto,done
		;
		; do geometry solution
		kcwi_stage3geom
		if last le 3 then goto,done
		;
		; do flat correction
		kcwi_stage4flat
		if last le 4 then goto,done
		;
		; do sky subtraction
		kcwi_stage5sky
		if last le 5 then goto,done
		;
		; do cube generation
		kcwi_stage6cube
		if last le 6 then goto,done
		;
		; do DAR correction
		kcwi_stage7dar
		if last le 7 then goto,done
		;
		; do standard star calibration
		kcwi_stage8std
		;
		; done
		done:
	endelse
	;
	; return to where we started
	cd,'..'
endfor	; loop over directories
;
return
end

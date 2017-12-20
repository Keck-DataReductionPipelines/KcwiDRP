;
; Copyright (c) 2017 California Institute of Technology.  All rights reserved.
;
;+
; NAME:
;	KCWI_EXTRACT
;
; MODIFICATION HISTORY:
;	Written by James D. Neill (neill@caltech.edu)
;	20-DEC-2017	Initial Version
;-
pro kcwi_extract, cubef
;
; setup
pre = 'KCWI_EXTRACT'
A = {kcwi_ppar}
ppar = struct_init(A)
ppar.initialized = 1
ppar.progid = pre
;
; read in cube
cub = mrdfits(cubef,0,hdr,/silent)
sz = size(cub,/dim)
;
; check it
if n_elements(sz) ne 3 then begin
	kcwi_print_info,ppar,pre,'Not a 3D cube',/error
	return
endif
if not sxpar(hdr,'DARCOR') then begin
	kcwi_print_info,ppar,pre,'Cube is not DAR corrected',/warning
endif
;
; get pixel scales
dpx = 3600.d0 * sxpar(hdr,'PXSCL')
dsl = 3600.d0 * sxpar(hdr,'SLSCL')
;
; field-of-view
xasec = fix( sz[0] * dsl + 0.5 )
yasec = fix( sz[1] * dpx + 0.5 )
xsize = fix( xasec/0.05 )
ysize = fix( yasec/0.05 )
;
; rectify cube
cube = congrid(cub, xasec/0.05, yasec/0.05, sz[2])
;
; display it
atv,cube
;
return
end

; $Id: kcwi_map_ccd.pro,v 1.3 2014/01/08 01:32:02 neill Exp $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	KCWI_MAP_CCD
;
; PURPOSE:
;	This procedure reads the CCD geometry from the input raw header and
;	converts these to IDL (0-biased) ranges.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (KCWI).
;
; CALLING SEQUENCE:
;	KCWI_MAP_CCD, Hdr, Asec, Bsec, Csec, Tasec
;
; INPUTS:
;	Hdr	- FITS header for raw KCWI image
;
; KEYWORDS:
;	NAMPS	- returns the number of amplifiers used to read out the CCD
;	TRIMMED_SIZE	- returns the dimensions of the trimmed CCD image
;	VERBOSE	- set to get extra screen output
;
; OUTPUTS:
;	Asec	- Array of pixel limits for each amplifier section
;	Bsec	- Array of pixel limits for each overscan section
;	Csec	- Array of pixel limits for each CCD section
;	Tsec	- Array of pixel limits for each amplifier after trimming
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;	Uses FITS keyword NVIDINP to determine how many amplifiers were used
;	to read out the CCD.  Then reads the corresponding ASECn, BSECn, and
;	CSECn keywords, where n is the amplifier number.  The indices are
;	converted to IDL (0-biased) indices and an array is constructed for
;	each of the three useful sections of the CCD as follows:
;	
;	Asec[0,0,0] - First amp, x lower limit
;	Asec[0,0,1] - First amp, x upper limit
;	Asec[0,1,0] - First amp, y lower limit
;	Asec[0,1,1] - First amp, y upper limit
;	Asec[1,0,0] - Second amp, x lower limit
;	etc.
;
;	Asec is the region of the raw CCD image that was read out with the
;	given amplifier and includes pre and post (over) scan data.  This is
;	used to perform gain correction on a per-amplifier basis.
;
;	Bsec is the full overscan region for the given amplifier and is used
;	to calculate and perform the overscan subtraction.
;
;	Csec is the full CCD region for the given amplifier and is used to
;	trim the image after overscan subtraction has been performed.
;
;	Tsec is the same as Asec but accounts for trimming the image
;	according to Csec.
;
;	Amps are assumed to organized as follows:
;	
;	(0,ny)	--------- (nx,ny)
;		| 3 | 4 |
;		---------
;		| 1 | 2 |
;	 (0,0)	--------- (nx, 0)
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-13	Initial version
;-
pro kcwi_map_ccd, hdr, asec, bsec, csec, tsec, $
	namps = namps, $
	trimmed_size=trimmed_size, $
	verbose=verbose
	;
	; setup
	pre = 'KCWI_MAP_CCD'
	;
	; check inputs
	namps = sxpar(hdr,'NVIDINP')
	if namps le 0 then begin
		print,pre+': Error - no amplifiers found, check NVIDINP'
		return
	endif
	;
	; set up outputs
	asec = intarr(namps,2,2)
	bsec = asec
	csec = asec
	tsec = asec
	;
	; get original image dimensions
	nx = sxpar(hdr,'NAXIS1')
	ny = sxpar(hdr,'NAXIS2')
	;
	; loop over amps
	for i = 0, namps-1 do begin
		;
		; ASEC
		as = kcwi_parse_imsec(sxpar(hdr,'ASEC'+strn(i+1)))
		asec[i,0,0] = (as[0] - 1) > 0
		asec[i,0,1] = (as[1] - 1) < (nx-1)
		asec[i,1,0] = (as[2] - 1) > 0
		asec[i,1,1] = (as[3] - 1) < (ny-1)
		;
		; BSEC
		bs = kcwi_parse_imsec(sxpar(hdr,'BSEC'+strn(i+1)))
		bsec[i,0,0] = (bs[0] - 1) > 0
		bsec[i,0,1] = (bs[1] - 1) < (nx-1)
		bsec[i,1,0] = (bs[2] - 1) > 0
		bsec[i,1,1] = (bs[3] - 1) < (ny-1)
		;
		; CSEC
		cs = kcwi_parse_imsec(sxpar(hdr,'CSEC'+strn(i+1)))
		csec[i,0,0] = (cs[0] - 1) > 0
		csec[i,0,1] = (cs[1] - 1) < (nx-1)
		csec[i,1,0] = (cs[2] - 1) > 0
		csec[i,1,1] = (cs[3] - 1) < (ny-1)
		;
		; TSEC
		case i of
			0: begin
				tsec[i,0,0] = 0
				tsec[i,0,1] = csec[i,0,1] - csec[i,0,0]
				tsec[i,1,0] = 0
				tsec[i,1,1] = csec[i,1,1] - csec[i,1,0]
			end
			1: begin
				tsec[i,0,0] = tsec[0,0,1] + 1
				tsec[i,0,1] = tsec[i,0,0] + $
					(csec[i,0,1] - csec[i,0,0])
				tsec[i,1,0] = 0
				tsec[i,1,1] = csec[i,1,1] - csec[i,1,0]
			end
			2: begin
				tsec[i,0,0] = 0
				tsec[i,0,1] = csec[i,0,1] - csec[i,0,0]
				tsec[i,1,0] = tsec[0,1,1] + 1
				tsec[i,1,1] = tsec[i,1,0] + $
					(csec[i,1,1] - csec[i,1,0])
			end
			3: begin
				tsec[i,0,0] = tsec[0,0,1] + 1
				tsec[i,0,1] = tsec[i,0,0] + $
					(csec[i,0,1] - csec[i,0,0])
				tsec[i,1,0] = tsec[0,1,1] + 1
				tsec[i,1,1] = tsec[i,1,0] + $
					(csec[i,1,1] - csec[i,1,0])
			end
			else: begin
				print,'Error - should not get here!'
				return
			end
		endcase
		trimmed_size = [max(tsec[*,0,*])+1,max(tsec[*,1,*])+1]
	endfor
	;
	return
end

pro rdfits1dspec,ifil,wave,flux,hdr,verbose=verbose, $
	wavezero=wavezero, deltawave=deltawave, refpix=refpix
;
	flux=mrdfits(ifil,0,hdr,/silent)
	s  = size(flux)
	wavezero	= sxpar(hdr,'CRVAL1')
	deltawave	= sxpar(hdr,'CDELT1')
	refpix		= sxpar(hdr,'CRPIX1') - 1.0	; IDL's zero bias
	if deltawave le 0 then deltawave = sxpar(hdr,'CD1_1')
	if keyword_set(verbose) then $
		print,'CRPIX,CRVAL1,CDELT1: ',refpix,wavezero,deltawave
	wave = wavezero + (findgen(s[1])-refpix)*deltawave
	return
end

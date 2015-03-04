function wavetopx, wave, wavezero, deltawave, refpix
	;
	; WCS linear dispersion
	if n_params(0) ge 4 then $
		px0 = refpix $
	else	px0 = 0
	return, (wave - wavezero)/deltawave + px0
end

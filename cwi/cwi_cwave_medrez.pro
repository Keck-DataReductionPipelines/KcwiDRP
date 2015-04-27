function cwi_cwave_medrez,camera,grating,verbose=verbose,disp

cam_enc = camera*1.0
grat_enc = grating*1.0
if keyword_set(verbose) then talk=1 else talk = 0 


; camera end of travel one angle
cam_end = 57.0; degrees
cam_enc_end = -1710600.0

; camera end of travel angle two
cam_start = 92.0; degrees
cam_enc_start = 280000.0 

; grating scale
grat_scale = 2000.0 ; counts/degree

; grating groove density
rho = 1.2 ; lines/um

; binning
bin = 2.0

;pix size
pix = 0.015; mm

; cam focal length
fcam = 305.0; mm 

; camera travel angle range
cam_range = cam_start-cam_end

; effective radius at encoder/step value
cam_radius = (cam_enc_start-cam_enc_end)/2./tan(cam_range/2/!radeg)

; total alpha + beta
alphaplusbeta = (cam_start+cam_end)/2./!radeg
extra = atan((cam_enc-(cam_enc_start+cam_enc_end)/2)/cam_radius)
alphaplusbeta += extra

;if talk then message,"a+b = "+string(alphaplusbeta*!radeg,"(f5.1)")+" deg.",/info


; grating is a mirror for cam_enc = -1710600, grat_enc = -202750.0.
grat_enc_mirror = -202750.

; from this and the angle at cam_end we can get the alpha
alpha = (cam_end/2. +(grat_enc-grat_enc_mirror)/grat_scale)/!radeg
if talk then message,"a = "+string(alpha*!radeg,"(f5.1)")+" deg.",/info

; from which we can get beta
beta = alphaplusbeta-alpha
if talk then message,"b = "+string(beta*!radeg,"(f5.1)")+" deg.",/info

; and a central wavelength
cwave = (sin(beta)-sin(alpha))/rho*1e4
if talk then message,"cwave = "+string(cwave,"(f6.1)")+" A",/info

; compute a dispersion for s+g. 
disp = cos(beta)/fcam/rho*pix*bin*1e4
if talk then message,"disp = "+string(disp,"(f6.4)")+" A/pix",/info

return, cwave
end; cwave_medrez;

;+
; NAME:
;    hstjit
; PURPOSE: (one line)
;    Read and interpret HST jitter information.
; DESCRIPTION:
;    See Tiny Tim User's Manual, Version 6.3, for details.
; CATEGORY:
;    Miscellaneous
; CALLING SEQUENCE:
;    hstjit, image, xsrc_in, ysrc_in, time, deltax, deltay,
;    undistortedx, undistortedy, distortedx, distortedy,
;    instr, path=in_path, DISTORT=distort
;
; INPUTS:
;    IMAGE - image name to be fit, must be a string
;    XSRC_IN  - x coordinate of object of interest
;    YSRC_IN  - y coordinate of object of interest
;
; OPTIONAL INPUT PARAMETERS:
;    PATH     - String, this is the name of the directory where the data are
;                stored.  The actual data directory used is PATH+'/'+image.
;                The default is '' (blank) and the file would be IMAGE
;                which would permit putting a leading path on the root.
;
; KEYWORD PARAMETERS:
;    DISTORT - The input x,y are taken to be in an undistorted reference
;              plane and will will be converted to the instrumental (distorted)
;              frame.
;
; OUTPUTS:
;    time    - intervals of jitter from start of observation [in seconds]
;    deltax  - delta x-axis jitter in pixels
;    deltay  - delta y-axis jitter in pixels
;    undistortedx  - x coordinate with jitter, undistorted reference frame
;    undistortedy  - y coordinate with jitter, undistorted reference frame
;    distortedx  - x coordinate with jitter, distorted reference frame
;    distortedy  - y coordinate with jitter, distorted reference frame
;    instr   - record of which instrument was used for this observation
;    rawjitPixelsV2  - raw jitter in pixels, v2 axis
;    rawjitPixelsV3  - raw jitter in pixels, v3 axis
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;    Assumes jitter file is in the same directory as the image file.
;
; PROCEDURE:
;
; NOTES: The timestep in the jitter file is one value over 3 seconds
;    Currently does not do distortion for ACS images
;
; MODIFICATION HISTORY:
;    Written by S. D. Kern, STScI, September 25, 2007.
;    2007/10/18, SDK, added support for camera specifics ACS, WFPC2
;    2007/10/24, SDK, conversion of undistorted/distorted coordinates,
;        output in pixels    
;    2008/04/22, MWB, added support for compressed data files.  Fixed
;        bug on reading wrong input quantity from jitter files.
;-
pro hstjit, image, xsrc_in, ysrc_in, $
   time, deltax, deltay, undistortedx, undistortedy, distortedx, distortedy,$
   instr, rawjitPixelsV2, rawjitPixelsV3, $
   PATH=path, DISTORT=distort, JITPATH=jit_path, EXPTIME=exptime

   self='HSTJIT: '
   if badpar(image,7,0,CALLER=self+ '(image) ') then return
   if badpar(path,[0,7],0,CALLER=self+ '(PATH) ', $
                             default='') then return
   if badpar(jit_path,[0,7],0,CALLER=self+ '(JITPATH) ', $
                             default='') then return
   if badpar(distort, [0,1,2,3], 0, caller=self+ '(DISTORT) ', $
             default=0) then return

   if path     ne '' then path    = addslash(path)
   if jit_path ne '' then jitpath = addslash(jit_path)

   ; read the image header
   tag=''
   compress=0
   if not exists(path+image) then begin
      tag='.gz'
      compress=1
      if not exists(path+image+tag) then begin
         print,self,path+image+tag,' not found.'
      endif
   endif
   imghdr = headfits(path+image+tag,compress=compress)
   instr = sxpar(imghdr,'INSTRUME')
   instr = STRTRIM(STRUPCASE(instr),2)

   ; get information from the image header about the observation 
   ; (maybe these values are unnecessary,leave here at the moment)
   ratarg = sxpar(imghdr,'RA_TARG')
   dectarg = sxpar(imghdr,'DEC_TARG')
   expstart = sxpar(imghdr, 'EXPSTART')
   exptime = sxpar(imghdr, 'EXPTIME')

   ; read in the jitter file and the header of the image
   if instr eq 'ACS' then begin
      jroot = strtrim(sxpar(imghdr,'ASN_TAB'),2)
      jitfile = strmid(jroot,0,9)+'_jit.fits'
      orientat = sxpar(imghdr,'ORIENTAT')
      pa_v3 = sxpar(imghdr,'PA_V3')
   endif

   if instr eq 'WFPC2' then begin
      jroot = strtrim(sxpar(imghdr,'ROOTNAME'),2)
      jitfile = strmid(jroot,0,8)+'j_jit.fits'
      orientat = sxpar(imghdr,'ORIENTAT')+15.0
      pa_v3 = sxpar(imghdr,'PA_V3')
   endif
orientat -= 15.0
;orientat *= -1.0
print,'PA_V3',pa_v3
print,'ORIENTAT',orientat

   orientat_r = orientat / !radeg
   pa_v3_r    = pa_v3 / !radeg

   fxbopen, lun, addslash(jitpath)+jitfile, 1, hdr

   ; contents of jitter file are documented at
   ;  http://archive.eso.org/archive/hst/observation_log/OL_10.html
   ; The header tells you which column to read to get a value
   ; select the V2 and V3 rms values from the jitter ball 
   ;  1 - Seconds     8 - SI_V2_P2P  15 - LimbAng     22 - Mag_V3
   ;  2 - V2_dom      9 - SI_V3_AVG  16 - TermAng     23 - BrightLimb
   ;  3 - V3_dom     10 - SI_V3_RMS  17 - LOS_Zenith  24 - FGS_flags
   ;  4 - V2_roll    11 - SI_V3_P2P  18 - Latitude    25 - DayNight
   ;  5 - V3_roll    12 - RA         19 - Longitude   26 - Recenter
   ;  6 - SI_V2_AVG  13 - DEC        20 - Mag_V1      27 - TakeData
   ;  7 - SI_V2_RMS  14 - Roll       21 - Mag_V2      28 - SlewFlag
   fxbread, lun, time, 1     
   fxbread, lun, ra, 12     ; in degrees
   fxbread, lun, dec, 13    ; in degrees

   ra_r  = ra / !radeg
   dec_r = dec / !radeg

   ; reference to first point
   delta_ra  = (ra_r - ra_r[0])/cos(dec_r)
   delta_dec = (dec_r - dec_r[0])

   delta_ra = delta_ra*!radeg*3600.0
   delta_dec = delta_dec*!radeg*3600.0

   ; do calculations for WFPC2
   if instr eq 'WFPC2' then begin

      rawjitPixelsV2 = (ra-ra[0])*3600.0/0.0457
      rawjitPixelsV3 = (dec-dec[0])*3600.0/0.0457

   ; pixel scale numbers from Anderson & King 2003 (PASP 115,p.113-131), Table 3
      deltax = -1.0*( delta_ra*cos(orientat_r)-delta_dec*sin(orientat_r))/0.045729
      deltay =      ( delta_ra*sin(orientat_r)+delta_dec*cos(orientat_r))/0.045729
;      deltax = -1.0*( delta_ra*cos(pa_v3_r)+delta_dec*sin(pa_v3_r))/0.045729
;      deltay =      (-delta_ra*sin(pa_v3_r)+delta_dec*cos(pa_v3_r))/0.045729

   ; coordinates in undistorted reference frame
      undistortedx = xsrc_in+deltax
      undistortedy = ysrc_in+deltay

   ; get distorted coordinates
      if distort gt 0 then begin
         wfpc2_metric,undistortedx,undistortedy,distortedx,distortedy,1
      endif
   endif

   ; do calculations for ACS/HRC --  kind of a placeholder, not complete
   ;  for distortion.
   if instr eq 'ACS' then begin

      rawjitPixelsV2 = SI_V2_AVG/0.02855
      rawjitPixelsV3 = SI_V3_AVG/0.02471

   ; pixel scale numbers from ACS handbook
      deltax = delta_ra*cos(orientat_r)/0.02843
      deltay = delta_dec*sin(orientat_r)/0.02484

   ; coordinates in undistorted reference frame
      undistortedx = xsrc_in+deltax
      undistortedy = ysrc_in+deltay

   ; get distorted coordinates
      if distort gt 0 then begin
      endif
   endif

   free_lun,lun

;print,SI_V2_AVG

;asdf

end




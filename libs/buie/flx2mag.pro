;+
; NAME:
;     flx2mag
; PURPOSE: (one line)
;     Convert from flux units to magnitudes with errors.
; DESCRIPTION:
; CATEGORY:
;     Photometry
; CALLING SEQUENCE:
;     flx2mag,flux,fluxerr,mag,magerr
; INPUTS:
;     flux    - Flux values for the magnitudes.
;     fluxerr - Uncertainties on the fluxes.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     ZEROPT  - Magnitude that corresponds to a flux of 1. (default=0)
; OUTPUTS:
;     mag    - Magnitudes (set to 99.9 if flux <= 0).
;     magerr - Uncertainties on the magnitudes (set to 9.9 if flux <= 0).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2007/10/29, MWB, modifications to make it more error tolerant
;-
pro flx2mag,flux,fluxerr,mag,magerr,ZEROPT=zeropt

   self='FLX2MAG: '
   if badpar(flux,[2,3,4,5],[0,1,2],caller=self+'(flux) ') then return
   if badpar(fluxerr,[2,3,4,5],[0,1,2],caller=self+'(fluxerr) ') then return
   if badpar(zeropt,[0,2,3,4,5],0,caller=self+'(ZEROPT) ', $
                                  default=0.0) then return

   mag=flux
   magerr=fluxerr

   zg=where(flux gt 0.,countg)
   if countg gt 0 then begin
      mag[zg] = zeropt - 2.5 * alog10(flux[zg])
      magerr[zg] = fluxerr[zg]/flux[zg]*2.5*alog10(exp(1.0))
   endif

   zb=where(flux le 0.,countb)
   if countb gt 0 then begin
      mag[zb] = 99.9
      magerr[zb] = 9.9
   endif

end

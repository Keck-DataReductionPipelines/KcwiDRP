;+
; NAME: 
;   salb
; PURPOSE: 
;   Compute single particle scattering albedo.
; DESCRIPTION:
; CATEGORY:
;   Mathematical
; CALLING SEQUENCE:
;   salb,radius,abscoef,se,si,w
; INPUTS:
;   radius - Radius of particle, same units as 1/abscoef
;   abscoef - absorption coefficient of material, units 1/radius
;   se      - external scattering efficiency
;   si      - internal scattering efficiency
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;   w       - single scattering albedo
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1995 May 13
;-
pro salb,radius,abscoef,se,si,w
   radeff = 2.0/3.0*(1.0-se)/(1.0-si)*radius
   tau    = radeff * abscoef
   w = 1.0/(1.0+tau)
end

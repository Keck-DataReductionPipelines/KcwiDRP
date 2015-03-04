;+
; NAME:
;  rdland2
; PURPOSE: (one line)
;  Read the Combined Landolt Standard system photometry file.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdland2,name,mags,codes
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     FILE    - Name of star catalog file to read.  Default is:
;                 '/net/frakir/raid/buie/photometry/landolt/landphot.dat'
;
; OUTPUTS:
;     name   - Name of the star.
;     mags   - Stellar magnitudes, UBVRI
;     codes  - Quality codes:
;               0 - not enough measurements for use as a quality standard.
;               1 - good to use as standard
;               2 - Known or suspected variable, don't ever use.
; COMMON BLOCKS:
;     None.
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/22, Written by Marc W. Buie, Lowell Observatory
;  2004/02/09, MWB, changed path to default catalog
;-
pro rdland2,name,mags,codes,FILE=file

if n_params() eq 0 then begin
   ;Show the calling sequence.
   print, 'rdland2,name,mags,codes,FILE=file'
   return
endif

if badpar(file,[0,7],0,CALLER='RDLAND2: (file) ', $
      default='/net/frakir/raid/buie/photometry/landolt/landphot.dat') then return

rdphocat,file,name,mags,codes,filname,nfil

end

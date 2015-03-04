;+
; NAME:
;	readtil
; PURPOSE: (one line)
;	Read and return the two images in a MaxEnt tile map file.
; DESCRIPTION:
; CATEGORY:
;       Miscellaneous
; CALLING SEQUENCE:
;	readtil,filename,pluto,charon
; INPUTS:
;	filename - String containing the filename to be read from disk.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	pluto  - The first map found in the file (Pluto) (1-d vector).
;	charon - The second map found in the file (Charon) (1-d vector).
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1991 Sep 27.
;-

pro readtil,filename,pluto,charon
   nannuli=0
   nelements=0
   str=string(80)
   openr,1,filename
   readf,1,str
   readf,1,nannuli,nelements
   pluto=fltarr(nelements)
   readf,1,pluto
   readf,1,nannuli,nelements
   charon=fltarr(nelements)
   readf,1,charon
   close,1
end

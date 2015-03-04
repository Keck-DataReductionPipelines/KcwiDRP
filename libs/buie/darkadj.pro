;+
; NAME:
;  darkadj
; PURPOSE:
;  Adjust a superdark CCD calibration frame by a multiplicative constant.
; DESCRIPTION:
;
;  This is a kludge routine that is needed for one of my CCD cameras.  It
;    _always_ generates a superdark frame that is too hot when used for
;    correcting normal stare-mode images.
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  darkadj,indark,factor
; INPUTS:
;  indark - String containing file name of dark image (FITS format) to adjust
;  factor - Multiplicative factor for adjustment
;               newdark = factor * indark
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  The adjusted dark frame is written to the file, indark+'a'
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1999 May 12
;-
pro darkadj,indark,factor
   raw = readfits(indark,hdr,/silent)
   dark = factor*raw
   writefits,indark+'a',dark,hdr
end

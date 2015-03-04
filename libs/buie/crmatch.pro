;+
; NAME:
;    crmatch
; PURPOSE: (one line)
;    Create a standard name --- non-standard name correspondence file.
; DESCRIPTION:
;    Creates, from a photometry log file (alternate format) and a standard
; name --- non-standard name correspondence file, a new standard name ---
; non-standard name correspondence file.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;    crmatch, in_logfile, in_namesfile, out_namesfile
; INPUTS:
;    in_logfile    : Photometry log file (alternate format).
;    in_namesfile  : Any existing correspondence file.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    out_namesfile : The new correspondence file.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;    Calls external routines badpar, rdphalt and exists.
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, September 30, 1993.
;    10/15/93, DWL, Added existence checks for the input file names and
; type validation for all parameters.
;    5/11/94, DWL, Changed call to rdphotalt to rdphalt.
;-
PRO crmatch, in_logfile, in_namesfile, out_namesfile

IF N_PARAMS() NE 3 THEN BEGIN
   ;Show the calling sequence.
   PRINT, 'crmatch, in_logfile, in_namesfile, out_namesfile'
   RETURN
ENDIF

;Check the parameter types -- Should be string scalars.
IF badpar( in_logfile, 7, 0, CALLER='CRMATCH: (in_logfile) ' ) THEN RETURN
IF badpar( in_namesfile, 7, 0, CALLER='CRMATCH: (in_namesfile) ' ) THEN RETURN
IF badpar( out_namesfile, 7, 0, CALLER='CRMATCH: (out_namesfile) ' ) THEN RETURN

IF NOT exists( in_logfile ) THEN BEGIN
   MESSAGE, 'Photometry log file ' + in_logfile + ' does not exist.', $
   /INFO
   RETURN
ENDIF

IF NOT exists( in_namesfile ) THEN BEGIN
   MESSAGE, 'Names file ' + in_namesfile + ' does not exist.', $
   /INFO
   RETURN
ENDIF

rdmatch, in_namesfile, proper, informal

photprmt,in_logfile

rdphalt, in_logfile, dummy, objnames

uniq_objnames = objnames[ UNIQ( objnames, SORT( objnames ) ) ]

n = N_ELEMENTS( uniq_objnames )
uniq_codes = REPLICATE( '', n )

FOR j = 0, n-1 DO BEGIN
   w = WHERE( informal EQ uniq_objnames[j], count )
   IF count GT 1 THEN BEGIN
      MESSAGE, 'Input names file duplicate entry: ' + uniq_objnames[j], /INFO
      RETURN
   ENDIF
   IF count EQ 1 THEN BEGIN
      uniq_codes[j] = proper[w]
   ENDIF
ENDFOR

wrmatch, uniq_codes, uniq_objnames, out_namesfile

END

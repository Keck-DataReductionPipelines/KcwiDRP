;+
; NAME:
;    rdkeylis
; PURPOSE: (one line)
;    Read a correspondence list file into arrays.
; DESCRIPTION:
;
; CATEGORY:
;    File I/O
; CALLING SEQUENCE:
;    rdkeylis, filnam, stdkeywords, flags, hdrkeywords
; INPUTS:
;    filnam      : The correspondence file to be read.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    stdkeywords : List of standard kewwords.
;    flags       : List of flags.
;    hdrkeywords : List of header keywords (and values).
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, November 18, 1993.
;  2000/11/08, MWB, removed use of obsolete ()
;-
PRO rdkeylis, in_filnam, out_stdkeywords, out_flags, out_hdrkeywords

IF N_PARAMS() EQ 0 THEN BEGIN
   PRINT, 'rdkeylis, filnam, stdkeywords, flags, hdrkeywords'
   RETURN
ENDIF

IF badpar( in_filnam, 7, 0, CALLER='% RDCORRL (filnam) ' ) THEN RETURN

IF NOT exists( in_filnam ) THEN BEGIN
   MESSAGE, 'Error. File ' + in_filnam + ' does not exist.', /INFO
ENDIF

ON_IOERROR, oops

GET_LUN, lu
OPENR, lu, in_filnam

line = ''
f1 = [ '' ]
f2 = [ '' ]
f3 = [ '' ]

WHILE NOT EOF( lu ) DO BEGIN
   READF, lu, line
   f1 = [ f1, GETTOK( line, ' ' ) ]
   f2 = [ f2, GETTOK( line, ' ' ) ]
   f3 = [ f3, GETTOK( line, ' ' ) ]
ENDWHILE

CLOSE, lu
FREE_LUN, lu

l = N_ELEMENTS( f1 )
IF l GT 1 THEN BEGIN
   out_stdkeywords = f1[ 1 : l-1 ]
   out_flags = f2[ 1 : l-1 ]
   out_hdrkeywords = f3[ 1 : l-1 ]
ENDIF ELSE BEGIN
   MESSAGE, 'Error. File ' + in_filnam + ' is empty.', /INFO
   RETURN
ENDELSE

RETURN

oops:
; Come here if there is a file read error.
MESSAGE, 'Error reading file ' + in_filnam, /INFO

END

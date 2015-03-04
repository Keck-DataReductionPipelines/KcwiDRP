;+
; NAME:
;    gxpar
; PURPOSE: (one line)
;    General purpose FITS file header keyword extraction.
; DESCRIPTION:
;   This procedure is a 'front-end' to SXPAR and may be used to extract the
; value of a keyword from a FITS file header.
;   Because there is so much variation of the keywords found in FITS file
; headers, the idea is to have and use a set of 'standard' keywords in
; programs that load and display FITS files.
;   To facilitate this, gxpar uses a correspondence list to associate a
; standard keyword with a keyword found in a given FITS file header (or with
; a value different from that which might be in the header).
;   This correspondence list consists of three linear arrays of the same
; length.  The first array contains standard keywords, the second contains
; flags which indicate the type of correspondence, and the third contains
; the items corresponding to the standard names in the first array:
;
;           stdkeywords   flags   hdrkeywords
;
;   The flags are single letters from the set 'K', 'D', 'F', 'L', or 'S',
; indicating a keyword, floating, double, longword, or string, respectively.
; Flags having the letter 'K' associate standard keywords with header keywords.
; All other flag letters associate standard keywords with 'direct' values.
;
;   Standard keywords in use are:
;           airmass
;           date
;           exptime
;           filter
;           jd
;           mjd
;           object
;           time
;
;   Examples:
;      airmass k  airmass
;      airmass f  1.73
;      filter  k  filtnam1
;      filter  s  f555w
;
;   Note: This procedure treats the correspondence list as case-insensitive,
; except for the third field of a record having a flag of 'S'.
;
; CATEGORY:
;    File I/O
; CALLING SEQUENCE:
;    gxpar, header, keyword, stdkeywords, flags, hdrkeywords, value, error
; INPUTS:
;    header      : A FITS file header array (as returned from READFITS).
;    keyword     : The standard keyword whose value is to be obtained.
;    stdkeywords : List of standard keywords (first correspondence field).
;    flags       : List of flags (second correspondence field).
;    hdrkeywords : List of header keywords (and values) (third correspondence
;                  field).
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    SILENT = If set, suppresses all message output.
; OUTPUTS:
;    value : The returned value.
;    error : Non-zero, if an error is encountered.  Incremented on each call
;            which detects an error.  The caller should set this to zero before
;            any sequence of calls to this procedure.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Doug Loucks, Lowell Observatory, November, 1993.
;  2000/11/08, MWB, removed use of !err
;-
PRO gxpar, in_header, in_keyword, in_stdkeywords, in_flags, in_hdrkeywords, $
         out_val, out_err, $
         SILENT=in_silent

IF N_PARAMS() EQ 0 THEN BEGIN
   ;Print the calling sequence.
   PRINT,'gxpar, header, keyword, stdkeywords, flags, hdrkeywords, value, error'
   RETURN
ENDIF

; Validate the required parameters.
IF badpar( in_header,      7, 1, CALLER='% GXPAR (header) ' ) THEN RETURN
IF badpar( in_keyword,     7, 0, CALLER='% GXPAR (keyword) ' ) THEN RETURN
IF badpar( in_stdkeywords, 7, [0,1], CALLER='% GXPAR (stdkeywords) ', $
   NPTS=l1 ) THEN RETURN
IF badpar( in_flags,       7, [0,1], CALLER='% GXPAR (flags) ', $
   NPTS=l2 ) THEN RETURN
IF badpar( in_hdrkeywords, 7, [0,1], CALLER='% GXPAR (hdrkeywords) ', $
   NPTS=l3 ) THEN RETURN
IF badpar( out_err,        [0,1,2,3], 0, CALLER='% GXPAR (error) ', $
   DEFAULT=0 ) THEN RETURN

; Check the input arrays for length equality.
lcheck = [ l1, l2, l3 ]
IF MAX( lcheck ) NE MIN( lcheck ) THEN BEGIN
   MESSAGE,  'Error. The input arrays are not equal in length.', /INFO
   RETURN
ENDIF

; Locate the keyword in the correspondence list.
keyword = STRUPCASE( in_keyword )
t = WHERE( keyword EQ STRUPCASE( in_stdkeywords ), count )
IF count EQ 0 THEN BEGIN
   msg = 'Error. Standard keyword ' + keyword + ' not in correspondence list.'
   MESSAGE, msg, /INFO
   out_err = out_err + 1
   RETURN
ENDIF

; The index needs to be a scalar to satisfy the use of the variable 'flag'
; in the case statement.
index = t[0]

; Extract flag and header keyword fields from the correspondence record.
flag    = STRUPCASE( in_flags[index] )
valu    = STRUPCASE( in_hdrkeywords[index] )

; If errors are detected, this will be set to a non-null string.
msg = ''

ON_IOERROR, oops

CASE flag OF
   'K' : BEGIN
      out_val = SXPAR( in_header, valu, count=count )
      IF count eq 0 THEN BEGIN
         msg = 'Error. Header keyword supplied for keyword ' + keyword + $
         ' not found.'
      ENDIF
   END
   'D' : BEGIN
      out_val = DOUBLE( valu )
   END
   'F' : BEGIN
      out_val = FLOAT( valu )
   END
   'L' : BEGIN
      out_val = LONG( valu )
   END
   'S' : BEGIN
      out_val = in_hdrkeywords[ index ]
   END
   ELSE : BEGIN
      msg = 'Error. Flag supplied for keyword ' + keyword + $
      ' must be K, D, F, L, or S.'
   END
ENDCASE

IF msg NE '' THEN BEGIN
   out_err = out_err + 1
   IF NOT KEYWORD_SET( in_silent ) THEN MESSAGE, msg, /INFO
ENDIF

RETURN

oops:
; Come here if there is a problem with the float, double, or longword
; conversion.
out_err = out_err + 1
msg = 'Error. Value supplied for keyword ' + keyword + ' is not a valid number.'
IF NOT KEYWORD_SET( in_silent ) THEN MESSAGE, msg, /INFO

END

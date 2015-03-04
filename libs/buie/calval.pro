;+
; NAME:
;    calval
; PURPOSE: (one line)
;    Validate overscan, cropping region, and calibration file settings.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;    calval, isize, csize, mlow, mhigh, errflg
; INPUTS:
;    isize  : Image size.
;    csize  : Calibration file size.
;    mlow   : Starting subscript in image.
;    nhigh  : Ending subscript in image.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    BANNER    : Banner line printed with error report (if SILENT=0).
;    CSIZELAB  : Label for csize parameter, if error condition is printed.
;    MLOWLAB   : Label for mlowlab parameter, if error condition is printed.
;    MHIGHLAB  : Label for mhighlab parameter, if error condition is printed.
;    DELTAMLAB : Label for deltamlab parameter, if error condition is printed.
;    SILENT    : If set, No error report is generated.
;    ISIZELAB  : Label for isize parameter, if error condition is printed.
; OUTPUTS:
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
;    Written by Doug Loucks, Lowell Observatory, April, 1994.
;-
PRO calval, in_isize, in_csize, in_mlow, in_mhigh, out_error, $
    BANNER=in_banner, CSIZELAB=in_csizelab, MLOWLAB=in_mlowlab, $
    MHIGHLAB=in_mhighlab, DELTAMLAB=in_deltamlab, ISIZELAB=in_isizelab, $
    SILENT=in_silent

IF N_PARAMS() NE 5 THEN BEGIN
   PRINT, 'calval, isize, csize, mlow, mhigh, errflg'
   RETURN
ENDIF

IF KEYWORD_SET( in_banner ) THEN BEGIN
   banner = in_banner
ENDIF ELSE BEGIN
   banner = 'Validation error(s) encountered:'
ENDELSE

IF KEYWORD_SET( in_isizelab  ) THEN isizelab=in_isizelab   ELSE isizelab=''
IF KEYWORD_SET( in_csizelab  ) THEN csizelab=in_csizelab   ELSE csizelab=''
IF KEYWORD_SET( in_mlowlab   ) THEN mlowlab=in_mlowlab     ELSE mlowlab=''
IF KEYWORD_SET( in_mhighlab  ) THEN mhighlab=in_mhighlab   ELSE mhighlab=''
IF KEYWORD_SET( in_deltamlab ) THEN deltamlab=in_deltamlab ELSE nmmlab=''

IF KEYWORD_SET( in_silent ) THEN spill = 0 ELSE spill = 1

out_error = 0
errtyp = 0

dm = in_mhigh - in_mlow + 1

IF (in_mlow LT 0) AND (in_mhigh LT 0) THEN BEGIN
   IF in_isize GT 0 THEN BEGIN
      IF in_csize GT 0 THEN BEGIN
         IF in_isize EQ in_csize THEN BEGIN
            ;NOP
         ENDIF ELSE BEGIN
            ;isize must equal csize.
            out_error = 1
            errtyp = 5
         ENDELSE
      ENDIF ELSE BEGIN
         ;NOP
      ENDELSE
   ENDIF ELSE BEGIN
      ; NOP
   ENDELSE
ENDIF ELSE BEGIN
   IF in_mlow GT in_mhigh THEN BEGIN
      ;mlow must be <= mhigh.
      out_error = 1
      errtyp = 1
   ENDIF ELSE BEGIN
      IF in_isize GT 0 THEN BEGIN
         IF in_csize GT 0 THEN BEGIN
            IF dm EQ in_csize THEN BEGIN
               IF in_mhigh LT in_isize THEN BEGIN
                  ;OK
               ENDIF ELSE BEGIN
                  out_error = 1
                  errtyp = 3
                  ;mhigh must be < isize.
               ENDELSE
            ENDIF ELSE BEGIN
               out_error = 1
               errtyp = 2
               ;deltam must equal csize.
            ENDELSE
         ENDIF ELSE BEGIN
            IF dm LE in_isize THEN BEGIN
               IF in_mhigh LT in_isize THEN BEGIN
                  ;OK
               ENDIF ELSE BEGIN
                  out_error = 1
                  errtyp = 3
                  ;mhigh must be < isize.
               ENDELSE
            ENDIF ELSE BEGIN
               out_error = 1
               errtyp = 4
               ;deltam must be <= isize.
            ENDELSE
         ENDELSE
      ENDIF ELSE BEGIN
         IF in_csize GT 0 THEN BEGIN
            IF dm EQ in_csize THEN BEGIN
               ;OK
            ENDIF ELSE BEGIN
               out_error = 1
               errtyp = 2
               ;deltam must equal csize.
            ENDELSE
         ENDIF ELSE BEGIN
            ;OK
         ENDELSE
      ENDELSE
   ENDELSE
ENDELSE

IF out_error AND spill THEN BEGIN
   fmt = '(I4)'
   PRINT, ''
   MESSAGE, banner + STRING( 7B ), /INFO

   IF errtyp EQ 1 THEN BEGIN
      ;mlow must be <= mhigh.
      MESSAGE, STRING( in_mlow,    FORMAT=fmt ) + ' = ' + mlowlab, /INFO
      MESSAGE, STRING( in_mhigh,    FORMAT=fmt ) + ' = ' + mhighlab, /INFO
      MESSAGE, mlowlab + ' must be <= ' + mhighlab, /INFO
   ENDIF

   IF errtyp EQ 2 THEN BEGIN
      ;deltam must equal csize.
      MESSAGE, STRING( in_csize,FORMAT=fmt ) + ' = ' + csizelab, /INFO
      MESSAGE, STRING( dm,      FORMAT=fmt ) + ' = ' + deltamlab, /INFO
      MESSAGE, deltamlab + ' must equal ' + csizelab, /INFO
   ENDIF

   IF errtyp EQ 3 THEN BEGIN
      ;mhigh must be < isize.
      MESSAGE, STRING( in_isize, FORMAT=fmt ) + ' = ' + isizelab, /INFO
      MESSAGE, STRING( in_mhigh,    FORMAT=fmt ) + ' = ' + mhighlab, /INFO
      MESSAGE, mhighlab + ' must be < ' + isizelab, /INFO
   ENDIF

   IF errtyp EQ 4 THEN BEGIN
      ;deltam must be <= isize.
      MESSAGE, STRING( in_isize, FORMAT=fmt ) + ' = ' + isizelab, /INFO
      MESSAGE, STRING( dm,      FORMAT=fmt ) + ' = ' + deltamlab, /INFO
      MESSAGE, deltamlab + ' must be <= ' + isizelab, /INFO
   ENDIF

   IF errtyp EQ 5 THEN BEGIN
      ;isize must equal csize.
      MESSAGE, STRING( in_isize, FORMAT=fmt ) + ' = ' + isizelab, /INFO
      MESSAGE, STRING( in_csize,FORMAT=fmt ) + ' = ' + csizelab, /INFO
      MESSAGE, isizelab + ' must equal ' + csizelab, /INFO
   ENDIF

ENDIF

END

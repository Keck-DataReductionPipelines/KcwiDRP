;+
; NAME:
;  loadkeys
; PURPOSE:
;  Load FITS header keyword correspondence list from file.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  loadkeys,keyfile,keylist
; INPUTS:
;  keyfile - string with file name to load, if '[[DEFAULT]]', doesn't load
;              a file but will return a default list.
;  keylist - string array
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  keylist - 3,n element string array.  For each key, there are three elements:
;                0 - "standard" key name
;                1 - item descriptor, K - keyword, T - template, V - value
;                2 - information (contents match what descriptor indicates)
;
; KEYWORD OUTPUT PARAMETERS:
; FOUNDIT - Flag, true if key file found, false if not.  It's up the to the
;            calling program to decide if this is a problem or not.
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
;  97/06/12, Written by Marc W. Buie, Lowell Observatory
;  97/07/21, MWB, added RA, DEC
;  2004/05/24, MWB, added FOUNDIT keyword
;  2007/10/02, MWB, changed DATETMPL in default list.
;-
pro loadkeys,keyfile,keylist,FOUNDIT=foundit

   self='LOADKEYS: '
   keylist = [ [ 'AIRMASS',  'K', 'AIRMASS'    ], $
               [ 'DATE',     'K', 'DATE-OBS'   ], $
               [ 'DATETMPL', 'T', 'YYYY-MM-DD' ], $
               [ 'EXPDELTA', 'V', '0.0'        ], $
               [ 'EXPTIME',  'K', 'EXPTIME'    ], $
               [ 'FILTER',   'K', 'FILTERS'    ], $
               [ 'FILENAME', 'K', 'CCDFNAME'   ], $
               [ 'OBJECT',   'K', 'OBJECT'     ], $
               [ 'RA',       'K', 'RA'         ], $
               [ 'DEC',      'K', 'DEC'        ], $
               [ 'EPOCH',    'K', 'EPOCH'      ], $
               [ 'UT',       'K', 'UT'         ]  ]

   foundit=1
   IF keyfile eq '[[DEFAULT]]' THEN return
   IF not exists(keyfile) THEN BEGIN
      print,self+'warning, '+keyfile[0]+' keyfile not found. Using defaults.'
      foundit=0
      return
   ENDIF

   readcol, keyfile, c1, c2, c3, FORMAT='a,a,a', /SILENT
   c1 = STRUPCASE( c1 )
   c2 = STRUPCASE( c2 )
   c3 = STRUPCASE( c3 )
   FOR j=0, N_ELEMENTS(c1)-1 DO BEGIN
      w = WHERE( keylist[0,*] EQ c1[j], count )
      IF count NE 0 THEN BEGIN
         keylist[1:2,w]=[c2[j],c3[j]]
      ENDIF ELSE BEGIN
         keylist = [ [ keylist ], [ c1[j], c2[j], c3[j] ] ]
      ENDELSE
   ENDFOR

end


;+
; NAME:
;  parsekey
; PURPOSE:
;  Extract FITS header information by parsing supplied keys
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  parsekey,hdr,keylist,info
; INPUTS:
;  hdr     - String array containing the FITS header to "read"
;  keylist - 3,n element string array.  For each key, there are three elements:
;                0 - "standard" key name
;                1 - item descriptor, K - keyword, T - template, V - value,
;                                     W - keyword but just take first word.
;                2 - information (contents match what descriptor indicates)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  info    - Anonymous structure containing information gleaned from header.
;             tags:
;                 airmass - float value for airmass as read from header
;                 jd      - Double, julian date of middle of exposure
;                 date    - String containing the UT date.
;                 ut      - String containing the UT start time of the image.
;                 expdelta - Time, in seconds between successive frames if
;                               the image is 3-d, not relevant otherwise
;                 exptime  - Exposure time in seconds.
;                 imfile   - Original filename of image.
;                 filter   - String that identifies the filter used.
;                                If you use a keyword that grabs more than
;                                one position (eg., FILT_*) then this field
;                                will contain information about all the filters
;                                in wheel order where the filter strings are
;                                separated by '+'.
;                 object   - String that identifies the object imaged.
;                 ra       - Right ascension of object (radians) (< 0 if invalid).
;                 dec      - Declination of object (radians) ( < -100 if invalid).
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
;   97/09/05, Written by Marc W. Buie, Lowell Observatory
;   2000/02/05, MWB, added kludge fix for new DATA-OBS "Y2K" format.
;  2002/09/03, MWB, changed Str_sep call to strsplit
;  2006/07/13, MWB, changed to support multiple filter wheel instruments
;  2006/08/09, MWB, changed to strip path from file name if found.
;-
pro parsekey,header,keylist,info,VERBOSE=verbose

   if badpar(header, 7,1,caller='PARSEKEY (header) ') then return
   if badpar(keylist,7,2,caller='PARSEKEY (keylist) ') then return
   if badpar(verbose,[0,1,2,3],0,caller='PARSEKEY (VERBOSE) ',default=0) then return

   info = { $
            airmass:  0.0, $
            jd:       0.0d0, $
            ra:      -1.0d0, $
            dec:   -100.0d0, $
            epoch:   2000.0, $
            date:     '', $
            ut:       '', $
            expdelta: 0.0, $
            exptime:  0.0, $
            imfile:   '', $
            filter:   '', $
            object:   '' $
            }

   stat = SIZE( keylist )
   n = stat[ 2 ]

   FOR j=0, n-1 DO BEGIN
      errflg = 0
      CASE keylist[0,j] OF
         'AIRMASS' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  info.airmass = SXPAR( header, keylist[2,j] )
                  if verbose then $
                     print,'parsekey: AIRMASS [',keylist[2,j],']=', $
                           strn(info.airmass,format='(f10.3)')
                end
               'V' : begin
                  info.airmass = FLOAT( keylist[2,j] )
                  if verbose then $
                     print,'parsekey: AIRMASS (value)', $
                           strn(info.airmass,format='(f10.3)')
                end
               ELSE : errflg = 1
            ENDCASE
         END

         'DATE' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  info.date = STRTRIM( SXPAR( header, keylist[2,j] ), 2 )
                  if verbose then $
                     print,'parsekey: DATE    [',keylist[2,j],']=',info.date
               end
               'V' : begin
                  info.date = keylist[2,j]
                  if verbose then $
                     print,'parsekey: DATE    (value)=',info.date
               end
               ELSE : errflg = 1
            ENDCASE
         END

         'DATETMPL' : BEGIN
            CASE keylist[1,j] OF
               'T' : datetmpl = keylist[2,j]
               ELSE : errflg = 1
            ENDCASE
         END

         'DEC' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  decs = STRTRIM( SXPAR( header, keylist[2,j] ), 2 )
                  if verbose then $
                     print,'parsekey: DEC     [',keylist[2,j],']=',decs
               end
               'V' : begin
                  decs = keylist[2,j]
                  if verbose then $
                     print,'parsekey: DEC     (value)=',decs
               end
               ELSE : errflg = 1
            ENDCASE
            IF decs ne 'NONE' THEN info.dec = decparse(decs)
         END

         'EPOCH' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  epochs = STRTRIM( SXPAR( header, keylist[2,j] ), 2 )
                  if verbose then $
                     print,'parsekey: EPOCH   [',keylist[2,j],']=',epochs
               end
               'V' : begin
                  epochs = keylist[2,j]
                  if verbose then $
                     print,'parsekey: EPOCH   (value)=',epochs
               end
               ELSE : errflg = 1
            ENDCASE
            IF epochs ne '' THEN info.epoch = float(epochs) else info.epoch=0.0
         END

         'EXPDELTA' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  info.expdelta = SXPAR( header, keylist[2,j] )
                  if verbose then $
                     print,'parsekey: EXPDELTA[',keylist[2,j],']=', $
                           strn(info.expdelta,format='(f10.3)')
               end
               'V' : begin
                  info.expdelta = FLOAT( keylist[2,j] )
                  if verbose then $
                     print,'parsekey: EXPDELTA(value)', $
                           strn(info.expdelta,format='(f10.3)')
               end
               ELSE : errflg = 1
            ENDCASE
         END

         'EXPTIME' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  info.exptime = SXPAR( header, keylist[2,j] )
                  if verbose then $
                     print,'parsekey: EXPTIME [',keylist[2,j],']=', $
                           strn(info.exptime,format='(f10.1)')
               end
               'V' : begin
                  info.exptime = FLOAT( keylist[2,j] )
                  if verbose then $
                     print,'parsekey: EXPTIME (value)', $
                           strn(info.exptime,format='(f10.1)')
               end
               ELSE : errflg = 1
            ENDCASE
         END

         'FILENAME' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  imfile  = STRTRIM( SXPAR( header, keylist[2,j] ), 2 )
                  fdecomp,imfile,disk,dir,name,qual,osfamily='unix'
                  info.imfile = name
                  info.imfile += '.'+qual
                  if verbose then $
                     print,'parsekey: FILENAME[',keylist[2,j],']=',info.imfile
               end
               'V' : begin
                  info.imfile  = keylist[2,j]
                  if verbose then $
                     print,'parsekey: FILENAME(value)=',info.imfile
               end
               ELSE : errflg = 1
            ENDCASE
         END

         'FILTER' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  answer = STRTRIM( SXPAR( header, keylist[2,j] ), 2 )
                  z=where(answer ne 'Open',count)
                  if count eq 0 then info.filter='Open' $
                  else if count eq 1 then info.filter=answer[z[0]] $
                  else info.filter = strjoin(answer[z],'+')
                  if verbose then $
                     print,'parsekey: FILTER  [',keylist[2,j],']=',info.filter
               end
               'V' : begin
                  info.filter = keylist[2,j]
                  if verbose then $
                     print,'parsekey: FILTER  (value)=',info.filter
               end
               ELSE : errflg = 1
            ENDCASE
            info.filter = nobname(info.filter)
         END

         'OBJECT' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  info.object  = STRTRIM( SXPAR( header, keylist[2,j] ), 2 )
                  if verbose then $
                     print,'parsekey: OBJECT  [',keylist[2,j],']=',info.object
               end
               'V' : begin
                  info.object  = keylist[2,j]
                  if verbose then $
                     print,'parsekey: OBJECT  (value)=',info.object
               end
               'W' : begin
                  object = sxpar( header, keylist[2,j] )
                  object = strtrim(strcompress(object),2)
                  words = strsplit(object,' ',/extract)
                  info.object  = words[0]
                  if verbose then $
                     print,'parsekey: OBJECT  {',keylist[2,j],'}=',info.object
                  end
               ELSE : errflg = 1
            ENDCASE
         END
         'RA' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  ras = STRTRIM( SXPAR( header, keylist[2,j] ), 2 )
                  if verbose then $
                     print,'parsekey: RA      [',keylist[2,j],']=',ras
               end
               'V' : begin
                  ras = keylist[2,j]
                  if verbose then $
                     print,'parsekey: RA      (value)=',ras
               end
               ELSE : errflg = 1
            ENDCASE
            IF ras ne 'NONE' THEN info.ra = raparse(ras)
         END

         'UT' : BEGIN
            CASE keylist[1,j] OF
               'K' : begin
                  info.ut = STRTRIM( SXPAR( header, keylist[2,j] ), 2 )
                  if verbose then $
                     print,'parsekey: UT      [',keylist[2,j],']=',info.ut
               end
               'V' : begin
                  info.ut = keylist[2,j]
                  if verbose then $
                     print,'parsekey: UT      (value)=',info.ut
               end
               ELSE : errflg = 1
            ENDCASE
         END

         ELSE : BEGIN
            print,'PARSEKEY: Warning. Unknown standard keylist name: ' + keylist[0,j]
         END
      ENDCASE
      IF errflg THEN print,'PARSEKEY: Warning. Unknown keylist flag: ' + keylist[1,j]
   ENDFOR

   errflg = 0
   ;Determine the date order and delimiter locations in the date template.
   ;Only the letters M, D, and Y are considered token characters. All others
   ;are treated as delimiters.
   x = BYTE( datetmpl )
   w = WHERE( (x NE 68) AND (x NE 77) AND (x NE 89), count )
   IF count EQ 2 THEN BEGIN
      ;May be good.
      n = STRLEN( datetmpl )
      toktmpl = [ STRMID( datetmpl, 0, w[0] ), $
                  STRMID( datetmpl, w[0]+1, w[1]-w[0]-1 ), $
                  STRMID( datetmpl, w[1]+1, n-w[1]-1 ) ]
   ENDIF ELSE BEGIN
      errflg = 1
   ENDELSE

   ;Parse the header date.
   tloc = strpos(info.date,'T')
   if tloc ge 0 then info.date = strmid(info.date,0,tloc)
   x = BYTE( info.date )
   w = WHERE( (x LT 48) OR (x GT 57), count )
   IF count EQ 2 THEN BEGIN
      ;May be good.
      n = STRLEN( info.date )
      tokhdr = [ STRMID( info.date, 0, w[0] ), $
                  STRMID( info.date, w[0]+1, w[1]-w[0]-1 ), $
                  STRMID( info.date, w[1]+1, n-w[1]-1 ) ]
   ENDIF ELSE BEGIN
      errflg = 1
   ENDELSE

   IF errflg EQ 0 THEN BEGIN
      ;Parse the date, according to the template.
      FOR j=0, 2 DO BEGIN
         CASE STRMID( toktmpl[j], 0, 1 ) OF
            'D' : BEGIN
               day = FLOAT( tokhdr[j] )
            END

            'M' : BEGIN
               month = FLOAT( tokhdr[j] )
            END

            'Y' : BEGIN
               year = FLOAT( tokhdr[j] )
            END

            ELSE : BEGIN
               errflg = 1
            END
         ENDCASE
      ENDFOR
   ENDIF

   ;HELP, day, month, year

   IF errflg GT 0 THEN BEGIN
      day   = 0.0
      month = 0.0
      year  = 0.0
      print,'PARSEKEY: Warning: Unable to parse date.  Template: '+ datetmpl + $
      '  Header Date: ' + info.date + '.'
   ENDIF

   IF (year GT 0.0) AND (year LT 100.0) THEN BEGIN
      IF year LT 50.0 THEN year=year+2000.0 ELSE year=year+1900.0
   ENDIF

   ;Parse the time fields.
   x = BYTE( info.ut )
   w = WHERE( x EQ 58, count )  ; 58 is a colon (:)
   ;w = WHERE( (x LT 48) OR (x GT 57), count ) ; old code, not quite right.

   IF count EQ 2 THEN BEGIN
      n  = STRLEN( info.ut )
      hh = FLOAT( STRMID( info.ut, 0, w[0] ) )
      mm = FLOAT( STRMID( info.ut, w[0]+1, w[1]-w[0]-1 ) )
      ss = FLOAT( STRMID( info.ut, w[1]+1, n-w[1]-1 ) )
   ENDIF ELSE BEGIN
      hh = 0.0
      mm = 0.0
      ss = 0.0
   ENDELSE

   ;HELP, hh, mm, ss

   ;Convert to decimal hours.
   hour = hh + ( mm / 60 ) + ( ss / 3600 )

   ;Compute the Julian Date.
   Jdcnv, year, month, day, hour, jd

   ; Store JD in image parameters adjusting to mid-time of exposure.
   info.jd = jd + double(info.exptime)/2.0d0/86400.0d0

   IF info.imfile EQ '' THEN info.imfile='(No name)'

end

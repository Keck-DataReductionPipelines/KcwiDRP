;+
; NAME:
;  fixsnap
; PURPOSE:
;  Munge a SNAPSHOT format ``FITS'' header and make it legal FITS
; DESCRIPTION:
;  This fixes a known epoch of SNAPSHOT headers to valid FITS.  The keyword
;    list and what to do with them is hardcoded here.  If you get no printed
;    messages to the screen, then the header was successfully processed.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  fixsnap,badhdr,goodhdr
; INPUTS:
;  badhdr - string array containing what should be a FITS header
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  goodhdr - Modified version of badhdr that fixes up the illegal stuff.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/07/02 - Written by Marc W. Buie, Lowell Observatory
;
pro fixsnap,in_badhdr,goodhdr

   if badpar(in_badhdr,7,1,CALLER='FIXSNAP: (badhdr) ') then return

   badhdr = in_badhdr

   if strmid(badhdr[0],0,8) ne 'SIMPLE  ' then $
      message,'Header does not start with SIMPLE'

   instrument = sxpar(badhdr,'INSTRUME')
   if instrument ne 'SNAPSHOT' then $
      message,'Instrument is not SNAPSHOT, header not processed'

   badhdr=repchar(badhdr,'"',"'")
   badhdr=repchar(badhdr,'= 36''','= 36"')
   badhdr=repchar(badhdr,'= 72''','= 72"')

   goodhdr = strarr(n_elements(badhdr))
   fixed   = intarr(n_elements(badhdr))

   blanks='                                                                                 '
   goodhdr[0]='SIMPLE  =                    T / SNAPSHOT header converted by FIXSNAP(IDL)'
   goodhdr[0]=strmid(goodhdr[0]+blanks,0,80)
   fixed[0]  = 1

   type1=['BITPIX','NAXIS','NAXIS1','NAXIS2','NAXIS3','NUMSUBF','SUBFNUM', $
          'BZERO','BSCALE','O_BSCALE','CRPIX1','CFINT1','CRPIX2','CFINT2', $
          'DETAXIS1','DETAXIS2']

   type2=['CTYPE','CCD','FILETYPE','FLTPVERS','VERSAUTH','BUNIT','CUNIT1', $
          'CUNIT2','CUNIT3','CLABL3','STRTTIME','PARMTIME','OBSERVAT', $
          'TELESCOP','INSTRUME','OBSERVER']

   type3=['VERSDATE','CLABL1','ORIGFILE','OPTICSL','OPTICSR','OBJECT', $
          'ORIGIN','ZONE-OBS','DATE-OBS','CDSCR1','CLABL2','CDSCR2', $
          'CDSCR3']

   type4=['SEQUENCE','SHUTTER','SETHEAT','NUMDOTS','NSHIFT','SFBIN', $
          'CDELT2','CRVAL2','CDELT3','CRVAL3','CFINT3','TEMP','SETTEMP', $
          'PFBIN','SEQ-PRGM','HEATER','FOCUSR','FOCUSL','FILTERR','FILTERL', $
          'CDELT1','CRVAL1','AUTOGUID']

   type5=['OBJ-RA','OBJ-DEC','LAT-TEL','LAT-TYPE','LONG-TEL','LOGIN1', $
          'COMP1','PTHNM1','PRGRM1','TIME1','DATE1','LICK']

   type6=['CRPIX3','ALT-TEL','TAPENUM']

   type7=['COMMENT','REMARK']

   blanks='                                                '

   for i=1,n_elements(badhdr)-1 do begin
      key = strtrim(strmid(badhdr[i],0,8),2)
      if key eq '' then begin
         goodhdr[i]=strmid(blanks,0,80)
         fixed[i]=1
      endif else if key eq 'END' then begin
         goodhdr[i]=strmid('END'+blanks,0,80)
         fixed[i]=1
      endif else begin

         ; Look for TYPE1 keywords, numeric values that are left justified.
         ;   To fix this type, look for the / character.  Look backward to
         ;   the first non-blank character. Save that character back to the
         ;   next blank, this is the value.  Output this value right justified
         ;   in column 30
         keyloc = where(key eq type1)
         if keyloc[0] ne -1 then begin
            slashpos = rstrpos(badhdr[i],'/')
            tag = strtrim(strmid(badhdr[i],slashpos,49),2)
            value = strtrim(strmid(badhdr[i],9,slashpos-9),2)
            goodhdr[i]=string(strmid(key+'        ',0,8),value,tag, $
                                 format='(a,"=",a21,1x,a)')
            fixed[i]=1
         endif

         ; Look for TYPE2 keywords.  These are actually string values that
         ;   have no ' delimiters.  These must be added and left in the
         ;   header left justified.
         keyloc = where(key eq type2)
         if keyloc[0] ne -1 then begin
            slashpos = rstrpos(badhdr[i],'/')
            tag = strtrim(strmid(badhdr[i],slashpos,49),2)
            value = strtrim(strmid(badhdr[i],9,slashpos-9),2)
            if strlen(value) lt 8 then value=strmid(value+blanks,0,8)
            goodhdr[i]=string(strmid(key+blanks,0,8), $
                              strmid("'"+value+"'"+blanks,0,20),tag, $
                                 format='(a,"=",a21,1x,a)')
            fixed[i]=1
         endif

         ; Look for TYPE3 keywords.  These are string values that are quote
         ;   delimited but may not be long enough and the tag needs to be
         ;   repositioned with possible truncation.
         keyloc = where(key eq type3)
         if keyloc[0] ne -1 then begin
            slashpos = rstrpos(badhdr[i],'/')
            tag = strtrim(strmid(badhdr[i],slashpos,49),2)
            value = strtrim(strmid(badhdr[i],9,slashpos-9),2)
            value = strtrim(strmid(value,1,strlen(value)-2),2)
            if strlen(value) lt 8 then value=strmid(value+blanks,0,8)
            if strlen(value) lt 18 then $
               goodhdr[i]=string(strmid(key+blanks,0,8), $
                                 strmid("'"+value+"'"+blanks,0,20),tag, $
                                    format='(a,"=",a21,1x,a)') $
            else $
               goodhdr[i]=string(strmid(key+blanks,0,8), $
                                 " '"+value+"'",strmid(tag,0,49-strlen(value)+21), $
                                    format='(a,"=",a,1x,a)')

            fixed[i]=1
         endif

         ; Look for TYPE4 keywords.  These are string values that are quote
         ;   delimited but are really numeric values.  The tag needs to be
         ;   repositioned with possible truncation.
         keyloc = where(key eq type4)
         if keyloc[0] ne -1 then begin
            slashpos = rstrpos(badhdr[i],'/')
            tag = strtrim(strmid(badhdr[i],slashpos,49),2)
            value = strtrim(strmid(badhdr[i],9,slashpos-9),2)
            value = strtrim(strmid(value,1,strlen(value)-2),2)
            goodhdr[i]=string(strmid(key+'        ',0,8),value,tag, $
                                 format='(a,"=",a21,1x,a)')

            fixed[i]=1
         endif

         ; Look for TYPE5 keywords.  These are values that might be strings
         ;   but have no value.  Add an empty string and repositioned the tag.
         keyloc = where(key eq type5)
         if keyloc[0] ne -1 then begin
            slashpos = rstrpos(badhdr[i],'/')
            tag = strtrim(strmid(badhdr[i],slashpos,49),2)
            value = '        '
            goodhdr[i]=string(strmid(key+'        ',0,8), $
                                 strmid("'"+value+"'"+blanks,0,20),tag, $
                                 format='(a,"=",a21,1x,a)')

            fixed[i]=1
         endif

         ; Look for TYPE6 keywords, might be numeric values but are left
         ;   empty.  Set to 0 and modify tag.
         keyloc = where(key eq type6)
         if keyloc[0] ne -1 then begin
            slashpos = rstrpos(badhdr[i],'/')
            tag = strtrim(strmid(badhdr[i],slashpos,49),2)
            value = '0'
            goodhdr[i]=string(strmid(key+'        ',0,8),value,tag, $
                                 format='(a,"=",a21,1x,a)')
            fixed[i]=1
         endif

         ; Look for TYPE7 keywords, no chnage.
         keyloc = where(key eq type7)
         if keyloc[0] ne -1 then begin
            goodhdr[i]=badhdr[i]
            fixed[i]=1
         endif

         if not fixed[i] then begin
            goodhdr[i]='HISTORY       bad header line, keyword '+key
            print,'bad ',strtrim(badhdr[i],2)
         endif else begin
            if strlen(goodhdr[i]) gt 80 then $
               print,'Long line!  key=',key,' length=',strlen(goodhdr[i]) $
            else $
               goodhdr[i]=strmid(goodhdr[i]+blanks,0,80)
         endelse
         
      endelse
   endfor

   if n_elements(badhdr) ne fix(total(fixed)) then $
      print,'Header length ',n_elements(badhdr),'  ', $
         n_elements(badhdr)-fix(total(fixed)),' not fixed'

end

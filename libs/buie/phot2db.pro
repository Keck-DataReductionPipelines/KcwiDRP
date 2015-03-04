;+
; NAME:
; phot2db
; PURPOSE:
; Process a file of external photometry and add to phot database.
; DESCRIPTION:
; Reads a file of external photometry data with format specified below. 
; It creates entries in two tables of the phot database, 
; reference and data. The file defines a REFID in its header section,
; which corresponds to an entry in phot.reference. Ordinarily this will
; be a new REFID- if not, the existing entry in phot.reference will be
; updated and existing entries in phot.data will be purged.
; CATEGORY:
; Photometry
; CALLING SEQUENCE:
; phot2db, fn
; INPUTS:
; fn   filename to be read and processed.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; ADMIN     - String, if specified, interpreted as an email address (or list)
;               to cc any email generated. If the contact information
;               obtained from the data file is unavailable or bad,
;               e-mail will addressed directly to ADMIN in the'to' field.
; DATABASE  - Name of database in which the reference and data tables reside,
;               by default 'phot'.
; DATATABLE - Name of data table in data base, by default 'data'.
; NOCONTACT - Flag, if set, the contact information specified in the data
;                file will not be used to send e-mail. The mail will be
;                sent to ADMIN only. If NOCONTACT is specified without ADMIN
;                it is equivalent to NOMAIL. Note that the contact field
;                is always mandatory in the data file.
; NOMAIL    - Flag, if set, the normal email sent in case of errors and
;               anomalies is suppressed. Errors will be printed to screen.
; PATH      - String, if specified, is a path that will be prepended to fn.
; REFTABLE  - Name of reference table in data base, by default 'reference'.
; ESQUELCH  - Maximum number of error lines in email messages, default 150.
;                Note that the preamble and data base update summary is always
;                printed regardless of ESQUELCH and that 'error lines' pertain
;                to header and data sections of the file, as well as a listing
;                of the header section of the file appended to the end. ESQUELCH
;                must be greater than 0.
;              
; TEST      --Flag, if set, no databases are modified, queries that would be
;                generated are listed to the screen.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; Updates tables data and reference in the phot database. It normally sends
; email to the specified contact person (and admin) if errors or anomalies are
; found in the file read. 
; RESTRICTIONS:
; The header section of the file must specify at least REFID, OBSCODE, and
; CONTACT.
; The data section of the file must have at least 1 valid line, otherwise,
; no changes are made to the database. 
; The REFID must not begin with "LO-" which is reserved for Lowell Observatory
; internal use. 
; PROCEDURE:
; The program is normally run as part of a cron job on a directory of incoming
; photometry data. The data base tables are silently updated, unless errors or
; anomalies are found. These are reported through email only. Once processed,
; an external script moves data files to an accumulating archive.
;
; The format of the photometry data files (as of 2006 Nov 21)::
;
; The data file is composed of two sections, the header information and
; the data.  The header can be seen as an unbroken list of lines that have
; the form of an identifier followed by text.  It is generally expected
; that one file will contain all the data that might result from a single
; night of observation.  However, this is not required.  It is allowed
; for a file to contain more than one night of data in which case the
; RunDate information will not be provided.  It is NOT allowed to have
; multiple files for a single night on a single telescope and instrument
; that share a RefID.  The basic rule will be that one file = one RefID.
; Also, RefID must be unique among all possible datasets.

; The specimen file below includes the recognized header keywords.  
; The order is not important- however the DATA line must follow
; all the header lines.
;
; REFID.....:
; RUNDATE...:
; INSTRUMENT:
; OBSCODE...:
; TELESCOPE.:
; OBSERVER..:
; MEASURER..:
; CONTACT...:
; COMMENTS..:
; DATA:
;   # JD RA DEC FILTER MAG ERR OBJNAME
;   # 2443741.49403 12:12:12.1 +35:35:35 V 13.456 0.023 SAO 160688
;   # 2443741.59403 x x R 14.567 0.031 SAO 120107
;   # 2443741.69403 14:12:34.2 +17:23:12 R 14.567 0.031
;   #
;
;
;
;
; The header contains keywords that are formatted as a name of up to
; 10 characters in length and padded to a length of 10 by periods.
; The padded name is to be followed by a colon and then a space before
; the data it contains.  The order is not important.  Some keywords
; can usefully appear more than once providing for multi-line entries.
; These fields are collected in the order seen in the header.  It is an
; error for a non-multi-line keyword to appear more than once.  Note that a
; multi-line comment need not be contiguous in the header.  Blank (empty)
; lines and any line that starts with '#' are ignored no matter where
; they appear in the file.  The end of the header is noted with 'DATA:'
; (not padded).  After this line is seen no further header information is
; allowed and the rest of the file is expected to contain the data.
;
; The header keywords contain the following information:
;
; REFID
;
; This is a string, currently limited to a maximum of 20 characters.
; This string must be unique across all datasets.  The string really
; should be uniquely tied to a single file.  When the database is loaded
; from the file, the REFID value is used to identify old values that may
; have been previously loaded.  Prior to loading the new file, these old
; values will be deleted in their entireity and the new values will be
; placed into the database.  No attempt is made to match up values in
; the file with previously loaded data.  The actual construction of the
; REFID is not defined here but a good working example is to construct a
; string that looks like: XXX-YYMMDD-IIII where XXX are the initials of
; the contact (presumed to be responsible for the data), YYMMDD is the
; rundate YY last two digits of the year, MM the month, and DD the day
; in UT.  In cases where the UT date changes during the night choose the
; dominant UT date for the night.  For example, at CTIO, the data often
; changes an hour or so into the night.  In this case the UT date at local
; midnight is appropriate.  Actually, the UT date of local midnight is
; usually appropriate though keep in mind that local midnight does not mean
; "when is it midnight according to a local clock".  Finally, IIII is a
; string that identifies the instrument (does not need to be exactly four
; characters long).
; It is currently required that the REFID be a string with three non-null
; substrings separated by hyphen (- not _). For example,
; XXX-YYMMDD-IIII is legal but YYMMDD or XXX-IIII are not (nor is XXX--AAAA).
; This restriction is needed to avoid propagating RefID's that could conflict 
; with internal usage at Lowell Observatory.
;
; RUNDATE
;
; This field is not required but is very helpful in many cases.  This should
; be a 6-digit string, YYMMDD with the same meaning discussed under REFID.
; If not supplied this will be set to NULL in the database.  If the data
; being loaded apply to a single night this field makes a lot of sense.
; Data that may come from a larger collective source (SDSS photometry database
; for instance) the RUNDATE will be meaningless.

; INSTRUMENT

; This is a short identifying string, maximum of 12 characters.  There should
; be a one-to-one correspondence between the instrument being used and this
; name.  It is most useful if this really is a useful identifying string that
; is unique and constant for a given instrument.

; OBSCODE

; This is the 3-character standard observatory code as defined by the
; Minor Planet center.

; TELESCOPE

; Descriptive name of the telescope used to collect the data.  Maximum length
; saved in the database is 80 characters.

; OBSERVER

; [Multiple line field] Name(s) of the observers who collected the data at
; the telescope.  This field is not required but is strongly encouraged for
; normal nightly datasets.  You can put multiple names on one line or use
; one line per observer.

; MEASURER

; [Multiple line field] Name(s) of the who processed the raw data and generated
; the numbers in this file.  Usually this will be one person but multiple names
; are allowed and encouraged if appropriate.

; CONTACT

; Email address of the primary contact regarding the data.  This should be the
; contact address of someone that can be contacted long after the data are
; reduced and posted in case of questions.  It is not appropriate to use a
; transient worker as the contact (eg., a summer student).  It is also assumed
; and normal to expect that the contact is mentioned by name as one of the
; measurers.

; COMMENTS

; [Multiple line field]  Any comments that should accompany the data would be
; placed in this field.  For that sake of formatting by any system that reads
; these commands and generates output, a blank comment line (keyword but no
; text) will be preserved and will indicate a paragraph break.  Line breaks
; within a "paragraph" may or may not be preserved.

; DATA:

; Once this keyword is seen the file processing switches to data mode
; reading.  The data format is not really a fixed format file.  Instead,
; the lines are read by word tokens and there must be a minimum of 6
; "words" on every line.  Words are separated by one or more blanks.
; Leading and trailing blanks are ignored.  The first word is the Julian
; Date and an F13.5 format is recommended though more digits can be provided
; if desired.  The second word is the J2000 Right Ascension of the object
; in either sexigesimal format (HH:MM:SS.s) or decimal hours.  The third
; word is the Declination (DD:MM:SS or decimal degrees).  The RA and Dec
; should NOT be supplied if this is not a measured quantity.  In other
; words, do not provide catalog positions or ephemeris calculations.
; If you do not have measured positions, then put a single 'x' character
; in place of each of the two fields. The fourth word is the name of the filter.
; Blanks are NOT allowed in a filter name.  This name should be a standard
; name to the extent possible.  For instance, Johnson filters would be V
; and SDSS would use something like r'.  The filter string may not execeed 10
; characters in length. The next two words are the standard
; magnitude and its error.  Use as many digits of precision as appropriate.
; The remainder of the line is used for the object name.  Embedded blanks are
; allowed in the object names though multiple blanks will get collapsed to a
; single blank.  It is allowed (and normal) to omit the object name when a
; position is provided.  If the position is not provided then the object name
; is required.  Again, blank lines and lines starting with # are skipped when
; reading.
;
;
; MODIFICATION HISTORY:
; 2006/11/21, Written by Peter L. Collins, Lowell Observatory
;-


pro phot2db, fn, ADMIN=admin,NOMAIL=nomail,PATH=path,DATABASE=database, $
                 REFTABLE=reftable,DATATABLE=datatable,ESQUELCH=esquelch, $
                 TEST=test,NOCONTACT=nocontact

   self= 'phot2db: '
   if badpar(fn,               7,0, caller=self + '(FN) ') then return
   if badpar(admin,        [0,7],0, caller=self + '(ADMIN) ') then return
   if badpar(nomail,   [0,1,2,3],0, caller=self + '(NOMAIL) ', $
             default=0) then return
   if badpar(esquelch, [0,1,2,3],0, caller=self + '(ESQUELCH) ', $
             default=150) then return
   if badpar(test,     [0,1,2,3],0, caller=self + '(TEST) ', $
             default=0) then return
   if badpar(nocontact,[0,1,2,3],0, caller=self + '(NOCONTACT) ', $
             default=0) then return
   if badpar(path,         [0,7],0, caller=self + '(PATH)', $
             default='') then return
   if badpar(database,     [0,7],0, caller=self + '(DATABASE)', $
             default='phot') then return
   if badpar(reftable,     [0,7],0, caller=self + '(REFTABLE)', $
             default='reference') then return
   if badpar(datatable,    [0,7],0, caller=self + '(DATATABLE)', $
             default='data') then return
   if esquelch lt 1 then begin
      print,  self + '(ESQUELCH) ' + ' must be > 0!'
      return
   endif

   hdrkeys=[ 'REFID.....:',   'RUNDATE...:',   'INSTRUMENT:', $
             'OBSCODE...:',   'TELESCOPE.:',   'OBSERVER..:', $
             'MEASURER..:',   'CONTACT...:',   'COMMENTS..:' $
           ] 
   ; these indices must be in hdrkeys order.
   i = 0
   hdr_refid = i++
   hdr_rundate = i++
   hdr_instrument = i++
   hdr_obscode = i++
   hdr_telescope = i++
   hdr_observer = i++
   hdr_measurer = i++
   hdr_contact = i++
   hdr_comments = i++

   ; extract plain names without the punctuation.
   hdrnames=hdrkeys
   for i=0, n_elements(hdrnames)-1 do begin
      names=strsplit(hdrnames[i],'.:',/EXTRACT)
      hdrnames[i]=names[0]
   endfor
      
   ; repository for hdr values grabbed from the phot file.
   hdrstrs = replicate('', n_elements(hdrkeys))

   ; lists of header field categories.
   multiline = [hdr_measurer, hdr_observer, hdr_comments] 
   mandatory = [hdr_refid, hdr_obscode, hdr_contact] 

   newline=string(10B)
   hdrstrs[multiline]=newline  ; initial nl  marks multiline fields- additional
                               ; entries are appended with another nl at end.
   datakey='DATA:'
   hdrkeysz=strlen(hdrkeys[0]); note- all hdr keys are the same size.
   datakeysz=strlen(datakey)
   refidsz=20
   obscodesz=3
   rundatesz=6
   contactsz=80
   instrsz=12

   ; Initialize strings for potential e-mail. E-mail is only sent in the
   ; event of errors or other situations (such as overwriting data for the
   ; RefID) of which the submitter should be apprised.
   ; emails will consist of the following string arrays, output in that order.
   ; It can be thought of as an html table with four rows and 1 column.
   ; The preamble is fixed length and updated in different places on the fly.
   ; The hdr and data msgs accumulate as errors appear.
   ; The hdr_file is just a copy of the file, including blank and comment
   ; lines, right up to the DATA keyword.
   ;
   ; the esquelch parameter is applied to the last 3 sections in order.
   preamble_msg = strarr(3) ; email preamble messages.
   hdr_emsgs = ['']     ; error messages for the header section
   data_emsgs = ['']    ; error messages for the data section
   hdr_file = ['']      ; list of file header lines.

   refdb_add = 0;
   refdb_del = 0
   datdb_add = 0;
   datdb_del = 0
   datlines = 0
   refid_exists = 0
   fnlines = -1         ; signal for no file.

   contact = ''

   if path ne '' then fn = addslash(path) + fn
   if not exists(fn) then begin
      hdr_emsgs=[hdr_emsgs, 'File name ' + fn + ' does not exist.']
      preamble_msg[0] = ' No Photometry data file available'
   endif else begin
      file_info = file_info(fn)
      fnlines = 0;
      photstr=''
      openr, lun,fn, /GET_LUN
      preamble_msg[0] =  'Data Entry from ' +  fn  ; add RefID later..
      preamble_msg[1] =   ' dated ' + systime(0,file_info.mtime) 

      ; build array of header fields and accumulate hdr_emsgs- all hdr_emsgs
      ; in this loop are ultimately fatal.
      while not eof(lun) do begin
         fnlines++
         readf,lun,photstr
         keystr=strmid(photstr,0, hdrkeysz)

         hki = where(keystr eq hdrkeys)
         if hki[0] ge 0 then begin  ; valid header key
            hki = hki[0]
            if strmid(photstr, hdrkeysz,1) ne ' ' then $ ; need a blank after :
               hdr_emsgs = [ hdr_emsgs, ' header line ill formed, line ' + $
                             strn(fnlines) ]
            ; insert string if single line or append with nl separator if 
            ; multiline- always trim field.
            r = strmid(hdrstrs[hki],0,1)
            if r ne newline then begin  
               if r ne '' then $  ; single line key
                  hdr_emsgs = [ hdr_emsgs, 'header key ' + hdrnames[hki] + $
                           ' defined more than once' + ' line '+strn(fnlines)] $
               else hdrstrs[hki] = strtrim(strmid(photstr,hdrkeysz),2) 
            endif else begin  ; multi line key
                hdrstrs[hki] += (strtrim(strmid(photstr,hdrkeysz),2) + newline)
            endelse
         endif else begin
            if strmid(photstr,0,5) eq datakey then break
            ; comment or blank line?
            if strmid(photstr,0,1) ne '#' and $
               strlen(strtrim(photstr)) ne 0 then  $
            hdr_emsgs = [ hdr_emsgs, 'header key ' + keystr +  ' unknown']
         endelse
         hdr_file= [hdr_file, photstr] ; save raw header line for email.
      endwhile

      ; validate header
      refid = hdrstrs[mandatory[0]]
      preamble_msg[0] += ' (RefID ' + refid + ')'
      ; make sure mandatory fields populated.
      empty = where(hdrstrs[mandatory] eq '' or hdrstrs[mandatory] eq newline)
      if empty[0] ne -1 then $
          hdr_emsgs = [ hdr_emsgs, 'required header field ' +  $
                            hdrnames[mandatory[empty]] + ' missing' ]

      ; make sure obscode three characters long.
      l = strlen(hdrstrs[hdr_obscode])
      if l gt 0 and l ne obscodesz then $
         hdr_emsgs=[hdr_emsgs, ' obscode must be exactly ' + strn(obscodesz) + $
                               ' characters long' ]
      ; make sure RefID no more than 20 characters long.
      z = where (hdrnames eq 'REFID' )
      l = strlen(hdrstrs[hdr_refid])
      if l gt refidsz then $
         hdr_emsgs=[hdr_emsgs,'RefID must be no more than ' + strn(refidsz) + $
                              ' characters long' ]
      ; make sure there are at least 3 hyphen delimited substrings in RefID.
      ; (to avoid conflict with internal usage.)
      ;
      subs = strsplit(hdrstrs[hdr_refid],'-')
      if hdrstrs[hdr_refid] ne '' and n_elements(subs) lt 3 then $
         hdr_emsgs=[hdr_emsgs, $
             'RefID must contain 3 or more substrings separated by hyphen'  + $
              ' e.g., XXXX-YYMMDD-IIII' ]
      ; make sure Instrument no more than 12 characters long.
      l = strlen(hdrstrs[hdr_instrument])
      if l gt instrsz then $
         hdr_emsgs=[hdr_emsgs,'Instrument must be no more than ' + $
                    strn(instrsz) + ' characters long' ]
      ; make sure Contact no more than 80 characters long.
      l = strlen(hdrstrs[hdr_contact])
      if l gt contactsz then $
         hdr_emsgs=[hdr_emsgs,'Contact must be no more than ' + $
                    strn(contactsz) + ' characters long' ]
      ; make sure rundate is six characters and all digits.
      l = strlen(hdrstrs[hdr_rundate])
      if l gt 0 then begin
         if l ne rundatesz then $
         hdr_emsgs=[hdr_emsgs,'Rundate must be ' + strn(rundatesz) + $
                              ' digits long' ] $
         else if not stregex(hdrstrs[hdr_rundate], $
                              '[0-9][0-9][0-9][0-9][0-9][0-9]',/BOOLEAN) then $
               hdr_emsgs=[hdr_emsgs,'Rundate contains non numerics' ]
      endif

      ; check on contact and nocontact,admin keywords.
      contact = hdrstrs[hdr_contact]
      if (contact eq '' or nocontact) and n_elements(admin) eq 0 then nomail = 1
   endelse

   ;If there are no header errors we will  process data.
   if n_elements(hdr_emsgs) eq 1 then begin  
      dataname = ['jd', 'ra', 'decl', 'filter', 'mag', 'err', 'ObjName' ]
      ; these indices must be in dataname order.
      i = 0
      data_jd = i++
      data_ra = i++
      data_decl = i++
      data_filter = i++
      data_mag = i++
      data_err = i++
      data_ObjName = i++

      while not eof(lun) do begin
         fnlines++
         readf,lun,photstr
         if strmid(photstr,0,1) eq '#' or $
            strlen(strtrim(photstr)) eq 0 then continue ;comment or blank.
         datlines++

         datafield=strsplit(photstr, ' ',/EXTRACT) ;get all tokens.
         lndataerrs = 0   ; if errors found on this line it will not enter db.
         if n_elements(datafield) lt 6 then  begin
            data_emsgs = [ data_emsgs, 'missing fields in data' + ' line ' + $
                           strn(fnlines) ] 
            lndataerrs++
         endif else begin
            xcount=0 ; count of "x-d" out ra,dec fields
            for j=0,data_err do begin
               ; catch numeric conversion errors and append info to data_emsgs
               catch, badnum
               if badnum ne 0 then begin
               badnum:
                  catch, /CANCEL
                  data_emsgs=[data_emsgs, 'bad numeric value in '+dataname[j]+ $
                           ' field, (' + datafield[j] + ') line '+strn(fnlines)]
                  lndataerrs++
               endif else begin
                  on_ioerror, badnum  ; thus badnum handles two kinds of errors.
                  if j eq data_ra or j eq data_decl then begin
                     ; ra and dec fields need to be tested via the
                     ; raparse, decparse code and we must
                     ; also must allow for 'x' as a valid field value. The
                     ; x value will cause the ra,decl columns to be set
                     ; in the data table as  NULL. (The strings are
                     ; turned into null strings below.) Ra and dec
                     ; are passed as strings to dbphot.
                     if datafield[j] ne 'x' then begin
                        if j eq data_ra then $
                           q = raparse(datafield[j]) $
                        else q = decparse(datafield[j])
                     endif else xcount++
                  endif else begin 
                     ; if not ra/dec, or 'filter', then ordinary numeric 
                     ; conversion on float (or double for jd) is done to test 
                     ; the field provided. We wish to guarantee that dbphot 
                     ; throws no errors. The fields will be passed as numbers
                     ; to dbphot below.
                     if j ne data_filter then begin
                        if j eq data_jd then val=double(datafield[j]) $
                         else val = float(datafield[j])
                         ; valid_num is needed because of holes in idl parse.
                         if not valid_num(datafield[j]) then begin
                            lndataerrs++
                            data_emsgs=[data_emsgs, 'bad numeric value in ' + $
                                        dataname[j] + ' field, (' +  $
                                        datafield[j] + ') line ' +strn(fnlines)]
                         endif
                     endif
                  endelse
               catch, /CANCEL
               on_ioerror, NULL
               endelse
            endfor
            ; ra and dec must both, or neither, be x'd. Xcount one of [0,1,2] 
            ; 
            if xcount eq 1 then begin
               lndataerrs++
               data_emsgs=[data_emsgs, 'ra and dec inconsistently specified' + $
                           ' in line ' + strn(fnlines)]
            endif

            if (xcount eq 2 ) and ( n_elements(datafield) le 6) then begin
               lndataerrs++
               data_emsgs=[ data_emsgs, $
                      'Neither Object Name nor position was specified' + $
                        ' in line ' + strn(fnlines)]
            endif

            if lndataerrs eq 0 then begin
               ; We will add a row to the data table using dbphot- 
               ; set up variables.
               jd = double(datafield[data_jd]) ;  can;t use jdparse on this
               ra = datafield[data_ra]
               decl = datafield[data_decl]
               if ra eq 'x' then begin
                  ra = ''
                  decl = ''
               endif
               filter =  datafield[data_filter]
               mag =  double(datafield[data_mag])
               err = double(datafield[data_err])
               if n_elements(datafield) gt 6 then $ ; set ObjName to tokens
                  objname = strjoin(datafield[6:*],' ') $
               else $
                  objname =  ''
               ; validate string lengths here.
               if strlen(objname) gt 20 then begin
                  lndataerrs++
                  data_emsgs=[ data_emsgs, $
                      'invalid string length for object name ' + objname + $
                       ' in line ' + strn(fnlines) ] 
               endif
               if  strlen(filter) gt 10 then begin
                  lndataerrs++
                  data_emsgs=[ data_emsgs, $
                      'invalid string length for filter ' + filter + $
                      ' in line ' + strn(fnlines)] 
               endif
               if lndataerrs eq 0 then begin
                  ; enter a row in data table- but check on the reference table 
                  ; entry if this is the first.
                  if datdb_add eq 0 then begin
                     rfi=''  ; first try tp locate RefID in reference table.
                     if not test then openmysql, dblun,database,nodb 
                     if nodb ne 0 then begin
                        print,'No database named ',database,' can be accessed!'
                        return
                     endif
   
                     query=' select RefID from ' + reftable + $
                           ' where RefID = ' + quote(refid) + ';' ;
                     if test then print, 'find refid:: ', query $
                     else mysqlquery, dblun, query, rfi, FORMAT="(A)"
                     if rfi ne '' or test  then begin
                        ; the RefID already existed so we need to remove it.
                        refid_exists = 1
                        hdr_emsgs=[hdr_emsgs,'RefID ' + refid + $
                                             ' already existed, ' + $ 
                                             'previous data were overwritten' ]
                        ; remove RefID (1 row) from reference table.
                        cmd = 'delete from ' + reftable + ' where RefID = ' + $
                              quote(refid) + ';'
                        if test then print, 'delete refid:: ', cmd $
                        else mysqlcmd, dblun,cmd,resp, nlines
                        ; the previous data entries need to be removed as well,
                        ; which dbphot handles when the first new data row is 
                        ; populated.
                     endif
                     ; Add new RefID to reference table.
                     ; find the header fields that were populated.
                     zi = where( hdrstrs ne '' and hdrstrs ne newline)
                     fi = hdrnames[zi] + ','
                     mzi = where( strmid(hdrstrs[zi],0,1) eq newline) 
                     if mzi[0] ne -1 then begin
                        mzi = zi[mzi] ; all populated multilines.
                        hdrstrs[mzi]= strmid(hdrstrs[mzi],1); remove leading nl.
                        ; remove the final newline as well
                        lzi = strlen(hdrstrs[mzi])
                        for j=0, n_elements(mzi)-1 do $
                           hdrstrs[mzi[j]]= $
                           strmid(hdrstrs[mzi[j]],0,strlen(hdrstrs[mzi[j]])-1)
                     endif
                     vi =''
                     for j=0, n_elements(zi)-1 do vi+=quote(hdrstrs[zi[j]])+ ','
                     ; get posted time
                     vi=[vi,'FROM_UNIXTIME( ' +strn(long(systime(/SECONDS))) + $
                              ' )']
                     ; create new refid row in reference table
                     cmd=['insert into ' + reftable, ' ( ', fi,  ' Posted ) ', $
                            ' Values ( ', vi, ' )',';' ]
                     if test then print, 'add refid:: ', cmd $
                     else mysqlcmd, dblun,cmd,resp, nlines
                     if not test then free_lun, dblun
               endif

                  first = (datdb_add eq 0)
                  ; set up so all previous obs (if any) are cleansed on the
                  ; first row added. This way the rows can be added one
                  ; at a time.
                  dbphot,refid,objname,jd,filter, mag,err, TEST=test, $
                                       RA=ra, DECL=decl, DATABASE=database, $
                                       TABLE=datatable,/SILENT, NREMOV=del, $
                                       CLEANBYREFID=(first),NOCLEAN=(not first)
               
                  if first then datdb_del = del ; for email
                  datdb_add++
               endif
            endif
         endelse
      endwhile

   endif
   if fnlines ne -1 then free_lun,lun

   ;email is sent for actual errors or duplicate RefID (which is a hdr_emsg).
   acterrs = n_elements(hdr_emsgs) + n_elements(data_emsgs) - 2
   if refid_exists then acterrs--
   if (acterrs gt 0 or refid_exists) then begin
      if datdb_add gt 0 then begin
         refdb_add = 1
         if refid_exists then begin 
            refdb_del = 1
            preamble_msg[2]='RefID ' + refid +  ' was in the data base and ' + $
                             'its data were deleted.'
            if acterrs gt 0 then preamble_msg= [ preamble_msg, $
                                'Errors were found in the data as well.' ]
         endif 
      endif
      if not refid_exists then preamble_msg[2]='there were errors in the data.'
      if n_elements(data_emsgs) gt 1 then $
          hdr_emsgs = [ hdr_emsgs,  '', '', data_emsgs[1:*] ]
      if n_elements(hdr_emsgs) gt esquelch then hdr_emsgs =  $
         [ hdr_emsgs[0:esquelch-1], 'Further Output Truncated ************'] $
      else begin
         hdr_emsgs = [ hdr_emsgs, '___________________', hdr_file, $
                        '______________________' ]
         if n_elements(hdr_emsgs) gt esquelch then hdr_emsgs =  $
           [hdr_emsgs[0:esquelch-1], 'Further Output Truncated ************']
      endelse
      
      mail= [preamble_msg[0:1], '','We are sending you this mail because', $
             preamble_msg[2:*], '', '', $
             strn(refdb_add) + ' row added to ' +database + '.' + reftable, $
             strn(refid_exists) + ' previous rows deleted', ' ', $
             strn(datdb_add) +  $
             ' rows out of ' + strn(datlines) +  ' added to ' + $
             database + '.' + datatable, $
             strn(datdb_del) + ' previous rows for this RefID ' + $
             'deleted']
             
      mail = [ mail, ' ' , ' Errors And Anomalies', ' ' ,hdr_emsgs ]
      if not nomail then begin
         if contact eq '' or nocontact then $
            contact = admin
         if admin ne '' and admin ne contact then $
            mailmsg, contact, 'Photometry Data Submission', mail,CCADDR=admin $
        else mailmsg, contact, 'Photometry Data Submission', mail
      endif else begin
         print, ' No Mail Output: '
         print, mail, format="(A)"
      endelse
   endif
end

;+
; NAME:
;  ccdcal
; PURPOSE:
;  Batch mode image calibration program (apply bias, dark and flats)
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  ccdcal,root,num1,num2
;
; INPUTS:
;  root - Root of image file names.  The file names are assumed to be in
;            the form of root.NNN where NNN is a 3 digit number.
;         This argument can also be a string array of fully formed file
;            names.  If the file names include a path, then the PATH keyword
;            should normally not be used since PATH will be prepended to
;            the name provided in this argument.  If a string or string array
;            is given for explicit names then num1 and num2 must not be
;            provided.
;  num1 - first image file to process, this argument can also be an explicit
;            vector list of number to process.  In this case, num2 is ignored.
;  num2 - last image file to process
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  CALIBFILE       : Calibration startup file.  Default is CALIBPATH/files.cal
;
;  CALIBPATH       : Path for calibration files.  Default is PATH/calib
;
;  KEYLIST         : Name of a file containing a correspondence list. This list
;                    associates a set of standard names with the actual keyword
;                    names found in a FITS file header. If this keyword is
;                    omitted, a default list is used, as if a file with the
;                    following contents had been supplied:
;                     AIRMASS   K  AIRMASS
;                     DATE      K  DATE-OBS
;                     DATETMPL  T  DD-MM-YYYY
;                     EXPDELTA  V  0.0
;                     EXPTIME   K  EXPTIME
;                     FILTER    K  FILTERS
;                     FILENAME  K  CCDFNAME
;                     OBJECT    K  OBJECT
;                     UT        K  UT 
;                    The middle column is a flag. It may be K, for Keyword,
;                    T, for Template, or V, for Value. If it is V, the contents
;                    of the third field on that line should make sense for the
;                    name in the first field.
;
;  KEYREQUIRED     : Optional keyword/value pair (string array).  If supplied,
;                      then only those images having the keyword/value pair
;                      in their headers will be processed.
;
;  NOFLOAT         : Flag, if set will force data to be rescaled back into
;                      a 2-byte integer before writing.  Note that this can
;                      cause loss of information for images that have little
;                      to no sky signal.
;
;  PATH            : Optional path for raw image and calibration directory.
;                    If not specified, the current directory is used.
;
;  PREFIX          : string to prepend to output files.  If PATH=OUTPATH the
;                    default is 'c_', otherwise, the default is no prefix.
;
;  OUTPATH         : Optional path for location to write final calibrated
;                    images.  Default is the current directory.
;
;  OVERWRITE - Flag, if set will force the program to overwrite any output
;                 images that already exit.  The default is to skip things
;                 that are already present.  This flag would normally be used
;                 only with the calibration prescription has changed.
;
;  SILENT - Flag, if set will suppress printed output (other than errors).
;
; OUTPUTS:
;
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
;  Written by Marc W. Buie, Lowell Observatory, 1999 Nov 11
;  2000/02/06, MWB, added .fits optional tag on file name
;  2001/11/09, MWB, added support for an input list of file numbers.
;  2004/05/06, MWB, now creates output directory if not present
;  2004/06/26, MWB, added new string array input for root input argument.
;                     Also added KEYREQUIRED and NOFLOAT keywords.
;  2007/01/03, MWB, fixed problem with BSCALE and floating point images.
;  2010/11/17, MWB, added SILENT keyword
;-
pro ccdcal,root,num1,num2, $
           path=path,outpath=outpath,keylist=in_keylist,prefix=prefix, $
           calibfile=calibfile,calibpath=calibpath,SILENT=silent, $
           KEYREQUIRED=keyrequired,NOFLOAT=nofloat,OVERWRITE=overwrite
   
   self='CCDCAL: '
   if badpar(root,7,[0,1], $
         CALLER=self+'(root) ',npts=numroot) then return
   if badpar(num1,[0,1,2,3],[0,1],CALLER=self+'(num1) ', $
         npts=numnum1,type=num1type) then return
   if badpar(num2,[0,1,2,3],0,CALLER=self+'(num2) ', $
         npts=numnum2,type=num2type) then return

   ; input validation
   if num1type ne 0 and numroot gt 1 then begin
      print,self+'root must be scalar when providing num1'
      return
   endif
   if num2type ne 0 and numroot gt 1 then begin
      print,self+'root must be scalar when providing num2'
      return
   endif

   if badpar(path,[0,7],0, $
         CALLER=self+'(PATH) ',DEFAULT='') then return
   if path ne '' then path=addslash(path)

   if badpar(outpath,[0,7],0, $
         CALLER=self+'(OUTPATH) ',DEFAULT='') then return
   if outpath ne '' then outpath=addslash(outpath)

   if path eq outpath then defprefix='c_' else defprefix=''
   if badpar(prefix,[0,7],0, $
         CALLER=self+'(PREFIX) ',DEFAULT=defprefix) then return

   if badpar(nofloat,[0,1,2,3],0, $
         CALLER=self+'(NOFLOAT) ',DEFAULT=0) then return
   if badpar(keyrequired,[0,7],1, $
         CALLER=self+'(KEYREQUIRED) ',DEFAULT=['','']) then return
   if badpar(overwrite,[0,1,2,3],0, $
         CALLER=self+'(OVERWRITE) ',DEFAULT=0) then return
   if badpar(silent,[0,1,2,3],0, $
         CALLER=self+'(SILENT) ',DEFAULT=0) then return

   if badpar(calibfile,[0,7],0, $
         CALLER=self+'(CALIBFILE) ',DEFAULT='files.cal') then return
   if badpar(calibpath,[0,7],0, $
         CALLER=self+'(CALIBPATH) ',DEFAULT=path+'calib') then return
   calibpath=addslash(calibpath)

   if not exists(outpath) then begin
      if not silent then print,'Make output directory'
      file_mkdir,outpath
      if not exists(outpath) then begin
         print,'Unable to make output directory, quitting.'
         return
      endif
   endif

   ; Get header correspondence list.
   if keyword_set( in_keylist ) then $
      loadkeys,in_keylist,hdrlist,foundit=foundit $
   else $
      loadkeys,'[[DEFAULT]]',hdrlis,foundit=foundit

   if not foundit then begin
      print,self,'Key file not found, unable to continue'
      return
   endif

   ldcalib,calibfile,calib,valid,CALIBPATH=calibpath,silent=silent
   if not valid then begin
      print,self+' Error!  Calibration information not found in'
      print,'    ',calibpath+calibfile
      return
   endif

   if num1type eq 0 then begin
      files = root
      nidx = n_elements(root)
   endif else begin
      if n_elements(num1) gt 1 or num2type eq 0 then begin
         nidx = n_elements(num1)
         idx  = num1
         numtoflist,idx,outlist
         files = root+outlist
      endif else begin
         nidx = num2-num1+1
         idx=indgen(nidx)+num1
         numtoflist,idx,outlist
         files = root+outlist
      endelse
   endelse

   for i=0,nidx-1 do begin
      infile = files[i]
      outfile = prefix+infile
      if exists(path+infile+'.fits') then ft='.fits' else ft=''
      if exists(path+infile+ft) then begin
         raw=0
         image=0
         header = headfits(path+infile+ft)
         if keyrequired[0] ne '' then begin
            keycheck = strtrim(sxpar(header,keyrequired[0]),2)
            processit = keyrequired[1] eq keycheck
         endif else begin
            processit = 1
         endelse
         if processit then begin
            if not overwrite and exists(outpath+outfile+ft) then begin
               if not silent then $
                  print,'Skip  ',outpath+outfile+ft,' already exists.'
               continue
            endif
            if not silent then print,'Read  ',path+infile+ft
            raw = readfits(path+infile+ft,header,/silent)
            parsekey,header,hdrlist,hdrinfo
            ccdproc,raw,hdrinfo,calib,image
            if not silent then print,'Write ',outpath+outfile+ft
            if nofloat then begin
               image = fix(temporary(image < 65535.0) - 32767.5)
               sz=size(image,/dimen)
               sxaddpar,header,'NAXIS1',sz[0]
               sxaddpar,header,'NAXIS2',sz[1]
               sxaddpar,header,'BSCALE',1.0,AFTER='NAXIS2'
               sxaddpar,header,'BZERO',32768.0,AFTER='BSCALE'
               sxaddpar,header,'BITPIX',16
            endif else begin
               sz=size(image,/dimen)
               sxaddpar,header,'NAXIS1',sz[0]
               sxaddpar,header,'NAXIS2',sz[1]
               sxaddpar,header,'BSCALE',1.0
               sxaddpar,header,'BZERO',0.0
            endelse
            writefits,outpath+outfile+ft,image,header
         endif else begin
            if not silent then print,'Skip  ',keyrequired[0],' = ',keycheck
         endelse
      endif else begin
         if not silent then print,path+infile+ft,' not found.'
      endelse
   endfor

end
